var printf = require('./c/printf');

// The maximum number of temporary objects that can be made visible to the GC
// at one time.
var WREN_MAX_TEMP_ROOTS = 5;

var Code = require('./opcodes.js');

// The behavior of realloc() when the size is 0 is implementation defined. It
// may return a non-NULL pointer which must not be dereferenced but nevertheless
// should be freed. To prevent that, we avoid calling realloc() with a zero
// size.
function defaultReallocate(ptr, newSize) {
  if (newSize === 0) {
    free(ptr);
    return null;
  }

  return realloc(ptr, newSize);
}

function wrenInitConfiguration(config) {
  config.reallocateFn = defaultReallocate;
  config.loadModuleFn = NULL;
  config.bindForeignMethodFn = NULL;
  config.bindForeignClassFn = NULL;
  config.writeFn = NULL;
  config.initialHeapSize = 1024 * 1024 * 10;
  config.minHeapSize = 1024 * 1024;
  config.heapGrowthPercent = 50;
}

function wrenNewVM(config) {
  var reallocate = defaultReallocate;
  if (config !== null) reallocate = config.reallocateFn;

  var vm = {};

  // Copy the configuration if given one.
  if (config !== null) {
    vm.config = config;
  } else {
    wrenInitConfiguration(vm.config);
  }

  // TODO: Should we allocate and free this during a GC?
  vm.grayCount = 0;
  // TODO: Tune this.
  vm.grayCapacity = 4;
  vm.gray = {};
  vm.nextGC = vm.config.initialHeapSize;

  wrenSymbolTableInit(vm.methodNames);

  vm.modules = wrenNewMap(vm);

  wrenInitializeCore(vm);

  return vm;
}

function wrenFreeVM(vm) {
  assert(vm.methodNames.count > 0, "VM appears to have already been freed.");

  // Free all of the GC objects.
  var obj = vm.first;
  while (obj !== null) {
    var next = obj.next;
    wrenFreeObj(vm, obj);
    obj = next;
  }

  // Free up the GC gray set.
  vm.gray = vm.config.reallocateFn(vm.gray, 0);

  // Tell the user if they didn't free any handles. We don't want to just free
  // them here because the host app may still have pointers to them that they
  // may try to use. Better to tell them about the bug early.
  assert(vm.valueHandles === null, "All values have not been released.");

  wrenSymbolTableClear(vm, vm.methodNames);

  DEALLOCATE(vm, vm);
}

function wrenCollectGarbage(vm) {
  var before, startTime;
  if (WREN_DEBUG_TRACE_MEMORY || WREN_DEBUG_TRACE_GC) {
    console.log("-- gc --\n");

    before = vm.bytesAllocated;
    startTime = clock() / CLOCKS_PER_SEC;
  }

  // Mark all reachable objects.

  // Reset this. As we mark objects, their size will be counted again so that
  // we can track how much memory is in use without needing to know the size
  // of each *freed* object.
  //
  // This is important because when freeing an unmarked object, we don't always
  // know how much memory it is using. For example, when freeing an instance,
  // we need to know its class to know how big it is, but its class may have
  // already been freed.
  vm.bytesAllocated = 0;

  wrenGrayObj(vm, vm.modules);

  // Temporary roots.
  for (var i = 0; i < vm.numTempRoots; i++) {
    wrenGrayObj(vm, vm.tempRoots[i]);
  }

  // The current fiber.
  wrenGrayObj(vm, vm.fiber);

  // The value handles.
  for (var value = vm.valueHandles;
       value !== null;
       value = value.next) {
    wrenGrayValue(vm, value.value);
  }

  // Any object the compiler is using (if there is one).
  if (vm.compiler !== null) {
    wrenMarkCompiler(vm, vm.compiler);
  }

  // Now that we have grayed the roots, do a depth-first search over all of the
  // reachable objects.
  wrenBlackenObjects(vm);

  // Collect the white objects.
  var obj = vm.first;
  while (obj !== null) {
    if (!(obj.isDark)) {
      // This object wasn't reached, so remove it from the list and free it.
      var unreached = obj;
      obj = unreached.next;
      wrenFreeObj(vm, unreached);
    } else {
      // This object was reached, so unmark it (for the next GC) and move on to
      // the next.
      obj.isDark = false;
      obj = obj.next;
    }
  }

  // +100 here because the configuration gives us the *additional* size of
  // the heap relative to the in-use memory, while heapScalePercent is the
  // *total* size of the heap relative to in-use.
  vm.nextGC = vm.bytesAllocated * (100 + vm.config.heapGrowthPercent) / 100;
  if (vm.nextGC < vm.config.minHeapSize) vm.nextGC = vm.config.minHeapSize;

  if (WREN_DEBUG_TRACE_MEMORY || WREN_DEBUG_TRACE_GC) {
    var elapsed = (clock() / CLOCKS_PER_SEC) - startTime;
    // Explicit cast because size_t has different sizes on 32-bit and 64-bit and
    // we need a consistent type for the format string.
    printf("GC %x before, %x after (%x collected), next at %x. Took %xs.\n",
           before,
           vm.bytesAllocated,
           (before - vm.bytesAllocated),
           vm.nextGC,
           elapsed);
  }
}

function wrenReallocate(vm, memory, oldSize, newSize) {
  if (WREN_DEBUG_TRACE_MEMORY) {
    // Explicit cast because size_t has different sizes on 32-bit and 64-bit and
    // we need a consistent type for the format string.
    printf("reallocate %x %x . %x\n",
           memory, oldSize, newSize);
  }

  // If new bytes are being allocated, add them to the total count. If objects
  // are being completely deallocated, we don't track that (since we don't
  // track the original size). Instead, that will be handled while marking
  // during the next GC.
  vm.bytesAllocated += newSize - oldSize;

  if (WREN_DEBUG_GC_STRESS) {
    // Since collecting calls this function to free things, make sure we don't
    // recurse.
    if (newSize > 0) wrenCollectGarbage(vm);
  } else {
    if (newSize > 0 && vm.bytesAllocated > vm.nextGC) wrenCollectGarbage(vm);
  }

  return vm.config.reallocateFn(memory, newSize);
}

// Captures the local variable [local] into an [Upvalue]. If that local is
// already in an upvalue, the existing one will be used. (This is important to
// ensure that multiple closures closing over the same variable actually see
// the same variable.) Otherwise, it will create a new open upvalue and add it
// the fiber's list of upvalues.
function captureUpvalue(vm, fiber, local) {
  // If there are no open upvalues at all, we must need a new one.
  if (fiber/openUpvalues === null) {
    fiber.openUpvalues = wrenNewUpvalue(vm, local);
    return fiber.openUpvalues;
  }

  var prevUpvalue = null;
  var upvalue = fiber.openUpvalues;

  // Walk towards the bottom of the stack until we find a previously existing
  // upvalue or pass where it should be.
  while (upvalue !== null && upvalue.value > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue.next;
  }

  // Found an existing upvalue for this local.
  if (upvalue !== null && upvalue.value === local) {
    return upvalue;
  }

  // We've walked past this local on the stack, so there must not be an
  // upvalue for it already. Make a new one and link it in in the right
  // place to keep the list sorted.
  var createdUpvalue = wrenNewUpvalue(vm, local);
  if (prevUpvalue === null) {
    // The new one is the first one in the list.
    fiber.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue.next = createdUpvalue;
  }

  createdUpvalue.next = upvalue;
  return createdUpvalue;
}

// Closes any open upvates that have been created for stack slots at [last] and
// above.
function closeUpvalues(fiber, last) {
  while (fiber.openUpvalues !== null &&
         fiber.openUpvalues.value >= last) {
    var upvalue = fiber.openUpvalues;

    // Move the value into the upvalue itself and point the upvalue to it.
    upvalue.closed = upvalue.value;
    upvalue.value = upvalue.closed;

    // Remove it from the open upvalue list.
    fiber.openUpvalues = upvalue.next;
  }
}

// Looks up a foreign method in [moduleName] on [className] with [signature].
//
// This will try the host's foreign method binder first. If that fails, it
// falls back to handling the built-in modules.
function findForeignMethod(vm, moduleName, className, isStatic, signature) {
  if (vm.config.bindForeignMethodFn === null) {
    return NULL;
  }

  return vm.config.bindForeignMethodFn(vm, moduleName, className, isStatic, signature);
}

// Defines [methodValue] as a method on [classObj].
//
// Handles both foreign methods where [methodValue] is a string containing the
// method's signature and Wren methods where [methodValue] is a function.
//
// Aborts the current fiber if the method is a foreign method that could not be
// found.
function bindMethod(vm, methodType, symbol, module, classObj, methodValue) {
  var className = classObj.name.value;
  if (methodType === Code.METHOD_STATIC) classObj = classObj.obj.classObj;

  var method = {};
  if (IS_STRING(methodValue)) {
    var name = AS_CSTRING(methodValue);
    method.type = METHOD_FOREIGN;
    method.fn.foreign = findForeignMethod(vm, module.name.value,
                                          className,
                                          methodType === Code.METHOD_STATIC,
                                          name);

    if (method.fn.foreign === null) {
      vm.fiber.error = wrenStringFormat(vm,
          "Could not find foreign method '@' for class $ in module '$'.",
          methodValue, classObj.name.value, module.name.value);
      return;
    }
  } else {
    var methodFn = IS_FN(methodValue) ? AS_FN(methodValue)
      : AS_CLOSURE(methodValue).fn;

    // Patch up the bytecode now that we know the superclass.
    wrenBindMethodCode(classObj, methodFn);

    method.type = METHOD_BLOCK;
    method.fn.obj = AS_OBJ(methodValue);
  }

  wrenBindMethod(vm, classObj, symbol, method);
}

function callForeign(vm, fiber, foreign, numArgs) {
  vm.apiStack = fiber.stackTop - numArgs;

  foreign(vm);

  // Discard the stack slots for the arguments and temporaries but leave one
  // for the result.
  fiber.stackTop = vm.apiStack + 1;
  vm.apiStack = null;
}

// Handles the current fiber having aborted because of an error. Switches to
// a new fiber if there is a fiber that will handle the error, otherwise, tells
// the VM to stop.
function runtimeError(vm) {
  assert(!IS_NULL(vm.fiber.error), "Should only call this after an error.");

  // Unhook the caller since we will never resume and return to it.
  var caller = vm.fiber.caller;
  vm.fiber.caller = null;

  // If the caller ran this fiber using "try", give it the error.
  if (vm.fiber.callerIsTrying) {
    // Make the caller's try method return the error message.
    caller.stackTop[-1] = vm.fiber.error;

    vm.fiber = caller;
    return;
  }

  // If we got here, nothing caught the error, so show the stack trace.
  wrenDebugPrintStackTrace(vm.fiber);
  vm.fiber = null;
}

// Aborts the current fiber with an appropriate method not found error for a
// method with [symbol] on [classObj].
function methodNotFound(vm, classObj, symbol) {
  vm.fiber.error = wrenStringFormat(vm, "@ does not implement '$'.",
      OBJ_VAL(classObj.name), vm.methodNames.data[symbol].buffer);
}

// Checks that [value], which must be a function or closure, does not require
// more parameters than are provided by [numArgs].
//
// If there are not enough arguments, aborts the current fiber and returns
// `false`.
function checkArity(vm, value, numArgs) {
  var fn = {};
  if (IS_CLOSURE(value)) {
    fn = AS_CLOSURE(value).fn;
  } else {
    assert(IS_FN(value), "Receiver must be a function or closure.");
    fn = AS_FN(value);
  }

  // We only care about missing arguments, not extras. The "- 1" is because
  // numArgs includes the receiver, the function itself, which we don't want to
  // count.
  if (numArgs - 1 >= fn.arity) return true;

  vm.fiber.error = CONST_STRING(vm, "Function expects more arguments.");
  return false;
}

// Pushes [function] onto [fiber]'s callstack and invokes it. Expects [numArgs]
// arguments (including the receiver) to be on the top of the stack already.
// [function] can be an `ObjFn` or `ObjClosure`.
function callFunction(vm, fiber, function_, numArgs) {
  // Grow the call frame array if needed.
  if (fiber.numFrames + 1 > fiber.frameCapacity) {
    var max = fiber.frameCapacity * 2;
    fiber.frames = wrenReallocate(vm, fiber.frames,
        sizeof(CallFrame) * fiber.frameCapacity,
        sizeof(CallFrame) * max);
    fiber.frameCapacity = max;
  }

  // Grow the stack if needed.
  var stackSize = (fiber.stackTop - fiber.stack);
  var needed = stackSize + wrenUnwrapClosure(function_).maxSlots;
  wrenEnsureStack(vm, fiber, needed);

  wrenAppendCallFrame(vm, fiber, function_, fiber.stackTop - numArgs);
}

// Looks up the previously loaded module with [name].
//
// Returns `NULL` if no module with that name has been loaded.
function getModule(vm, name) {
  var moduleValue = wrenMapGet(vm.modules, name);
  return !IS_UNDEFINED(moduleValue) ? AS_MODULE(moduleValue) : null;
}

function loadModule(vm, name, source) {
  var module = getModule(vm, name);

  // See if the module has already been loaded.
  if (module === null) {
    module = wrenNewModule(vm, AS_STRING(name));

    // Store it in the VM's module registry so we don't load the same module
    // multiple times.
    wrenMapSet(vm, vm.modules, name, OBJ_VAL(module));

    // Implicitly import the core module.
    var coreModule = getModule(vm, NULL_VAL);
    for (var i = 0; i < coreModule.variables.count; i++) {
      wrenDefineVariable(vm, module,
                         coreModule.variableNames.data[i].buffer,
                         coreModule.variableNames.data[i].length,
                         coreModule.variables.data[i]);
    }
  }

  var fn = wrenCompile(vm, module, source, true);
  if (fn === null)
  {
    // TODO: Should we still store the module even if it didn't compile?
    return null;
  }

  wrenPushRoot(vm, fn);

  var moduleFiber = wrenNewFiber(vm, fn);

  wrenPopRoot(vm); // fn.

  // Return the fiber that executes the module.
  return moduleFiber;
}

// Verifies that [superclassValue] is a valid object to inherit from. That
// means it must be a class and cannot be the class of any built-in type.
//
// Also validates that it doesn't result in a class with too many fields and
// the other limitations foreign classes have.
//
// If successful, returns `null`. Otherwise, returns a string for the runtime
// error message.
function validateSuperclass(vm, name, superclassValue, numFields) {
  // Make sure the superclass is a class.
  if (!IS_CLASS(superclassValue)) {
    return wrenStringFormat(vm,
        "Class '@' cannot inherit from a non-class object.",
        name);
  }

  // Make sure it doesn't inherit from a sealed built-in type. Primitive methods
  // on these classes assume the instance is one of the other Obj___ types and
  // will fail horribly if it's actually an ObjInstance.
  var superclass = AS_CLASS(superclassValue);
  if (superclass === vm.classClass ||
      superclass === vm.fiberClass ||
      superclass === vm.fnClass || // Includes OBJ_CLOSURE.
      superclass === vm.listClass ||
      superclass === vm.mapClass ||
      superclass === vm.rangeClass ||
      superclass === vm.stringClass) {
    return wrenStringFormat(vm,
        "Class '@' cannot inherit from built-in class '@'.",
        name, OBJ_VAL(superclass.name));
  }

  if (superclass.numFields === -1) {
    return wrenStringFormat(vm,
        "Class '@' cannot inherit from foreign class '@'.",
        name, OBJ_VAL(superclass.name));
  }

  if (numFields === -1 && superclass.numFields > 0) {
    return wrenStringFormat(vm,
        "Foreign class '@' may not inherit from a class with fields.",
        name);
  }

  if (superclass.numFields + numFields > MAX_FIELDS) {
    return wrenStringFormat(vm,
        "Class '@' may not have more than 255 fields, including inherited " +
        "ones.", name);
  }

  return NULL_VAL;
}

function bindForeignClass(vm, classObj, module) {
  // TODO: Make this a runtime error?
  assert(vm.config.bindForeignClassFn !== null,
      "Cannot declare foreign classes without a bindForeignClassFn.");

  var methods = vm.config.bindForeignClassFn(
      vm, module.name.value, classObj.name.value);

  var method = {};
  method.type = METHOD_FOREIGN;

  // Add the symbol even if there is no finalizer so we can ensure that the
  // symbol itself is always in the symbol table.
  int symbol = wrenSymbolTableEnsure(vm, vm.methodNames, "<allocate>", 10);
  if (methods.allocate !== null)
  {
    method.fn.foreign = methods.allocate;
    wrenBindMethod(vm, classObj, symbol, method);
  }

  // Add the symbol even if there is no finalizer so we can ensure that the
  // symbol itself is always in the symbol table.
  symbol = wrenSymbolTableEnsure(vm, vm.methodNames, "<finalize>", 10);
  if (methods.finalize !== null) {
    method.fn.foreign = methods.finalize;
    wrenBindMethod(vm, classObj, symbol, method);
  }
}

// Creates a new class.
//
// If [numFields] is -1, the class is a foreign class. The name and superclass
// should be on top of the fiber's stack. After calling this, the top of the
// stack will contain the new class.
//
// Aborts the current fiber if an error occurs.
function createClass(vm, numFields, module) {
  // Pull the name and superclass off the stack.
  var name = vm.fiber.stackTop[-2];
  var superclass = vm.fiber.stackTop[-1];

  // We have two values on the stack and we are going to leave one, so discard
  // the other slot.
  vm.fiber.stackTop--;

  vm.fiber.error = validateSuperclass(vm, name, superclass, numFields);
  if (!IS_NULL(vm.fiber.error)) {
    return;
  }

  var classObj = wrenNewClass(vm, AS_CLASS(superclass), numFields,
                                    AS_STRING(name));
  vm.fiber.stackTop[-1] = OBJ_VAL(classObj);

  if (numFields === -1) {
    bindForeignClass(vm, classObj, module);
  }
}

function createForeign(vm, fiber, stack) {
  var classObj = AS_CLASS(stack[0]);
  assert(classObj.numFields === -1, "Class must be a foreign class.");

  // TODO: Don't look up every time.
  var symbol = wrenSymbolTableFind(vm.methodNames, "<allocate>", 10);
  assert(symbol !== -1, "Should have defined <allocate> symbol.");

  assert(classObj.methods.count > symbol, "Class should have allocator.");
  var method = classObj.methods.data[symbol];
  assert(method.type === METHOD_FOREIGN, "Allocator should be foreign.");

  // Pass the constructor arguments to the allocator as well.
  vm.apiStack = stack;

  method.fn.foreign(vm);

  // TODO: Check that allocateForeign was called.
}

function wrenFinalizeForeign(vm, foreign) {
  // TODO: Don't look up every time.
  var symbol = wrenSymbolTableFind(vm.methodNames, "<finalize>", 10);
  assert(symbol !== -1, "Should have defined <finalize> symbol.");

  // If there are no finalizers, don't finalize it.
  if (symbol === -1) return;

  // If the class doesn't have a finalizer, bail out.
  var classObj = foreign.obj.classObj;
  if (symbol >= classObj.methods.count) return;

  var method = classObj.methods.data[symbol];
  if (method.type === METHOD_NONE) return;

  assert(method.type === METHOD_FOREIGN, "Finalizer should be foreign.");

  var finalizer = method.fn.foreign;
  finalizer(foreign.data);
}

// The main bytecode interpreter loop. This is where the magic happens. It is
// also, as you can imagine, highly performance critical. Returns `true` if the
// fiber completed without error.
function runInterpreter(vm, fiber) {
  console.log("INTERPRET");

  /*
  // Remember the current fiber so we can find it if a GC happens.
  vm->fiber = fiber;

  // Hoist these into local variables. They are accessed frequently in the loop
  // but assigned less frequently. Keeping them in locals and updating them when
  // a call frame has been pushed or popped gives a large speed boost.
  register CallFrame* frame;
  register Value* stackStart;
  register uint8_t* ip;
  register ObjFn* fn;

  // These macros are designed to only be invoked within this function.
  #define PUSH(value)  (*fiber->stackTop++ = value)
  #define POP()        (*(--fiber->stackTop))
  #define DROP()       (fiber->stackTop--)
  #define PEEK()       (*(fiber->stackTop - 1))
  #define PEEK2()      (*(fiber->stackTop - 2))
  #define READ_BYTE()  (*ip++)
  #define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))

  // Use this before a CallFrame is pushed to store the local variables back
  // into the current one.
  #define STORE_FRAME() frame->ip = ip

  // Use this after a CallFrame has been pushed or popped to refresh the local
  // variables.
  #define LOAD_FRAME()                                 \
      frame = &fiber->frames[fiber->numFrames - 1];    \
      stackStart = frame->stackStart;                  \
      ip = frame->ip;                                  \
      fn = wrenUnwrapClosure(frame->fn);

  // Terminates the current fiber with error string [error]. If another calling
  // fiber is willing to catch the error, transfers control to it, otherwise
  // exits the interpreter.
  #define RUNTIME_ERROR()                                         \
      do                                                          \
      {                                                           \
        STORE_FRAME();                                            \
        runtimeError(vm);                                         \
        if (vm->fiber == NULL) return WREN_RESULT_RUNTIME_ERROR;  \
        fiber = vm->fiber;                                        \
        LOAD_FRAME();                                             \
        DISPATCH();                                               \
      }                                                           \
      while (false)

  #if WREN_DEBUG_TRACE_INSTRUCTIONS
    // Prints the stack and instruction before each instruction is executed.
    #define DEBUG_TRACE_INSTRUCTIONS()                            \
        do                                                        \
        {                                                         \
          wrenDumpStack(fiber);                                   \
          wrenDumpInstruction(vm, fn, (int)(ip - fn->bytecode));  \
        }                                                         \
        while (false)
  #else
    #define DEBUG_TRACE_INSTRUCTIONS() do { } while (false)
  #endif

  #if WREN_COMPUTED_GOTO

  static void* dispatchTable[] = {
    #define OPCODE(name, _) &&code_##name,
    #include "wren_opcodes.h"
    #undef OPCODE
  };

  #define INTERPRET_LOOP    DISPATCH();
  #define CASE_CODE(name)   code_##name

  #define DISPATCH()                                            \
      do                                                        \
      {                                                         \
        DEBUG_TRACE_INSTRUCTIONS();                             \
        goto *dispatchTable[instruction = (Code)READ_BYTE()];   \
      }                                                         \
      while (false)

  #else

  #define INTERPRET_LOOP                                        \
      loop:                                                     \
        DEBUG_TRACE_INSTRUCTIONS();                             \
        switch (instruction = (Code)READ_BYTE())

  #define CASE_CODE(name)  case CODE_##name
  #define DISPATCH()       goto loop

  #endif

  LOAD_FRAME();

  Code instruction;
  INTERPRET_LOOP
  {
    CASE_CODE(LOAD_LOCAL_0):
    CASE_CODE(LOAD_LOCAL_1):
    CASE_CODE(LOAD_LOCAL_2):
    CASE_CODE(LOAD_LOCAL_3):
    CASE_CODE(LOAD_LOCAL_4):
    CASE_CODE(LOAD_LOCAL_5):
    CASE_CODE(LOAD_LOCAL_6):
    CASE_CODE(LOAD_LOCAL_7):
    CASE_CODE(LOAD_LOCAL_8):
      PUSH(stackStart[instruction - CODE_LOAD_LOCAL_0]);
      DISPATCH();

    CASE_CODE(LOAD_LOCAL):
      PUSH(stackStart[READ_BYTE()]);
      DISPATCH();

    CASE_CODE(LOAD_FIELD_THIS):
    {
      uint8_t field = READ_BYTE();
      Value receiver = stackStart[0];
      ASSERT(IS_INSTANCE(receiver), "Receiver should be instance.");
      ObjInstance* instance = AS_INSTANCE(receiver);
      ASSERT(field < instance->obj.classObj->numFields, "Out of bounds field.");
      PUSH(instance->fields[field]);
      DISPATCH();
    }

    CASE_CODE(POP):   DROP(); DISPATCH();
    CASE_CODE(DUP):
    {
      Value value = PEEK();
      PUSH(value); DISPATCH();
    }

    CASE_CODE(NULL):  PUSH(NULL_VAL); DISPATCH();
    CASE_CODE(FALSE): PUSH(FALSE_VAL); DISPATCH();
    CASE_CODE(TRUE):  PUSH(TRUE_VAL); DISPATCH();

    CASE_CODE(STORE_LOCAL):
      stackStart[READ_BYTE()] = PEEK();
      DISPATCH();

    CASE_CODE(CONSTANT):
      PUSH(fn->constants.data[READ_SHORT()]);
      DISPATCH();

    {
      // The opcodes for doing method and superclass calls share a lot of code.
      // However, doing an if() test in the middle of the instruction sequence
      // to handle the bit that is special to super calls makes the non-super
      // call path noticeably slower.
      //
      // Instead, we do this old school using an explicit goto to share code for
      // everything at the tail end of the call-handling code that is the same
      // between normal and superclass calls.
      int numArgs;
      int symbol;

      Value* args;
      ObjClass* classObj;

      Method* method;

    CASE_CODE(CALL_0):
    CASE_CODE(CALL_1):
    CASE_CODE(CALL_2):
    CASE_CODE(CALL_3):
    CASE_CODE(CALL_4):
    CASE_CODE(CALL_5):
    CASE_CODE(CALL_6):
    CASE_CODE(CALL_7):
    CASE_CODE(CALL_8):
    CASE_CODE(CALL_9):
    CASE_CODE(CALL_10):
    CASE_CODE(CALL_11):
    CASE_CODE(CALL_12):
    CASE_CODE(CALL_13):
    CASE_CODE(CALL_14):
    CASE_CODE(CALL_15):
    CASE_CODE(CALL_16):
      // Add one for the implicit receiver argument.
      numArgs = instruction - CODE_CALL_0 + 1;
      symbol = READ_SHORT();

      // The receiver is the first argument.
      args = fiber->stackTop - numArgs;
      classObj = wrenGetClassInline(vm, args[0]);
      goto completeCall;

    CASE_CODE(SUPER_0):
    CASE_CODE(SUPER_1):
    CASE_CODE(SUPER_2):
    CASE_CODE(SUPER_3):
    CASE_CODE(SUPER_4):
    CASE_CODE(SUPER_5):
    CASE_CODE(SUPER_6):
    CASE_CODE(SUPER_7):
    CASE_CODE(SUPER_8):
    CASE_CODE(SUPER_9):
    CASE_CODE(SUPER_10):
    CASE_CODE(SUPER_11):
    CASE_CODE(SUPER_12):
    CASE_CODE(SUPER_13):
    CASE_CODE(SUPER_14):
    CASE_CODE(SUPER_15):
    CASE_CODE(SUPER_16):
      // Add one for the implicit receiver argument.
      numArgs = instruction - CODE_SUPER_0 + 1;
      symbol = READ_SHORT();

      // The receiver is the first argument.
      args = fiber->stackTop - numArgs;

      // The superclass is stored in a constant.
      classObj = AS_CLASS(fn->constants.data[READ_SHORT()]);
      goto completeCall;

    completeCall:
      // If the class's method table doesn't include the symbol, bail.
      if (symbol >= classObj->methods.count ||
          (method = &classObj->methods.data[symbol])->type == METHOD_NONE)
      {
        methodNotFound(vm, classObj, symbol);
        RUNTIME_ERROR();
      }

      switch (method->type)
      {
        case METHOD_PRIMITIVE:
          if (method->fn.primitive(vm, args))
          {
            // The result is now in the first arg slot. Discard the other
            // stack slots.
            fiber->stackTop -= numArgs - 1;
          } else {
            // An error or fiber switch occurred.
            STORE_FRAME();

            // If we don't have a fiber to switch to, stop interpreting.
            fiber = vm->fiber;
            if (fiber == NULL) return WREN_RESULT_SUCCESS;
            if (!IS_NULL(fiber->error)) RUNTIME_ERROR();
            LOAD_FRAME();
          }
          break;

        case METHOD_FOREIGN:
          callForeign(vm, fiber, method->fn.foreign, numArgs);
          break;

        case METHOD_FN_CALL:
          if (!checkArity(vm, args[0], numArgs)) RUNTIME_ERROR();

          STORE_FRAME();
          callFunction(vm, fiber, AS_OBJ(args[0]), numArgs);
          LOAD_FRAME();
          break;

        case METHOD_BLOCK:
          STORE_FRAME();
          callFunction(vm, fiber, method->fn.obj, numArgs);
          LOAD_FRAME();
          break;

        case METHOD_NONE:
          UNREACHABLE();
          break;
      }
      DISPATCH();
    }

    CASE_CODE(LOAD_UPVALUE):
    {
      ObjUpvalue** upvalues = ((ObjClosure*)frame->fn)->upvalues;
      PUSH(*upvalues[READ_BYTE()]->value);
      DISPATCH();
    }

    CASE_CODE(STORE_UPVALUE):
    {
      ObjUpvalue** upvalues = ((ObjClosure*)frame->fn)->upvalues;
      *upvalues[READ_BYTE()]->value = PEEK();
      DISPATCH();
    }

    CASE_CODE(LOAD_MODULE_VAR):
      PUSH(fn->module->variables.data[READ_SHORT()]);
      DISPATCH();

    CASE_CODE(STORE_MODULE_VAR):
      fn->module->variables.data[READ_SHORT()] = PEEK();
      DISPATCH();

    CASE_CODE(STORE_FIELD_THIS):
    {
      uint8_t field = READ_BYTE();
      Value receiver = stackStart[0];
      ASSERT(IS_INSTANCE(receiver), "Receiver should be instance.");
      ObjInstance* instance = AS_INSTANCE(receiver);
      ASSERT(field < instance->obj.classObj->numFields, "Out of bounds field.");
      instance->fields[field] = PEEK();
      DISPATCH();
    }

    CASE_CODE(LOAD_FIELD):
    {
      uint8_t field = READ_BYTE();
      Value receiver = POP();
      ASSERT(IS_INSTANCE(receiver), "Receiver should be instance.");
      ObjInstance* instance = AS_INSTANCE(receiver);
      ASSERT(field < instance->obj.classObj->numFields, "Out of bounds field.");
      PUSH(instance->fields[field]);
      DISPATCH();
    }

    CASE_CODE(STORE_FIELD):
    {
      uint8_t field = READ_BYTE();
      Value receiver = POP();
      ASSERT(IS_INSTANCE(receiver), "Receiver should be instance.");
      ObjInstance* instance = AS_INSTANCE(receiver);
      ASSERT(field < instance->obj.classObj->numFields, "Out of bounds field.");
      instance->fields[field] = PEEK();
      DISPATCH();
    }

    CASE_CODE(JUMP):
    {
      uint16_t offset = READ_SHORT();
      ip += offset;
      DISPATCH();
    }

    CASE_CODE(LOOP):
    {
      // Jump back to the top of the loop.
      uint16_t offset = READ_SHORT();
      ip -= offset;
      DISPATCH();
    }

    CASE_CODE(JUMP_IF):
    {
      uint16_t offset = READ_SHORT();
      Value condition = POP();

      if (IS_FALSE(condition) || IS_NULL(condition)) ip += offset;
      DISPATCH();
    }

    CASE_CODE(AND):
    {
      uint16_t offset = READ_SHORT();
      Value condition = PEEK();

      if (IS_FALSE(condition) || IS_NULL(condition))
      {
        // Short-circuit the right hand side.
        ip += offset;
      }
      else
      {
        // Discard the condition and evaluate the right hand side.
        DROP();
      }
      DISPATCH();
    }

    CASE_CODE(OR):
    {
      uint16_t offset = READ_SHORT();
      Value condition = PEEK();

      if (IS_FALSE(condition) || IS_NULL(condition))
      {
        // Discard the condition and evaluate the right hand side.
        DROP();
      }
      else
      {
        // Short-circuit the right hand side.
        ip += offset;
      }
      DISPATCH();
    }

    CASE_CODE(CLOSE_UPVALUE):
      // Close the upvalue for the local if we have one.
      closeUpvalues(fiber, fiber->stackTop - 1);
      DROP();
      DISPATCH();

    CASE_CODE(RETURN):
    {
      Value result = POP();
      fiber->numFrames--;

      // Close any upvalues still in scope.
      closeUpvalues(fiber, stackStart);

      // If the fiber is complete, end it.
      if (fiber->numFrames == 0)
      {
        // See if there's another fiber to return to. If not, we're done.
        if (fiber->caller == NULL)
        {
          // Store the final result value at the beginning of the stack so the
          // C API can get it.
          fiber->stack[0] = result;
          fiber->stackTop = fiber->stack + 1;
          return WREN_RESULT_SUCCESS;
        }

        ObjFiber* resumingFiber = fiber->caller;
        fiber->caller = NULL;
        fiber = resumingFiber;
        vm->fiber = resumingFiber;

        // Store the result in the resuming fiber.
        fiber->stackTop[-1] = result;
      }
      else
      {
        // Store the result of the block in the first slot, which is where the
        // caller expects it.
        stackStart[0] = result;

        // Discard the stack slots for the call frame (leaving one slot for the
        // result).
        fiber->stackTop = frame->stackStart + 1;
      }

      LOAD_FRAME();
      DISPATCH();
    }

    CASE_CODE(CONSTRUCT):
      ASSERT(IS_CLASS(stackStart[0]), "'this' should be a class.");
      stackStart[0] = wrenNewInstance(vm, AS_CLASS(stackStart[0]));
      DISPATCH();

    CASE_CODE(FOREIGN_CONSTRUCT):
      ASSERT(IS_CLASS(stackStart[0]), "'this' should be a class.");
      createForeign(vm, fiber, stackStart);
      DISPATCH();

    CASE_CODE(CLOSURE):
    {
      ObjFn* prototype = AS_FN(fn->constants.data[READ_SHORT()]);

      ASSERT(prototype->numUpvalues > 0,
             "Should not create closure for functions that don't need it.");

      // Create the closure and push it on the stack before creating upvalues
      // so that it doesn't get collected.
      ObjClosure* closure = wrenNewClosure(vm, prototype);
      PUSH(OBJ_VAL(closure));

      // Capture upvalues.
      for (int i = 0; i < prototype->numUpvalues; i++)
      {
        uint8_t isLocal = READ_BYTE();
        uint8_t index = READ_BYTE();
        if (isLocal)
        {
          // Make an new upvalue to close over the parent's local variable.
          closure->upvalues[i] = captureUpvalue(vm, fiber,
                                                frame->stackStart + index);
        }
        else
        {
          // Use the same upvalue as the current call frame.
          closure->upvalues[i] = ((ObjClosure*)frame->fn)->upvalues[index];
        }
      }

      DISPATCH();
    }

    CASE_CODE(CLASS):
    {
      createClass(vm, READ_BYTE(), NULL);
      if (!IS_NULL(fiber->error)) RUNTIME_ERROR();
      DISPATCH();
    }

    CASE_CODE(FOREIGN_CLASS):
    {
      createClass(vm, -1, fn->module);
      if (!IS_NULL(fiber->error)) RUNTIME_ERROR();
      DISPATCH();
    }

    CASE_CODE(METHOD_INSTANCE):
    CASE_CODE(METHOD_STATIC):
    {
      uint16_t symbol = READ_SHORT();
      ObjClass* classObj = AS_CLASS(PEEK());
      Value method = PEEK2();
      bindMethod(vm, instruction, symbol, fn->module, classObj, method);
      if (!IS_NULL(fiber->error)) RUNTIME_ERROR();
      DROP();
      DROP();
      DISPATCH();
    }

    CASE_CODE(END):
      // A CODE_END should always be preceded by a CODE_RETURN. If we get here,
      // the compiler generated wrong code.
      UNREACHABLE();
  }

  // We should only exit this function from an explicit return from CODE_RETURN
  // or a runtime error.
  UNREACHABLE();
  return WREN_RESULT_RUNTIME_ERROR;

  #undef READ_BYTE
  #undef READ_SHORT */
}

function wrenMakeCallHandle(vm, signature) {
  var signatureLength = signature.length;

  // Count the number parameters the method expects.
  var numParams = 0;
  if (signature[signatureLength - 1] === ')') {
    for (var s = signature + signatureLength - 2;
         s > signature && s !== '('; s--) {
      if (s === '_') {
        numParams++;
      }
    }
  }

  // Add the signatue to the method table.
  var method =  wrenSymbolTableEnsure(vm, vm.methodNames,
                                      signature, signatureLength);

  // Create a little stub function that assumes the arguments are on the stack
  // and calls the method.
  var fn = wrenNewFunction(vm, null, numParams + 1);

  // Wrap the function in a handle. Do this here so it doesn't get collected as
  // we fill it in.
  var value = wrenCaptureValue(vm, OBJ_VAL(fn));

  wrenByteBufferWrite(vm, fn.code, (Code.CALL_0 + numParams));
  wrenByteBufferWrite(vm, fn.code, (method >> 8) & 0xff);
  wrenByteBufferWrite(vm, fn.code, method & 0xff);
  wrenByteBufferWrite(vm, fn.code, Code.RETURN);
  wrenByteBufferWrite(vm, fn.code, Code.END);
  wrenIntBufferFill(vm, fn.debug.sourceLines, 0, 5);
  wrenFunctionBindName(vm, fn, signature, signatureLength);

  return value;
}

function wrenCall(vm, method) {
  assert(method !== NULL, "Method cannot be NULL.");
  assert(IS_FN(method.value), "Method must be a method handle.");
  assert(vm.fiber !== NULL, "Must set up arguments for call first.");
  assert(vm.apiStack !== NULL, "Must set up arguments for call first.");
  assert(vm.fiber.numFrames === 0, "Can not call from a foreign method.");

  var fn = AS_FN(method.value);

  assert(vm.fiber.stackTop - vm.fiber.stack >= fn.arity,
         "Stack must have enough arguments for method.");

  // Discard any extra temporary slots. We take for granted that the stub
  // function has exactly one slot for each argument.
  vm.fiber.stackTop = vm.fiber.stack[fn.maxSlots];

  callFunction(vm, vm.fiber, fn, 0);
  return runInterpreter(vm, vm.fiber);
}

function wrenCaptureValue(vm, value) {
  if (IS_OBJ(value)) wrenPushRoot(vm, AS_OBJ(value));

  // Make a handle for it.
  var wrappedValue = ALLOCATE(vm, WrenValue);
  wrappedValue.value = value;

  if (IS_OBJ(value)) wrenPopRoot(vm);

  // Add it to the front of the linked list of handles.
  if (vm.valueHandles !== null) vm.valueHandles.prev = wrappedValue;
  wrappedValue.prev = null;
  wrappedValue.next = vm.valueHandles;
  vm.valueHandles = wrappedValue;

  return wrappedValue;
}

function wrenReleaseValue(vm, value) {
  assert(value !== null, "Value cannot be null.");

  // Update the VM's head pointer if we're releasing the first handle.
  if (vm.valueHandles === value) vm.valueHandles = value.next;

  // Unlink it from the list.
  if (value.prev !== null) value.prev.next = value.next;
  if (value.next !== null) value.next.prev = value.prev;

  // Clear it out. This isn't strictly necessary since we're going to free it,
  // but it makes for easier debugging.
  value.prev = null;
  value.next = null;
  value.value = NULL_VAL;
  DEALLOCATE(vm, value);
}

function wrenInterpret(vm, source) {
  return wrenInterpretInModule(vm, "main", source);
}

function wrenInterpretInModule(vm, module, source) {
  var nameValue = NULL_VAL;
  if (module !== null) {
    nameValue = wrenStringFormat(vm, "$", module);
    wrenPushRoot(vm, AS_OBJ(nameValue));
  }

  var fiber = loadModule(vm, nameValue, source);
  if (fiber === null) {
    wrenPopRoot(vm);
    return WREN_RESULT_COMPILE_ERROR;
  }

  if (module !== null) {
    wrenPopRoot(vm); // nameValue.
  }

  return runInterpreter(vm, fiber);
}

function wrenImportModule(vm, name) {
  // If the module is already loaded, we don't need to do anything.
  if (!IS_UNDEFINED(wrenMapGet(vm.modules, name))) {
    return NULL_VAL;
  }

  // Load the module's source code from the embedder.
  var source = vm.config.loadModuleFn(vm, AS_CSTRING(name));
  if (source === null) {
    vm.fiber.error = wrenStringFormat(vm, "Could not load module '@'.", name);
    return NULL_VAL;
  }

  var moduleFiber = loadModule(vm, name, source);
  if (moduleFiber === null) {
    vm.fiber.error = wrenStringFormat(vm,
                                        "Could not compile module '@'.", name);
    return NULL_VAL;
  }

  // Return the fiber that executes the module.
  return OBJ_VAL(moduleFiber);
}

function wrenGetModuleVariable(vm, moduleName, variableName) {
  var module = getModule(vm, moduleName);
  if (module === null) {
    vm.fiber.error = wrenStringFormat(vm, "Module '@' is not loaded.",
                                        moduleName);
    return NULL_VAL;
  }

  var variable = AS_STRING(variableName);
  var variableEntry = wrenSymbolTableFind(module.variableNames,
                                               variable.value,
                                               variable.length);

  // It's a runtime error if the imported variable does not exist.
  if (variableEntry !== UINT32_MAX) {
    return module.variables.data[variableEntry];
  }

  vm.fiber.error = wrenStringFormat(vm,
      "Could not find a variable named '@' in module '@'.",
      variableName, moduleName);
  return NULL_VAL;
}

function wrenFindVariable(vm, module, name) {
  var symbol = wrenSymbolTableFind(module.variableNames, name, name.length);
  return module.variables.data[symbol];
}

function wrenDeclareVariable(vm, module, name, length) {
  if (module.variables.count === MAX_MODULE_VARS) return -2;

  wrenValueBufferWrite(vm, module.variables, UNDEFINED_VAL);
  return wrenSymbolTableAdd(vm, module.variableNames, name, length);
}

function wrenDefineVariable(vm, module, name, length, value) {
  if (module.variables.count === MAX_MODULE_VARS) return -2;

  if (IS_OBJ(value)) wrenPushRoot(vm, AS_OBJ(value));

  // See if the variable is already explicitly or implicitly declared.
  var symbol = wrenSymbolTableFind(module.variableNames, name, length);

  if (symbol === -1) {
    // Brand new variable.
    symbol = wrenSymbolTableAdd(vm, module.variableNames, name, length);
    wrenValueBufferWrite(vm, module.variables, value);
  } else if (IS_UNDEFINED(module.variables.data[symbol])) {
    // Explicitly declaring an implicitly declared one. Mark it as defined.
    module.variables.data[symbol] = value;
  } else {
    // Already explicitly declared.
    symbol = -1;
  }

  if (IS_OBJ(value)) {
    wrenPopRoot(vm);
  }

  return symbol;
}

// TODO: Inline?
function wrenPushRoot(vm, obj) {
  assert(obj !== null, "Can't root NULL.");
  assert(vm.numTempRoots < WREN_MAX_TEMP_ROOTS, "Too many temporary roots.");

  vm.tempRoots[vm.numTempRoots++] = obj;
}

function wrenPopRoot(vm) {
  assert(vm.numTempRoots > 0, "No temporary roots to release.");
  vm.numTempRoots--;
}

function wrenGetSlotCount(vm) {
  if (vm.apiStack === null) return 0;

  return (vm.fiber.stackTop - vm.apiStack);
}

function wrenEnsureSlots(vm, numSlots) {
  // If we don't have a fiber accessible, create one for the API to use.
  if (vm.apiStack === null) {
    vm.fiber = wrenNewFiber(vm, null);
    vm.apiStack = vm.fiber.stack;
  }

  var currentSize = (vm.fiber.stackTop - vm.apiStack);
  if (currentSize >= numSlots) {
    return;
  }

  // Grow the stack if needed.
  var needed = (vm.apiStack - vm.fiber.stack) + numSlots;
  wrenEnsureStack(vm, vm.fiber, needed);

  vm.fiber.stackTop = vm.apiStack + numSlots;
}

// Ensures that [slot] is a valid index into the API's stack of slots.
function validateApiSlot(vm, slot) {
  assert(slot >= 0, "Slot cannot be negative.");
  assert(slot < wrenGetSlotCount(vm), "Not that many slots.");
}

// Gets the type of the object in [slot].
function wrenGetSlotType(vm, slot) {
  validateApiSlot(vm, slot);
  if (IS_BOOL(vm.apiStack[slot])) return WREN_TYPE_BOOL;
  if (IS_NUM(vm.apiStack[slot])) return WREN_TYPE_NUM;
  if (IS_FOREIGN(vm.apiStack[slot])) return WREN_TYPE_FOREIGN;
  if (IS_LIST(vm.apiStack[slot])) return WREN_TYPE_LIST;
  if (IS_NULL(vm.apiStack[slot])) return WREN_TYPE_NULL;
  if (IS_STRING(vm.apiStack[slot])) return WREN_TYPE_STRING;

  return WREN_TYPE_UNKNOWN;
}

function wrenGetSlotBool(vm, slot) {
  validateApiSlot(vm, slot);
  ASSERT(IS_BOOL(vm.apiStack[slot]), "Slot must hold a bool.");

  return AS_BOOL(vm.apiStack[slot]);
}

function wrenGetSlotBytes(vm, slot, length) {
  validateApiSlot(vm, slot);
  ASSERT(IS_STRING(vm.apiStack[slot]), "Slot must hold a string.");

  var string = AS_STRING(vm.apiStack[slot]);
  length = string.length;
  return string.value;
}

function wrenGetSlotDouble(vm, slot) {
  validateApiSlot(vm, slot);
  assert(IS_NUM(vm.apiStack[slot]), "Slot must hold a number.");

  return AS_NUM(vm.apiStack[slot]);
}

function wrenGetSlotForeign(vm, slot) {
  validateApiSlot(vm, slot);
  assert(IS_FOREIGN(vm.apiStack[slot]),
         "Slot must hold a foreign instance.");

  return AS_FOREIGN(vm.apiStack[slot]).data;
}

function wrenGetSlotString(vm, slot) {
  validateApiSlot(vm, slot);
  assert(IS_STRING(vm.apiStack[slot]), "Slot must hold a string.");

  return AS_CSTRING(vm.apiStack[slot]);
}

function wrenGetSlotValue(vm, slot) {
  validateApiSlot(vm, slot);
  return wrenCaptureValue(vm, vm.apiStack[slot]);
}

// Stores [value] in [slot] in the foreign call stack.
function setSlot(vm, slot, value) {
  validateApiSlot(vm, slot);
  vm.apiStack[slot] = value;
}

function wrenSetSlotBool(vm, slot, value) {
  setSlot(vm, slot, BOOL_VAL(value));
}

function wrenSetSlotBytes(vm, slot, bytes, length) {
  assert(bytes !== null, "Byte array cannot be NULL.");
  setSlot(vm, slot, wrenNewString(vm, bytes, length));
}

function wrenSetSlotDouble(vm, slot, value) {
  setSlot(vm, slot, NUM_VAL(value));
}

function wrenSetSlotNewForeign(vm, slot, classSlot, size) {
  validateApiSlot(vm, slot);
  validateApiSlot(vm, classSlot);
  assert(IS_CLASS(vm.apiStack[classSlot]), "Slot must hold a class.");

  var classObj = AS_CLASS(vm.apiStack[classSlot]);
  assert(classObj.numFields === -1, "Class must be a foreign class.");

  var foreign = wrenNewForeign(vm, classObj, size);
  vm.apiStack[slot] = OBJ_VAL(foreign);

  return foreign.data;
}

function wrenSetSlotNewList(vm, slot) {
  setSlot(vm, slot, OBJ_VAL(wrenNewList(vm, 0)));
}

function wrenSetSlotNull(vm, slot) {
  setSlot(vm, slot, NULL_VAL);
}

function wrenSetSlotString(vm, slot, text) {
  assert(text !== null, "String cannot be NULL.");

  setSlot(vm, slot, wrenNewString(vm, text, text.length));
}

function wrenSetSlotValue(vm, slot,value) {
  assert(value !== null, "Value cannot be NULL.");

  setSlot(vm, slot, value.value);
}

function wrenInsertInList(vm, listSlot, index, elementSlot) {
  validateApiSlot(vm, listSlot);
  validateApiSlot(vm, elementSlot);
  assert(IS_LIST(vm.apiStack[listSlot]), "Must insert into a list.");

  var list = AS_LIST(vm.apiStack[listSlot]);

  // Negative indices count from the end.
  if (index < 0) index = list.elements.count + 1 + index;

  assert(index <= list.elements.count, "Index out of bounds.");

  wrenListInsert(vm, list, vm.apiStack[elementSlot], index);
}

// TODO: Maybe just have this always return a WrenValue* instead of having to
// deal with slots?
function wrenGetVariable(vm, module, name, slot) {
  assert(module !== NULL, "Module cannot be NULL.");
  assert(module !== NULL, "Variable name cannot be NULL.");
  validateApiSlot(vm, slot);

  var moduleName = wrenStringFormat(vm, "$", module);
  wrenPushRoot(vm, AS_OBJ(moduleName));

  var moduleObj = getModule(vm, moduleName);
  assert(moduleObj !== NULL, "Could not find module.");

  wrenPopRoot(vm); // moduleName.

  var variableSlot = wrenSymbolTableFind(moduleObj.variableNames,
                                         name, name.length);
  assert(variableSlot !== -1, "Could not find variable.");

  setSlot(vm, slot, moduleObj.variables.data[variableSlot]);
}

////////////////////////////////////////////////////////////////////////////////
// Adding properties to this object will make them available to outside scripts.
module.exports = {
  Code: Code
};
