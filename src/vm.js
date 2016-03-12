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

function wrenInitConfiguration(config)
{
  config.reallocateFn = defaultReallocate;
  config.loadModuleFn = NULL;
  config.bindForeignMethodFn = NULL;
  config.bindForeignClassFn = NULL;
  config.writeFn = NULL;
  config.initialHeapSize = 1024 * 1024 * 10;
  config.minHeapSize = 1024 * 1024;
  config.heapGrowthPercent = 50;
}

function wrenNewVM(config)
{
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

function wrenFreeVM(vm)
{
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
    printf("reallocate %x %x -> %x\n",
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

////////////////////////////////////////////////////////////////////////////////
// Adding properties to this object will make them available to outside scripts.
module.exports = {
  Code: Code
};
