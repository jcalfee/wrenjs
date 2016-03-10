// This defines the bytecode instructions used by the VM. It does so by invoking
// an ) macro which is expected to be defined at the point that this is
// included. (See: http://en.wikipedia.org/wiki/X_Macro for more.)
//
// The first argument is the name of the opcode. The second is its "stack
// effect" -- the amount that the op code changes the size of the stack. A
// stack effect of 1 means it pushes a value and the stack grows one larger.
// -2 means it pops two values, etc.
//
// Note that the order of instructions here affects the order of the dispatch
// table in the VM's interpreter loop. That in turn affects caching which
// affects overall performance. Take care to run benchmarks if you change the
// order here.

var OPCODE = {
  // Load the constant at index [arg].
  CONSTANT: 1,

  // Push null onto the stack.
  NULL: 1,

  // Push false onto the stack.
  FALSE: 1,

  // Push true onto the stack.
  TRUE: 1,

  // Pushes the value in the given local slot.
  LOAD_LOCAL_0: 1,
  LOAD_LOCAL_1: 1,
  LOAD_LOCAL_2: 1,
  LOAD_LOCAL_3: 1,
  LOAD_LOCAL_4: 1,
  LOAD_LOCAL_5: 1,
  LOAD_LOCAL_6: 1,
  LOAD_LOCAL_7: 1,
  LOAD_LOCAL_8: 1,

  // Note: The compiler assumes the following _STORE instructions always
  // immediately follow their corresponding _LOAD ones.

  // Pushes the value in local slot [arg].
  LOAD_LOCAL: 1,

  // Stores the top of stack in local slot [arg]. Does not pop it.
  STORE_LOCAL: 0,

  // Pushes the value in upvalue [arg].
  LOAD_UPVALUE: 1,

  // Stores the top of stack in upvalue [arg]. Does not pop it.
  STORE_UPVALUE: 0,

  // Pushes the value of the top-level variable in slot [arg].
  LOAD_MODULE_VAR: 1,

  // Stores the top of stack in top-level variable slot [arg]. Does not pop it.
  STORE_MODULE_VAR: 0,

  // Pushes the value of the field in slot [arg] of the receiver of the current
  // function. This is used for regular field accesses on "this" directly in
  // methods. This instruction is faster than the more general CODE_LOAD_FIELD
  // instruction.
  LOAD_FIELD_THIS: 1,

  // Stores the top of the stack in field slot [arg] in the receiver of the
  // current value. Does not pop the value. This instruction is faster than the
  // more general CODE_LOAD_FIELD instruction.
  STORE_FIELD_THIS: 0,

  // Pops an instance and pushes the value of the field in slot [arg] of it.
  LOAD_FIELD: 0,

  // Pops an instance and stores the subsequent top of stack in field slot
  // [arg] in it. Does not pop the value.
  STORE_FIELD: -1,

  // Pop and discard the top of stack.
  POP: -1,

  // Push a copy of the value currently on the top of the stack.
  DUP: 1,

  // Invoke the method with symbol [arg]. The number indicates the number of
  // arguments (not including the receiver).
  CALL_0: 0,
  CALL_1: -1,
  CALL_2: -2,
  CALL_3: -3,
  CALL_4: -4,
  CALL_5: -5,
  CALL_6: -6,
  CALL_7: -7,
  CALL_8: -8,
  CALL_9: -9,
  CALL_10: -10,
  CALL_11: -11,
  CALL_12: -12,
  CALL_13: -13,
  CALL_14: -14,
  CALL_15: -15,
  CALL_16: -16,

  // Invoke a superclass method with symbol [arg]. The number indicates the
  // number of arguments (not including the receiver).
  SUPER_0: 0,
  SUPER_1: -1,
  SUPER_2: -2,
  SUPER_3: -3,
  SUPER_4: -4,
  SUPER_5: -5,
  SUPER_6: -6,
  SUPER_7: -7,
  SUPER_8: -8,
  SUPER_9: -9,
  SUPER_10: -10,
  SUPER_11: -11,
  SUPER_12: -12,
  SUPER_13: -13,
  SUPER_14: -14,
  SUPER_15: -15,
  SUPER_16: -16,

  // Jump the instruction pointer [arg] forward.
  JUMP: 0,

  // Jump the instruction pointer [arg] backward.
  LOOP: 0,

  // Pop and if not truthy then jump the instruction pointer [arg] forward.
  JUMP_IF: -1,

  // If the top of the stack is false, jump [arg] forward. Otherwise, pop and
  // continue.
  AND: -1,

  // If the top of the stack is non-false, jump [arg] forward. Otherwise, pop
  // and continue.
  OR: -1,

  // Close the upvalue for the local on the top of the stack, then pop it.
  CLOSE_UPVALUE: -1,

  // Exit from the current function and return the value on the top of the
  // stack.
  RETURN: 0,

  // Creates a closure for the function stored at [arg] in the constant table.
  //
  // Following the function argument is a number of arguments, two for each
  // upvalue. The first is true if the variable being captured is a local (as
  // opposed to an upvalue), and the second is the index of the local or
  // upvalue being captured.
  //
  // Pushes the created closure.
  CLOSURE: 1,

  // Creates a new instance of a class.
  //
  // Assumes the class object is in slot zero, and replaces it with the new
  // uninitialized instance of that class. This opcode is only emitted by the
  // compiler-generated constructor metaclass methods.
  CONSTRUCT: 0,

  // Creates a new instance of a foreign class.
  //
  // Assumes the class object is in slot zero, and replaces it with the new
  // uninitialized instance of that class. This opcode is only emitted by the
  // compiler-generated constructor metaclass methods.
  FOREIGN_CONSTRUCT: 0,

  // Creates a class. Top of stack is the superclass. Below that is a string for
  // the name of the class. Byte [arg] is the number of fields in the class.
  CLASS: -1,

  // Creates a foreign class. Top of stack is the superclass. Below that is a
  // string for the name of the class.
  FOREIGN_CLASS: -1,

  // Define a method for symbol [arg]. The class receiving the method is popped
  // off the stack, then the function defining the body is popped.
  //
  // If a foreign method is being defined, the "function" will be a string
  // identifying the foreign method. Otherwise, it will be a function or
  // closure.
  METHOD_INSTANCE: -2,

  // Define a method for symbol [arg]. The class whose metaclass will receive
  // the method is popped off the stack, then the function defining the body is
  // popped.
  //
  // If a foreign method is being defined, the "function" will be a string
  // identifying the foreign method. Otherwise, it will be a function or
  // closure.
  METHOD_STATIC: -2,

  // This pseudo-instruction indicates the end of the bytecode. It should
  // always be preceded by a `CODE_RETURN`, so is never actually executed.
  END: 0
};
