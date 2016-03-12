var common = require('./common');
var compiler = require('./compiler');
var vm = require('./vm');


// Initializes [configuration] with all of its default values.
//
// Call this before setting the particular fields you care about.
this.wrenInitConfiguration = vm.wrenInitConfiguration;

// Creates a new Wren virtual machine using the given [configuration]. Wren
// will copy the configuration data, so the argument passed to this can be
// freed after calling this. If [configuration] is `NULL`, uses a default
// configuration.
this.wrenNewVM = vm.wrenNewVM;

// Disposes of all resources is use by [vm], which was previously created by a
// call to [wrenNewVM].
this.wrenFreeVM = vm.wrenFreeVM;

// Immediately run the garbage collector to free unused memory.
this.wrenCollectGarbage = vm.wrenCollectGarbage;

// Runs [source], a string of Wren source code in a new fiber in [vm].
this.wrenInterpret = vm.wrenInterpret;

// Creates a handle that can be used to invoke a method with [signature] on
// using a receiver and arguments that are set up on the stack.
//
// This handle can be used repeatedly to directly invoke that method from C
// code using [wrenCall].
//
// When you are done with this handle, it must be released using
// [wrenReleaseValue].
this.wrenMakeCallHandle = vm.wrenMakeCallHandle;

// Calls [method], using the receiver and arguments previously set up on the
// stack.
//
// [method] must have been created by a call to [wrenMakeCallHandle]. The
// arguments to the method must be already on the stack. The receiver should be
// in slot 0 with the remaining arguments following it, in order. It is an
// error if the number of arguments provided does not match the method's
// signature.
//
// After this returns, you can access the return value from slot 0 on the stack.
this.wrenCall = vm.wrenCall;

// Releases the reference stored in [value]. After calling this, [value] can no
// longer be used.
this.wrenReleaseValue = vm.wrenReleaseValue;

// The following functions are intended to be called from foreign methods or
// finalizers. The interface Wren provides to a foreign method is like a
// register machine: you are given a numbered array of slots that values can be
// read from and written to. Values always live in a slot (unless explicitly
// captured using wrenGetSlotValue(), which ensures the garbage collector can
// find them.
//
// When your foreign function is called, you are given one slot for the receiver
// and each argument to the method. The receiver is in slot 0 and the arguments
// are in increasingly numbered slots after that. You are free to read and
// write to those slots as you want. If you want more slots to use as scratch
// space, you can call wrenEnsureSlots() to add more.
//
// When your function returns, every slot except slot zero is discarded and the
// value in slot zero is used as the return value of the method. If you don't
// store a return value in that slot yourself, it will retain its previous
// value, the receiver.
//
// While Wren is dynamically typed, C is not. This means the C interface has to
// support the various types of primitive values a Wren variable can hold: bool,
// double, string, etc. If we supported this for every operation in the C API,
// there would be a combinatorial explosion of functions, like "get a
// double-valued element from a list", "insert a string key and double value
// into a map", etc.
//
// To athis.that, the only way to convert to and from a raw C value is by going
// into and out of a slot. All other functions work with values already in a
// slot. So, to add an element to a list, you put the list in one slot, and the
// element in another. Then there is a single API function wrenInsertInList()
// that takes the element out of that slot and puts it into the list.
//
// The goal of this API is to be easy to use while not compromising performance.
// The latter means it does not do type or bounds checking at runtime except
// using assertions which are generally removed from release builds. C is an
// unsafe language, so it's up to you to be careful to use it correctly. In
// return, you get a very fast FFI.

// Returns the number of slots available to the current foreign method.
this.wrenGetSlotCount = vm.wrenGetSlotCount;

// Ensures that the foreign method stack has at least [numSlots] available for
// use, growing the stack if needed.
//
// Does not shrink the stack if it has more than enough slots.
//
// It is an error to call this from a finalizer.
this.wrenEnsureSlots = vm.wrenEnsureSlots;

// Gets the type of the object in [slot].
this.wrenGetSlotType = vm.wrenGetSlotType;

// Reads a boolean value from [slot].
//
// It is an error to call this if the slot does not contain a boolean value.
this.wrenGetSlotBool = vm.wrenGetSlotBool;

// Reads a byte array from [slot].
//
// The memory for the returned string is owned by Wren. You can inspect it
// while in your foreign method, but cannot keep a pointer to it after the
// function returns, since the garbage collector may reclaim it.
//
// Returns a pointer to the first byte of the array and fill [length] with the
// number of bytes in the array.
//
// It is an error to call this if the slot does not contain a string.
this.wrenGetSlotBytes = vm.wrenGetSlotBytes;

// Reads a number from [slot].
//
// It is an error to call this if the slot does not contain a number.
this.wrenGetSlotDouble = vm.wrenGetSlotDouble;

// Reads a foreign object from [slot] and returns a pointer to the foreign data
// stored with it.
//
// It is an error to call this if the slot does not contain an instance of a
// foreign class.
this.wrenGetSlotForeign = vm.wrenGetSlotForeign;

// Reads a string from [slot].
//
// The memory for the returned string is owned by Wren. You can inspect it
// while in your foreign method, but cannot keep a pointer to it after the
// function returns, since the garbage collector may reclaim it.
//
// It is an error to call this if the slot does not contain a string.
this.wrenGetSlotString = vm.wrenGetSlotString;

// Creates a handle for the value stored in [slot].
//
// This will prevent the object that is referred to from being garbage collected
// until the handle is released by calling [wrenReleaseValue()].
this.wrenGetSlotValue = vm.wrenGetSlotValue;

// The following functions provide the return value for a foreign method back
// to Wren. Like above, they may only be called during a foreign call invoked
// by Wren.
//
// If none of these is called by the time the foreign function returns, the
// method implicitly returns `null`. Within a given foreign call, you may only
// call one of these once. It is an error to access any of the foreign calls
// arguments after one of these has been called.

// Stores the boolean [value] in [slot].
this.wrenSetSlotBool = vm.wrenSetSlotBool;

// Stores the array [length] of [bytes] in [slot].
//
// The bytes are copied to a new string within Wren's heap, so you can free
// memory used by them after this is called.
this.wrenSetSlotBytes = vm.wrenSetSlotBytes;

// Stores the numeric [value] in [slot].
this.wrenSetSlotDouble = vm.wrenSetSlotDouble;

// Creates a new instance of the foreign class stored in [classSlot] with [size]
// bytes of raw storage and places the resulting object in [slot].
//
// This does not invoke the foreign class's constructor on the new instance. If
// you need that to happen, call the constructor from Wren, which will then
// call the allocator foreign method. In there, call this to create the object
// and then the constructor will be invoked when the allocator returns.
//
// Returns a pointer to the foreign object's data.
this.wrenSetSlotNewForeign = vm.wrenSetSlotNewForeign;

// Stores a new empty list in [slot].
this.wrenSetSlotNewList = vm.wrenSetSlotNewList;

// Stores null in [slot].
this.wrenSetSlotNull = vm.wrenSetSlotNull;

// Stores the string [text] in [slot].
//
// The [text] is copied to a new string within Wren's heap, so you can free
// memory used by it after this is called. The length is calculated using
// [strlen()]. If the string may contain any null bytes in the middle, then you
// should use [wrenSetSlotBytes()] instead.
this.wrenSetSlotString = vm.wrenSetSlotString;

// Stores the value captured in [value] in [slot].
//
// This does not release the handle for the value.
this.wrenSetSlotValue = vm.wrenSetSlotValue;

// Takes the value stored at [elementSlot] and inserts it into the list stored
// at [listSlot] at [index].
//
// As in Wren, negative indexes can be used to insert from the end. To append
// an element, use `-1` for the index.
this.wrenInsertInList = vm.wrenInsertInList;

// Looks up the top level variable with [name] in [module] and stores it in
// [slot].
this.wrenGetVariable = vm.wrenGetVariable;
