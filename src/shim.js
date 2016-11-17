
Wren = {
    WREN_OBJECTS: {},
    CURRENT_INDEX: 1,
    VM_MAP: {},


    shimNewVM: Module.cwrap('shimNewVM', 'number'),
    shimFreeVM: Module.cwrap('wrenFreeVM', null, ['number']),
    interpret: Module.cwrap('wrenInterpret', 'number', ['number', 'string']),

    newVM: function(wrenVM) {
        var c_vm = Wren.shimNewVM();
        Wren.VM_MAP[c_vm] = wrenVM;
        return c_vm;
    },

    freeVM: function(c_vm) {
        Wren.shimFreeVM(c_vm);
        Wren.VM_MAP[c_vm] = undefined;
    },

    writeFn: function(c_vm, string) {
        Wren.VM_MAP[c_vm].writeFn(string);
    },

    errorFn: function(source_module, line, message) {
        WrenVM.prototype.errorFn(source_module, line, message);
    },

    loadModuleFn: function(c_vm, string) {
        return Wren.VM_MAP[c_vm].loadModuleFn(string);
    }

};

/* Constructor */
WrenVM = function(disallowJS) {
    this._vm = Wren.newVM(this);

    // This makes it impossible for the 'main' script to use the JS module.
    if (disallowJS) {
        this.interpret('class JS {}');
        this.interpret('class JsObject {}');
    }
};

/* Methods */
WrenVM.prototype.freeVM = function() {
    Wren.freeVM(this._vm);
};

WrenVM.prototype.writeFn = function(string) {
    console.log(string);
};

WrenVM.prototype.errorFn = function(source_module, line, message) {
    throw(message + "\n  " + source_module + ":" + line);
};

WrenVM.prototype.interpret = function(wren) {
    var code = Wren.interpret(this._vm, wren);
    // 0 is good
    return code;
}

WrenVM.prototype.loadModuleFn = function(module) {
    throw('Module loading function not defined!');
}

WrenVM._lookup = function(id) {
    return Wren.WREN_OBJECTS[id];
};

WrenVM._register = function(object) {
  for (var index in Wren.WREN_OBJECTS) {
    if (Wren.WREN_OBJECTS[index] === object)
    return index;
  }

  Wren.WREN_OBJECTS[Wren.CURRENT_INDEX] = object;
  Wren.CURRENT_INDEX++;
  return Wren.CURRENT_INDEX - 1;
};

WrenVM._free = function(id) {
    Wren.WREN_OBJECTS[id] = null;
};
