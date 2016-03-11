# wrenjs
This branch will contain a hand-written JavaScript port of the wren VM.

## Strategy

- C enums are converted to object literals, using integers as values.

      var Enum {
        ENUM_1: 0,
        ENUM_2: 1
      };

- The content of C header files should be expressed in the `module.exports` object at the top of each file.
