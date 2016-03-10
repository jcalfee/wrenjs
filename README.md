# wrenjs
This branch will contain a hand-written JavaScript port of the wren VM.

## Strategy

- C enums are converted to object literals, using integers as values.

      var ENUM {
        ENUM_1: 0,
        ENUM_2: 1
      }
