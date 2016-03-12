module.value = require('./value');
module.vm = require('./vm');

var printf = require('./c/printf');

// Code enum from opcodes.js via vm.js
var Code = module.vm.Code;

// This module defines the compiler for Wren. It takes a string of source code
// and lexes, parses, and compiles it. Wren uses a single-pass compiler. It
// does not build an actual AST during parsing and then consume that to
// generate code. Instead, the parser directly emits bytecode.
//
// This forces a few restrictions on the grammar and semantics of the language.
// Things like forward references and arbitrary lookahead are much harder. We
// get a lot in return for that, though.
//
// The implementation is much simpler since we don't need to define a bunch of
// AST data structures. More so, we don't have to deal with managing memory for
// AST objects. The compiler does almost no dynamic allocation while running.
//
// Compilation is also faster since we don't create a bunch of temporary data
// structures and destroy them after generating code.

// This is written in bottom-up order, so the tokenization comes first, then
// parsing/code generation. This minimizes the number of explicit forward
// declarations needed.

// The maximum number of local (i.e. not module level) variables that can be
// declared in a single function, method, or chunk of top level code. This is
// the maximum number of variables in scope at one time, and spans block scopes.
//
// Note that this limitation is also explicit in the bytecode. Since
// `Code.LOAD_LOCAL` and `Code.STORE_LOCAL` use a single argument byte to
// identify the local, only 256 can be in scope at one time.
var MAX_LOCALS = 256;

// The maximum number of upvalues (i.e. variables from enclosing functions)
// that a function can close over.
var MAX_UPVALUES = 256;

// The maximum number of distinct constants that a function can contain. This
// value is explicit in the bytecode since `Code.CONSTANT` only takes a single
// two-byte argument.
var MAX_CONSTANTS = (1 << 16);

// The maximum distance a Code.JUMP or Code.JUMP_IF instruction can move the
// instruction pointer.
var MAX_JUMP = (1 << 16);

// The maximum depth that interpolation can nest. For example, this string has
// three levels:
//
//      "outside %(one + "%(two + "%(three)")")"
var MAX_INTERPOLATION_NESTING = 8;

var TokenType = {
  TOKEN_LEFT_PAREN: 0,
  TOKEN_RIGHT_PAREN: 1,
  TOKEN_LEFT_BRACKET: 2,
  TOKEN_RIGHT_BRACKET: 3,
  TOKEN_LEFT_BRACE: 4,
  TOKEN_RIGHT_BRACE: 5,
  TOKEN_COLON: 6,
  TOKEN_DOT: 7,
  TOKEN_DOTDOT: 8,
  TOKEN_DOTDOTDOT: 9,
  TOKEN_COMMA: 10,
  TOKEN_STAR: 12,
  TOKEN_SLASH: 13,
  TOKEN_PERCENT: 14,
  TOKEN_PLUS:15,
  TOKEN_MINUS: 16,
  TOKEN_LTLT: 17,
  TOKEN_GTGT: 18,
  TOKEN_PIPE: 19,
  TOKEN_PIPEPIPE: 20,
  TOKEN_CARET: 21,
  TOKEN_AMP: 22,
  TOKEN_AMPAMP: 23,
  TOKEN_BANG: 24,
  TOKEN_TILDE: 25,
  TOKEN_QUESTION: 26,
  TOKEN_EQ: 27,
  TOKEN_LT: 28,
  TOKEN_GT: 29,
  TOKEN_LTEQ: 30,
  TOKEN_GTEQ: 31,
  TOKEN_EQEQ: 32,
  TOKEN_BANGEQ: 33,

  TOKEN_BREAK: 34,
  TOKEN_CLASS: 35,
  TOKEN_CONSTRUCT: 36,
  TOKEN_ELSE: 37,
  TOKEN_FALSE: 38,
  TOKEN_FOR: 39,
  TOKEN_FOREIGN: 40,
  TOKEN_IF: 41,
  TOKEN_IMPORT: 42,
  TOKEN_IN: 43,
  TOKEN_IS: 44,
  TOKEN_NULL: 45,
  TOKEN_RETURN: 46,
  TOKEN_STATIC: 47,
  TOKEN_SUPER: 48,
  TOKEN_THIS: 49,
  TOKEN_TRUE: 50,
  TOKEN_VAR: 51,
  TOKEN_WHILE: 52,

  TOKEN_FIELD: 53,
  TOKEN_STATIC_FIELD: 54,
  TOKEN_NAME: 55,
  TOKEN_NUMBER: 56,

  // A string literal without any interpolation, or the last section of a
  // string following the last interpolated expression.
  TOKEN_STRING: 57,

  // A portion of a string literal preceding an interpolated expression. This
  // string:
  //
  //     "a %(b) c %(d) e"
  //
  // is tokenized to:
  //
  //     TOKEN_INTERPOLATION "a "
  //     TOKEN_NAME          b
  //     TOKEN_INTERPOLATION " c "
  //     TOKEN_NAME          d
  //     TOKEN_STRING        " e"
  TOKEN_INTERPOLATION: 58,

  TOKEN_LINE: 59,

  TOKEN_ERROR: 60,
  TOKEN_EOF: 61
};

/*
// Format of a Token
var Token = {
  type: null,

  // The beginning of the token, pointing directly into the source.
  start: null,

  // The length of the token in characters.
  length: null,

  // The 1-based line where the token appears.
  line: null,

  // The parsed value if the token is a literal.
  value: null,
};
*/

// Creates a Parser object literal
var Parser = function () {
  return {
    vm: null,

    // The module being parsed.
    module: null,

    // The source code being parsed.
    source: null,

    // The beginning of the currently-being-lexed token in [source].
    tokenStart: null,

    // The current character being lexed in [source].
    currentChar: null,

    // The 1-based line number of [currentChar].
    currentLine: null,

    // The most recently lexed token.
    current: {},

    // The most recently consumed/advanced token.
    previous: {},

    // Tracks the lexing state when tokenizing interpolated strings.
    //
    // Interpolated strings make the lexer not strictly regular: we don't know
    // whether a ")" should be treated as a RIGHT_PAREN token or as ending an
    // interpolated expression unless we know whether we are inside a string
    // interpolation and how many unmatched "(" there are. This is particularly
    // complex because interpolation can nest:
    //
    //     " %( " %( inner ) " ) "
    //
    // This tracks that state. The parser maintains a stack of ints, one for each
    // level of current interpolation nesting. Each value is the number of
    // unmatched "(" that are waiting to be closed.
    parens: [],
    numParens: null,

    // If subsequent newline tokens should be discarded.
    skipNewlines: null,

    // Whether compile errors should be printed to stderr or discarded.
    printErrors: null,

    // If a syntax or compile error has occurred.
    hasError: null
  };
};


/*
// Format of a Local
var Local = {
  // The name of the local variable. This points directly into the original
  // source code string.
  name: null,

  // The length of the local variable's name.
  length: null,

  // The depth in the scope chain that this variable was declared at. Zero is
  // the outermost scope--parameters for a method, or the first local block in
  // top level code. One is the scope within that, etc.
  depth: null,

  // If this local variable is being used as an upvalue.
  isUpvalue: null
};
*/

/*
// Format of a CompilerUpvalue
var CompilerUpvalue = {
  // True if this upvalue is capturing a local variable from the enclosing
  // function. False if it's capturing an upvalue.
  isLocal: null,

  // The index of the local or upvalue being captured in the enclosing function.
  index: null
};
*/

/*
// Bookkeeping information for the current loop being compiled.
var Loop = {
  // Index of the instruction that the loop should jump back to.
  start: null,

  // Index of the argument for the Code.JUMP_IF instruction used to exit the
  // loop. Stored so we can patch it once we know where the loop ends.
  exitJump: null,

  // Index of the first instruction of the body of the loop.
  body: null,

  // Depth of the scope(s) that need to be exited if a break is hit inside the
  // loop.
  scopeDepth: null,

  // The loop enclosing this one, or null if this is the outermost loop.
  enclosing: null
};
*/

// The different signature syntaxes for different kinds of methods.
var SignatureType = {
  // A name followed by a (possibly empty) parenthesized parameter list. Also
  // used for binary operators.
  SIG_METHOD: 0,

  // Just a name. Also used for unary operators.
  SIG_GETTER: 1,

  // A name followed by "=".
  SIG_SETTER: 2,

  // A square bracketed parameter list.
  SIG_SUBSCRIPT: 3,

  // A square bracketed parameter list followed by "=".
  SIG_SUBSCRIPT_SETTER: 4,

  // A constructor initializer function. This has a distinct signature to
  // prevent it from being invoked directly outside of the constructor on the
  // metaclass.
  SIG_INITIALIZER: 5
};

/*
var Signature = {
  name: null,
  length: null,
  type: null,
  arity: null
};
*/

/*
// Bookkeeping information for compiling a class definition.
var ClassCompiler = {
  // Symbol table for the fields of the class.
  fields: null,

  // True if the class being compiled is a foreign class.
  isForeign: null,

  // True if the current method being compiled is static.
  inStatic: null,

  // The signature of the method being compiled.
  signature: null
};
*/

var sCompiler = {
  parser: null,

  // The compiler for the function enclosing this one, or null if it's the
  // top level.
  parent: null,

  // The currently in scope local variables.
  locals: [],

  // The number of local variables currently in scope.
  numLocals: null,

  // The upvalues that this function has captured from outer scopes. The count
  // of them is stored in [numUpvalues].
  upvalues: [],

  // The current level of block scope nesting, where zero is no nesting. A -1
  // here means top-level code is being compiled and there is no block scope
  // in effect at all. Any variables declared will be module-level.
  copeDepth: null,

  // The current number of slots (locals and temporaries) in use.
  //
  // We use this and maxSlots to track the maximum number of additional slots
  // a function may need while executing. When the function is called, the
  // fiber will check to ensure its stack has enough room to cover that worst
  // case and grow the stack if needed.
  //
  // This value here doesn't include parameters to the function. Since those
  // are already pushed onto the stack by the caller and tracked there, we
  // don't need to double count them here.
  numSlots: null,

  // The current innermost loop being compiled, or null if not in a loop.
  loop: null,

  // If this is a compiler for a method, keeps track of the class enclosing it.
  enclosingClass: null,

  // The function being compiled.
  fn: null
};

// Describes where a variable is declared.
var Scope = {
  // A local variable in the current function.
  SCOPE_LOCAL: 0,

  // A local variable declared in an enclosing function.
  SCOPE_UPVALUE: 1,

  // A top-level module variable.
  SCOPE_MODULE: 2
};

/*
// A reference to a variable and the scope where it is defined. This contains
// enough information to emit correct code to load or store the variable.
var Variable = {
  // The stack slot, upvalue slot, or module symbol defining the variable.
  index: null,

  // Where the variable is declared.
  scope: null
};
*/

// The stack effect of each opcode. The index in the array is the opcode, and
// the value is the stack effect of that instruction.
var stackEffects = require('./opcodes.js');

// Outputs a compile or syntax error. This also marks the compilation as having
// an error, which ensures that the resulting code will be discarded and never
// run. This means that after calling lexError(), it's fine to generate whatever
// invalid bytecode you want since it won't be used.
function lexError(parser, format) {
  'use strict';

  parser.hasError = true;
  if (!parser.printErrors) {
    return;
  }

  var err = printf("[%x line %x] Error: ",
          parser.module.name.value, parser.currentLine);

  arguments.forEach(function(arg, i, args) {
    if (i > 0) {
      err += printf(format, args);
    }
  });
  err += "\n";

  console.log(err);
}

// Outputs a compile or syntax error. This also marks the compilation as having
// an error, which ensures that the resulting code will be discarded and never
// run. This means that after calling error(), it's fine to generate whatever
// invalid bytecode you want since it won't be used.
//
// You'll note that most places that call error() continue to parse and compile
// after that. That's so that we can try to find as many compilation errors in
// one pass as possible instead of just bailing at the first one.
function error(compiler, format) {
  'use strict';

  compiler.parser.hasError = true;
  if (!compiler.parser.printErrors) {
    return;
  }

  var token = compiler.parser.previous;

  // If the parse error was caused by an error token, the lexer has already
  // reported it.
  if (token.type === TokenType.TOKEN_ERROR) {
    return;
  }

  var err = printf("[%x line %x] Error at ",
    compiler.parser.module.name.value, token.line);

  if (token.type === TokenType.TOKEN_LINE) {
    err += printf("newline: ");
  } else if (token.type === TokenType.TOKEN_EOF) {
    err += printf("end of file: ");
  } else {
    err += printf("'%x': ", token.length); // See wren_compiler.c line 428
  }

  arguments.forEach(function(arg, i, args) {
    if (i > 2) {
      err += fprintf(format, arg);
    }
  });

  err += "\n";

  console.log(err);
}

// Adds [constant] to the constant pool and returns its index.
function addConstant(compiler, constant) {
  'use strict';

  if (compiler.parser.hasError) {
    return -1;
  }

  if (compiler.fn.constants.count < MAX_CONSTANTS) {
    if (module.value.IS_OBJ(constant)) {
      wrenPushRoot(compiler.parser.vm, constant);
    }
    wrenValueBufferWrite(compiler.parser.vm, compiler.fn.constants,
                         constant);
    if (module.value.IS_OBJ(constant)) {
      wrenPopRoot(compiler.parser.vm);
    }
  } else {
    error(compiler, "A function may only contain %x unique constants.",
      MAX_CONSTANTS);
  }

  return compiler.fn.constants.count - 1;
}

// Initializes [compiler]
function initCompiler (compiler, parser, parent, isFunction) {
  'use strict';

  compiler.parser = parser;
  compiler.parent = parent;
  compiler.loop = null;
  compiler.enclosingClass = null;

  // Initialize this to null before allocating in case a GC gets triggered in
  // the middle of initializing the compiler.
  compiler.fn = null;

  parser.vm.compiler = compiler;

  if (parent === null) {
    compiler.numLocals = 0;

    // Compiling top-level code, so the initial scope is module-level.
    compiler.scopeDepth = -1;
  } else {
    // Declare a fake local variable for the receiver so that it's slot in the
    // stack is taken. For methods, we call this "this", so that we can resolve
    // references to that like a normal variable. For functions, they have no
    // explicit "this". So we pick a bogus name. That way references to "this"
    // inside a function will try to walk up the parent chain to find a method
    // enclosing the function whose "this" we can close over.
    compiler.numLocals = 1;
    if (isFunction) {
      compiler.locals[0].name = null;
      compiler.locals[0].length = 0;
    } else {
      compiler.locals[0].name = "this";
      compiler.locals[0].length = 4;
    }
    compiler.locals[0].depth = -1;
    compiler.locals[0].isUpvalue = false;

    // The initial scope for function or method is a local scope.
    compiler.scopeDepth = 0;
  }

  compiler.numSlots = compiler.numLocals;

  compiler.fn = module.value.wrenNewFunction(parser.vm, parser.module,
                                 compiler.numLocals);
}

// Lexing ----------------------------------------------------------------------

// Keyword class
var Keyword = function(identifier, length, tokenType) {
  'use strict';
  this.identifier = identifier;
  this.length = length;
  this.tokenType = tokenType;
};

// The table of reserved words and their associated token types.
var keywords = [
  new Keyword('break',      5, TokenType.TOKEN_BREAK),
  new Keyword('class',      5, TokenType.TOKEN_CLASS),
  new Keyword('construct',  9, TokenType.TOKEN_CONSTRUCT),
  new Keyword('else',       4, TokenType.TOKEN_ELSE),
  new Keyword('false',      5, TokenType.TOKEN_FALSE),
  new Keyword('for',        3, TokenType.TOKEN_FOR),
  new Keyword('foreign',    7, TokenType.TOKEN_FOREIGN),
  new Keyword('if',         2, TokenType.TOKEN_IF),
  new Keyword('import',     6, TokenType.TOKEN_IMPORT),
  new Keyword('in',         2, TokenType.TOKEN_IN),
  new Keyword('is',         2, TokenType.TOKEN_IS),
  new Keyword('null',       4, TokenType.TOKEN_NULL),
  new Keyword('return',     6, TokenType.TOKEN_RETURN),
  new Keyword('static',     6, TokenType.TOKEN_STATIC),
  new Keyword('super',      5, TokenType.TOKEN_SUPER),
  new Keyword('this',       4, TokenType.TOKEN_THIS),
  new Keyword('true',       4, TokenType.TOKEN_TRUE),
  new Keyword('var',        3, TokenType.TOKEN_VAR),
  new Keyword('while',      5, TokenType.TOKEN_WHILE),
  new Keyword(null ,        0, TokenType.TOKEN_EOF)
];

// Returns true if [c] is a valid (non-initial) identifier character.
function isName(c) {
  'use strict';
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c === '_';
}

// Returns true if [c] is a digit.
function isDigit(c) {
  'use strict';
  return c >= '0' && c <= '9';
}

// Returns the current character the parser is sitting on.
function peekChar(parser) {
  'use strict';
  return parser.currentChar;
}

// Returns the character after the current character.
function peekNextChar(parser) {
  'use strict';
  // If we're at the end of the source, don't read past it.
  if (peekChar(parser) === '\u0000') {
    return '\u0000';
  }
  return (parser.currentChar + 1);
}

// Advances the parser forward one character.
function nextChar(parser) {
  'use strict';
  var c = peekChar(parser);
  parser.currentChar += 1;
  if (c === '\n') {
    parser.currentLine += 1;
  }
  return c;
}

// If the current character is [c], consumes it and returns `true`.
function matchChar(parser, c) {
  'use strict';
  if (peekChar(parser) !== c) {
    return false;
  }
  nextChar(parser);
  return true;
}

// Sets the parser's current token to the given [type] and current character
// range.
function makeToken(parser,type) {
  'use strict';
  parser.current.type = type;
  parser.current.start = parser.tokenStart;
  parser.current.length = (parser.currentChar - parser.tokenStart);
  parser.current.line = parser.currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type === TokenType.TOKEN_LINE) {
    parser.current.line -= 1;
  }
}


// If the current character is [c], then consumes it and makes a token of type
// [two]. Otherwise makes a token of type [one].
function twoCharToken(parser, c, two, one) {
  'use strict';
  makeToken(parser, matchChar(parser, c) ? two : one);
}

// Skips the rest of the current line.
function skipLineComment(parser) {
  'use strict';
  while (peekChar(parser) !== '\n' && peekChar(parser) !== '\0') {
    nextChar(parser);
  }
}

// Skips the rest of a block comment.
function skipBlockComment(parser) {
  'use strict';
  var nesting = 1;
  while (nesting > 0) {
    if (peekChar(parser) === '\u0000') {
      lexError(parser, "Unterminated block comment.");
      return;
    }

    if (peekChar(parser) === '/' && peekNextChar(parser) === '*') {
      nextChar(parser);
      nextChar(parser);
      nesting += 1;
      continue;
    }

    if (peekChar(parser) === '*' && peekNextChar(parser) === '/') {
      nextChar(parser);
      nextChar(parser);
      nesting -= 1;
      continue;
    }

    // Regular comment character.
    nextChar(parser);
  }
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
function readHexDigit(parser) {
  'use strict';
  var c = nextChar(parser);
  if (c >= '0' && c <= '9') {
    return c - '0';
  }
  if (c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }
  if (c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  }

  // Don't consume it if it isn't expected. Keeps us from reading past the end
  // of an unterminated string.
  parser.currentChar -= 1;
  return -1;
}

// Parses the numeric value of the current token.
function makeNumber(parser, isHex) {
  'use strict';
  var errno = 0;

  // We don't check that the entire token is consumed because we've already
  // scanned it ourselves and know it's valid.
  parser.current.value = isHex ? strtol(parser.tokenStart, null, 16)
                                        : strtod(parser.tokenStart, null);

  if (errno === ERANGE) {
    lexError(parser, "Number literal was too large.");
    parser.current.value = 0;
  }

  makeToken(parser, TokenType.TOKEN_NUMBER);
}

// Finishes lexing a hexadecimal number literal.
function readHexNumber(parser) {
  'use strict';
  // Skip past the `x` used to denote a hexadecimal literal.
  nextChar(parser);

  // Iterate over all the valid hexadecimal digits found.
  while (readHexDigit(parser) !== -1) {
    continue;
  }

  makeNumber(parser, true);
}

// Finishes lexing a number literal.
function readNumber(parser) {
  'use strict';
  while (isDigit(peekChar(parser))) {
    nextChar(parser);
  }

  // See if it has a floating point. Make sure there is a digit after the "."
  // so we don't get confused by method calls on number literals.
  if (peekChar(parser) === '.' && isDigit(peekNextChar(parser))) {
    nextChar(parser);
    while (isDigit(peekChar(parser))) {
      nextChar(parser);
    }
  }

  // See if the number is in scientific notation.
  if (matchChar(parser, 'e') || matchChar(parser, 'E')) {
    // Allow a negative exponent.
    matchChar(parser, '-');

    if (!isDigit(peekChar(parser))) {
      lexError(parser, "Unterminated scientific notation.");
    }

    while (isDigit(peekChar(parser))) {
      nextChar(parser);
    }
  }

  makeNumber(parser, false);
}

// Finishes lexing an identifier. Handles reserved words.
function readName(parser, type) {
  while (isName(peekChar(parser)) || isDigit(peekChar(parser))) {
    nextChar(parser);
  }

  // Update the type if it's a keyword.
  var length = parser.currentChar - parser.tokenStart;
  for (var i = 0; keywords[i].identifier !== null; i++) {
    if (length === keywords[i].length && parser.tokenStart === keywords[i].identifier) {
      type = keywords[i].tokenType;
      break;
    }
  }

  makeToken(parser, type);
}

// Reads [digits] hex digits in a string literal and returns their number value.
function readHexEscape(parser, digits, description) {
  var value = 0;
  for (var i = 0; i < digits; i++) {
    if (peekChar(parser) === '"' || peekChar(parser) === '\u0000') {
      lexError(parser, "Incomplete %x escape sequence.", description);

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser.currentChar--;
      break;
    }

    var digit = readHexDigit(parser);
    if (digit === -1) {
      lexError(parser, "Invalid %x escape sequence.", description);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
function readUnicodeEscape(parser, string, length) {
  var value = readHexEscape(parser, length, "Unicode");

  // Grow the buffer enough for the encoded result.
  var numBytes = wrenUtf8EncodeNumBytes(value);
  if (numBytes !== 0) {
    wrenByteBufferFill(parser.vm, string, 0, numBytes);
    wrenUtf8Encode(value, string.data + string.count - numBytes);
  }
}

// Finishes lexing a string literal.
function readString(parser) {
  var string = [];
  var type = TokenType.TOKEN_STRING;
  wrenByteBufferInit(string);

  for (;;) {
    var c = nextChar(parser);
    if (c === '"') break;

    if (c === '\n0000')
    {
      lexError(parser, "Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser.currentChar--;
      break;
    }

    if (c === '%') {
      if (parser.numParens < MAX_INTERPOLATION_NESTING) {
        // TODO: Allow format string.
        if (nextChar(parser) !== '(') lexError(parser, "Expect '(' after '%%'.");

        parser.parens[parser.numParens++] = 1;
        type = TokenType.TOKEN_INTERPOLATION;
        break;
      }

      lexError(parser, "Interpolation may only nest %x levels deep.",
               MAX_INTERPOLATION_NESTING);
    }

    if (c === '\\') {
      switch (nextChar(parser)) {
        case '"':  wrenByteBufferWrite(parser.vm, string, '"'); break;
        case '\\': wrenByteBufferWrite(parser.vm, string, '\\'); break;
        case '%':  wrenByteBufferWrite(parser.vm, string, '%'); break;
        case '0':  wrenByteBufferWrite(parser.vm, string, '\n0000'); break;
        case 'a':  wrenByteBufferWrite(parser.vm, string, '\a'); break;
        case 'b':  wrenByteBufferWrite(parser.vm, string, '\b'); break;
        case 'f':  wrenByteBufferWrite(parser.vm, string, '\f'); break;
        case 'n':  wrenByteBufferWrite(parser.vm, string, '\n'); break;
        case 'r':  wrenByteBufferWrite(parser.vm, string, '\r'); break;
        case 't':  wrenByteBufferWrite(parser.vm, string, '\t'); break;
        case 'u':  readUnicodeEscape(parser, string, 4); break;
        case 'U':  readUnicodeEscape(parser, string, 8); break;
        case 'v':  wrenByteBufferWrite(parser.vm, string, '\v'); break;
        case 'x':
          wrenByteBufferWrite(parser.vm, string, readHexEscape(parser, 2, "byte"));
          break;

        default:
          lexError(parser, "Invalid escape character '%x'.",
                   (parser.currentChar - 1));
          break;
      }
    }
    else
    {
      wrenByteBufferWrite(parser.vm, string, c);
    }
  }

  parser.current.value = wrenNewString(parser.vm, string.data, string.count);

  wrenByteBufferClear(parser.vm, string);
  makeToken(parser, type);
}

// Lex the next token and store it in [parser.current].
function nextToken(parser) {
  parser.previous = parser.current;

  // If we are out of tokens, don't try to tokenize any more. We *do* still
  // copy the TOKEN_EOF to previous so that code that expects it to be consumed
  // will still work.
  if (parser.current.type === TokenType.TOKEN_EOF) {
    return;
  }

  while (peekChar(parser) !== '\n0000') {
    parser.tokenStart = parser.currentChar;

    var c = nextChar(parser);
    switch (c) {
      case '(':
        // If we are inside an interpolated expression, count the unmatched "(".
        if (parser.numParens > 0) parser.parens[parser.numParens - 1]++;
        makeToken(parser, TokenType.TOKEN_LEFT_PAREN);
        return;

      case ')':
        // If we are inside an interpolated expression, count the ")".
        if (parser.numParens > 0 &&
            --parser.parens[parser.numParens - 1] === 0) {
          // This is the final ")", so the interpolation expression has ended.
          // This ")" now begins the next section of the template string.
          parser.numParens--;
          readString(parser);
          return;
        }

        makeToken(parser, TokenType.TOKEN_RIGHT_PAREN);
        return;

      case '[': makeToken(parser, TokenType.TOKEN_LEFT_BRACKET); return;
      case ']': makeToken(parser, TokenType.TOKEN_RIGHT_BRACKET); return;
      case '{': makeToken(parser, TokenType.TOKEN_LEFT_BRACE); return;
      case '}': makeToken(parser, TokenType.TOKEN_RIGHT_BRACE); return;
      case ':': makeToken(parser, TokenType.TOKEN_COLON); return;
      case ',': makeToken(parser, TokenType.TOKEN_COMMA); return;
      case '*': makeToken(parser, TokenType.TOKEN_STAR); return;
      case '%': makeToken(parser, TokenType.TOKEN_PERCENT); return;
      case '^': makeToken(parser, TokenType.TOKEN_CARET); return;
      case '+': makeToken(parser, TokenType.TOKEN_PLUS); return;
      case '-': makeToken(parser, TokenType.TOKEN_MINUS); return;
      case '~': makeToken(parser, TokenType.TOKEN_TILDE); return;
      case '?': makeToken(parser, TokenType.TOKEN_QUESTION); return;

      case '|': twoCharToken(parser, '|', TokenType.TOKEN_PIPEPIPE, TokenType.TOKEN_PIPE); return;
      case '&': twoCharToken(parser, '&', TokenType.TOKEN_AMPAMP, TokenType.TOKEN_AMP); return;
      case '=': twoCharToken(parser, '=', TokenType.TOKEN_EQEQ, TokenType.TOKEN_EQ); return;
      case '!': twoCharToken(parser, '=', TokenType.TOKEN_BANGEQ, TokenType.TOKEN_BANG); return;

      case '.':
        if (matchChar(parser, '.')) {
          twoCharToken(parser, '.', TokenType.TOKEN_DOTDOTDOT, TokenType.TOKEN_DOTDOT);
          return;
        }

        makeToken(parser, TokenType.TOKEN_DOT);
        return;

      case '/':
        if (matchChar(parser, '/')) {
          skipLineComment(parser);
          break;
        }

        if (matchChar(parser, '*')) {
          skipBlockComment(parser);
          break;
        }

        makeToken(parser, TokenType.TOKEN_SLASH);
        return;

      case '<':
        if (matchChar(parser, '<')) {
          makeToken(parser, TokenType.TOKEN_LTLT);
        } else {
          twoCharToken(parser, '=', TokenType.TOKEN_LTEQ, TokenType.TOKEN_LT);
        }
        return;

      case '>':
        if (matchChar(parser, '>')) {
          makeToken(parser, TokenType.TOKEN_GTGT);
        } else {
          twoCharToken(parser, '=', TokenType.TOKEN_GTEQ, TokenType.TOKEN_GT);
        }
        return;

      case '\n':
        makeToken(parser, TokenType.TOKEN_LINE);
        return;

      case ' ':
      case '\r':
      case '\t':
        // Skip forward until we run out of whitespace.
        while (peekChar(parser) === ' ' ||
               peekChar(parser) === '\r' ||
               peekChar(parser) === '\t') {
          nextChar(parser);
        }
        break;

      case '"': readString(parser); return;
      case '_':
        readName(parser,
                 peekChar(parser) === '_' ? TokenType.TOKEN_STATIC_FIELD : TokenType.TOKEN_FIELD);
        return;

      case '#':
        // Ignore shebang on the first line.
        if (peekChar(parser) === '!' && parser.currentLine === 1) {
          skipLineComment(parser);
          break;
        }

        lexError(parser, "Invalid character '%x'.", c);
        return;

      case '0':
        if (peekChar(parser) === 'x') {
          readHexNumber(parser);
          return;
        }

        readNumber(parser);
        return;

      default:
        if (isName(c)) {
          readName(parser, TokenType.TOKEN_NAME);
        } else if (isDigit(c)) {
          readNumber(parser);
        } else {
          lexError(parser, "Invalid character '%x'.", c);
        }
        return;
    }
  }

  // If we get here, we're out of source, so just make EOF tokens.
  parser.tokenStart = parser.currentChar;
  makeToken(parser, TokenType.TOKEN_EOF);
}

// Parsing ---------------------------------------------------------------------

// Returns the type of the current token.
function peek(compiler) {
  return compiler.parser.current.type;
}

// Consumes the current token if its type is [expected]. Returns true if a
// token was consumed.
function match(compiler, expected) {
  if (peek(compiler) !== expected) return false;

  nextToken(compiler.parser);
  return true;
}

// Consumes the current token. Emits an error if its type is not [expected].
function consume(compiler, expected, errorMessage) {
  nextToken(compiler.parser);
  if (compiler.parser.previous.type !== expected) {
    error(compiler, errorMessage);

    // If the next token is the one we want, assume the current one is just a
    // spurious error and discard it to minimize the number of cascaded errors.
    if (compile.parser.current.type === expected) {
      nextToken(compiler.parser);
    }
  }
}

// Matches one or more newlines. Returns true if at least one was found.
function matchLine(compiler) {
  if (!match(compiler, TokenType.TOKEN_LINE)) {
    return false;
  }

  while (match(compiler, TokenType.TOKEN_LINE));
  return true;
}

// Discards any newlines starting at the current token.
function ignoreNewlines(compiler) {
  matchLine(compiler);
}

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
function consumeLine(compiler, errorMessage) {
  consume(compiler, TokenType.TOKEN_LINE, errorMessage);
  ignoreNewlines(compiler);
}

// Variables and scopes --------------------------------------------------------

// Emits one single-byte argument. Returns its index.
function emitByte(compiler, byte) {
  wrenByteBufferWrite(compiler.parser.vm, compiler.fn.code, byte);

  // Assume the instruction is associated with the most recently consumed token.
  wrenIntBufferWrite(compiler.parser.vm, compiler.fn.debug.sourceLines,
                     compiler.parser.previous.line);

  return compiler.fn.code.count - 1;
}

// Emits one bytecode instruction.
function emitOp(compiler, instruction) {
  emitByte(compiler, instruction);

  // Keep track of the stack's high water mark.
  compiler.numSlots += stackEffects[instruction];
  if (compiler.numSlots > compiler.fn.maxSlots) {
    compiler.fn.maxSlots = compiler.numSlots;
  }
}

// Emits one 16-bit argument, which will be written big endian.
function emitShort(compiler, arg) {
  emitByte(compiler, (arg >> 8) & 0xff);
  emitByte(compiler, arg & 0xff);
}

// Emits one bytecode instruction followed by a 8-bit argument. Returns the
// index of the argument in the bytecode.
function emitByteArg(compiler, instruction, arg) {
  emitOp(compiler, instruction);
  return emitByte(compiler, arg);
}

// Emits one bytecode instruction followed by a 16-bit argument, which will be
// written big endian.
function emitShortArg(compiler, instruction, arg) {
  emitOp(compiler, instruction);
  emitShort(compiler, arg);
}

// Emits [instruction] followed by a placeholder for a jump offset. The
// placeholder can be patched by calling [jumpPatch]. Returns the index of the
// placeholder.
function emitJump(compiler, instruction) {
  emitOp(compiler, instruction);
  emitByte(compiler, 0xff);
  return emitByte(compiler, 0xff) - 1;
}

// Creates a new constant for the current value and emits the bytecode to load
// it from the constant table.
function emitConstant(compiler, value) {
  var constant = addConstant(compiler, value);

  // Compile the code to load the constant.
  emitShortArg(compiler, Code.CONSTANT, constant);
}

// Create a new local variable with [name]. Assumes the current scope is local
// and the name is unique.
function addLocal(compiler, name, length) {
  var local = compiler.locals[compiler.numLocals];
  local.name = name;
  local.length = length;
  local.depth = compiler.scopeDepth;
  local.isUpvalue = false;
  return compiler.numLocals++;
}

// Declares a variable in the current scope whose name is the given token.
//
// If [token] is `null`, uses the previously consumed token. Returns its symbol.
function declareVariable(compiler, token) {
  if (token === null) {
    token = compiler.parser.previous;
  }

  if (token.length > MAX_VARIABLE_NAME) {
    error(compiler, "Variable name cannot be longer than %x characters.",
          MAX_VARIABLE_NAME);
  }

  // Top-level module scope.
  if (compiler.scopeDepth === -1)   {
    var symbol = wrenDefineVariable(compiler.parser.vm,
                                    compiler.parser.module,
                                    token.start, token.length, NULL_VAL);

    if (symbol === -1) {
      error(compiler, "Module variable is already defined.");
    } else if (symbol === -2) {
      error(compiler, "Too many module variables defined.");
    }

    return symbol;
  }

  // See if there is already a variable with this name declared in the current
  // scope. (Outer scopes are OK: those get shadowed.)
  for (var i = compiler.numLocals - 1; i >= 0; i--) {
    var local = compiler.locals[i];

    // Once we escape this scope and hit an outer one, we can stop.
    if (local.depth < compiler.scopeDepth) break;

    if (local.length === token.length && local.name === token.start) {
      error(compiler, "Variable is already declared in this scope.");
      return i;
    }
  }

  if (compiler.numLocals === MAX_LOCALS) {
    error(compiler, "Cannot declare more than %x variables in one scope.",
          MAX_LOCALS);
    return -1;
  }

  return addLocal(compiler, token.start, token.length);
}

// Parses a name token and declares a variable in the current scope with that
// name. Returns its slot.
function declareNamedVariable(compiler) {
  consume(compiler, TokenType.TOKEN_NAME, "Expect variable name.");
  return declareVariable(compiler, null);
}

// Stores a variable with the previously defined symbol in the current scope.
function defineVariable(compiler, symbol) {
  // Store the variable. If it's a local, the result of the initializer is
  // in the correct slot on the stack already so we're done.
  if (compiler.scopeDepth >= 0) {
    return;
  }

  // It's a module-level variable, so store the value in the module slot and
  // then discard the temporary for the initializer.
  emitShortArg(compiler, Code.STORE_MODULE_VAR, symbol);
  emitOp(compiler, Code.POP);
}

// Starts a new local block scope.
function pushScope(compiler) {
  compiler.scopeDepth++;
}

// Generates code to discard local variables at [depth] or greater. Does *not*
// actually undeclare variables or pop any scopes, though. This is called
// directly when compiling "break" statements to ditch the local variables
// before jumping out of the loop even though they are still in scope *past*
// the break instruction.
//
// Returns the number of local variables that were eliminated.
function discardLocals(compiler, depth) {
  assert(compiler.scopeDepth > -1, "Cannot exit top-level scope.");

  var local = compiler.numLocals - 1;
  while (local >= 0 && compiler.locals[local].depth >= depth) {
    // If the local was closed over, make sure the upvalue gets closed when it
    // goes out of scope on the stack. We use emitByte() and not emitOp() here
    // because we don't want to track that stack effect of these pops since the
    // variables are still in scope after the break.
    if (compiler.locals[local].isUpvalue) {
      emitByte(compiler, Code.CLOSE_UPVALUE);
    } else {
      emitByte(compiler, Code.POP);
    }

    local--;
  }

  return compiler.numLocals - local - 1;
}

// Closes the last pushed block scope and discards any local variables declared
// in that scope. This should only be called in a statement context where no
// temporaries are still on the stack.
function popScope(compiler) {
  var popped = discardLocals(compiler, compiler.scopeDepth);
  compiler.numLocals -= popped;
  compiler.numSlots -= popped;
  compiler.scopeDepth--;
}

// Attempts to look up the name in the local variables of [compiler]. If found,
// returns its index, otherwise returns -1.
function resolveLocal(compiler, name, length) {
  // Look it up in the local scopes. Look in reverse order so that the most
  // nested variable is found first and shadows outer ones.
  for (var i = compiler.numLocals - 1; i >= 0; i--) {
    if (compiler.locals[i].length === length &&
      name === compiler.locals[i].name) {
      return i;
    }
  }

  return -1;
}

// Adds an upvalue to [compiler]'s function with the given properties. Does not
// add one if an upvalue for that variable is already in the list. Returns the
// index of the upvalue.
function addUpvalue(compiler, isLocal, index) {
  // Look for an existing one.
  for (var i = 0; i < compiler.fn.numUpvalues; i++) {
    var upvalue = compiler.upvalues[i];
    if (upvalue.index === index && upvalue.isLocal === isLocal) return i;
  }

  // If we got here, it's a new upvalue.
  compiler.upvalues[compiler.fn.numUpvalues].isLocal = isLocal;
  compiler.upvalues[compiler.fn.numUpvalues].index = index;
  return compiler.fn.numUpvalues++;
}

// Attempts to look up [name] in the functions enclosing the one being compiled
// by [compiler]. If found, it adds an upvalue for it to this compiler's list
// of upvalues (unless it's already in there) and returns its index. If not
// found, returns -1.
//
// If the name is found outside of the immediately enclosing function, this
// will flatten the closure and add upvalues to all of the intermediate
// functions so that it gets walked down to this one.
//
// If it reaches a method boundary, this stops and returns -1 since methods do
// not close over local variables.
function findUpvalue(compiler, name, length) {
  // If we are at the top level, we didn't find it.
  if (compiler.parent === null) {
    return -1;
  }

  // If we hit the method boundary (and the name isn't a static field), then
  // stop looking for it. We'll instead treat it as a self send.
  if (name[0] !== '_' && compiler.parent.enclosingClass !== null) {
    return -1;
  }

  // See if it's a local variable in the immediately enclosing function.
  var local = resolveLocal(compiler.parent, name, length);
  if (local !== -1) {
    // Mark the local as an upvalue so we know to close it when it goes out of
    // scope.
    compiler.parent.locals[local].isUpvalue = true;

    return addUpvalue(compiler, true, local);
  }

  // See if it's an upvalue in the immediately enclosing function. In other
  // words, if it's a local variable in a non-immediately enclosing function.
  // This "flattens" closures automatically: it adds upvalues to all of the
  // intermediate functions to get from the function where a local is declared
  // all the way into the possibly deeply nested function that is closing over
  // it.
  var upvalue = findUpvalue(compiler.parent, name, length);
  if (upvalue !== -1) {
    return addUpvalue(compiler, false, upvalue);
  }

  // If we got here, we walked all the way up the parent chain and couldn't
  // find it.
  return -1;
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in local scope, or the enclosing function's
// upvalue list. Does not search the module scope. Returns a variable with
// index -1 if not found.
function resolveNonmodule(compiler, name, length) {
  // Look it up in the local scopes.
  var variable;
  variable.scope = Scope.SCOPE_LOCAL;
  variable.index = resolveLocal(compiler, name, length);
  if (variable.index !== -1) return variable;

  // It's not a local, so guess that it's an upvalue.
  variable.scope = Scope.SCOPE_UPVALUE;
  variable.index = findUpvalue(compiler, name, length);
  return variable;
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in module scope, local scope, or the enclosing
// function's upvalue list. Returns a variable with index -1 if not found.
function resolveName(compiler, name, length) {
  var variable = resolveNonmodule(compiler, name, length);
  if (variable.index !== -1) {
    return variable;
  }

  variable.scope = Scope.SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(compiler.parser.module.variableNames,
                                       name, length);
  return variable;
}

function loadLocal(compiler, slot) {
  if (slot <= 8) {
    emitOp(compiler, (Code.LOAD_LOCAL_0 + slot));
    return;
  }

  emitByteArg(compiler, Code.LOAD_LOCAL, slot);
}

// Finishes [compiler], which is compiling a function, method, or chunk of top
// level code. If there is a parent compiler, then this emits code in the
// parent compiler to load the resulting function.
function endCompiler(compiler, debugName, debugNameLength) {
  // If we hit an error, don't finish the function since it's borked anyway.
  if (compiler.parser.hasError) {
    compiler.parser.vm.compiler = compiler.parent;
    return null;
  }

  // Mark the end of the bytecode. Since it may contain multiple early returns,
  // we can't rely on Code.RETURN to tell us we're at the end.
  emitOp(compiler, Code.END);

  wrenFunctionBindName(compiler.parser.vm, compiler.fn,
                       debugName, debugNameLength);

  // In the function that contains this one, load the resulting function object.
  if (compiler.parent !== null) {
    var constant = addConstant(compiler.parent, compiler.fn);

    // If the function has no upvalues, we don't need to create a closure.
    // We can just load and run the function directly.
    if (compiler.fn.numUpvalues === 0) {
      emitShortArg(compiler.parent, Code.CONSTANT, constant);
    } else {
      // Capture the upvalues in the new closure object.
      emitShortArg(compiler.parent, Code.CLOSURE, constant);

      // Emit arguments for each upvalue to know whether to capture a local or
      // an upvalue.
      for (var i = 0; i < compiler.fn.numUpvalues; i++) {
        emitByte(compiler.parent, compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.parent, compiler.upvalues[i].index);
      }
    }
  }

  // Pop this compiler off the stack.
  compiler.parser.vm.compiler = compiler.parent;

  if (WREN_DEBUG_DUMP_COMPILED_CODE) {
    wrenDumpCode(compiler.parser.vm, compiler.fn);
  }

  return compiler.fn;
}

// Grammar ---------------------------------------------------------------------

var Precedence = {
  PREC_NONE: 0,
  PREC_LOWEST: 1,
  PREC_ASSIGNMENT: 2,     // =
  PREC_CONDITIONAL: 3,    // ?:
  PREC_LOGICAL_OR: 4,     // ||
  PREC_LOGICAL_AND: 5,    // &&
  PREC_EQUALITY: 6,       // === !=
  PREC_IS: 7,             // is
  PREC_COMPARISON: 8,     // < > <= >=
  PREC_BITWISE_OR: 9,     // |
  PREC_BITWISE_XOR: 10,   // ^
  PREC_BITWISE_AND: 11,   // &
  PREC_BITWISE_SHIFT: 12, // << >>
  PREC_RANGE: 13,         // .. ...
  PREC_TERM: 14,          // + -
  PREC_FACTOR: 15,        // * / %
  PREC_UNARY: 16,         // unary - ! ~
  PREC_CALL: 17,          // . () []
  PREC_PRIMARY: 18
};

var GrammarRule = {
  prefix: null,
  infix: null,
  method: null,
  precedence: null,
  name: null
};

// Forward declarations since the grammar is recursive.
function getRule(type){}
function expression(compiler) {}
function statement(compiler) {}
function definition(compiler) {}
function parsePrecedence(compiler, precedence) {}

// Replaces the placeholder argument for a previous Code.JUMP or Code.JUMP_IF
// instruction with an offset that jumps to the current end of bytecode.
function patchJump(compiler, offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  var jump = compiler.fn.code.count - offset - 2;
  if (jump > MAX_JUMP) {
    error(compiler, "Too much code to jump over.");
  }

  compiler.fn.code.data[offset] = (jump >> 8) & 0xff;
  compiler.fn.code.data[offset + 1] = jump & 0xff;
}

// Parses a block body, after the initial "{" has been consumed.
//
// Returns true if it was a expression body, false if it was a statement body.
// (More precisely, returns true if a value was left on the stack. An empty
// block returns false.)
function finishBlock(compiler) {
  // Empty blocks do nothing.
  if (match(compiler, TokenType.TOKEN_RIGHT_BRACE)) {
    return false;
  }

  // If there's no line after the "{", it's a single-expression body.
  if (!matchLine(compiler)) {
    expression(compiler);
    consume(compiler, TokenType.TOKEN_RIGHT_BRACE, "Expect '}' at end of block.");
    return true;
  }

  // Empty blocks (with just a newline inside) do nothing.
  if (match(compiler, TokenType.TOKEN_RIGHT_BRACE)) {
    return false;
  }

  // Compile the definition list.
  do {
    definition(compiler);

    // If we got into a weird error state, don't get stuck in a loop.
    if (peek(compiler) === TokenType.TOKEN_EOF) return true;

    consumeLine(compiler, "Expect newline after statement.");
  } while (!match(compiler, TokenType.TOKEN_RIGHT_BRACE));
  return false;
}

// Parses a method or function body, after the initial "{" has been consumed.
//
// It [isInitializer] is `true`, this is the body of a constructor initializer.
// In that case, this adds the code to ensure it returns `this`.
function finishBody(compiler, isInitializer) {
  var isExpressionBody = finishBlock(compiler);

  if (isInitializer) {
    // If the initializer body evaluates to a value, discard it.
    if (isExpressionBody) {
      emitOp(compiler, Code.POP);
    }

    // The receiver is always stored in the first local slot.
    emitOp(compiler, Code.LOAD_LOCAL_0);
  } else if (!isExpressionBody) {
    // Implicitly return null in statement bodies.
    emitOp(compiler, Code.NULL);
  }

  emitOp(compiler, Code.RETURN);
}

// The VM can only handle a certain number of parameters, so check that we
// haven't exceeded that and give a usable error.
function validateNumParameters(compiler, numArgs) {
  if (numArgs === MAX_PARAMETERS + 1) {
    // Only show an error at exactly max + 1 so that we can keep parsing the
    // parameters and minimize cascaded errors.
    error(compiler, "Methods cannot have more than %x parameters.",
          MAX_PARAMETERS);
  }
}

// Parses the rest of a comma-separated parameter list after the opening
// delimeter. Updates `arity` in [signature] with the number of parameters.
function finishParameterList(compiler, signature) {
  do {
    ignoreNewlines(compiler);
    validateNumParameters(compiler, ++signature.arity);

    // Define a local variable in the method for the parameter.
    declareNamedVariable(compiler);
  }
  while (match(compiler, TokenType.TOKEN_COMMA));
}

// Gets the symbol for a method [name] with [length].
function methodSymbol(compiler, name, length) {
  return wrenSymbolTableEnsure(compiler.parser.vm,
      compiler.parser.vm.methodNames, name, length);
}

// Appends characters to [name] (and updates [length]) for [numParams] "_"
// surrounded by [leftBracket] and [rightBracket].
function signatureParameterList(name, length, numParams, leftBracket, rightBracket) {
  name[(length)++] = leftBracket;
  for (var i = 0; i < numParams; i++) {
    if (i > 0) name[(length)++] = ',';
    name[(length)++] = '_';
  }
  name[(length)++] = rightBracket;
}

// Fills [name] with the stringified version of [signature] and updates
// [length] to the resulting length.
function signatureToString(signature, name, length) {
  length = 0;

  // Build the full name from the signature.
  memcpy(name + length, signature.name, signature.length);
  length += signature.length;

  switch (signature.type) {
    case SignatureType.SIG_METHOD:
      signatureParameterList(name, length, signature.arity, '(', ')');
      break;

    case SignatureType.SIG_GETTER:
      // The signature is just the name.
      break;

    case SignatureType.SIG_SETTER:
      name[(length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SignatureType.SIG_SUBSCRIPT:
      signatureParameterList(name, length, signature.arity, '[', ']');
      break;

    case SignatureType.SIG_SUBSCRIPT_SETTER:
      signatureParameterList(name, length, signature.arity - 1, '[', ']');
      name[(length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SignatureType.SIG_INITIALIZER:
      memcpy(name, "init ", 5);
      memcpy(name + 5, signature.name, signature.length);
      length = 5 + signature.length;
      signatureParameterList(name, length, signature.arity, '(', ')');
      break;
  }

  name[length] = '\n0000';
}

// Gets the symbol for a method with [signature].
function signatureSymbol(compiler, signature) {
  // Build the full name from the signature.
  var name;
  var length;
  signatureToString(signature, name, length);

  return methodSymbol(compiler, name, length);
}

// Returns a signature with [type] whose name is from the last consumed token.
function signatureFromToken(compiler, type) {
  var signature;

  // Get the token for the method name.
  var token = compiler.parser.previous;
  signature.name = token.start;
  signature.length = token.length;
  signature.type = type;
  signature.arity = 0;

  if (signature.length > MAX_METHOD_NAME) {
    error(compiler, "Method names cannot be longer than %x characters.",
          MAX_METHOD_NAME);
    signature.length = MAX_METHOD_NAME;
  }

  return signature;
}

// Parses a comma-separated list of arguments. Modifies [signature] to include
// the arity of the argument list.
function finishArgumentList(compiler, signature) {
  do {
    ignoreNewlines(compiler);
    validateNumParameters(compiler, ++signature.arity);
    expression(compiler);
  } while (match(compiler, TokenType.TOKEN_COMMA));

  // Allow a newline before the closing delimiter.
  ignoreNewlines(compiler);
}

// Compiles a method call with [signature] using [instruction].
function callSignature(compiler, instruction, signature) {
  var symbol = signatureSymbol(compiler, signature);
  emitShortArg(compiler, instruction + signature.arity, symbol);

  if (instruction === Code.SUPER_0) {
    // Super calls need to be statically bound to the class's superclass. This
    // ensures we call the right method even when a method containing a super
    // call is inherited by another subclass.
    //
    // We bind it at class definition time by storing a reference to the
    // superclass in a constant. So, here, we create a slot in the constant
    // table and store null in it. When the method is bound, we'll look up the
    // superclass then and store it in the constant slot.
    emitShort(compiler, addConstant(compiler, NULL_VAL));
  }
}

// Compiles a method call with [numArgs] for a method with [name] with [length].
function callMethod(compiler, numArgs, name, length) {
  var symbol = methodSymbol(compiler, name, length);
  emitShortArg(compiler, Code.CALL_0 + numArgs, symbol);
}

// Compiles an (optional) argument list for a method call with [methodSignature]
// and then calls it.
function methodCall(compiler, instruction, signature) {
  // Make a new signature that contains the updated arity and type based on
  // the arguments we find.
  var called = {
    name: signature.name,
    length: signature.length,
    type: SignatureType.SIG_GETTER,
    arity: 0
  };

  // Parse the argument list, if any.
  if (match(compiler, TokenType.TOKEN_LEFT_PAREN)) {
    called.type = SignatureType.SIG_METHOD;

    // Allow empty an argument list.
    if (peek(compiler) !== TokenType.TOKEN_RIGHT_PAREN) {
      finishArgumentList(compiler, called);
    }
    consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  }

  // Parse the block argument, if any.
  if (match(compiler, TokenType.TOKEN_LEFT_BRACE)) {
    // Include the block argument in the arity.
    called.type = SignatureType.SIG_METHOD;
    called.arity++;

    var fnCompiler;
    initCompiler(fnCompiler, compiler.parser, compiler, true);

    // Make a dummy signature to track the arity.
    var fnSignature = {
      name: "",
      length: 0,
      type: SignatureType.SIG_METHOD,
      arity: 0
    };

    // Parse the parameter list, if any.
    if (match(compiler, TokenType.TOKEN_PIPE)) {
      finishParameterList(fnCompiler, fnSignature);
      consume(compiler, TokenType.TOKEN_PIPE, "Expect '|' after function parameters.");
    }

    fnCompiler.fn.arity = fnSignature.arity;

    finishBody(fnCompiler, false);

    // Name the function based on the method its passed to.
    var blockName;
    var blockLength;
    signatureToString(called, blockName, blockLength);
    memmove(blockName + blockLength, " block argument", 16);

    endCompiler(fnCompiler, blockName, blockLength + 15);
  }

  // TODO: Allow Grace-style mixfix methods?

  // If this is a super() call for an initializer, make sure we got an actual
  // argument list.
  if (signature.type === SignatureType.SIG_INITIALIZER) {
    if (called.type !== SignatureType.SIG_METHOD) {
      error(compiler, "A superclass constructor must have an argument list.");
    }

    called.type = SignatureType.SIG_INITIALIZER;
  }

  callSignature(compiler, instruction, called);
}

// Compiles a call whose name is the previously consumed token. This includes
// getters, method calls with arguments, and setter calls.
function namedCall(compiler, canAssign, instruction) {
  // Get the token for the method name.
  var signature = signatureFromToken(compiler, SignatureType.SIG_GETTER);

  if (canAssign && match(compiler, TokenType.TOKEN_EQ)) {
    ignoreNewlines(compiler);

    // Build the setter signature.
    signature.type = SignatureType.SIG_SETTER;
    signature.arity = 1;

    // Compile the assigned value.
    expression(compiler);
    callSignature(compiler, instruction, signature);
  } else {
    methodCall(compiler, instruction, signature);
  }
}

// Emits the code to load [variable] onto the stack.
function loadVariable(compiler, variable) {
  switch (variable.scope) {
    case Scope.SCOPE_LOCAL:
      loadLocal(compiler, variable.index);
      break;
    case Scope.SCOPE_UPVALUE:
      emitByteArg(compiler, Code.LOAD_UPVALUE, variable.index);
      break;
    case Scope.SCOPE_MODULE:
      emitShortArg(compiler, Code.LOAD_MODULE_VAR, variable.index);
      break;
    default:
      UNREACHABLE();
  }
}

// Loads the receiver of the currently enclosing method. Correctly handles
// functions defined inside methods.
function loadThis(compiler) {
  loadVariable(compiler, resolveNonmodule(compiler, "this", 4));
}

// Pushes the value for a module-level variable implicitly imported from core.
function loadCoreVariable(compiler, name) {
  var symbol = wrenSymbolTableFind(compiler.parser.module.variableNames,
                                   name, strlen(name));
  assert(symbol !== -1, "Should have already defined core name.");
  emitShortArg(compiler, Code.LOAD_MODULE_VAR, symbol);
}

// A parenthesized expression.
function grouping(compiler, canAssign) {
  expression(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// A list literal.
function list(compiler, canAssign) {
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);

  // Compile the list elements. Each one compiles to a ".add()" call.
  do
  {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the list.
    if (peek(compiler) === TokenType.TOKEN_RIGHT_BRACKET) break;

    // The element.
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);
  } while (match(compiler, TokenType.TOKEN_COMMA));

  // Allow newlines before the closing ']'.
  ignoreNewlines(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");
}

// A map literal.
function map(compiler, canAssign) {
  // Instantiate a new map.
  loadCoreVariable(compiler, "Map");
  callMethod(compiler, 0, "new()", 5);

  // Compile the map elements. Each one is compiled to just invoke the
  // subscript setter on the map.
  do {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the map.
    if (peek(compiler) === TokenType.TOKEN_RIGHT_BRACE) {
      break;
    }

    // The key.
    parsePrecedence(compiler, Precedence.PREC_UNARY);
    consume(compiler, TokenType.TOKEN_COLON, "Expect ':' after map key.");
    ignoreNewlines(compiler);

    // The value.
    expression(compiler);
    callMethod(compiler, 2, "addCore_(_,_)", 13);
  } while (match(compiler, TokenType.TOKEN_COMMA));

  // Allow newlines before the closing '}'.
  ignoreNewlines(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_BRACE, "Expect '}' after map entries.");
}

// Unary operators like `-foo`.
function unaryOp(compiler, canAssign) {
  var rule = getRule(compiler.parser.previous.type);

  ignoreNewlines(compiler);

  // Compile the argument.
  parsePrecedence(compiler, Precedence.PREC_UNARY + 1);

  // Call the operator method on the left-hand side.
  callMethod(compiler, 0, rule.name, 1);
}

function boolean(compiler, canAssign) {
  emitOp(compiler,
      compiler.parser.previous.type === TokenType.TOKEN_FALSE ? Code.FALSE : Code.TRUE);
}

// Walks the compiler chain to find the compiler for the nearest class
// enclosing this one. Returns null if not currently inside a class definition.
function getEnclosingClassCompiler(compiler) {
  while (compiler !== null) {
    if (compiler.enclosingClass !== null) {
      return compiler;
    }
    compiler = compiler.parent;
  }

  return null;
}

// Walks the compiler chain to find the nearest class enclosing this one.
// Returns null if not currently inside a class definition.
function getEnclosingClass(compiler) {
  compiler = getEnclosingClassCompiler(compiler);
  return compiler === null ? null : compiler.enclosingClass;
}

function field(compiler, canAssign) {
  // Initialize it with a fake value so we can keep parsing and minimize the
  // number of cascaded errors.
  var field = 255;

  var enclosingClass = getEnclosingClass(compiler);

  if (enclosingClass === null) {
    error(compiler, "Cannot reference a field outside of a class definition.");
  } else if (enclosingClass.isForeign) {
    error(compiler, "Cannot define fields in a foreign class.");
  } else if (enclosingClass.inStatic) {
    error(compiler, "Cannot use an instance field in a static method.");
  } else {
    // Look up the field, or implicitly define it.
    field = wrenSymbolTableEnsure(compiler.parser.vm, enclosingClass.fields,
        compiler.parser.previous.start,
        compiler.parser.previous.length);

    if (field >= MAX_FIELDS) {
      error(compiler, "A class can only have %x fields.", MAX_FIELDS);
    }
  }

  // If there's an "=" after a field name, it's an assignment.
  var isLoad = true;
  if (canAssign && match(compiler, TokenType.TOKEN_EQ)) {
    // Compile the right-hand side.
    expression(compiler);
    isLoad = false;
  }

  // If we're directly inside a method, use a more optimal instruction.
  if (compiler.parent !== null &&
      compiler.parent.enclosingClass === enclosingClass) {
    emitByteArg(compiler, isLoad ? Code.LOAD_FIELD_THIS : Code.STORE_FIELD_THIS,
                field);
  } else {
    loadThis(compiler);
    emitByteArg(compiler, isLoad ? Code.LOAD_FIELD : Code.STORE_FIELD, field);
  }
}

// Compiles a read or assignment to [variable].
function bareName(compiler, canAssign, variable) {
  // If there's an "=" after a bare name, it's a variable assignment.
  if (canAssign && match(compiler, TokenType.TOKEN_EQ)) {
    // Compile the right-hand side.
    expression(compiler);

    // Emit the store instruction.
    switch (variable.scope) {
      case Scope.SCOPE_LOCAL:
        emitByteArg(compiler, Code.STORE_LOCAL, variable.index);
        break;
      case Scope.SCOPE_UPVALUE:
        emitByteArg(compiler, Code.STORE_UPVALUE, variable.index);
        break;
      case Scope.SCOPE_MODULE:
        emitShortArg(compiler, Code.STORE_MODULE_VAR, variable.index);
        break;
      default:
        UNREACHABLE();
    }
    return;
  }

  // Emit the load instruction.
  loadVariable(compiler, variable);
}

function staticField(compiler, canAssign) {
  var classCompiler = getEnclosingClassCompiler(compiler);
  if (classCompiler === null) {
    error(compiler, "Cannot use a static field outside of a class definition.");
    return;
  }

  // Look up the name in the scope chain.
  var token = compiler.parser.previous;

  // If this is the first time we've seen this static field, implicitly
  // define it as a variable in the scope surrounding the class definition.
  if (resolveLocal(classCompiler, token.start, token.length) === -1) {
    var symbol = declareVariable(classCompiler, null);

    // Implicitly initialize it to null.
    emitOp(classCompiler, Code.NULL);
    defineVariable(classCompiler, symbol);
  }

  // It definitely exists now, so resolve it properly. This is different from
  // the above resolveLocal() call because we may have already closed over it
  // as an upvalue.
  var variable = resolveName(compiler, token.start, token.length);
  bareName(compiler, canAssign, variable);
}

// Returns `true` if [name] is a local variable name (starts with a lowercase
// letter).
function isLocalName(name) {
  return name[0] >= 'a' && name[0] <= 'z';
}

// Compiles a variable name or method call with an implicit receiver.
function name(compiler, canAssign) {
  // Look for the name in the scope chain up to the nearest enclosing method.
  var token = compiler.parser.previous;

  var variable = resolveNonmodule(compiler, token.start, token.length);
  if (variable.index !== -1) {
    bareName(compiler, canAssign, variable);
    return;
  }

  // TODO: The fact that we return above here if the variable is known and parse
  // an optional argument list below if not means that the grammar is not
  // context-free. A line of code in a method like "someName(foo)" is a parse
  // error if "someName" is a defined variable in the surrounding scope and not
  // if it isn't. Fix this. One option is to have "someName(foo)" always
  // resolve to a self-call if there is an argument list, but that makes
  // getters a little confusing.

  // If we're inside a method and the name is lowercase, treat it as a method
  // on this.
  if (isLocalName(token.start) && getEnclosingClass(compiler) !== null) {
    loadThis(compiler);
    namedCall(compiler, canAssign, Code.CALL_0);
    return;
  }

  // Otherwise, look for a module-level variable with the name.
  variable.scope = Scope.SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(compiler.parser.module.variableNames,
                                       token.start, token.length);
  if (variable.index === -1)
  {
    if (isLocalName(token.start))
    {
      error(compiler, "null variable.");
      return;
    }

    // If it's a nonlocal name, implicitly define a module-level variable in
    // the hopes that we get a real definition later.
    variable.index = wrenDeclareVariable(compiler.parser.vm,
                                         compiler.parser.module,
                                         token.start, token.length);

    if (variable.index === -2) {
      error(compiler, "Too many module variables defined.");
    }
  }

  bareName(compiler, canAssign, variable);
}

function null_(compiler, canAssign) {
  emitOp(compiler, Code.NULL);
}

// A number or string literal.
function literal(compiler, canAssign) {
  emitConstant(compiler, compiler.parser.previous.value);
}

// A string literal that contains interpolated expressions.
//
// Interpolation is syntactic sugar for calling ".join()" on a list. So the
// string:
//
//     "a %(b + c) d"
//
// is compiled roughly like:
//
//     ["a ", b + c, " d"].join()
function stringInterpolation(compiler, canAssign) {
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);

  do {
    // The opening string part.
    literal(compiler, false);
    callMethod(compiler, 1, "addCore_(_)", 11);

    // The interpolated expression.
    ignoreNewlines(compiler);
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);

    ignoreNewlines(compiler);
  } while (match(compiler, TokenType.TOKEN_INTERPOLATION));

  // The trailing string part.
  consume(compiler, TokenType.TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);
  callMethod(compiler, 1, "addCore_(_)", 11);

  // The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6);
}

function super_(compiler, canAssign) {
  var enclosingClass = getEnclosingClass(compiler);

  if (enclosingClass === null) {
    error(compiler, "Cannot use 'super' outside of a method.");
  }

  loadThis(compiler);

  // TODO: Super operator calls.
  // TODO: There's no syntax for invoking a superclass constructor with a
  // different name from the enclosing one. Figure that out.

  // See if it's a named super call, or an unnamed one.
  if (match(compiler, TokenType.TOKEN_DOT)) {
    // Compile the superclass call.
    consume(compiler, TokenType.TOKEN_NAME, "Expect method name after 'super.'.");
    namedCall(compiler, canAssign, Code.SUPER_0);
  } else if (enclosingClass !== null) {
    // No explicit name, so use the name of the enclosing method. Make sure we
    // check that enclosingClass isn't null first. We've already reported the
    // error, but we don't want to crash here.
    methodCall(compiler, Code.SUPER_0, enclosingClass.signature);
  }
}

function this_(compiler, canAssign) {
  if (getEnclosingClass(compiler) === null) {
    error(compiler, "Cannot use 'this' outside of a method.");
    return;
  }

  loadThis(compiler);
}

// Subscript or "array indexing" operator like `foo[bar]`.
function subscript(compiler, canAssign) {
  var signature = {
    name: "",
    length: 0,
    type: SignatureType.SIG_SUBSCRIPT,
    arity: 0
  };

  // Parse the argument list.
  finishArgumentList(compiler, signature);
  consume(compiler, TokenType.TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");

  if (canAssign && match(compiler, TokenType.TOKEN_EQ)) {
    signature.type = SignatureType.SIG_SUBSCRIPT_SETTER;

    // Compile the assigned value.
    validateNumParameters(compiler, ++signature.arity);
    expression(compiler);
  }

  callSignature(compiler, Code.CALL_0, signature);
}

function call(compiler, canAssign) {
  ignoreNewlines(compiler);
  consume(compiler, TokenType.TOKEN_NAME, "Expect method name after '.'.");
  namedCall(compiler, canAssign, Code.CALL_0);
}

function and_(compiler, canAssign) {
  ignoreNewlines(compiler);

  // Skip the right argument if the left is false.
  var jump = emitJump(compiler, Code.AND);
  parsePrecedence(compiler, Precedence.PREC_LOGICAL_AND);
  patchJump(compiler, jump);
}

function or_(compiler, canAssign) {
  ignoreNewlines(compiler);

  // Skip the right argument if the left is true.
  var jump = emitJump(compiler, Code.OR);
  parsePrecedence(compiler, Precedence.PREC_LOGICAL_OR);
  patchJump(compiler, jump);
}

function conditional(compiler, canAssign) {
  // Ignore newline after '?'.
  ignoreNewlines(compiler);

  // Jump to the else branch if the condition is false.
  var ifJump = emitJump(compiler, Code.JUMP_IF);

  // Compile the then branch.
  parsePrecedence(compiler, Precedence.PREC_CONDITIONAL);

  consume(compiler, TokenType.TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.");
  ignoreNewlines(compiler);

  // Jump over the else branch when the if branch is taken.
  var elseJump = emitJump(compiler, Code.JUMP);

  // Compile the else branch.
  patchJump(compiler, ifJump);

  parsePrecedence(compiler, Precedence.PREC_ASSIGNMENT);

  // Patch the jump over the else.
  patchJump(compiler, elseJump);
}

function infixOp(compiler, canAssign) {
  var rule = getRule(compiler.parser.previous.type);

  // An infix operator cannot end an expression.
  ignoreNewlines(compiler);

  // Compile the right-hand side.
  parsePrecedence(compiler, (Precedence)(rule.precedence + 1));

  // Call the operator method on the left-hand side.
  var signature = {
    name: rule.name,
    length: strlen(rule.name),
    type: SignatureType.SIG_METHOD,
    arity: 1
  };
  callSignature(compiler, Code.CALL_0, signature);
}

// Compiles a method signature for an infix operator.
function infixSignature(compiler, signature) {
  // Add the RHS parameter.
  signature.type = SignatureType.SIG_METHOD;
  signature.arity = 1;

  // Parse the parameter name.
  consume(compiler, TokenType.TOKEN_LEFT_PAREN, "Expect '(' after operator name.");
  declareNamedVariable(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
}

// Compiles a method signature for an unary operator (i.e. "!").
function unarySignature(compiler, signature) {
  // Do nothing. The name is already complete.
  signature.type = SignatureType.SIG_GETTER;
}

// Compiles a method signature for an operator that can either be unary or
// infix (i.e. "-").
function mixedSignature(compiler, signature) {
  signature.type = SignatureType.SIG_GETTER;

  // If there is a parameter, it's an infix operator, otherwise it's unary.
  if (match(compiler, TokenType.TOKEN_LEFT_PAREN)) {
    // Add the RHS parameter.
    signature.type = SignatureType.SIG_METHOD;
    signature.arity = 1;

    // Parse the parameter name.
    declareNamedVariable(compiler);
    consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
  }
}

// Compiles an optional setter parameter in a method [signature].
//
// Returns `true` if it was a setter.
function maybeSetter(compiler, signature) {
  // See if it's a setter.
  if (!match(compiler, TokenType.TOKEN_EQ)) {
    return false;
  }

  // It's a setter.
  if (signature.type === SignatureType.SIG_SUBSCRIPT) {
    signature.type = SignatureType.SIG_SUBSCRIPT_SETTER;
  } else {
    signature.type = SignatureType.SIG_SETTER;
  }

  // Parse the value parameter.
  consume(compiler, TokenType.TOKEN_LEFT_PAREN, "Expect '(' after '='.");
  declareNamedVariable(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");

  signature.arity++;

  return true;
}

// Compiles a method signature for a subscript operator.
function subscriptSignature(compiler, signature) {
  signature.type = SignatureType.SIG_SUBSCRIPT;

  // The signature currently has "[" as its name since that was the token that
  // matched it. Clear that out.
  signature.length = 0;

  // Parse the parameters inside the subscript.
  finishParameterList(compiler, signature);
  consume(compiler, TokenType.TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.");

  maybeSetter(compiler, signature);
}

// Parses an optional parenthesized parameter list. Updates `type` and `arity`
// in [signature] to match what was parsed.
function parameterList(compiler, signature) {
  // The parameter list is optional.
  if (!match(compiler, TokenType.TOKEN_LEFT_PAREN)) {
    return;
  }

  signature.type = SignatureType.SIG_METHOD;

  // Allow an empty parameter list.
  if (match(compiler, TokenType.TOKEN_RIGHT_PAREN)) {
    return;
  }

  finishParameterList(compiler, signature);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// Compiles a method signature for a named method or setter.
function namedSignature(compiler, signature) {
  signature.type = SignatureType.SIG_GETTER;

  // If it's a setter, it can't also have a parameter list.
  if (maybeSetter(compiler, signature)) {
    return;
  }

  // Regular named method with an optional parameter list.
  parameterList(compiler, signature);
}

// Compiles a method signature for a constructor.
function constructorSignature(compiler, signature) {
  consume(compiler, TokenType.TOKEN_NAME, "Expect constructor name after 'construct'.");

  // Capture the name.
  signature = signatureFromToken(compiler, SignatureType.SIG_INITIALIZER);

  if (match(compiler, TokenType.TOKEN_EQ)) {
    error(compiler, "A constructor cannot be a setter.");
  }

  if (!match(compiler, TokenType.TOKEN_LEFT_PAREN)) {
    error(compiler, "A constructor cannot be a getter.");
    return;
  }

  // Allow an empty parameter list.
  if (match(compiler, TokenType.TOKEN_RIGHT_PAREN)) {
    return;
  }

  finishParameterList(compiler, signature);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// This table defines all of the parsing rules for the prefix and infix
// expressions in the grammar. Expressions are parsed using a Pratt parser.
//
// See: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
function UNUSED() {
  return {
    prefix: null,
    infix: null,
    method: null,
    precedence: Precedence.PREC_NONE,
    name: null
  };
}
function PREFIX(fn) {
  return {
    prefix: fn,
    infix: null,
    method: null,
    precedence: Precedence.PREC_NONE,
    name: null
  };
}
function INFIX(prec, fn) {
  return {
    prefix: null,
    infix: fn,
    method: null,
    precedence: prec,
    name: null
  };
}
function INFIX_OPERATOR(prec, name) {
  return {
    prefix: null,
    infix: infixOp,
    method: infixSignature,
    precedence: prec,
    name: name
  };
}
function PREFIX_OPERATOR(name) {
  return {
    prefix: unaryOp,
    infix: null,
    method: unarySignature,
    precedence: Precedence.PREC_NONE,
    name: name
  };
}
function OPERATOR(name) {
  return {
    prefix: unaryOp,
    infix: infixOp,
    method: mixedSignature,
    precedence: Precedence.PREC_TERM,
    name: name
  };
}

var rules = [
  /* TOKEN_LEFT_PAREN    */ PREFIX(grouping),
  /* TOKEN_RIGHT_PAREN   */ UNUSED(),
  /* TOKEN_LEFT_BRACKET  */ { prefix: list,
                              infix: subscript,
                              method: subscriptSignature,
                              precedence: Precedence.PREC_CALL,
                              name: null
                            },
  /* TOKEN_RIGHT_BRACKET */ UNUSED(),
  /* TOKEN_LEFT_BRACE    */ PREFIX(map),
  /* TOKEN_RIGHT_BRACE   */ UNUSED(),
  /* TOKEN_COLON         */ UNUSED(),
  /* TOKEN_DOT           */ INFIX(Precedence.PREC_CALL, call),
  /* TOKEN_DOTDOT        */ INFIX_OPERATOR(Precedence.PREC_RANGE, ".."),
  /* TOKEN_DOTDOTDOT     */ INFIX_OPERATOR(Precedence.PREC_RANGE, "..."),
  /* TOKEN_COMMA         */ UNUSED(),
  /* TOKEN_STAR          */ INFIX_OPERATOR(Precedence.PREC_FACTOR, "*"),
  /* TOKEN_SLASH         */ INFIX_OPERATOR(Precedence.PREC_FACTOR, "/"),
  /* TOKEN_PERCENT       */ INFIX_OPERATOR(Precedence.PREC_FACTOR, "%"),
  /* TOKEN_PLUS          */ INFIX_OPERATOR(Precedence.PREC_TERM, "+"),
  /* TOKEN_MINUS         */ OPERATOR("-"),
  /* TOKEN_LTLT          */ INFIX_OPERATOR(Precedence.PREC_BITWISE_SHIFT, "<<"),
  /* TOKEN_GTGT          */ INFIX_OPERATOR(Precedence.PREC_BITWISE_SHIFT, ">>"),
  /* TOKEN_PIPE          */ INFIX_OPERATOR(Precedence.PREC_BITWISE_OR, "|"),
  /* TOKEN_PIPEPIPE      */ INFIX(Precedence.PREC_LOGICAL_OR, or_),
  /* TOKEN_CARET         */ INFIX_OPERATOR(Precedence.PREC_BITWISE_XOR, "^"),
  /* TOKEN_AMP           */ INFIX_OPERATOR(Precedence.PREC_BITWISE_AND, "&"),
  /* TOKEN_AMPAMP        */ INFIX(Precedence.PREC_LOGICAL_AND, and_),
  /* TOKEN_BANG          */ PREFIX_OPERATOR("!"),
  /* TOKEN_TILDE         */ PREFIX_OPERATOR("~"),
  /* TOKEN_QUESTION      */ INFIX(Precedence.PREC_ASSIGNMENT, conditional),
  /* TOKEN_EQ            */ UNUSED(),
  /* TOKEN_LT            */ INFIX_OPERATOR(Precedence.PREC_COMPARISON, "<"),
  /* TOKEN_GT            */ INFIX_OPERATOR(Precedence.PREC_COMPARISON, ">"),
  /* TOKEN_LTEQ          */ INFIX_OPERATOR(Precedence.PREC_COMPARISON, "<="),
  /* TOKEN_GTEQ          */ INFIX_OPERATOR(Precedence.PREC_COMPARISON, ">="),
  /* TOKEN_EQEQ          */ INFIX_OPERATOR(Precedence.PREC_EQUALITY, "=="),
  /* TOKEN_BANGEQ        */ INFIX_OPERATOR(Precedence.PREC_EQUALITY, "!="),
  /* TOKEN_BREAK         */ UNUSED(),
  /* TOKEN_CLASS         */ UNUSED(),
  /* TOKEN_CONSTRUCT     */ { prefix: null,
                              infix: null,
                              method: constructorSignature,
                              precendence: Precedence.PREC_NONE,
                              name: null
                            },
  /* TOKEN_ELSE          */ UNUSED(),
  /* TOKEN_FALSE         */ PREFIX(boolean),
  /* TOKEN_FOR           */ UNUSED(),
  /* TOKEN_FOREIGN       */ UNUSED(),
  /* TOKEN_IF            */ UNUSED(),
  /* TOKEN_IMPORT        */ UNUSED(),
  /* TOKEN_IN            */ UNUSED(),
  /* TOKEN_IS            */ INFIX_OPERATOR(Precedence.PREC_IS, "is"),
  /* TOKEN_NULL          */ PREFIX(null_),
  /* TOKEN_RETURN        */ UNUSED(),
  /* TOKEN_STATIC        */ UNUSED(),
  /* TOKEN_SUPER         */ PREFIX(super_),
  /* TOKEN_THIS          */ PREFIX(this_),
  /* TOKEN_TRUE          */ PREFIX(boolean),
  /* TOKEN_VAR           */ UNUSED(),
  /* TOKEN_WHILE         */ UNUSED(),
  /* TOKEN_FIELD         */ PREFIX(field),
  /* TOKEN_STATIC_FIELD  */ PREFIX(staticField),
  /* TOKEN_NAME          */ { prefix: name,
                              infix: null,
                              method: namedSignature,
                              precedence: Precedence.PREC_NONE,
                              name: null
                            },
  /* TOKEN_NUMBER        */ PREFIX(literal),
  /* TOKEN_STRING        */ PREFIX(literal),
  /* TOKEN_INTERPOLATION */ PREFIX(stringInterpolation),
  /* TOKEN_LINE          */ UNUSED(),
  /* TOKEN_ERROR         */ UNUSED(),
  /* TOKEN_EOF           */ UNUSED()
];

// Gets the [GrammarRule] associated with tokens of [type].
function getRule(type)
{
  return rules[type];
}

// The main entrypoint for the top-down operator precedence parser.
function parsePrecedence(compiler, precedence) {
  nextToken(compiler.parser);
  var prefix = rules[compiler.parser.previous.type].prefix;

  if (prefix === null) {
    error(compiler, "Expected expression.");
    return;
  }

  // Track if the precendence of the surrounding expression is low enough to
  // allow an assignment inside this one. We can't compile an assignment like
  // a normal expression because it requires us to handle the LHS specially --
  // it needs to be an lvalue, not an rvalue. So, for each of the kinds of
  // expressions that are valid lvalues -- names, subscripts, fields, etc. --
  // we pass in whether or not it appears in a context loose enough to allow
  // "=". If so, it will parse the "=" itself and handle it appropriately.
  var canAssign = precedence <= Precedence.PREC_CONDITIONAL;
  prefix(compiler, canAssign);

  while (precedence <= rules[compiler.parser.current.type].precedence) {
    nextToken(compiler.parser);
    var infix = rules[compiler.parser.previous.type].infix;
    infix(compiler, canAssign);
  }
}

// Parses an expression. Unlike statements, expressions leave a resulting value
// on the stack.
function expression(compiler) {
  parsePrecedence(compiler, Precedence.PREC_LOWEST);
}

// Returns the number of arguments to the instruction at [ip] in [fn]'s
// bytecode.
function getNumArguments(bytecode, constants, ip) {
  var instruction = bytecode[ip];
  switch (instruction) {
    case Code.NULL:
    case Code.FALSE:
    case Code.TRUE:
    case Code.POP:
    case Code.DUP:
    case Code.CLOSE_UPVALUE:
    case Code.RETURN:
    case Code.END:
    case Code.LOAD_LOCAL_0:
    case Code.LOAD_LOCAL_1:
    case Code.LOAD_LOCAL_2:
    case Code.LOAD_LOCAL_3:
    case Code.LOAD_LOCAL_4:
    case Code.LOAD_LOCAL_5:
    case Code.LOAD_LOCAL_6:
    case Code.LOAD_LOCAL_7:
    case Code.LOAD_LOCAL_8:
    case Code.CONSTRUCT:
    case Code.FOREIGN_CONSTRUCT:
    case Code.FOREIGN_CLASS:
      return 0;

    case Code.LOAD_LOCAL:
    case Code.STORE_LOCAL:
    case Code.LOAD_UPVALUE:
    case Code.STORE_UPVALUE:
    case Code.LOAD_FIELD_THIS:
    case Code.STORE_FIELD_THIS:
    case Code.LOAD_FIELD:
    case Code.STORE_FIELD:
    case Code.CLASS:
      return 1;

    case Code.CONSTANT:
    case Code.LOAD_MODULE_VAR:
    case Code.STORE_MODULE_VAR:
    case Code.CALL_0:
    case Code.CALL_1:
    case Code.CALL_2:
    case Code.CALL_3:
    case Code.CALL_4:
    case Code.CALL_5:
    case Code.CALL_6:
    case Code.CALL_7:
    case Code.CALL_8:
    case Code.CALL_9:
    case Code.CALL_10:
    case Code.CALL_11:
    case Code.CALL_12:
    case Code.CALL_13:
    case Code.CALL_14:
    case Code.CALL_15:
    case Code.CALL_16:
    case Code.JUMP:
    case Code.LOOP:
    case Code.JUMP_IF:
    case Code.AND:
    case Code.OR:
    case Code.METHOD_INSTANCE:
    case Code.METHOD_STATIC:
      return 2;

    case Code.SUPER_0:
    case Code.SUPER_1:
    case Code.SUPER_2:
    case Code.SUPER_3:
    case Code.SUPER_4:
    case Code.SUPER_5:
    case Code.SUPER_6:
    case Code.SUPER_7:
    case Code.SUPER_8:
    case Code.SUPER_9:
    case Code.SUPER_10:
    case Code.SUPER_11:
    case Code.SUPER_12:
    case Code.SUPER_13:
    case Code.SUPER_14:
    case Code.SUPER_15:
    case Code.SUPER_16:
      return 4;

    case Code.CLOSURE: {
      var constant = (bytecode[ip + 1] << 8) | bytecode[ip + 2];
      var loadedFn = AS_FN(constants[constant]);

      // There are two bytes for the constant, then two for each upvalue.
      return 2 + (loadedFn.numUpvalues * 2);
    }

    default:
      UNREACHABLE();
      return 0;
  }
}

// Marks the beginning of a loop. Keeps track of the current instruction so we
// know what to loop back to at the end of the body.
function startLoop(compiler, loop) {
  loop.enclosing = compiler.loop;
  loop.start = compiler.fn.code.count - 1;
  loop.scopeDepth = compiler.scopeDepth;
  compiler.loop = loop;
}

// Emits the [Code.JUMP_IF] instruction used to test the loop condition and
// potentially exit the loop. Keeps track of the instruction so we can patch it
// later once we know where the end of the body is.
function testExitLoop(compiler) {
  compiler.loop.exitJump = emitJump(compiler, Code.JUMP_IF);
}

// Compiles the body of the loop and tracks its extent so that contained "break"
// statements can be handled correctly.
function loopBody(compiler) {
  compiler.loop.body = compiler.fn.code.count;
  statement(compiler);
}

// Ends the current innermost loop. Patches up all jumps and breaks now that
// we know where the end of the loop is.
function endLoop(compiler) {
  // We don't check for overflow here since the forward jump over the loop body
  // will report an error for the same problem.
  var loopOffset = compiler.fn.code.count - compiler.loop.start + 2;
  emitShortArg(compiler, Code.LOOP, loopOffset);

  patchJump(compiler, compiler.loop.exitJump);

  // Find any break placeholder instructions (which will be Code.END in the
  // bytecode) and replace them with real jumps.
  var i = compiler.loop.body;
  while (i < compiler.fn.code.count) {
    if (compiler.fn.code.data[i] === Code.END) {
      compiler.fn.code.data[i] = Code.JUMP;
      patchJump(compiler, i + 1);
      i += 3;
    } else {
      // Skip this instruction and its arguments.
      i += 1 + getNumArguments(compiler.fn.code.data,
                               compiler.fn.constants.data, i);
    }
  }

  compiler.loop = compiler.loop.enclosing;
}

function forStatement(compiler) {
  // A for statement like:
  //
  //     for (i in sequence.expression) {
  //       System.print(i)
  //     }
  //
  // Is compiled to bytecode almost as if the source looked like this:
  //
  //     {
  //       var seq_ = sequence.expression
  //       var iter_
  //       while (iter_ = seq_.iterate(iter_)) {
  //         var i = seq_.iteratorValue(iter_)
  //         System.print(i)
  //       }
  //     }
  //
  // It's not exactly this, because the synthetic variables `seq_` and `iter_`
  // actually get names that aren't valid Wren identfiers, but that's the basic
  // idea.
  //
  // The important parts are:
  // - The sequence expression is only evaluated once.
  // - The .iterate() method is used to advance the iterator and determine if
  //   it should exit the loop.
  // - The .iteratorValue() method is used to get the value at the current
  //   iterator position.

  // Create a scope for the hidden local variables used for the iterator.
  pushScope(compiler);

  consume(compiler, TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  consume(compiler, TokenType.TOKEN_NAME, "Expect for loop variable name.");

  // Remember the name of the loop variable.
  var name = compiler.parser.previous.start;
  var length = compiler.parser.previous.length;

  consume(compiler, TokenType.TOKEN_IN, "Expect 'in' after loop variable.");
  ignoreNewlines(compiler);

  // Evaluate the sequence expression and store it in a hidden local variable.
  // The space in the variable name ensures it won't collide with a user-defined
  // variable.
  expression(compiler);
  var seqSlot = addLocal(compiler, "seq ", 4);

  // Create another hidden local for the iterator object.
  null_(compiler, false);
  var iterSlot = addLocal(compiler, "iter ", 5);

  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.");

  var loop;
  startLoop(compiler, loop);

  // Advance the iterator by calling the ".iterate" method on the sequence.
  loadLocal(compiler, seqSlot);
  loadLocal(compiler, iterSlot);

  // Update and test the iterator.
  callMethod(compiler, 1, "iterate(_)", 10);
  emitByteArg(compiler, Code.STORE_LOCAL, iterSlot);
  testExitLoop(compiler);

  // Get the current value in the sequence by calling ".iteratorValue".
  loadLocal(compiler, seqSlot);
  loadLocal(compiler, iterSlot);
  callMethod(compiler, 1, "iteratorValue(_)", 16);

  // Bind the loop variable in its own scope. This ensures we get a fresh
  // variable each iteration so that closures for it don't all see the same one.
  pushScope(compiler);
  addLocal(compiler, name, length);

  loopBody(compiler);

  // Loop variable.
  popScope(compiler);

  endLoop(compiler);

  // Hidden variables.
  popScope(compiler);
}

function whileStatement(compiler) {
  var loop;
  startLoop(compiler, loop);

  // Compile the condition.
  consume(compiler, TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler);
  consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

  testExitLoop(compiler);
  loopBody(compiler);
  endLoop(compiler);
}

// Compiles a simple statement. These can only appear at the top-level or
// within curly blocks. Simple statements exclude variable binding statements
// like "var" and "class" which are not allowed directly in places like the
// branches of an "if" statement.
//
// Unlike expressions, statements do not leave a value on the stack.
function statement(compiler) {
  if (match(compiler, TokenType.TOKEN_BREAK)) {
    if (compiler.loop === null) {
      error(compiler, "Cannot use 'break' outside of a loop.");
      return;
    }

    // Since we will be jumping out of the scope, make sure any locals in it
    // are discarded first.
    discardLocals(compiler, compiler.loop.scopeDepth + 1);

    // Emit a placeholder instruction for the jump to the end of the body. When
    // we're done compiling the loop body and know where the end is, we'll
    // replace these with `Code.JUMP` instructions with appropriate offsets.
    // We use `Code.END` here because that can't occur in the middle of
    // bytecode.
    emitJump(compiler, Code.END);
    return;
  }

  if (match(compiler, TokenType.TOKEN_FOR)) {
    forStatement(compiler);
    return;
  }

  if (match(compiler, TokenType.TOKEN_IF)) {
    // Compile the condition.
    consume(compiler, TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(compiler);
    consume(compiler, TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

    // Jump to the else branch if the condition is false.
    var ifJump = emitJump(compiler, Code.JUMP_IF);

    // Compile the then branch.
    statement(compiler);

    // Compile the else branch if there is one.
    if (match(compiler, TokenType.TOKEN_ELSE)) {
      // Jump over the else branch when the if branch is taken.
      var elseJump = emitJump(compiler, Code.JUMP);
      patchJump(compiler, ifJump);

      statement(compiler);

      // Patch the jump over the else.
      patchJump(compiler, elseJump);
    } else {
      patchJump(compiler, ifJump);
    }

    return;
  }

  if (match(compiler, TokenType.TOKEN_RETURN)) {
    // Compile the return value.
    if (peek(compiler) === TokenType.TOKEN_LINE) {
      // Implicitly return null if there is no value.
      emitOp(compiler, Code.NULL);
    } else {
      expression(compiler);
    }

    emitOp(compiler, Code.RETURN);
    return;
  }

  if (match(compiler, TokenType.TOKEN_WHILE)) {
    whileStatement(compiler);
    return;
  }

  // Block statement.
  if (match(compiler, TokenType.TOKEN_LEFT_BRACE)) {
    pushScope(compiler);
    if (finishBlock(compiler)) {
      // Block was an expression, so discard it.
      emitOp(compiler, Code.POP);
    }
    popScope(compiler);
    return;
  }

  // Expression statement.
  expression(compiler);
  emitOp(compiler, Code.POP);
}

// Creates a matching constructor method for an initializer with [signature]
// and [initializerSymbol].
//
// Construction is a two-stage process in Wren that involves two separate
// methods. There is a static method that allocates a new instance of the class.
// It then invokes an initializer method on the new instance, forwarding all of
// the constructor arguments to it.
//
// The allocator method always has a fixed implementation:
//
//     Code.CONSTRUCT - Replace the class in slot 0 with a new instance of it.
//     Code.CALL      - Invoke the initializer on the new instance.
//
// This creates that method and calls the initializer with [initializerSymbol].
function createConstructor(compiler, signature, initializerSymbol) {
  var methodCompiler;
  initCompiler(methodCompiler, compiler.parser, compiler, false);

  // Allocate the instance.
  emitOp(methodCompiler,
    compiler.enclosingClass.isForeign ? Code.FOREIGN_CONSTRUCT : Code.CONSTRUCT);

  // Run its initializer.
  emitShortArg(methodCompiler, (Code.CALL_0 + signature.arity),
               initializerSymbol);

  // Return the instance.
  emitOp(methodCompiler, Code.RETURN);

  endCompiler(methodCompiler, "", 0);
}

// Loads the enclosing class onto the stack and then binds the function already
// on the stack as a method on that class.
function defineMethod(compiler, classVariable, isStatic, methodSymbol) {
  // Load the class. We have to do this for each method because we can't
  // keep the class on top of the stack. If there are static fields, they
  // will be locals above the initial variable slot for the class on the
  // stack. To skip past those, we just load the class each time right before
  // defining a method.
  loadVariable(compiler, classVariable);

  // Define the method.
  var instruction = isStatic ? Code.METHOD_STATIC : Code.METHOD_INSTANCE;
  emitShortArg(compiler, instruction, methodSymbol);
}

// Compiles a method definition inside a class body.
//
// Returns `true` if it compiled successfully, or `false` if the method couldn't
// be parsed.
function method(compiler, classVariable) {
  // TODO: What about foreign constructors?
  var isForeign = match(compiler, TokenType.TOKEN_FOREIGN);
  compiler.enclosingClass.inStatic = match(compiler, TokenType.TOKEN_STATIC);

  var signatureFn = rules[compiler.parser.current.type].method;
  nextToken(compiler.parser);

  if (signatureFn === null) {
    error(compiler, "Expect method definition.");
    return false;
  }

  // Build the method signature.
  var signature = signatureFromToken(compiler, SignatureType.SIG_GETTER);
  compiler.enclosingClass.signature = signature;

  var methodCompiler;
  initCompiler(methodCompiler, compiler.parser, compiler, false);

  // Compile the method signature.
  signatureFn(methodCompiler, signature);

  if (compiler.enclosingClass.inStatic &&
    signature.type === SignatureType.SIG_INITIALIZER) {
    error(compiler, "A constructor cannot be static.");
  }

  // Include the full signature in debug messages in stack traces.
  var fullSignature = [];
  var length;
  signatureToString(signature, fullSignature, length);

  if (isForeign) {
    // Define a constant for the signature.
    emitConstant(compiler, wrenNewString(compiler.parser.vm,
                                         fullSignature, length));

    // We don't need the function we started compiling in the parameter list
    // any more.
    methodCompiler.parser.vm.compiler = methodCompiler.parent;
  } else {
    consume(compiler, TokenType.TOKEN_LEFT_BRACE, "Expect '{' to begin method body.");
    finishBody(methodCompiler, signature.type === SignatureType.SIG_INITIALIZER);
    endCompiler(methodCompiler, fullSignature, length);
  }

  // Define the method. For a constructor, this defines the instance
  // initializer method.
  var methodSymbol = signatureSymbol(compiler, signature);
  defineMethod(compiler, classVariable, compiler.enclosingClass.inStatic,
               methodSymbol);

  if (signature.type === SignatureType.SIG_INITIALIZER) {
    // Also define a matching constructor method on the metaclass.
    signature.type = SignatureType.SIG_METHOD;
    var constructorSymbol = signatureSymbol(compiler, signature);

    createConstructor(compiler, signature, methodSymbol);
    defineMethod(compiler, classVariable, true, constructorSymbol);
  }

  return true;
}

// Compiles a class definition. Assumes the "class" token has already been
// consumed (along with a possibly preceding "foreign" token).
function classDefinition(compiler, isForeign) {
  // Create a variable to store the class in.
  var classVariable;
  classVariable.scope = compiler.scopeDepth === -1 ? Scope.SCOPE_MODULE : Scope.SCOPE_LOCAL;
  classVariable.index = declareNamedVariable(compiler);

  // Make a string constant for the name.
  emitConstant(compiler, wrenNewString(compiler.parser.vm,
      compiler.parser.previous.start, compiler.parser.previous.length));

  // Load the superclass (if there is one).
  if (match(compiler, TokenType.TOKEN_IS)) {
    parsePrecedence(compiler, Precedence.PREC_CALL);
  } else {
    // Implicitly inherit from Object.
    loadCoreVariable(compiler, "Object");
  }

  // Store a placeholder for the number of fields argument. We don't know
  // the value until we've compiled all the methods to see which fields are
  // used.
  var numFieldsInstruction = -1;
  if (isForeign) {
    emitOp(compiler, Code.FOREIGN_CLASS);
  } else {
    numFieldsInstruction = emitByteArg(compiler, Code.CLASS, 255);
  }

  // Store it in its name.
  defineVariable(compiler, classVariable.index);

  // Push a local variable scope. Static fields in a class body are hoisted out
  // into local variables declared in this scope. Methods that use them will
  // have upvalues referencing them.
  pushScope(compiler);

  var classCompiler;
  classCompiler.isForeign = isForeign;

  // Set up a symbol table for the class's fields. We'll initially compile
  // them to slots starting at zero. When the method is bound to the class, the
  // bytecode will be adjusted by [wrenBindMethod] to take inherited fields
  // into account.
  wrenSymbolTableInit(classCompiler.fields);
  compiler.enclosingClass = classCompiler;

  // Compile the method definitions.
  consume(compiler, TokenType.TOKEN_LEFT_BRACE, "Expect '{' after class declaration.");
  matchLine(compiler);

  while (!match(compiler, TokenType.TOKEN_RIGHT_BRACE)) {
    if (!method(compiler, classVariable)) {
      break;
    }

    // Don't require a newline after the last definition.
    if (match(compiler, TokenType.TOKEN_RIGHT_BRACE)) {
      break;
    }

    consumeLine(compiler, "Expect newline after definition in class.");
  }

  // Update the class with the number of fields.
  if (!isForeign) {
    compiler.fn.code.data[numFieldsInstruction] = classCompiler.fields.count;
  }

  wrenSymbolTableClear(compiler.parser.vm, classCompiler.fields);
  compiler.enclosingClass = null;
  popScope(compiler);
}

// Compiles an "import" statement.
//
// An import just desugars to calling a few special core methods. Given:
//
//     import "foo" for Bar, Baz
//
// We compile it to:
//
//     System.importModule("foo")
//     var Bar = System.getModuleVariable("foo", "Bar")
//     var Baz = System.getModuleVariable("foo", "Baz")
function import_(compiler) {
  ignoreNewlines(compiler);
  consume(compiler, TokenType.TOKEN_STRING, "Expect a string after 'import'.");
  var moduleConstant = addConstant(compiler, compiler.parser.previous.value);

  // Load the module.
  loadCoreVariable(compiler, "System");
  emitShortArg(compiler, Code.CONSTANT, moduleConstant);
  callMethod(compiler, 1, "importModule(_)", 15);

  // Discard the unused result value from calling the module's fiber.
  emitOp(compiler, Code.POP);

  // The for clause is optional.
  if (!match(compiler, TokenType.TOKEN_FOR)) {
    return;
  }

  // Compile the comma-separated list of variables to import.
  do
  {
    ignoreNewlines(compiler);
    var slot = declareNamedVariable(compiler);

    // Define a string constant for the variable name.
    var variableConstant = addConstant(compiler,
        wrenNewString(compiler.parser.vm,
                      compiler.parser.previous.start,
                      compiler.parser.previous.length));

    // Load the variable from the other module.
    loadCoreVariable(compiler, "System");
    emitShortArg(compiler, Code.CONSTANT, moduleConstant);
    emitShortArg(compiler, Code.CONSTANT, variableConstant);
    callMethod(compiler, 2, "getModuleVariable(_,_)", 22);

    // Store the result in the variable here.
    defineVariable(compiler, slot);
  } while (match(compiler, TokenType.TOKEN_COMMA));
}

// Compiles a "var" variable definition statement.
function variableDefinition(compiler) {
  // Grab its name, but don't declare it yet. A (local) variable shouldn't be
  // in scope in its own initializer.
  consume(compiler, TokenType.TOKEN_NAME, "Expect variable name.");
  var nameToken = compiler.parser.previous;

  // Compile the initializer.
  if (match(compiler, TokenType.TOKEN_EQ)) {
    expression(compiler);
  } else {
    // Default initialize it to null.
    null_(compiler, false);
  }

  // Now put it in scope.
  var symbol = declareVariable(compiler, nameToken);
  defineVariable(compiler, symbol);
}

// Compiles a "definition". These are the statements that bind new variables.
// They can only appear at the top level of a block and are prohibited in places
// like the non-curly body of an if or while.
function definition( compiler) {
  if (match(compiler, TokenType.TOKEN_CLASS)) {
    classDefinition(compiler, false);
  } else if (match(compiler, TokenType.TOKEN_FOREIGN)) {
    consume(compiler, TokenType.TOKEN_CLASS, "Expect 'class' after 'foreign'.");
    classDefinition(compiler, true);
  } else if (match(compiler, TokenType.TOKEN_IMPORT)) {
    import_(compiler);
  } else if (match(compiler, TokenType.TOKEN_VAR)) {
    variableDefinition(compiler);
  } else {
    statement(compiler);
  }
}

function wrenCompile(vm, module, source, printErrors) {
  var parser = Parser();
  parser.vm = vm;
  parser.module = module;
  parser.source = source;

  parser.tokenStart = source;
  parser.currentChar = source;
  parser.currentLine = 1;
  parser.numParens = 0;

  console.log(parser);

  // Zero-init the current token. This will get copied to previous when
  // advance() is called below.
  parser.current.type = TokenType.TOKEN_ERROR;
  parser.current.start = source;
  parser.current.length = 0;
  parser.current.line = 0;
  parser.current.value = UNDEFINED_VAL;

  // Ignore leading newlines.
  parser.skipNewlines = true;
  parser.printErrors = printErrors;
  parser.hasError = false;

  // Read the first token.
  nextToken(parser);

  var compiler;
  initCompiler(compiler, parser, null, true);
  ignoreNewlines(compiler);

  while (!match(compiler, TokenType.TOKEN_EOF)) {
    definition(compiler);

    // If there is no newline, it must be the end of the block on the same line.
    if (!matchLine(compiler)) {
      consume(compiler, TokenType.TOKEN_EOF, "Expect end of file.");
      break;
    }
  }

  emitOp(compiler, Code.NULL);
  emitOp(compiler, Code.RETURN);

  // See if there are any implicitly declared module-level variables that never
  // got an explicit definition.
  // TODO: It would be nice if the error was on the line where it was used.
  for (var i = 0; i < parser.module.variables.count; i++) {
    if (IS_UNDEFINED(parser.module.variables.data[i])) {
      error(compiler, "Variable '%x' is used but not defined.",
            parser.module.variableNames.data[i].buffer);
    }
  }

  return endCompiler(compiler, "(script)", 8);
}

function wrenBindMethodCode(classObj, fn) {
  var ip = 0;
  for (;;) {
    var instruction = fn.code.data[ip++];
    switch (instruction) {
      case Code.LOAD_FIELD:
      case Code.STORE_FIELD:
      case Code.LOAD_FIELD_THIS:
      case Code.STORE_FIELD_THIS:
        // Shift this class's fields down past the inherited ones. We don't
        // check for overflow here because we'll see if the number of fields
        // overflows when the subclass is created.
        fn.code.data[ip++] += classObj.superclass.numFields;
        break;

      case Code.SUPER_0:
      case Code.SUPER_1:
      case Code.SUPER_2:
      case Code.SUPER_3:
      case Code.SUPER_4:
      case Code.SUPER_5:
      case Code.SUPER_6:
      case Code.SUPER_7:
      case Code.SUPER_8:
      case Code.SUPER_9:
      case Code.SUPER_10:
      case Code.SUPER_11:
      case Code.SUPER_12:
      case Code.SUPER_13:
      case Code.SUPER_14:
      case Code.SUPER_15:
      case Code.SUPER_16: {
        // Skip over the symbol.
        ip += 2;

        // Fill in the constant slot with a reference to the superclass.
        var constant = (fn.code.data[ip] << 8) | fn.code.data[ip + 1];
        fn.constants.data[constant] = OBJ_VAL(classObj.superclass);
        break;
      }

      case Code.CLOSURE: {
        // Bind the nested closure too.
        // JS lint complains because of the previous constant?
        var constant2 = (fn.code.data[ip] << 8) | fn.code.data[ip + 1];
        wrenBindMethodCode(classObj, AS_FN(fn.constants.data[constant2]));

        ip += getNumArguments(fn.code.data, fn.constants.data, ip - 1);
        break;
      }

      case CODE_END:
        return;

      default:
        // Other instructions are unaffected, so just skip over them.
        ip += getNumArguments(fn.code.data, fn.constants.data, ip - 1);
        break;
    }
  }
}

function wrenMarkCompiler(vm, compiler) {
  wrenGrayValue(vm, compiler.parser.current.value);
  wrenGrayValue(vm, compiler.parser.previous.value);

  // Walk up the parent chain to mark the outer compilers too. The VM only
  // tracks the innermost one.
  do {
    wrenGrayObj(vm, compiler.fn);
    compiler = compiler.parent;
  } while (compiler !== null);
}

////////////////////////////////////////////////////////////////////////////////
// Adding properties to this object will make them available to outside scripts.
module.exports = {
  sCompiler: sCompiler,

  // Compiles [source], a string of Wren source code located in [module], to an
  // [ObjFn] that will execute that code when invoked. Returns `null` if the
  // source contains any syntax errors.
  //
  // If [printErrors] is `true`, any compile errors are output to stderr.
  // Otherwise, they are silently discarded.
  wrenCompile: wrenCompile,

  // When a class is defined, its superclass is not known until runtime since
  // class definitions are just imperative statements. Most of the bytecode for a
  // a method doesn't care, but there are two places where it matters:
  //
  //   - To load or store a field, we need to know the index of the field in the
  //     instance's field array. We need to adjust this so that subclass fields
  //     are positioned after superclass fields, and we don't know this until the
  //     superclass is known.
  //
  //   - Superclass calls need to know which superclass to dispatch to.
  //
  // We could handle this dynamically, but that adds overhead. Instead, when a
  // method is bound, we walk the bytecode for the function and patch it up.
  wrenBindMethodCode: wrenBindMethodCode,

  // Reaches all of the heap-allocated objects in use by [compiler] (and all of
  // its parents) so that they are not collected by the GC.
  wrenMarkCompiler: wrenMarkCompiler
};
