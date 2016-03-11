var printf = require('./c/printf');
module.value = require('./value');

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

// Adding properties to this object will make them available to outside scripts.
module.exports = {
  sCompiler: sCompiler,

  // Compiles [source], a string of Wren source code located in [module], to an
  // [ObjFn] that will execute that code when invoked. Returns `NULL` if the
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

// This is written in bottom-up order, so the tokenization comes first, then
// parsing/code generation. This minimizes the number of explicit forward
// declarations needed.

// The maximum number of local (i.e. not module level) variables that can be
// declared in a single function, method, or chunk of top level code. This is
// the maximum number of variables in scope at one time, and spans block scopes.
//
// Note that this limitation is also explicit in the bytecode. Since
// `CODE_LOAD_LOCAL` and `CODE_STORE_LOCAL` use a single argument byte to
// identify the local, only 256 can be in scope at one time.
var MAX_LOCALS = 256;

// The maximum number of upvalues (i.e. variables from enclosing functions)
// that a function can close over.
var MAX_UPVALUES = 256;

// The maximum number of distinct constants that a function can contain. This
// value is explicit in the bytecode since `CODE_CONSTANT` only takes a single
// two-byte argument.
var MAX_CONSTANTS = (1 << 16);

// The maximum distance a CODE_JUMP or CODE_JUMP_IF instruction can move the
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

var Token = {
  type: undefined,

  // The beginning of the token, pointing directly into the source.
  start: undefined,

  // The length of the token in characters.
  length: undefined,

  // The 1-based line where the token appears.
  line: undefined,

  // The parsed value if the token is a literal.
  value: undefined
};

var Parser = {
  vm: undefined,

  // The module being parsed.
  module: undefined,

  // The source code being parsed.
  source: undefined,

  // The beginning of the currently-being-lexed token in [source].
  tokenStart: undefined,

  // The current character being lexed in [source].
  currentChar: undefined,

  // The 1-based line number of [currentChar].
  currentLine: undefined,

  // The most recently lexed token.
  current: undefined,

  // The most recently consumed/advanced token.
  previous: undefined,

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
  numParens: undefined,

  // If subsequent newline tokens should be discarded.
  skipNewlines: undefined,

  // Whether compile errors should be printed to stderr or discarded.
  printErrors: undefined,

  // If a syntax or compile error has occurred.
  hasError: undefined
};

var Local = {
  // The name of the local variable. This points directly into the original
  // source code string.
  name: undefined,

  // The length of the local variable's name.
  length: undefined,

  // The depth in the scope chain that this variable was declared at. Zero is
  // the outermost scope--parameters for a method, or the first local block in
  // top level code. One is the scope within that, etc.
  depth: undefined,

  // If this local variable is being used as an upvalue.
  isUpvalue: undefined
};

var CompilerUpvalue = {
  // True if this upvalue is capturing a local variable from the enclosing
  // function. False if it's capturing an upvalue.
  isLocal: undefined,

  // The index of the local or upvalue being captured in the enclosing function.
  index: undefined
};

// Bookkeeping information for the current loop being compiled.
var Loop = {
  // Index of the instruction that the loop should jump back to.
  start: undefined,

  // Index of the argument for the CODE_JUMP_IF instruction used to exit the
  // loop. Stored so we can patch it once we know where the loop ends.
  exitJump: undefined,

  // Index of the first instruction of the body of the loop.
  body: undefined,

  // Depth of the scope(s) that need to be exited if a break is hit inside the
  // loop.
  scopeDepth: undefined,

  // The loop enclosing this one, or NULL if this is the outermost loop.
  enclosing: undefined
};

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

var Signature = {
  name: undefined,
  length: undefined,
  type: undefined,
  arity: undefined
};

// Bookkeeping information for compiling a class definition.
var ClassCompiler = {
  // Symbol table for the fields of the class.
  fields: undefined,

  // True if the class being compiled is a foreign class.
  isForeign: undefined,

  // True if the current method being compiled is static.
  inStatic: undefined,

  // The signature of the method being compiled.
  signature: undefined
};

var sCompiler = {
  parser: undefined,

  // The compiler for the function enclosing this one, or NULL if it's the
  // top level.
  parent: undefined,

  // The currently in scope local variables.
  locals: [],

  // The number of local variables currently in scope.
  numLocals: undefined,

  // The upvalues that this function has captured from outer scopes. The count
  // of them is stored in [numUpvalues].
  upvalues: [],

  // The current level of block scope nesting, where zero is no nesting. A -1
  // here means top-level code is being compiled and there is no block scope
  // in effect at all. Any variables declared will be module-level.
  copeDepth: undefined,

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
  numSlots: undefined,

  // The current innermost loop being compiled, or NULL if not in a loop.
  loop: undefined,

  // If this is a compiler for a method, keeps track of the class enclosing it.
  enclosingClass: undefined,

  // The function being compiled.
  fn: undefined
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

// A reference to a variable and the scope where it is defined. This contains
// enough information to emit correct code to load or store the variable.
var Variable = {
  // The stack slot, upvalue slot, or module symbol defining the variable.
  index: undefined,

  // Where the variable is declared.
  scope: undefined
};

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
    err += printf("'%x': ", token.length, token.start);
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

  // Initialize this to NULL before allocating in case a GC gets triggered in
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
      lexError(parser, "Invalid %s escape sequence.", description);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
function readUnicodeEscape(parser, string, length)
{
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
  var string;
  type = TokenType.TOKEN_STRING;
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
        while (peekChar(parser) == ' ' ||
               peekChar(parser) == '\r' ||
               peekChar(parser) == '\t') {
          nextChar(parser);
        }
        break;

      case '"': readString(parser); return;
      case '_':
        readName(parser,
                 peekChar(parser) == '_' ? TokenType.TOKEN_STATIC_FIELD : TokenType.TOKEN_FIELD);
        return;

      case '#':
        // Ignore shebang on the first line.
        if (peekChar(parser) == '!' && parser.currentLine == 1) {
          skipLineComment(parser);
          break;
        }

        lexError(parser, "Invalid character '%x'.", c);
        return;

      case '0':
        if (peekChar(parser) == 'x') {
          readHexNumber(parser);
          return;
        }

        readNumber(parser);
        return;

      default:
        if (isName(c)) {
          readName(parser, TOKEN_NAME);
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
  makeToken(parser, TOKEN_EOF);
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
function matchLine(compiler)
{
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
  consume(compiler, TOKEN_LINE, errorMessage);
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
  emitShortArg(compiler, CODE_CONSTANT, constant);
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
// If [token] is `NULL`, uses the previously consumed token. Returns its symbol.
function declareVariable(compiler, token) {
  if (token === NULL) {
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

    if (symbol == -1) {
      error(compiler, "Module variable is already defined.");
    } else if (symbol == -2) {
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

    if (local.length == token.length && local.name === token.start) {
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
  emitShortArg(compiler, CODE_STORE_MODULE_VAR, symbol);
  emitOp(compiler, CODE_POP);
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
      emitByte(compiler, CODE_CLOSE_UPVALUE);
    } else {
      emitByte(compiler, CODE_POP);
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
    if (compiler.locals[i].length == length &&
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
  if (local != -1) {
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
  variable.scope = SCOPE_LOCAL;
  variable.index = resolveLocal(compiler, name, length);
  if (variable.index != -1) return variable;

  // It's not a local, so guess that it's an upvalue.
  variable.scope = SCOPE_UPVALUE;
  variable.index = findUpvalue(compiler, name, length);
  return variable;
}

// Look up [name] in the current scope to see what variable it refers to.
// Returns the variable either in module scope, local scope, or the enclosing
// function's upvalue list. Returns a variable with index -1 if not found.
function resolveName(compiler, name, length) {
  var variable = resolveNonmodule(compiler, name, length);
  if (variable.index != -1) {
    return variable;
  }

  variable.scope = SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(compiler.parser.module.variableNames,
                                       name, length);
  return variable;
}

function loadLocal(compiler, slot) {
  if (slot <= 8) {
    emitOp(compiler, (CODE_LOAD_LOCAL_0 + slot));
    return;
  }

  emitByteArg(compiler, CODE_LOAD_LOCAL, slot);
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
  // we can't rely on CODE_RETURN to tell us we're at the end.
  emitOp(compiler, CODE_END);

  wrenFunctionBindName(compiler.parser.vm, compiler.fn,
                       debugName, debugNameLength);

  // In the function that contains this one, load the resulting function object.
  if (compiler.parent !== null) {
    var constant = addConstant(compiler.parent, compiler.fn);

    // If the function has no upvalues, we don't need to create a closure.
    // We can just load and run the function directly.
    if (compiler.fn.numUpvalues === 0) {
      emitShortArg(compiler.parent, CODE_CONSTANT, constant);
    } else {
      // Capture the upvalues in the new closure object.
      emitShortArg(compiler.parent, CODE_CLOSURE, constant);

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
  PREC_EQUALITY: 6,       // == !=
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
  prefix: undefined,
  infix: undefined,
  method: undefined,
  precedence: undefined,
  name: undefined
};

// Forward declarations since the grammar is recursive.
function getRule(type){}
function expression(compiler) {}
function statement(compiler) {}
function definition(compiler) {}
function parsePrecedence(compiler, precedence) {}

// Replaces the placeholder argument for a previous CODE_JUMP or CODE_JUMP_IF
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
      emitOp(compiler, CODE_POP);
    }

    // The receiver is always stored in the first local slot.
    emitOp(compiler, CODE_LOAD_LOCAL_0);
  } else if (!isExpressionBody) {
    // Implicitly return null in statement bodies.
    emitOp(compiler, CODE_NULL);
  }

  emitOp(compiler, CODE_RETURN);
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
    case SIG_METHOD:
      signatureParameterList(name, length, signature.arity, '(', ')');
      break;

    case SIG_GETTER:
      // The signature is just the name.
      break;

    case SIG_SETTER:
      name[(length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SIG_SUBSCRIPT:
      signatureParameterList(name, length, signature.arity, '[', ']');
      break;

    case SIG_SUBSCRIPT_SETTER:
      signatureParameterList(name, length, signature.arity - 1, '[', ']');
      name[(length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SIG_INITIALIZER:
      memcpy(name, "init ", 5);
      memcpy(name + 5, signature.name, signature.length);
      length = 5 + signature.length;
      signatureParameterList(name, length, signature.arity, '(', ')');
      break;
  }

  name[length] = '\n0000';
}
