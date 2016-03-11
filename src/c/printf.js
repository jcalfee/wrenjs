
// Somewhat like the c 'printf' instead of varying types, we use %x.
function printf(format) {
  var string = format;

  arguments.forEach(function(arg, i, args) {
    if (i > 0) {
      string = string.replace("%x", arg);
    }
  });

  return string;
}

module.exports.printf = printf;
