
// Somewhat like the c 'printf' instead of varying types, we use %x.
function printf(format) {
  var string = format;

  for (var i = 0 ; i< arguments.length; i++) {
    if (i > 0) {
      string = string.replace("%x", arguments[i]);
    }
  }

  return string;
}

module.exports = printf;
