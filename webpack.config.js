module.exports = {
  entry: "./src/common.js",
  output: {
    library: "Wren",
    path: __dirname + '/out',
    filename: "wren.js",
    sourceMapFilename: "wren.map",
    devtoolLineToLine: true
  }
};
