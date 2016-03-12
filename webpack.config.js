module.exports = {
  entry: "./src/common.js",
  devtool: 'source-map',
  output: {
    library: "Wren",
    path: __dirname + '/out',
    filename: "wren.js"
  }
};
