module.exports = {
  entry: "./src/wren.js",
  devtool: 'source-map',
  output: {
    library: "Wren",
    path: __dirname + '/out',
    filename: "wren.js"
  }
};
