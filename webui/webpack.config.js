const path = require('path');

module.exports = {
  entry: './assets/app.js',
  output: {
    path: path.join(__dirname, '../static/js/'),
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      {
        test: /\.vue$/,
        loader: 'vue-loader',
      },
    ]
  },
  resolve: {
    alias: {
      'vue$': 'vue/dist/vue.esm.js',
    }
  }
};
