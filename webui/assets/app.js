import Vue from 'vue';

import MogulExpr from './components/MogulExpr.vue';


var app = new Vue({
  el: '.container',

  components: {
    MogulExpr,
  },

  data: {
    message: 'Hello Vue!',
    srcExprStr: '``EQ ``ADD 1 1 2',
    loading: false,
    transition: [],
  },

  methods: {
    submit: function() {
      fetch('/eval', {
        method: 'POST',
        body: JSON.stringify({
          request: 'eval',
          exprStr: this.srcExprStr,
          transition: [],
        }),
      })
      .then((response) => {
        return response.json().then((data) => {
          console.info(data.transition);
          this.transition = data.transition;
        });
      })
      .catch((error) => {
        console.error(error);
      })
    },
  },
})


