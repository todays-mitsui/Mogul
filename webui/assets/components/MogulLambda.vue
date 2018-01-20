<template>
  <div class="mogul-lambda" role="mogul-lambda">
    <span class="mogul-lambda-lambda" role="mogul-lambda-lambda">^</span>
    <span class="mogul-lambda-param" role="mogul-lambda-param"><mogul-variable v-bind:identifier="param"></mogul-variable></span>
    <span class="mogul-lambda-dot" role="mogul-lambda-dot">.</span>
    <span class="mogul-lambda-body" role="mogul-lambda-body"><mogul-expr v-bind:expr="body"></mogul-expr></span>
  </div>
</template>

<style scoped>
.mogul-lambda {
  display: inline;
  font-size: 0;
}

.mogul-lambda-lambda,
.mogul-lambda-param,
.mogul-lambda-dot,
.mogul-lambda-body {
  font-size: 14px;
}
</style>

<script>
import MogulExpr     from './MogulExpr.vue';
import MogulVariable from './MogulVariable.vue';

export default {
  props: ['param', 'body'],
  components: {
    MogulExpr,
    MogulVariable,
  },
}
</script>
