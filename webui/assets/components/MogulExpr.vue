<template>
  <mogul-apply
    v-if="expr.tag == 'Apply'"
    v-bind:left="expr.left"
    v-bind:right="expr.right"
  ></mogul-apply>

  <mogul-lambda
    v-else-if="expr.tag == 'Lambda'"
    v-bind:param="expr.param"
    v-bind:body="expr.body"
  ></mogul-lambda>

  <mogul-variable
    v-else-if="expr.tag == 'Variable'"
    v-bind:identifier="expr.identifier"
  ></mogul-variable>

  <mogul-combinator
    v-else-if="expr.tag == 'Combinator'"
    v-bind:identifier="expr.identifier"
  ></mogul-combinator>

  <span v-else>null</span>
</template>

<style scoped>
</style>

<script>
// import MogulApply      from './MogulApply.vue';
// import MogulLambda     from './MogulLambda.vue';
import MogulVariable   from './MogulVariable.vue';
import MogulCombinator from './MogulCombinator.vue';

export default {
  beforeCreate: function () {
    this.$options.components.MogulApply      = require('./MogulApply.vue').default;
    this.$options.components.MogulLambda     = require('./MogulLambda.vue').default;
    // this.$options.components.MogulVariable   = require('./MogulVariable.vue');
    // this.$options.components.MogulCombinator = require('./MogulCombinator.vue');
  },
  // name: 'mogul-expr',
  props: ['expr'],
  components: {
    // MogulApply,
    // MogulLambda,
    MogulVariable,
    MogulCombinator,
  },
}
</script>
