<template>
  <div
    v-bind:class="classObject"
    role="mogul-apply"
  >
    <span
      class="mogul-apply-backquot"
      role="mogul-apply-backquot"
      v-on:mouseenter="isActive = true"
      v-on:mouseleave="isActive = false"
    >`</span>

    <span
      class="mogul-apply-left"
      role="mogul-apply-left"
    ><mogul-expr v-bind:expr="left"></mogul-expr></span>

    <span
      class="mogul-apply-right"
      role="mogul-apply-right"
    ><mogul-expr v-bind:expr="right"></mogul-expr></span>
  </div>
</template>

<style scoped>
.mogul-apply {
  display: inline;
  font-size: 0;
}

.mogul-apply-backquot,
.mogul-apply-left,
.mogul-apply-right {
  font-size: 14px;
}

.mogul-apply-backquot {
  cursor: pointer;
}

.mogul-apply {
  border-radius: 2px;
}
.mogul-apply.active {
  display: inline-block;
  outline: 1px solid #ec8;
}

.mogul-apply > .mogul-apply-left,
.mogul-apply > .mogul-apply-right {
  margin: 0 1px;
  border: 1px solid transparent;
  border-radius: 2px;
}
.mogul-apply.active > .mogul-apply-left {
  font-weight: 700;
  background-color: #fee;
  border: 1px solid #baa;
}
.mogul-apply.active > .mogul-apply-right {
  font-weight: 700;
  background-color: #eef;
  border: 1px solid #aab;
}
</style>

<script>
import MogulExpr from './MogulExpr.vue';

export default {
  props: ['left', 'right'],

  components: {
    MogulExpr,
  },

  data: function() {
    return {
      isActive: false,
    }
  },

  computed: {
    classObject: function () {
      return {
        'mogul-apply': true,
        active: this.isActive,
      }
    }
  },
}
</script>
