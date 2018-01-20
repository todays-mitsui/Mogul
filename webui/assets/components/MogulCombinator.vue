<template>
  <span role="mogul-combinator">{{ identifier }}</span>
</template>

<style scoped>
span, div {
  display: inline-block;
}
</style>

<script>
export default {
  props: ['identifier'],
}
</script>
