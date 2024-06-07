<template>
    <div class="hero-overlay max-w-full relative rounded-2xl shadow-lg">
        <img src="../assets/concentric_taipei_mrt_metro_map_60186.jpg"
            class="object-cover max-w-full h-auto rounded-2xl lg:h-4/5 lg:w-auto" alt="mrt metro map" />
        <button class="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('R10')"></button>
        <button class="absolute pos-G13 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('G13')"></button>
        <button class="absolute pos-G15 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('G15')"></button>
        <button class="absolute pos-G12 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('G12')"></button>
        <button class="absolute pos-G16 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('G16')"></button>
        <button class="absolute pos-R05 transform -translate-x-1/2 -translate-y-1/2 h-4 w-4" type="submit" @click="clickStation('R05')"></button>
    </div>
</template>
<script>
export default {
  name: 'FoodMap',
  methods: {
    // create clickStation to choose the station with their id
    clickStation (id) {
      console.log('Food Map: ', id)
      this.$emit('clickStation', id)
    }
  }
}
</script>
<style>
.pos-G13 {
    top: 46.88%;
    left: 44.59%;
}
.pos-R05 {
    top: 59.44%;
    left: 66.25%;
}
.pos-G15 {
    top: 43.75%;
    left: 60.78%;
}
.pos-G12 {
    top: 53.14%;
    left: 44.59%;
}
.pos-G16 {
    top: 40.59%;
    left: 66.19%;
}
</style>
