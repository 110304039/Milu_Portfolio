<template>
    <div class="hero-overlay relative rounded-2xl shadow-lg">
        <img src="../assets/concentric_taipei_mrt_metro_map_60186.jpg"
            class="object-cover w-full h-auto rounded-2xl lg:h-4/5 lg:w-auto" alt="mrt metro map" />
        <button v-for="station in amountofpeople" :key="station.StationID" :class="['rounded-full bg-opacity-90 hover:bg-opacity-50', 'btn-'+ station.Status, 'absolute', 'pos-' + station.StationID, 'transform -translate-x-1/2 -translate-y-1/2 h-4 w-4']" type="submit"></button>
    </div>
</template>
<script>
export default {
  name: 'MealPalMap',
  data () {
    return {
      amountofpeople: []
    }
  },
  methods: {
    getAmountofPeople: async function () {
      const response = await fetch('http://127.0.0.1:5000/api/amountofpeople')
      this.amountofpeople = await response.json()
      console.log(this.amountofpeople)
    }
  },
  mounted () {
    this.getAmountofPeople()
  }
}
</script>
<style>
.pos-BR09 {
    top: 59.44%;
    left: 66.25%;
}
.pos-BR10 {
    top: 50%;
    left: 68.72%;
}
.pos-BR11 {
    top: 40.59%;
    left: 66.19%;
}

.pos-G12 {
    top: 53.14%;
    left: 44.59%;
}
.pos-G13 {
    top: 46.88%;
    left: 44.59%;
}

.pos-O06 {
    top: 61%;
    left: 56.2%;
}
.pos-O07 {
    top: 50%;
    left: 62.45%;
}
.pos-O08 {
    top: 43.75%;
    left: 60.78%;
}

.pos-R08 {
    top: 56.41%;
    left: 50%;
}
.pos-R10 {
    top: 50%;
    left: 50%;
}
.pos-R11 {
    top: 43.84%;
    left: 50%;
}
</style>
