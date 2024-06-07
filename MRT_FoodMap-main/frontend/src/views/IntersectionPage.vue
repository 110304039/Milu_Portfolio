<!-- 暫時沒在用了 -->
<template>
  <div>
    <header class="inter_header">
      <h2>Intersection</h2>
    </header>

    <button @click='backToFirst' type="button" name="backToFirstPage" class="backToFirst">
      <img src="../assets/home.png" width="100" height="100"/>
    </button>

    <div>
      <h4>Date:</h4>
      <h4>Time:</h4>
      <h4>MRT Station:</h4>
      <h4>Types:</h4>
    </div>

    <div>
      <button @click='confirm' name="yes">
        <p class="yes">Yes</p>
      </button>
      <button @click='reject' name="No">
        <p class="yes">No</p>
      </button>
    </div>
  </div>
</template>

<script>
export default {
  name: 'IntersectionPage',
  data () {
    return {
      submittedData: null
    }
  },
  methods: {
    confirm () {
      this.$router.push('/mealpal')
    },
    reject () {
      this.$router.push('/mealpal')
    },
    backToFirst () {
      this.$router.push('/')
    }
  },
  mounted () {
    this.submittedData = {
      date: this.$route.query.date || '',
      time: this.$route.query.time || '',
      station: this.$route.query.station || '',
      type: this.$route.query.type || ''
    }
  }
}
</script>

<style>
.inter_header {
  font-weight: bold;
}

.backToFirst {
  position: absolute;
  right: 0;
  top: 0px;
}
</style>

<style>
.inter_header {
font-weight: bold;
}

.backToFirst {
position: absolute;
right: 0;
top: 0px;
}
</style>
