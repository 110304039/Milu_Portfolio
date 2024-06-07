<!-- 暫時沒在用了 -->
<template>
  <div class="invite">
    <div class="text1">
      Invitation
    </div>
    <div v-if="submittedData !== null">
      <div class="text2">
        Date: {{ submittedData.date }}<br>
        Time: {{ submittedData.time }}<br>
        Station: {{ submittedData.station }}<br>
        Type: {{ submittedData.type }}<br>
      </div>
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
  name: 'InvitationPage',
  data () {
    return {
      submittedData: null
    }
  },
  methods: {
    confirm () {
      this.$router.push({
        path: '/intersection',
        query: {
          date: this.$route.query.date || '',
          time: this.$route.query.time || '',
          station: this.$route.query.station || '',
          type: this.$route.query.type || ''
        }
      })
    },
    reject () {
      this.$router.push('/findMealPal')
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

<style scoped>
.invite{
  max-width: 400px;
  margin: 0 auto;
  padding: 20px;
  border: 1px solid #ccc;
  border-radius: 4px;
}
.text1{
  font-family: 'Inter';
  font-style: normal;
  font-weight: 400;
  font-size: 64px;
  line-height: 77px;
  background-color: #FFEDED;
  border-radius: 50px;
  align-items: center;
  text-align: center;
}
.text2{
  font-family: 'Cantarell';
  font-style: normal;
  font-weight: 500;
  font-size: 30px;
  line-height: 68px;
  text-align: center;
}
</style>
