<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="hero-content flex-col lg:flex-row">
        <img src="../assets/personalPage/shinee15.png" class="max-w-sm rounded-lg my-8" alt="my info image" />
        <div class="px-6">
          <h1 class="text-5xl font-bold py-6"><span class="text-primary" v-if="person">{{person.Name}}</span>'s Page</h1>
          <div class="py-6">
            <span class="font-bold mr-2">User's id: </span><span v-if="person"> {{ person.PersonID }} </span><br>
            <span class="font-bold mr-2">Location: </span> <span v-if="person"> {{ person.Location }} </span><br>
            <span class="mr-2"><router-link to="/chatroom">Meal:Pal List </router-link> </span><br>
          </div>
          <div class="flex justify-between py-6">
            <div class="flex mx-2">
              <router-link to="/history">
                <img src="../assets/clock.png" class="h-7 w-auto" />
              </router-link>
              <div class="px-4 mt-1">History List</div>
            </div>
            <div class="flex mx-2">
              <router-link to="/favorite">
                <img src="../assets/red_heart.png" class="h-6 mt-1 w-auto" />
              </router-link>
              <div class="px-4 mt-1">Favorite List</div>
            </div>
          </div>
        </div>
      </div>
      <!-- mealpals -->
    </div>
    <!-- hero end -->
  </div>
</template>
<script>
import axios from 'axios'

export default {
  name: 'PersonalPage',
  data () {
    return {
      person: null
    }
  },
  methods: {
    fetchPerson: async function () {
      const P1_ID = this.$store.state.P1_ID
      console.log(P1_ID)
      try {
        const response = await axios.get(`http://127.0.0.1:5000/api/person/${P1_ID}`)
        this.person = response.data
      } catch (error) {
        console.error(error)
        this.person = null
      }
    }
  },
  async mounted () {
    await this.fetchPerson()
  }
}
</script>

<style>
</style>
