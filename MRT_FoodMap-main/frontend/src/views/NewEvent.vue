<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="w-5/6 hero-content flex-col lg:flex-row">
        <div class="px-6 w-4/5 lg:w-2/5">
          <h1 class="text-5xl font-bold py-6 h-1/3 text-center"> New <span class="text-primary">E.v.ent</span></h1>
          <p class="py-4 md-6 text-natural h-2/3 text-center"> Start to make a new friend now!</p>
        </div>
        <div class="px-6 w-full lg:w-3/5">
          <!-- left component -->
          <div class="max-w-full">
            <!-- ========== right component ========== -->
            <div class="isolate bg-white px-12 py-4 rounded-3xl">
              <form action="#" method="POST" class="mx-auto mt-16 max-w-xl sm:mt-20">
                <div class="grid grid-cols-1 gap-x-8 gap-y-6 sm:grid-cols-2">
                  <div>
                    <label for="date" class="block text-sm font-semibold leading-6 text-gray-900">Date</label>
                    <div class="mt-6 text-primary">
                      <span>{{ this.date }}</span>
                    </div>
                  </div>
                  <div>
                    <label for="time" class="block text-sm font-semibold leading-6 text-gray-900">Time</label>
                    <div class="mt-2.5">
                      <input type="time" name="time" v-model="time" class="input input-bordered w-full"/>
                    </div>
                  </div>
                  <div class="sm:col-span-2">
                    <label for="station" class="block text-sm font-semibold leading-6 text-gray-900">MRT Station</label>
                    <select id="station" v-model="station" class="flex justify-end select select-bordered w-full font-normal">
                      <option disabled value="">What do .u. want to go today?</option>
                      <option v-for="[key, value] in Object.entries(stationList)" :key="key" :value="key">
                        {{ value }}
                      </option>
                    </select>
                  </div>
                  <div class="sm:col-span-2">
                    <label for="foodtype" class="block text-sm font-semibold leading-6 text-gray-900">Food Type</label>
                    <div class="mt-2.5">
                      <select name="types" id="types" v-model="type" class="flex justify-end select select-bordered w-full font-normal">
                        <option disabled value="">What do .u. want to eat today?</option>
                        <option v-for="[key, value] in Object.entries(foodTypes)" :key="key" :value="key">
                          {{ value }}
                        </option>
                      </select>
                    </div>
                  </div>
                </div>
                  <div class="my-12 py-6 flex flex-row justify-between">
                    <button class="flex btn btn-outline btn-error mx-12" @click="cancel">
                      <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
                      </svg>Cancel
                    </button>
                    <button type="submit" class="flex btn btn-outline btn-success mx-12" @click="submit">
                      <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                      </svg>Confirm
                    </button>
                  </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import axios from 'axios'

export default {
  name: 'NewEvent',
  data () {
    return {
      date: '',
      time: '',
      station: '',
      type: '',
      foodTypes: {
        BBQ: 'BBQ',
        hotpot: 'Hot Pot',
        ramen: 'Ramen'
      },
      stationList: {
        R10: 'Taipei Main Station',
        R11: 'Zhongshan Station',
        G12: 'Ximen'
      }
    }
  },
  mounted () {
    this.getDate()
  },
  methods: {
    cancel: function () {
      this.$router.push('/findMealPal')
    },
    submit: function () {
      this.addEvent()
      this.$router.push('/findMealPal')
    },
    addEvent: async function () {
      const P1_ID = this.$store.state.P1_ID
      console.log(this.$store)
      console.log(P1_ID, this.time, this.date, this.station, this.type)
      try {
        const config = { headers: { 'Content-Type': 'application/json' } }
        const timeDate = this.date + ' ' + this.time + ':00'
        const response = await axios.post('http://127.0.0.1:5000/api/addEvent', { P1_ID: P1_ID, Time: timeDate, FoodType: this.type, StationID: this.station }, config)
        console.log(response.data)
      } catch (error) {
        console.error('An error occurred:', error)
      }
    },
    getDate: function () {
      try {
        const currentDate = new Date()
        // 設定日期格式選項
        const options = { year: 'numeric', month: '2-digit', day: '2-digit' }
        this.date = currentDate.toLocaleDateString(undefined, options)
        this.date = this.date.replaceAll('/', '-')
        console.log(this.date)
      } catch (error) {
        console.error('An error occurred:', error)
      }
    }
  }
}
</script>
<style></style>
