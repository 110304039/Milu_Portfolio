<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="w-5/6 hero-content flex-col lg:flex-row-reverse">
        <div class="px-6 w-4/5 lg:w-2/5">
          <h1 class="text-5xl font-bold py-6 h-1/3 text-center"> Random <span class="text-primary">:Pair</span></h1>
          <p class="py-4 md-6 text-natural h-2/3 text-center"> Start to make a new friend now!</p>
        </div>
        <!-- <div class="px-6 w-4/5 lg:w-2/5" v-if="!pairstatus">
          <h1 class="text-5xl font-bold py-6 h-1/3 text-center"> :( Pair <span class="text-primary">Unsuccessfully</span></h1>
          <p class="py-4 md-6 text-natural h-2/3 text-center"> Try other conditions~ </p>
        </div> -->
        <div class="px-6 w-full lg:w-3/5">
          <!-- left component -->
          <div class="max-w-full">
            <!-- ========== right component ========== -->
            <div class="isolate bg-white px-12 py-4 rounded-3xl" v-if="showcontent">
              <!-- form begin -->
              <form action="#" method="POST" class="mx-auto mt-16 max-w-xl sm:mt-20">
                <div class="grid grid-cols-1 gap-x-8 gap-y-6 sm:grid-cols-2">
                  <div>
                    <label for="data" class="block text-sm font-semibold leading-6 text-gray-900">Date</label>
                    <div class="mt-6 text-primary">
                      <span>{{ this.date }}</span>
                    </div>
                  </div>
                  <div>
                    <label for="time" class="block text-sm font-semibold leading-6 text-gray-900">Time</label>
                    <div class="mt-2.5">
                      <input type="time" name="time" v-model="time" class="input input-bordered w-full" />
                    </div>
                  </div>
                  <!-- <div class="sm:col-span-2">
                    <label for="station" class="block text-sm font-semibold leading-6 text-gray-900">MRT Station</label>
                    <select name="types" id="types" class="flex justify-end select select-bordered w-full">
                      <option disabled selected class="text-primary">What do .u. want to go today?</option>
                      <option v-for="[key, value] in Object.entries(stationList)" :key="key">
                        {{ value }}
                      </option>
                    </select>
                  </div> -->
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
                  <button class="flex btn btn-outline btn-error mx-12" @click="goMealpal">
                    <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24"
                      stroke="currentColor">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
                    </svg>Cancel
                  </button>
                  <button type="submit" class="flex btn btn-outline btn-warning mx-12" @click="submitForm">
                    <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24"
                      stroke="currentColor">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                    </svg>Search
                  </button>
                </div>
              </form>
              <!-- form end -->
            </div>
            <!-- ========== invitation part ========== -->
            <div class="isolate bg-white px-12 py-4 rounded-3xl" v-if="!showcontent">
              <div class="p-6">
                <div class="p-4">
                  <h3 class="text-3xl text-center font-semibold leading-7 text-primary"> Invitation Success! </h3>
                </div>
                <div class="mt-2 max-w-full">
                  <dl class="divide-y divide-gray-100">
                    <!-- <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
                      <dt class="text-sm font-medium leading-6 text-gray-900">Date</dt>
                      <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ '2023/06/06' }}</dd>
                    </div> -->
                    <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
                      <dt class="text-sm font-medium leading-6 text-gray-900">Time</dt>
                      <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ this.time }}</dd>
                    </div>
                    <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
                      <dt class="text-sm font-medium leading-6 text-gray-900">Food Type</dt>
                      <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ this.type }}</dd>
                    </div>
                    <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
                      <dt class="text-sm font-medium leading-6 text-gray-900">MRT Station</dt>
                      <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ this.station }}
                      </dd>
                    </div>
                    <div class="px-4 py-4 sm:grid sm:grid-cols-2 sm:gap-4 sm:px-0">
                      <dt class="text-sm font-medium leading-6 text-gray-900"> Restaurant .u. may like</dt>
                      <!-- <dd class="mt-1 leading-6 sm:col-span-2 sm:mt-0"> -->
                      <!-- <div>Add a comment</div> -->
                      <!-- </dd> -->
                    </div>
                    <!-- ========== carousel component - Comment ========== -->
                    <div class="max-w-full w-full">
                      <div class="carousel carousel-center p-2 space-x-4 bg-primary rounded-box">
                        <div class="carousel-item">
                          <div class="card w-96 bg-base-100 shadow-xl">
                            <div class="card-body">
                              <h2 class="card-title">{{ this.storeList[0].Name }}</h2>
                              <p>{{ this.storeList[0].Location }}</p>
                            </div>
                          </div>
                        </div>
                        <div class="carousel-item">
                          <div class="card w-96 bg-base-100 shadow-xl">
                            <div class="card-body">
                              <h2 class="card-title">{{ this.storeList[1].Name }}</h2>
                              <p>{{ this.storeList[1].Location }}</p>
                            </div>
                          </div>
                        </div>
                        <div class="carousel-item">
                          <div class="card w-96 bg-base-100 shadow-xl">
                            <div class="card-body">
                              <h2 class="card-title cursor-pointer"> See More! </h2>
                              <p> Go to our f.o^o.d map <br> to find more tasty food! </p>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </dl>
                </div>
                <div class="mt-6 py-6 flex flex-row justify-between">
                    <button class="flex btn btn-outline btn-error mx-12" @click="cancel">
                      <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
                      </svg>Cancel
                    </button>
                    <button type="submit" class="flex btn btn-outline btn-success mx-12" @click="confirm">
                      <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
                      </svg>Confirm
                    </button>
                  </div>
              </div>
            </div>
            <!-- invitation part end -->
          </div>
        </div>
      </div>
    </div>
  </div>
</template>
<script>
import axios from 'axios'

export default {
  name: 'RandomPair',
  data () {
    return {
      eventId: '',
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
      },
      showcontent: true,
      pairstatus: true,
      person: '',
      storeList: [{ Name: ' ', Location: ' ' }, { Name: ' ', Location: ' ' }]
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
    },
    submitForm () {
      this.randomPair()
      this.showcontent = false
    },
    goMealpal () {
      this.$router.push('/findMealPal')
    },
    goFindFood () {
      this.$router.push('/findTastyFood')
    },
    randomPair: async function () {
      const data = {
        P2_ID: this.person.PersonID,
        Time: this.date + ' ' + this.time + ':00',
        FoodType: this.type,
        StationID: this.station
      }
      console.log(data.Time)
      const response = await axios.post('http://localhost:5000/api/randomPair', data)
      if (response.data.status === 'success') {
        console.log(response.data.event)
        this.getStores(response.data.event.FoodType, response.data.event.StationID)
        this.eventId = response.data.event.EventID
        this.getStation(response.data.event.StationID)
        this.date = response.data.event.Time
        this.time = response.data.event.Time
        this.type = response.data.event.FoodType
      } else {
        this.showcontent = true
        // this.pairstatus = false
        alert('Unsuccessfully pair\nMaybe you can try other station, food type, or time')
        // console.error(response.data.message)
      }
    },
    getDate: function () {
      try {
        const currentDate = new Date()
        // 設定日期格式選項
        const options = { year: 'numeric', month: '2-digit', day: '2-digit' }
        this.date = currentDate.toLocaleDateString(undefined, options)
        this.date = this.date.replaceAll('/', '-')
      } catch (error) {
        console.error('An error occurred:', error)
      }
    },
    getStation: async function (id) {
      try {
        const response = await axios.get(`http://127.0.0.1:5000/api/station/${id}`)
        console.log('get station', response)
        this.station = response.data.Name
        console.log(this.station)
      } catch (error) {
        console.error(error)
        this.station = null
      }
    },
    getStores: async function (type, stationId) {
      try {
        const response = await axios.get(`http://127.0.0.1:5000//api/stores/${type}/${stationId}`)
        console.log('get store', response)
        if (response.data.status === 'success') {
          this.storeList = response.data.storeList
        }
        console.log('stroeList', this.storeList[0])
      } catch (error) {
        console.error(error)
        this.station = null
      }
    },
    cancel () {
      this.showcontent = true
      this.getDate()
    },
    confirm: async function () {
      const pairData = {
        P2_ID: this.$store.state.P1_ID,
        EventID: this.eventId
      }
      console.log('event id', this.EventID)
      console.log(pairData)
      const response = await axios.post('http://localhost:5000/api/pairSuccess', pairData)
      console.log('pair success', response)
      if (response.data.status === 'success') {
        this.$router.push('/chatroom')
      }
    }
  },
  async mounted () {
    this.getDate()
    await this.fetchPerson()
  }
}
</script>
<style></style>
