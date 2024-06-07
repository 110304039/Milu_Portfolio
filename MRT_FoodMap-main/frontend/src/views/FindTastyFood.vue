<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="hero-content flex-col lg:flex-row-reverse">
        <div class="px-6 w-full lg:w-1/2">
          <h1 class="text-center text-5xl font-bold py-6">Find Tasty <span class="text-primary">F.o^o.d </span></h1>
          <div class="w-full flex flex-row justify-between">
            <!-- button with a arror point to left -->
            <button class="flex justify-start btn btn-ghost hover:bg-primary hover:text-white" @click="showMap = true"><p>&larr;</p></button>
            <!-- selection bar -->
            <select v-model="foodType" class="flex justify-end font-normal select select-primary w-full max-w-xs">
              <option disabled value="">What do .u. want to eat today?</option>
              <option v-for="[key, value] in Object.entries(foodTypes)" :key="key" :value="key">
                {{ value }}
              </option>
            </select>
          </div>
          <!-- ========== table component ========== -->
          <div class="h-80 overflow-y-auto w-full mt-4 bg-white rounded-lg py-2">
            <table class="table table-auto w-full whitespace-normal">
              <!-- head -->
              <tbody>
                <!-- row 1 -->
                <tr v-for="(row, index) in stores" :key="index">
                  <td>
                    <div class="flex-row mx-2">
                      <div class="flex items-center space-x-3">
                        <div class="font-bold cursor-pointer" @click="toggleMap(index, row)">{{ row.Name }}</div>
                        <a role="button" class="btn btn-ghost btn-xs" :href="row.URL">Map</a>
                      </div>
                      <div class="text-sm">Loc. {{ row.Location }}</div>
                    </div>
                  </td>
                  <th class="text-right">
                    <button
                      name="add fav"
                      type="submit"
                      class="btn btn-sm mask mask-heart transition-colors duration-200 mx-2"
                      :class="{ 'bg-red-400': row.isFav, 'bg-gray-400': !row.isFav }"
                      @click="toggleColor(index)"
                    ></button>
                  </th>
                </tr>
                <!-- row end -->
              </tbody>
            </table>
          </div>
          <!-- ========== table component end ========== -->
        </div>
        <div class="px-6 w-full py-4 lg:w-1/2">
          <!-- left conponent -->
          <div class="max-w-full rounded-lg" v-if="showMap">
            <FoodMap @clickStation="setStation"></FoodMap>
          </div>
          <div class="max-w-full rounded-lg" v-if="!showMap">
            <StoreDetailArea :selectedStore="selectedStore"></StoreDetailArea>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>
<script>
// import Store Table component
import FoodMap from '@/components/FoodMap.vue'
import StoreDetailArea from '@/components/StoreDetailArea.vue'
import axios from 'axios'
export default {
  name: 'findTastyFood',
  data () {
    return {
      stationID: '',
      foodType: '',
      foodTypes: {
        BBQ: 'BBQ',
        hotpot: 'Hot Pot',
        ramen: 'Ramen'
      },
      showMap: true,
      // todo: get data from db
      stores: [],
      selectedStore: {
        Category: 'ramen',
        Distance: '450m',
        Location: '\u53f0\u5317\u5e02\u4e2d\u6b63\u5340\u5fe0\u5b5d\u897f\u8def\u4e00\u6bb536\u865fB1',
        Name: '\u9eb5\u5c4b\u6b66\u85cf \u672c\u5e97',
        StationName: 'Taipei Main Station',
        StoreID: 'S031',
        URL: 'https://reurl.cc/GAmgmd',
        isFav: false
      }
    }
  },
  components: {
    FoodMap,
    StoreDetailArea
  },
  methods: {
    toggleColor (index) {
      // this.tableRows[index].isClicked = !this.tableRows[index].isClicked
      this.stores[index].isFav = !this.stores[index].isFav
      if (this.stores[index].isFav) { // 被按喜歡
        this.addFavorite(this.stores[index].Name)
      } else {
        this.deleteFavorite(this.stores[index].Name)
      }
    },
    toggleMap: async function (index, store) {
      this.showMap = false
      const P1_ID = this.$store.state.P1_ID
      this.selectedStore = store
      console.log('selectedStore: ', this.selectedStore)
      const StoreID = await axios.get(`http://127.0.0.1:5000/api/storeID/${this.stores[index].Name}`)
      const config = { headers: { 'Content-Type': 'application/json' } }
      try {
        const response = await axios.post('http://127.0.0.1:5000/api/addHistory', { StoreID: StoreID, P1_ID: P1_ID }, config)
        console.log(response.data)
      } catch (error) {
        console.error('An error occurred:', error)
      }
      console.log('toggleMap: ', this.showMap)
    },
    setStation: async function (stationID) {
      this.stationID = stationID
      console.log('Find Tasty Food: ', this.stationID)
      console.log('Find Tasty Food: ', this.foodType)
      this.getStores(this.foodType, this.stationID)
    },
    addFavorite: async function (storeName) {
      const config = { headers: { 'Content-Type': 'application/json' } }
      const P1_ID = this.$store.state.P1_ID
      const StoreID = await axios.get(`http://127.0.0.1:5000/api/storeID/${storeName}`)
      try {
        const response = await axios.post('http://127.0.0.1:5000/api/addFavorite', { StoreID: StoreID, P1_ID: P1_ID }, config)
        console.log(response.data)
      } catch (error) {
        console.error('An error occurred:', error)
      }
    },
    deleteFavorite: async function (storeName) {
      const config = { headers: { 'Content-Type': 'application/json' } }
      const P1_ID = this.$store.state.P1_ID
      const StoreID = await axios.get(`http://127.0.0.1:5000/api/storeID/${storeName}`)
      try {
        const response = await axios.post('http://127.0.0.1:5000/api/deleteFavorite', { StoreID: StoreID, P1_ID: P1_ID }, config)
        console.log(response.data)
      } catch (error) {
        console.error('An error occurred:', error)
      }
    },
    // get stores arr
    getStores: async function (foodType, stationID) {
      console.log('---getStores start---')
      const config = { headers: { 'Content-Type': 'application/json' } }
      try {
        let P1_ID = this.$store.state.P1_ID
        if (P1_ID === '') {
          P1_ID = '00000000'
        }
        console.log('P1_ID: ', P1_ID)
        if (this.stationID === '' | this.foodType === '') {
          const response = await axios.get(`http://127.0.0.1:5000/api/stores/${P1_ID}`, config)
          this.stores = response.data
        } else {
          const response = await axios.get(`http://127.0.0.1:5000/api/stores/${P1_ID}/${foodType}/${stationID}`, config)
          this.stores = response.data
        }
      } catch (error) {
        console.error('An error occurred:', error)
      }
      console.log('---getStores end---')
    }
  },
  mounted () {
    this.getStores(this.foodType, this.stationID)
  }
}
</script>
<style>
</style>
