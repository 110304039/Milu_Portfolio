<template>
  <div>
    <!-- ========== 1st row ========== -->
    <div class="flex p-6">
      <!-- find a person with specific id -->
      <div class="w-1/3 flex-col">
        <div class="form-control">
          <div class="input-group">
            <input type="text" placeholder="00001002" class="input input-bordered" v-model.lazy="person_input"/>
            <button class="btn btn-square"  @click="fetchPerson(person_input)">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
              </svg>
            </button>
          </div>
        </div>
        <div v-if="person" class="max-w-full">
          ID: {{ person.PersonID }} <br>
          Name: {{ person.Name }} <br>
          Password: {{ person.Password }} <br>
          Location: {{ person.Location }}
        </div>
        <div v-else class="max-w-full">
          Person (ID: {{ person_input }}) not found.
        </div>
      </div>

      <!-- find a station with specific station -->
      <div class="w-1/3 flex-col">
        <div class="form-control">
          <div class="input-group">
            <input type="text" placeholder="R10" class="input input-bordered" v-model.lazy="station_input"/>
            <button class="btn btn-square"  @click="fetchStation(station_input)">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
              </svg>
            </button>
          </div>
        </div>
        <div v-if="station" class="result">
          ID: {{ station.StationID }}<br>
          Name: {{ station.Name }}<br>
        </div>
        <div v-else class="result">
          Station(ID: {{ station_input }}) not found.
        </div>
      </div>

      <!-- find a event with specific id -->
      <div class="w-1/3 flex-col">
        <div class="form-control">
          <div class="input-group">
            <input type="text" placeholder="3" class="input input-bordered" v-model.lazy="event_input"/>
            <button class="btn btn-square"  @click="fetchEvent(event_input)">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
              </svg>
            </button>
          </div>
        </div>
        <div v-if="event" class="result">
          ID: {{ event.EventID }}<br>
          P1ID: {{ event.P1_ID }}<br>
          P2ID: {{ event.P2_ID }}<br>
          Time: {{ event.Time }}<br>
          FoodType: {{ event.FoodType }}<br>
          Station: {{ event.StationID }}<br>
        </div>
        <div v-else class="result">
          Event(ID: {{ event_input }}) not found.
        </div>
      </div>
    </div>
    <!-- ========== second row ========== -->
    <div class="flex p-6">
      <!-- get all people -->
      <div class="w-1/3 flex-col">
        <button class="btn btn-outline" @click="getPeople()">Get PersonList</button>
        <div v-for="(person, index) in people" :key="index" class="result">
          ID: {{ person.PersonID }}<br>
          Name: {{ person.Name }}<br>
          Password: {{ person.Password }}<br>
          Location: {{ person.Location }}<br>
        </div>
      </div>
      <!-- get all station -->
      <div class="w-1/3 flex-col">
        <button class="btn btn-outline" @click="getStations()">Get Stations</button>
        <div v-for="(station, index) in stations" :key="index" class="result">
          ID: {{ station.StationID }}<br>
          Name: {{ station.Name }}<br>
        </div>
      </div>
      <!-- get all event -->
      <div class="w-1/3 flex-col">
        <button class="btn btn-outline" @click="getEvents()">Get Events</button>
        <div v-for="(event, index) in events" :key="index" class="result">
          ID: {{ event.EventID }}<br>
          P1ID: {{ event.P1_ID }}<br>
          P2ID: {{ event.P2_ID }}<br>
          Time: {{ event.Time }}<br>
          FoodType: {{ event.FoodType }}<br>
          Station: {{ event.StationID }}<br>
        </div>
      </div>
    </div>
    <!-- ========== third part ========== -->
    <!-- add/delete station -->
    <div class="flex py-6">
      <div>
        <form @submit.prevent="addStation">
          <input type="text" placeholder="R10" v-model="stationID" class="input input-bordered" />
          <input type="text" v-model="stationName" placeholder="Station Name" class="input input-bordered" />
          <button class=" btn btn-outline" type="submit">Add Station</button>
        </form>
        <div>
          <div v-if="addStationStatus" :key="index" class="result">
            msg: {{ addStationStatus.message }}<br>
          </div>
        </div>
      </div>
      <div>
        <form @submit.prevent="deleteStation">
          <input type="text" placeholder="R10" v-model="dstationID" class="input input-bordered" />
          <button class=" btn btn-outline" type="submit">Delete Station</button>
        </form>
        <div>
          <div v-if="deleteStationStatus" :key="index" class="result">
            msg: {{ deleteStationStatus.message }}<br>
          </div>
        </div>
      </div>
    </div>
    <!-- ========== fourth part ========== -->
    <div class="py-6">
      <form @submit.prevent="addFavorite">
        <input type="text" placeholder="R10" v-model="person_input" class="input input-bordered" />
        <input type="text" v-model="stationID" placeholder="Station ID" class="input input-bordered" />
        <button class=" btn btn-outline" type="submit">Add Favorite</button>
      </form>
      <div>
        <div v-if="addFavoriteStatus" :key="index" class="result">
          msg: {{ addFavoriteStatus.message }}<br>
        </div>
      </div>
    </div>
    <!-- ========== fifth part ========== -->
    <div class="py-6">
      <form @submit.prevent="getStore">
        <input type="text" v-model="foodtype" placeholder="foodtype" class="input input-bordered" />
        <button class=" btn btn-outline" type="submit">Get Store</button>
      </form>
      <div>
        <div v-for="(store, index) in stores" :key="index" class="result">
          Store Name: {{ store.Name }}<br>
          Station Name: {{ store.StoreName }}<br>
          Address: {{ store.Location }}<br>
          Distance: {{ store.Distance }}<br>
          <a v-bind:href=store.URL>Go to Map</a>
          <button class="btn btn-ghost btn-xs" @click="toggleMap">Details</button>
          IsFavorite: {{ store.IsFavorite }}<br>

        </div>
      </div>
    </div>
  </div>
</template>

<script>
import axios from 'axios'

// find a person with specific id
async function fetchPerson (personInput) {
  try {
    const response = await axios.get(`http://127.0.0.1:5000/api/person/${personInput}`)
    this.person = response.data
  } catch (error) {
    console.error(error)
    console.log('This person: ', this.person)
    this.person = null
  }
}
// get all people
async function getPeople () {
  try {
    const response = await axios.get('http://127.0.0.1:5000/api/person')
    this.people = response.data
  } catch (error) {
    console.error(error)
    this.people = []
  }
}

// find a station with specific station
async function fetchStation (stationInput) {
  try {
    const response = await axios.get(`http://127.0.0.1:5000/api/station/${stationInput}`)
    this.station = response.data
  } catch (error) {
    console.error(error)
    this.station = null
  }
}

// get all station
async function getStations () {
  try {
    const response = await axios.get('http://127.0.0.1:5000/api/station')
    this.stations = response.data
  } catch (error) {
    console.error(error)
    this.stations = []
  }
}

// create an addStation function with inputStationID and stationName
async function addStation () {
  console.log(this.stationID, this.stationName)
  try {
    var config = { headers: { 'Content-Type': 'application/json' } }
    const response = await axios.post('http://127.0.0.1:5000/api/addStation', { StationID: this.stationID, Name: this.stationName }, config)
    this.addStationStatus = response.data
  } catch (error) {
    console.error('An error occurred:', error)
  }
}

// create an deleteStation function with inputStationID
async function deleteStation () {
  console.log(this.stationID)
  try {
    var config = { headers: { 'Content-Type': 'application/json' } }
    const response = await axios.post('http://127.0.0.1:5000/api/deleteStation', { StationID: this.dstationID }, config)
    this.addStationStatus = response.data
  } catch (error) {
    console.error('An error occurred:', error)
  }
}

// find a event with specific id
async function fetchEvent (eventInput) {
  try {
    const response = await axios.get(`http://127.0.0.1:5000/api/event/${eventInput}`)
    this.event = response.data
  } catch (error) {
    console.error(error)
    this.event = null
  }
}
// get all event
async function getEvents () {
  try {
    const response = await axios.get('http://127.0.0.1:5000/api/event')
    this.events = response.data
  } catch (error) {
    console.error(error)
    this.events = []
  }
}

// add a favorite store with specific person_id and station_id
async function addFavorite () {
  console.log(this.person_input, this.stationID)
  try {
    const response = await axios.post(`http://127.0.0.1:5000/api/favorite/${this.person_input}/${this.stationID}`)
    this.addFavoriteStatus = response.data
  } catch (error) {
    console.error(error)
    this.addFavoriteStatus = null
  }
}
// get all store
async function getStore (foodtype) {
  try {
    if (foodtype !== '') {
      // get top 4 stores without specific foodtype
      const response = await axios.get('http://127.0.0.1:5000/api/store')
      this.stores = response.data
    } else {
      // get stores with specific foodtype
      const response = await axios.get(`http://127.0.0.1:5000/api/store/${foodtype}`)
      this.stores = response.data
    }
  } catch (error) {
    console.error(error)
  }
}

export default {
  name: 'HelloWorld',
  data () {
    return {
      person_input: 'P001',
      person: null,
      people: [],

      station_input: 'R10',
      station: null,
      stations: [],

      event_input: '3',
      event: null,
      events: [],

      stationID: '2345',
      stationName: 'zz',
      dstationID: '2345',
      addStationStatus: null,
      deleteStationStatus: null,

      foodtype: 'BBQ',
      stores: []
    }
  },
  methods: {
    fetchPerson: fetchPerson,
    getPeople: getPeople,

    fetchStation: fetchStation,
    getStations: getStations,

    fetchEvent: fetchEvent,
    getEvents: getEvents,

    addStation: addStation,
    deleteStation: deleteStation,

    addFavorite: addFavorite,

    getStore: getStore
  }
}
</script>
<style>
</style>
