<template>
  <div class="hero-overlay min-w-full max-w-full bg-white shadow-lg rounded-2xl">
    <!-- ========== info component ========== -->
    <div class="p-6 w-full">
      <div class="p-4">
        <h3 class="text-3xl text-center font-semibold leading-7 text-primary">{{ selectedStore.Name}}</h3>
      </div>
      <div class="mt-6">
        <dl class="divide-y divide-gray-100">
          <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
            <dt class="text-sm font-medium leading-6 text-gray-900">Food Type</dt>
            <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ selectedStore.Category }}</dd>
          </div>
          <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
            <dt class="text-sm font-medium leading-6 text-gray-900">Near to</dt>
            <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ selectedStore.Location }}</dd>
          </div>
          <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
            <dt class="text-sm font-medium leading-6 text-gray-900">Distance to <br> MRT Station</dt>
            <dd class="mt-1 text-sm leading-6 text-gray-700 sm:col-span-2 sm:mt-0">{{ selectedStore.Distance }}</dd>
          </div>
          <div class="px-4 py-4 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-0">
            <dt class="text-sm font-medium leading-6 text-gray-900">Comments </dt>
            <!-- <dd class="mt-1 leading-6 sm:col-span-2 sm:mt-0"> -->
              <!-- <div>Add a comment</div> -->
            <!-- </dd> -->
          </div>
          <!-- ========== carousel component - Comment ========== -->
          <div class=" overflow-hidden max-w-full min-w-full">
            <div class="carousel carousel-center p-2 space-x-4 bg-primary rounded-box" style="display: flex; align-items: stretch;">
              <!-- card 1 -->
              <div v-for="comment in comments" :key="comment.PersonName">
                <div class="carousel-item">
                  <div class="card w-96 h-full bg-base-100 shadow-xl">
                    <div class="card-body h-full">
                      <h2 class="card-title mt-2">{{ comment.PersonName }}</h2>
                      <p class="text-sm h-auto">{{ comment.Content }}</p>
                    </div>
                  </div>
                </div>
              </div>
              <div class="carousel-item">
                <div class="card w-96 bg-base-100 shadow-xl">
                  <div class="card-body">
                    <h2 class="card-title"> &#10133; Add a comment </h2>
                    <p class="text-sm"> Write down <br> your comments right now! </p>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <!-- ========== carousel component end ========== -->
        </dl>
      </div>
    </div>
    <!-- ========== info component end ========== -->
  </div>
</template>
<script>
import axios from 'axios'

export default {
  name: 'StoreDetailArea',
  data () {
    return {
      comments: []
    }
  },
  props: {
    selectedStore: {
      type: Object,
      required: true
    }
  },
  methods: {
    getComment: async function (storeId) {
      const response = await axios.get(`http://127.0.0.1:5000/api/comments/${storeId}`)
      this.comments = response.data
      console.log('comments: ', this.comments)
    }
  },
  watch: {
    selectedStore: {
      immediate: true,
      handler (newSelectedStore) {
        this.getComment(newSelectedStore.StoreID)
      }
    }
  },
  mounted () {
    console.log('selectedStore: ', this.selectedStore)
    console.log('selectedStore.StoreID: ', this.selectedStore.StoreID)
    this.getComment(this.selectedStore.StoreID)
  }
}
</script>
