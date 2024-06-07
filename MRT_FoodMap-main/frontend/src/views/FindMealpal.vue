<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="hero-content flex-col lg:flex-row-reverse">
        <div class="px-6 w-full lg:w-1/2">
          <h1 class="text-center text-5xl font-bold py-6">Find Lovely <span class="text-primary">Meal:Pal</span></h1>
          <div class="w-full flex justify-center">
            <h3 class="text-gray-500 w-auto font-bold mx-8">Amount of People waiting to pair</h3>
            <div class="flex flex-row justify-end mx-8">
              <ul class="steps">
                <li class="step step-success" data-content=":)">few</li>
                <li class="step step-warning" data-content=":>">a few</li>
                <li class="step step-error" data-content=":O">many</li>
              </ul>
            </div>
          </div>
          <div class="flex flex-col w-full my-8 lg:flex-row">
            <div class="grid flex-grow h-52 w-auto card bg-white shadow-lg rounded-box place-items-center cursor-pointer"
              @click="goToNewEvent">
              <div class="h-24 w-24 mt-8">
                <img src="../assets/mealpal/newEvent.png" class="h-24 w-auto" alt="mealpal">
              </div>
              <div class="my-6">
                New E.v.ent
              </div>
            </div>
            <div class="divider lg:divider-horizontal">OR</div>
            <div class="grid flex-grow h-52 w-auto card bg-white shadow-lg rounded-box place-items-center cursor-pointer"
              @click="goToRandomPair">
              <div class="h-24 w-24 mt-8">
                <img src="../assets/mealpal/randomPair.png" class="h-24 w-auto" alt="mealpal">
              </div>
              <div class="my-6">
                Random :Pair
              </div>
            </div>
          </div>
        </div>
        <div class="px-6 w-full py-4 lg:w-1/2">
          <!-- left conponent -->
          <MealPalMap />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import MealPalMap from '@/components/MealPalMap.vue'

export default {
  name: 'findMealpal',
  components: {
    MealPalMap
  },
  methods: {
    goToNewEvent () {
      this.$router.push('/newEvent')
    },
    goToRandomPair () {
      this.$router.push('/randomPair')
    }
  }
}
</script>
<style></style>
