<template>
  <div>
    <nav class =" mx-auto max-w-7xl justify-between py-2 lg:px-4 text-center">
      <router-link to="/homepage" class="p-6">MRT Food Map System</router-link>|
      <!-- <router-link to="/testpage" class="p-6">Test Page</router-link> | -->
      <router-link to="/findTastyFood" class="p-6">Find Food</router-link> |
      <router-link to="/findMealPal" class="p-6">Find MealPal</router-link> |
      <router-link to="/newEvent" class="p-6">New Event</router-link> |
      <router-link to="/randomPair" class="p-6">Random Pair</router-link> |
      <router-link to="/chatroom" class="p-6">Chat Room</router-link>
      <!-- vendor NavBar component -->
      <NavBar />
    </nav>
    <!-- add backgound -->
    <!-- todo: img not showed -->
    <!-- <div>
      <img src="assets/backgound/rainy.jpg" alt="background" class="absolute object-cover w-full h-full"/>
    </div>
    <div class="hero min-h-screen bg-cover bg-center" style="background-image: url('./assets/backgound/rainy.jpg');"></div>-->
    <router-view></router-view>
    <!-- vendor Footer component -->
    <AppFooter />
  </div>
</template>

<script>
import NavBar from '@/components/NavBar.vue'
import AppFooter from '@/components/AppFooter.vue'

export default {
  name: 'App',
  components: {
    NavBar,
    AppFooter
  }
}
</script>
<style>
@import url('https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@500;800&display=swap');
body {
  font-family: 'Source Code Pro', monospace;
}
</style>
