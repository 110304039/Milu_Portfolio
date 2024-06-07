<template>
<div>
  <div class="hero min-h-[80vh] bg-base-200 bg-cover bg-center" style="background-image: url('./assets/backgound/rainy.jpg');">
    <!-- <div class="hero-overlay bg-opacity-20"></div> -->
    <div class="hero-content text-center ">
      <div class="my-8 ld: w-full">
        <div class="my-8">
          <h1 class="text-5xl font-bold my-6">MRT <span class="text-primary"> F.o^o.d</span> Map</h1>
          <p class="py-4 md-6 text-natural">Enjoy your f.o^o.d trip with meal:Pals in Taipei city!</p>
        </div>
        <div class="flex flex-col w-full my-8 lg:flex-row">
          <div class="grid flex-grow h-52 w-auto card bg-white shadow-lg rounded-box place-items-center cursor-pointer" @click="goFindFood">
            <div class="h-24 w-24 mt-8">
              <img src="../assets/mainpage/findfood2.png" class="h-24 w-auto" alt="f.o^o.d">
            </div>
            <div class="my-6">
              Find F.o^o.d
            </div>
          </div>
          <div class="divider lg:divider-horizontal">OR</div>
          <div class="grid flex-grow h-52 w-auto card bg-white shadow-lg rounded-box place-items-center cursor-pointer" @click="goMealpal">
            <div class="h-24 w-24 mt-8">
              <img src="../assets/mainpage/malpals2.png" class="h-24 w-auto" alt="meal:Pal">
            </div>
            <div class=" my-6">
              Find Meal:Pal
            </div>
          </div>
        </div>
      </div>
    </div>

  </div>
</div>
</template>

<script>
export default {
  name: 'HomePage',
  methods: {
    goFindFood () {
      this.$router.push('/findTastyFood')
    },
    goMealpal () {
      this.$router.push('/findMealPal')
    }
  }
}
</script>
<style>
</style>
