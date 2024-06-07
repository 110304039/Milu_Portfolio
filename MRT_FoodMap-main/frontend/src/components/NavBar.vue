<template>
    <header class="bg-white">
        <nav class="mx-auto flex max-w-7xl items-center justify-between pt-4" aria-label="Global">
            <div class="flex lg=justify-start">
                <div class="p-4 flex lg:flex-1">
                    <a href="#" class="-m-1.5 p-1.5">
                        <img class="h-8 w-auto" src="../assets/login_logo.png" alt="logo" />
                    </a>
                </div>
                <div class="p-4">
                    <p>MRT <span class="text-primary">F.o^o.d</span> Map</p>
                </div>
            </div>
            <div class="flex lg:jusitify-end">
                <div class="p-4 lg:justify-end" v-if="$store.state.P1_ID.length === 0">
                    <router-link to="/login" class="text-sm font-semibold leading-6 text-gray-900">
                            Log in
                    </router-link>
                </div>
                <div class="p-4 lg:justify-end">
                    <router-link to="/person" class="text-sm font-semibold leading-6 text-gray-900">
                        <img class="h-8 w-auto" src="../assets/user.png">
                    </router-link>
                </div>
                <div class="p-4 lg:justify-end">
                    <router-link to="/" class="text-sm font-semibold leading-6 text-gray-900">
                        <img class="h-8 w-auto" src="../assets/home.png">
                    </router-link>
                </div>
            </div>
        </nav>
    </header>
</template>
<script>
export default {
  name: 'NavBar',
  data () {
  },
  methods: {
  }
//   created () {
//     const P1_ID = this.$store.state.P1_ID
//     console.log('p1_ID::::', P1_ID, '---')
//     console.log(P1_ID.length)
//   }
}
</script>
<style>
</style>
