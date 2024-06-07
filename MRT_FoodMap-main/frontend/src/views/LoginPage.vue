<template>
  <div class="hero min-h-[80vh] bg-base-200">
    <div class="hero-content flex-col lg:flex-row-reverse">
      <div class="text-center p-8 lg:text-left lg:w-1/2">
        <div class="flex justify-center lg:justify-start">
          <img class="h-40 w-auto md-4" src="../assets/login_logo.png" alt="logo" />
        </div>
        <h1 class="text-5xl font-bold">Log in now!</h1>
        <p class="py-6">Log in to your account and start your f.o^o.d trip!</p>
        <div class="mt-2 text-center text-sm text-gray-500 lg:text-left">
          Don't have an account?
          {{ '   ' }}
          <router-link to="/regist" class="font-semibold leading-6 text-primary hover:text-secondary">
                            Sign up
          </router-link>
        </div>
      </div>
      <form class="w-full lg:w-1/2" action="#" method="POST" @submit.prevent="login">
        <div class="card flex-shrink-0 w-full shadow-lg bg-base-100">
          <div class="card-body">
            <div class="form-control">
              <label class="label">
                <span class="label-text">Account</span>
              </label>
              <input v-model="id" id="text" name="text" type="text" autocomplete="userid" class="input input-bordered" required=""/>
            </div>
            <div class="form-control">
              <label class="label">
                <span class="label-text">Password</span>
              </label>
              <input v-model="password" id="password" name="password" type="password" autocomplete="current-password" class="input input-bordered" required=""/>
              <label class="label">
                <a href="#" class="label-text-alt link link-hover text-primary hover:text-secondary">Forgot password?</a>
              </label>
              <div class="form-control">
                <label class="label cursor-pointer">
                  <span class="label-text">Remember me</span>
                  <input type="checkbox" checked="checked" class="checkbox" />
                </label>
              </div>
            </div>
            <div class="form-control mt-6">
              <button class="btn btn-primary" type="submit">Log in</button>
            </div>
          </div>
        </div>
      </form>
    </div>
  </div>
</template>
<script>
import axios from 'axios'

async function login () {
  try {
    const response = await axios.get(`http://127.0.0.1:5000/api/login/${this.id}/${this.password}`)
    console.log(response.data)
    this.$store.commit('setP1_ID', this.id)
    const P1_ID = this.$store.state.P1_ID
    console.log('p1_ID::::', P1_ID, '---')
    this.goHomePage()
  } catch (error) {
    console.error(error)
  }
}

function goMealpal () {
  this.$router.push('/mealpal')
}

function goRegist () {
  this.$router.push('/regist')
}

function goHomePage () {
  this.$router.push('/homepage')
}

export default {
  name: 'LoginPage',
  data () {
    return {
      id: '',
      password: ''
    }
  },
  methods: {
    goMealpal: goMealpal,
    goRegist: goRegist,
    goHomePage: goHomePage,
    login: login
  }
}
</script>

<style></style>
