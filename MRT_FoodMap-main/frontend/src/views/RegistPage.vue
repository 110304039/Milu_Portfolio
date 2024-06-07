<template>
   <div class="hero min-h-[80vh] bg-base-200">
    <div class="hero-content flex-col lg:flex-row-reverse">
      <div class="text-center p-8 lg:text-left lg:w-1/2">
        <div class="flex justify-center lg:justify-start">
          <img class="h-40 w-auto md-4" src="../assets/login_logo.png" alt="logo" />
        </div>
        <h1 class="text-5xl font-bold">Sign up now!</h1>
        <p class="py-6">Sign up an new account and start your f.o^o.d trip now!</p>
        <div class="mt-2 text-center text-sm text-gray-500 lg:text-left">
          Have an account already?
          {{ '   ' }}
          <router-link to="/login" class="font-semibold leading-6 text-primary hover:text-secondary">
                            Log in
          </router-link>
        </div>
      </div>
      <form class="w-full lg:w-1/2" action="#" method="POST" @submit.prevent="addPerson">
        <div class="card flex-shrink-0 w-full shadow-2xl bg-base-100">
          <div class="card-body">
            <div class="form-control">
              <label class="label">
                <span class="label-text text-primary">Account</span>
              </label>
              <input v-model="id" id="text" name="text" type="text" autocomplete="userid" class="input input-bordered" required=""/>
            </div>
            <div class="form-control">
              <label class="label">
                <span class="label-text text-primary">Password</span>
              </label>
              <input v-model="password" id="password" name="password" type="password" autocomplete="current-password" class="input input-bordered" required=""/>
            </div>
            <div class="form-control">
              <label class="label">
                <span class="label-text">Name</span>
              </label>
              <input v-model="name" id="name" name="name" type="text" autocomplete="username" class="input input-bordered" required=""/>
            </div>
            <div class="form-control">
              <label class="label">
                <span class="label-text">Location</span>
              </label>
              <input v-model="location" id="location" name="location" autocomplete="location" type="text" class="input input-bordered" required=""/>
            </div>
            <div class="form-control mt-6">
              <button class="btn btn-primary" type="submit">Sign up</button>
            </div>
          </div>
        </div>
    </form>

    </div>
  </div>
</template>

<script>
import axios from 'axios'

function goToLogin () {
  this.$router.push('/login')
}

function regist () {
  console.log('Username:', this.username)
  console.log('Password:', this.password)
  // 可以向後端發送登入請求或執行其他相關操作
}

async function addPerson () {
  console.log(this.id, this.name, this.password, this.location)
  try {
    const config = { headers: { 'Content-Type': 'application/json' } }
    const response = await axios.post('http://127.0.0.1:5000/api/addPerson', { PersonID: this.id, Name: this.name, Password: this.password, Location: this.location }, config)
    console.log(response.data)
    this.goToLogin()
  } catch (error) {
    console.error('An error occurred:', error)
  }
}

export default {
  name: 'RegistPage',
  data () {
    return {
      id: '',
      password: '',
      name: '',
      location: ''
    }
  },
  methods: {
    regist: regist,
    goToLogin: goToLogin,
    addPerson: addPerson
  }
}
</script>

<style>
</style>
