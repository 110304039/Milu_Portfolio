import { createRouter, createWebHistory } from 'vue-router'
import LoginPage from '../views/LoginPage.vue'
import HomePage from '../views/HomePage.vue'
import RandomPair from '../views/RandomPair.vue'
import InvitationPage from '../views/InvitationPage.vue'
import FavoriteList from '../views/FavoriteList.vue'
import HistoryList from '../views/HistoryList.vue'
import FindTastyFood from '../views/FindTastyFood.vue'
import FindMealpal from '../views/FindMealpal.vue'
import NewEvent from '../views/NewEvent.vue'
import IntersectionPage from '../views/IntersectionPage.vue'
import PersonalPage from '../views/PersonalPage.vue'
import RegistPage from '../views/RegistPage.vue'
import HomeView from '../views/HomeView.vue'
import ChatRoom from '../views/ChatRoom.vue'

const routes = [
  {
    path: '/',
    name: 'home',
    component: HomePage
  },
  { path: '/login', name: 'LoginPage', component: LoginPage },
  { path: '/homepage', name: 'HomePage', component: HomePage },
  { path: '/invite', name: 'InvitationPage', component: InvitationPage },
  { path: '/randomPair', name: 'RandomPair', component: RandomPair },
  { path: '/favorite', name: 'FavoriteList', component: FavoriteList },
  { path: '/history', name: 'HistoryList', component: HistoryList },
  { path: '/findTastyFood', name: 'FindTastyFood', component: FindTastyFood },
  { path: '/findMealPal', name: 'FindMealpal', component: FindMealpal },
  { path: '/newEvent', name: 'NewEvent', component: NewEvent },
  { path: '/intersection', name: 'IntersectionPage', component: IntersectionPage },
  { path: '/person', name: 'PersonalPage', component: PersonalPage },
  { path: '/regist', name: 'RegistPage', component: RegistPage },
  { path: '/testpage', name: 'TestPage', component: HomeView },
  { path: '/chatroom', name: 'ChatRoom', component: ChatRoom }
]

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes
})

export default router
