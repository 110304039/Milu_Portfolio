import { createStore } from 'vuex'

const store = createStore({
  state () {
    return {
      favorites: [],
      histories: [],
      P1_ID: ''
    }
  },
  mutations: {
    addFavorite (state, favorite) {
      state.favorites.push(favorite)
    },
    removeFavorite (state, favoriteId) {
      state.favorites = state.favorites.filter(
        favorite => favorite.id !== favoriteId
      )
    },
    addHistory (state, history) {
      state.histories.push(history)
    },
    setP1_ID (state, id) {
      state.P1_ID = id
    }
  },
  actions: {
    addFavorite (context, favorite) {
      context.commit('addFavorite', favorite)
    },
    removeFavorite (context, favoriteId) {
      context.commit('removeFavorite', favoriteId)
    },
    addHistory (context, history) {
      context.commit('addHistory', history)
    }
  },
  getters: {
    favorites (state) {
      return state.favorites
    },
    histories (state) {
      return state.histories
    }
  }
})

export default store
