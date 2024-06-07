/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./public/**/*.html', './src/**/*.{vue,js,ts,jsx,tsx}'],
  theme: {
    fontFamily: {
      custom: ['Source Code Pro', 'sans-serif'],
    },
    extend: {}
  },
  daisyui: {
    themes: ['bumblebee']
  },
  plugins: [
    require('daisyui'),
    require('@tailwindcss/forms')
  ]
}
