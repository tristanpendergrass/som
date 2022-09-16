/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.{html,js,ts,jsx,tsx,elm}",
  ],
  theme: {
    extend: {
      transitionProperty: {
        'height': 'height',
      },
      animation: {
        'flash': 'flash 1s linear'
      },
      keyframes: {
        'flash': {
          '0%': {'background-color': 'hsl(var(--a))'},
          '100%': {'background-color': 'hsl(var(--b1))'}
        }
      }
    },
  },
  plugins: [require("daisyui")],
}