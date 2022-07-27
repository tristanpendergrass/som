/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.{html,js,ts,jsx,tsx,elm}",
  ],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
}