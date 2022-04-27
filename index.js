const express = require('express')
const app = express()
const port = 3000

app.get('/', (req, res) => {
  res.sendFile(__dirname+'/index.html')
})
app.get('/main.js', (req, res) => {
  res.sendFile(__dirname+'/main.js')
})
app.listen(port, () => {
  console.log(`Test Server listening at http://localhost:${port}`)
})