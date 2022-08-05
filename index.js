const r = require('child_process');

const options = {
	stdio: "inherit"
}
const thing = r.spawn("./elm",  ["make","src/Main.elm","--output=compiler.js"],options)
thing.on('close',(code)=> {
	console.log(`Exited with code ${code}`)
})