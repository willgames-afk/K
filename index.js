const r = require('child_process');

r.exec("./elm make src/Main.elm --output=compiler.js")
const code = require("./compiler.js")