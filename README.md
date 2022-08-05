# KPL

***WARNING! THIS LANGUAGE IS STILL IN DEVELOPMENT- ANYTHING MAY CHANGE AT ANY MOMENT WITHOUT WARNING!***

OK Programming Language (KPL) is a (mostly) procedural C-style programming language designed to be 
extremely fast to write, extremely fast to run, and extremely reliable once it compiles.

## Dev Milestones

- [X] Compile to native instruction set (Currently only MacOS)
- [X] Statically Typed
- [ ] Turing complete
- [ ] Self Hosted (KPL compiler is written in KPL)
- [ ] Optimized
- [ ] Cross-Platform (MacOS, Windows and at least some Linuxes)

## Examples

Hello World:
```kpl
main() {
    print("Hello, world!")
}
```

## "Quick" start

Currently, the only way to use the compiler is to run
```bash
./elm make src/Main.elm
node index.js
```
This will launch the compiler in a webpage at `http://localhost:3000`. 

Enter (paste) your code into the text box, and hit `Compile`, then `Download Assembly`. This will give you a file containing x86-64 assembly. This then needs to be compiled by [nasm](https://nasm.us/) using
```sh
./nasm-2.15.05/nasm -fmacho64 ./prog.asm
ld -static -o prog prog.o
```
You can now run the executable
