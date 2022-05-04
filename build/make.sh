#!/bin/bash
./nasm-2.15.05/nasm -fmacho64 ./prog.asm && ld -static -o prog prog.o && ./prog || echo "exited with status" $?