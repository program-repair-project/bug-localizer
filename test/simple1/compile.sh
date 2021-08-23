#!/usr/bin/env bash
cd src
gcc -o bug --coverage --save-temps -lgcov -g bug.c
cd ..
