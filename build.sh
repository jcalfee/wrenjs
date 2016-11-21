#!/bin/bash

# Setup the PATH
source emsdk/emsdk_portable/emsdk_env.sh

# clone wren
#git clone https://github.com/munificent/wren.git

# Create a place for our outgoing wren.js file
mkdir -p out

# Move into the wren directory
cd wren

# clean up previous wren builds
make clean

# Use emscripten to generate a bytecode libwren.a, with extras
emmake make bytecode

# Move out of the wren directory
cd ..

# Compile the custom libwren.a with the js interface
$EMSCRIPTEN/emcc --bind src/shim.cpp wren/lib/wren.bc -o out/wren.js