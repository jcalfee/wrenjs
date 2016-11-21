#!/bin/bash

# Setup the PATH
source emsdk/emsdk_portable/emsdk_env.sh

# clone wren
git clone https://github.com/munificent/wren.git

# Create a place for our outgoing wren.js file
mkdir -p out

# Move into the wren directory
cd wren

# clean up previous wren builds
make clean

# Use emscripten to generate a bytecode libwren.a, with extras
emmake make shared

# Move out of the wren directory
cd ..

# Copy our bytecode lib
cp wren/lib/libwren.so src/wren.bc

# Compile the custom libwren.a with the js interface
$EMSCRIPTEN/emcc --bind src/shim.cpp src/wren.bc -o out/wren.js