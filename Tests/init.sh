#!/bin/bash

if [ ! -d build ]
then
    mkdir build && cd build
    cmake -GNinja ../cxx
else
    cd build
fi

ninja
cd ../..