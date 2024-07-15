#!/bin/bash

if [ ! -d essamath/maxima ]
then
    wget -c https://sourceforge.net/projects/maxima/files/Maxima-source/5.47.0-source/maxima-5.47.0.tar.gz/download -O maxima-5.47.0.tar.gz
    tar -xvzf maxima-5.47.0.tar.gz
    rm maxima-5.47.0.tar.gz
    mv maxima-5.47.0 essamath/maxima
    cat essamath/src/lisp/cxx-api.lisp >> essamath/maxima/src/init-cl.lisp
    cd essamath/maxima && ./configure --with-ecl
    cd ../..
fi
conan profile detect
conan install . --output-folder=build --build=missing
