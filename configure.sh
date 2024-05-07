#!/bin/bash

if [ ! -d src/maxima ]
then
    wget -c https://sourceforge.net/projects/maxima/files/Maxima-source/5.47.0-source/maxima-5.47.0.tar.gz/download -O maxima-5.47.0.tar.gz
    tar -xvzf maxima-5.47.0.tar.gz
    rm maxima-5.47.0.tar.gz
    mv maxima-5.47.0 EssaMath/src/maxima
    cat EssaMath/src/lisp/cxx-api.lisp >> EssaMath/src/maxima/src/init-cl.lisp
    cd EssaMath/src/maxima && ./configure --with-ecl
    cd ../../..
fi
