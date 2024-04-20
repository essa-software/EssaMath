pwd

if [ ! -d src/maxima ]
then
    wget -c https://sourceforge.net/projects/maxima/files/Maxima-source/5.47.0-source/maxima-5.47.0.tar.gz/download -O maxima-5.47.0.tar.gz
    tar -xvzf maxima-5.47.0.tar.gz
    rm maxima-5.47.0.tar.gz
    mv maxima-5.47.0 src/maxima
    cat src/lisp/cxx-api.lisp >> src/maxima/src/init-cl.lisp
    cd src/maxima && ./configure --with-ecl
    cd ../..
fi

if [ ! -d build ]
then
    mkdir build && cd build
    cmake -GNinja ../..
else
    cd build
fi

sudo ninja -j3 install

cd ../src/go
sudo chmod +x init.sh
./init.sh

cd ../..
