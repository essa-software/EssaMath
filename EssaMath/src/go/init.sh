if [ ! -d build ]
then
    mkdir build && cd build
    cmake -GNinja ..
else
    cd build
fi

sudo ninja -j3 install
# go build ..
cd ../../..
