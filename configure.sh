#!/bin/bash

cd EssaMath
sudo chmod +x init.sh
./init.sh

cd ../Tests
sudo chmod +x init.sh
./init.sh && cd ..
