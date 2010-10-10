#! /bin/sh

make distclean

make clean
make SCHEME=1
sleep 60
./Bench -m run --compare= --name=adams42

make clean
make SCHEME=2
sleep 60
./Bench -m run --compare= --name=random

./Bench -m graph --compare=adams42,random
