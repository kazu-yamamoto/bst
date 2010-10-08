#! /bin/sh

make distclean

make clean
make METHOD=3
sleep 60
./Bench -m run --compare= --name=adams42

make clean
make METHOD=4
sleep 60
./Bench -m run --compare= --name=adams32

make clean
make METHOD=5
sleep 60
./Bench -m run --compare= --name=nr32

./Bench -m graph --compare=nr32,adams32,adams42
