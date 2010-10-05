#! /bin/sh

make distclean

make clean
make METHOD=3
./Bench --compare= --name=adams42

make clean
make METHOD=4
./Bench --compare= --name=adams32

make clean
make METHOD=5
./Bench --compare= --name=nr32

./Bench -m graph --compare=nr32,adams32,adams42
