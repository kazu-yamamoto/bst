#! /bin/sh

make distclean

make clean
make METHOD=2
sleep 60
./Bench -m run --compare= --name=log

make clean
make METHOD=3
sleep 60
./Bench -m run --compare= --name=variant42

make clean
make METHOD=4
sleep 60
./Bench -m run --compare= --name=variant32

make clean
make METHOD=5
sleep 60
./Bench -m run --compare= --name=original32

./Bench -m graph --compare=original32,variant32,variant42,log
