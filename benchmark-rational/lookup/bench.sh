#! /bin/sh

make distclean

make clean
make METHOD=5
sleep 60
./Bench -m run --compare= --name=original3-2

make clean
make METHOD=6
sleep 60
./Bench -m run --compare= --name=original4-54

make clean
make METHOD=7
sleep 60
./Bench -m run --compare= --name=original92-119

make clean
make METHOD=9
sleep 60
./Bench -m run --compare= --name=original92-53

make clean
make METHOD=9
sleep 60
./Bench -m run --compare= --name=original52-32

make clean
make METHOD=10
sleep 60
./Bench -m run --compare= --name=original92-52

./Bench -m graph --compare=original3-2,original4-54,original92-119,original92-53,original52-32,original92-52

sh plot.sh
