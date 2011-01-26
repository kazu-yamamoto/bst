#! /bin/sh

make distclean

make clean
make METHOD=5
sleep 60
./Bench -m run --compare= --name=c

make clean
make METHOD=6
sleep 60
./Bench -m run --compare= --name=a

make clean
make METHOD=7
sleep 60
./Bench -m run --compare= --name=b

make clean
make METHOD=9
sleep 60
./Bench -m run --compare= --name=d

make clean
make METHOD=9
sleep 60
./Bench -m run --compare= --name=f

make clean
make METHOD=10
sleep 60
./Bench -m run --compare= --name=h

make clean
make METHOD=11
sleep 60
./Bench -m run --compare= --name=g

make clean
make METHOD=12
sleep 60
./Bench -m run --compare= --name=e

./Bench -m graph  --plot=delete.png --compare=a,b,c,d,e,f,g,h
