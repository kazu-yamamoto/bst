#! /bin/sh

gnuplot <<EOF
set title "Sized balance search tree"
set xlabel "delta"
set ylabel "ratio"
set nokey
set xrange [2:5]
set yrange [0.8:2.2]
f1(x)=(x+1)/x
f2(x)=x-1
plot 'pass.dat' using 1:2 with points, 'fail.dat' using 1:2 with points, f1(x),f2(x)
set terminal png inter size 1024,768
set output "results.png"
replot
EOF
