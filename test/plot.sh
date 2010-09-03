#! /bin/sh

gnuplot <<EOF
set title "Weight balance search tree"
set xlabel "weight"
set ylabel "ratio"
set xrange [1:7]
set yrange [0.8:2.2]
f1(x)=(x+1)/x
f2(x)=x-1
plot 'pass.dat' using 1:2 with points, 'fail.dat' using 1:2 with points, f1(x),f2(x)
set terminal png inter size 1024,768
set output "results.png"
replot
EOF
