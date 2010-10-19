#! /bin/sh

gnuplot <<EOF
set terminal postscript eps enhanced
set output "plot.eps"
set size 0.7,0.7
set xtics rotate;
set xrange [-0.125:5.375];
set yrange [0:1.4];
set datafile separator ',';
set style data boxerrorbars;set style fill pattern;
plot 'plot.csv' using (\$0+0.0):2:3:4:(0.125):xtic(1) title 'original32','plot.csv' using (\$0+0.125):5:6:7:(0.125):xtic(1) title 'variant32','plot.csv' using (\$0+0.25):8:9:10:(0.125):xtic(1) title 'variant42'
