#! /bin/sh

gnuplot <<EOF
set terminal postscript eps enhanced
set output "plot.eps"
set size 0.7,0.7
set xtics rotate;
set xrange [-0.125:6];
set yrange [0:1.4];
set datafile separator ',';
set style data boxerrorbars;set style fill pattern;
plot 'plot.csv' using (\$0+0.0):2:3:4:(0.125):xtic(1) title 'original3-2','plot.csv' using (\$0+0.125):5:6:7:(0.125):xtic(1) title 'original4-54','plot.csv' using (\$0+0.25):8:9:10:(0.125):xtic(1) title 'original52-32','plot.csv' using (\$0+0.375):11:12:13:(0.125):xtic(1) title 'original92-119','plot.csv' using (\$0+0.5):14:15:16:(0.125):xtic(1) title 'original92-52','plot.csv' using (\$0+0.625):17:18:19:(0.125):xtic(1) title 'original92-53'
