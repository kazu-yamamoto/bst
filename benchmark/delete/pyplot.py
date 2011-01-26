#! /usr/bin/env python

# find the csv files

import fnmatch
import os

csv_files = []

for file in os.listdir('.'):
    if fnmatch.fnmatch(file, 'bench-*.csv'):
        csv_files.append(file)

# decide labels

def b(bench_type):
    return 'bench-' + bench_type + '.csv'

labels = {
    b('variant32'): '(3,2)',
    b('original32'): '<3,2>',
    b('variant42'): '(4,2)',
    b('log'): 'Log',
    b('c'): '<3/1, 2/1>',
    b('a'): '<5/2, 3/2>',
    b('b'): '<5/2, 7/5>',
    b('d'): '<3/1, 4/3>',
    b('f'): '<7/2, 9/7>',
    b('h'): '<4/1, 5/4>',
    b('g'): '<4/1, 5/3>',
    b('e'): '<7/2, 4/3>',
}

hatches = {
    b('variant32'): '/',
    b('original32'): '\\',
    b('variant42'): '-',
    b('log'): 'O',
    b('c'): '\\',
    b('a'): '-',
    b('b'): 'O',
    b('d'): '*',
    b('f'): '.',
    b('h'): '/ ',
    b('g'): 'o',
    b('e'): 'x',
}    

for file in csv_files:
    labels[file] # when this raise exception, fix labels

# read csv

import csv

data = {}

for file in csv_files:
    reader = csv.DictReader(open(file, 'rb'), delimiter=',', quotechar="\"")
    file_data = {}
    for row in reader:
        file_data[row['Name']] = row
    data[file] = file_data

measurements = [] # will contain 'rnd 10^3' and so on

for file in csv_files:
    reader = csv.DictReader(open(file, 'rb'), delimiter=',', quotechar="\"")
    for row in reader:
        measurements.append(row['Name'])
    break
    
# plot

import numpy as np
import matplotlib.pyplot as plt

# how many lines in each csv?
for file in csv_files:
    N = len(data[file])
    break

for file in csv_files:
    if N != len(data[file]):
        raise LookupException

# rnd 10^3 -> 1000
def name2num(name):
    tail = name[-4:]
    if tail == '10^3':
        return 1000.0
    if tail == '10^4':
        return 10000.0
    if tail == '10^5':
        return 100000.0
    
# taking numbers

Means = {}
Err = {}
    
for file in csv_files:
    means_list = []
    errrs_list = []
    d = data[file]
    for m in measurements:
        num = name2num(m) / 1000000.0
        means_list.append(float(d[m]['Mean']) / num)
        errrs_list.append((float(d[m]['MeanUB']) - float(d[m]['MeanLB'])) / num)

    Means[file] = tuple(means_list) # in microseconds
    Err[file] = tuple(errrs_list)

ind = np.arange(N)  # the x locations for the groups
orig_ind = ind
width = 0.08     # the width of the bars
space = 0.02
Width = width + space

offset = (ind[1] - ind[0] - Width * len(csv_files)) / 2
ind = ind + offset

fig = plt.figure(figsize=(12,6))
ax = fig.add_subplot(111)

rects = {}
for file in csv_files:
    rects[file] = ax.bar(ind, Means[file], width, color='white', hatch=hatches[file], yerr=Err[file], ecolor='black')
    ind = ind + Width

ax.set_ylabel('time (micro sec) per operation')
# ax.set_title('Scores by group and gender')
ax.set_xticks(orig_ind+ (ind[1] - ind[0]) / 2.0)
ax.set_xticklabels( tuple(measurements) )

import matplotlib.patches as pa

legend0_list = []
legend1_list = []
for file in csv_files:
    legend0_list.append( pa.Rectangle((0,0),1,1,hatch=hatches[file],color='white',ec='black') )
    legend1_list.append( labels[file] )

ax.legend( tuple(legend0_list), tuple(legend1_list), bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0. )

# def autolabel(rects):
#     # attach some text labels
#     for rect in rects:
#         height = rect.get_height()
#         ax.text(rect.get_x()+rect.get_width()/2., 1.05*height, '%d'%int(height),
#                 ha='center', va='bottom')

# autolabel(rects1)
# autolabel(rects2)

plt.subplots_adjust(right=0.7)

plt.show()
