#!/usr/bin/env python3


def R(fpath):
    with open(fpath,'r') as fin:
        return fin.read().splitlines()



import numpy as np
import sys
import hypertools as hyp

if len(sys.argv) != 2:
    print("Usage: %s l.txt" % sys.argv[0])
    exit()


rs = R(sys.argv[1])


data = []
loss = []
for line in rs:
    vs = list( map(float, line.split(', ')) )
    loss.append(vs[0])
    data.append( vs[1:] )

#print(loss)

hyp.plot( np.array( data ), reduce='PCA')

