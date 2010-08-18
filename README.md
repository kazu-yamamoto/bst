# Yet another binary search tree

Since array and hash is not friendly to purely functional programming,
binary search tree is an important data structure in Haskell.  Both
Data.Set and Data.Map is the binary search tree based on Adams's
technital report.

	http://www.swiss.ai.mit.edu/~adams/BB/

A bug of Data.Map was reported:

	http://hackage.haskell.org/trac/ghc/ticket/4242

This bug does not exist in Data.Set. The difference between Data.Set
and Data.Map is the value of ratio (1/alpha). Data.Set uses 4 while
Data.Map used 5.

Chaning ratio from 4 to 5 fixed the bug of Data.Map. But we cannot
explain it theoretically. I suspect that Adam's technical paper
includes some errors.

You can read the thread entitled "Bug in Data.Map":

	http://news.gmane.org/gmane.comp.lang.haskell.libraries

To know Data.Map well and to show another solution for binary search
tree, I'm implementing this based on Roura's paper:

	http://www.brics.dk/BRICS/ALCOM-FT/TR/ALCOMFT-TR-01-167.html


