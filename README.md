# Sized binary search tree

- Nievergelt & Reingold <1+sqrt 2,sqrt2> (METHOD=1)
- Roura (METHOD=2)
- Adams (4,2) (METHOD=3) -- this is the default code
- Adams (3,2) (METHOD=4)
- Adams <3,2> (METHOD=5)

# Tests with HUnit and QuickCheck

You can run tests:

	% cd test
	% runghc -i.. -DMETHOD=1 Test.hs
	% runghc -i.. Test.hs --maximum-generated-tests=10000 -t difference
