# Sized binary search tree

- Nievergelt (METHOD=1)
- Roura (METHOD=2)
- Adams (METHOD=3)

# Tests with HUnit and QuickCheck

You can run tests:

	% cd test
	% runghc -i.. -DMETHOD=1 Test.hs
	% runghc -i.. Test.hs --maximum-generated-tests=10000 -t difference
