#!/bin/sh
# rm -r _solution
mkdir _solution
mkdir -p _solution/solution
mkdir -p _solution/code
cp LM/lambdaman.hl.out _solution/solution/lambdaman.gcc
for i in 1 2 3 4; do cp GH/v2/ghost$i.hg.out _solution/solution/ghost`expr $i - 1`.ghc; done;
cp -r lib _solution/code
cp -r src _solution/code
cp -r LM _solution/code
cp -r GH _solution/code
cp  ICFP.cabal _solution/code
cp  README _solution/code
tar -czf ICFP.tar.gz _solution
sha1sum ICFP.tar.gz > ICFP.tar.gz.sha1sum
rm -r _solution