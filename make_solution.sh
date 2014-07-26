#!/bin/sh
# rm -r _solution
mkdir _solution
mkdir -p _solution/solution
mkdir -p _solution/code
cp test.hl.out _solution/solution/lambdaman.gcc
cp -r lib _solution/code
cp -r src _solution/code
cp  ICFP.cabal _solution/code
cp  README _solution/code
tar -czf ICFP.tar.gz _solution
sha1sum ICFP.tar.gz > ICFP.tar.gz.sha1sum
rm -r _solution