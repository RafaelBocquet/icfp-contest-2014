#!/bin/sh
(
  echo "let not = \\ b:I.if b then 0 else 1 in let or  = \\ a:I. \\ b:I. if b then 1 else a in let and = \\ a:I. \\ b:I. if b then a else 0 in";
  python make_quadtree_256.py;
  python make_btree.py;
  python make_heap.py;
  cat lambdaman_source.hl
) > lambdaman.hl