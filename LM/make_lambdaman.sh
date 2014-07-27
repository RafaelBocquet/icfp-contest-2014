#!/bin/sh
(
  echo "let not = \\ b:I.if b then 0 else 1 in ";
  python make_quadtree.py;
  python make_btree.py;
  cat lambdaman_source.hl
) > lambdaman.hl