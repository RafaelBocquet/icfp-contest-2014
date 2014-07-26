#!/bin/sh
(
  echo "let not = \\ b:I.if b then 0 else 1 in ";
#  python make_quadtree.py;
  python make_btree.py;
#  cat lambdaman_source.hl
echo " __internal_btree_set_I_sort_list (cons 2 with cons 1 with cons (0-95) with empty I)"
) > lambdaman.hl