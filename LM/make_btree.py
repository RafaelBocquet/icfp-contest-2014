#!/usr/bin/env python

def make_btree_set(name, ty_key, lte):
  print(
"""
let __internal_btree_set_{0}_lte = {2} in
type __internal_btree_set_{0}_maybe = <I, {1}> in

let __internal_btree_set_{0}_partition_list =
  fold
    ( \\x:{1}. \\acc:([{1}], [{1}]).
        (cons x with acc[1], acc[0])
    ) with (empty {1}, empty {1})
  in

letrec __internal_btree_set_{0}_fusion_list : [{1}] -> [{1}] -> [{1}] = \\l:[{1}]. \\l_:[{1}].
  if isempty l
    then l_
  else if isempty l_
    then l
  else if (head l) <= (head l_)
    then cons (head l) with (__internal_btree_set_{0}_fusion_list (tail l) l_)
  else
    cons (head l_) with (__internal_btree_set_{0}_fusion_list (tail l_) l)
  in

letrec __internal_btree_set_{0}_sort_list : [{1}] -> [{1}] = \\l:[{1}].
  if isempty l
    then empty {1}
  else if isempty (tail l)
    then l
  else
    let p = (\ok:([{1}],[{1}]). ok) (__internal_btree_set_{0}_partition_list l) in
    __internal_btree_set_{0}_fusion_list (__internal_btree_set_{0}_sort_list p[0]) (__internal_btree_set_{0}_sort_list p[1])
  in

type btree_set_{0} = <I, (?, {1}, ?)> in
let btree_set_{0}_empty = make btree_set_{0} 0 0 in
let btree_set_{0}_node = \\l:btree_set_{0}. \\x:{1}. \\r:btree_set_{0}. make btree_set_{0} 1 (l, x, r) in

""".format(name, ty_key, lte))

make_btree_set("I", "I", "\\x:I. \\y:I. x <= y")
# make_btree_set("II", "(I, I)", "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0]")