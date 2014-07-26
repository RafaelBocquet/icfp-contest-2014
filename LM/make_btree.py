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
  else if __internal_btree_set_{0}_lte (head l) (head l_)
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

let __internal_btree_set_{0}_length_list = fold (\\_:{1}. \\acc:I. acc + 1) with 0 in

type btree_set_{0} = <I, (?, {1}, ?)> in
let btree_set_{0}_empty = make btree_set_{0} 0 0 in
let btree_set_{0}_node = \\l:btree_set_{0}. \\x:{1}. \\r:btree_set_{0}. make btree_set_{0} 1 (l, x, r) in

letrec __internal_btree_set_{0}_from_list : I -> I -> [{1}] -> btree_set_{0} = \\b:I. \\n:I. \\li:[{1}]. 
  if isempty li
    then btree_set_{0}_empty
    else 
      let p = (natfold (\_:I. \p:([{1}],[{1}]). (cons (head p[1]) with p[0], tail p[1])) with (empty {1}, li)) (n / 2) in
      let l = __internal_btree_set_{0}_from_list (not b) (n / 2) p[0] in
      let r = __internal_btree_set_{0}_from_list b ((n - 1) / 2) (tail p[1]) in
      if b
        then btree_set_{0}_node l (head p[1]) r
        else btree_set_{0}_node r (head p[1]) l
  in

let btree_set_{0}_from_list = \\li:[{1}].
  let len = __internal_btree_set_{0}_length_list li in
  let sorted = __internal_btree_set_{0}_sort_list li in
  __internal_btree_set_{0}_from_list 1 len sorted
  in

""".format(name, ty_key, lte))

make_btree_set("I", "I", "\\x:I. \\y:I. x <= y")
make_btree_set("II", "(I, I)", "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0]")