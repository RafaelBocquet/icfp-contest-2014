#!/usr/bin/env python

def make_btree_set(name, ty_key, eq, lte):
  print(
"""
let __internal_btree_set_{0}_eq = {2} in
let __internal_btree_set_{0}_lte = {3} in

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

letrec btree_set_{0}_insert : {1} -> btree_set_{0} -> btree_set_{0} = \\x:{1}.
  destruct
    ( \\_:I. btree_set_{0}_node btree_set_{0}_empty x btree_set_{0}_empty
    , \\h:(btree_set_{0}, {1}, btree_set_{0}).
        if __internal_btree_set_{0}_eq x h[1]
          then make btree_set_{0} 1 h
        else if __internal_btree_set_{0}_lte x h[1]
          then btree_set_{0}_node (btree_set_{0}_insert x h[0]) h[1] h[2] 
          else btree_set_{0}_node h[0] h[1] (btree_set_{0}_insert x h[2])
    )
  in

letrec __internal_btree_set_{0}_merge : I -> btree_set_{0} -> btree_set_{0} -> btree_set_{0} = \\b:I.
  destruct
    ( \\_:I. \\r:btree_set_{0}. r
    , \\l:(btree_set_{0}, {1}, btree_set_{0}).
        destruct
          ( \\_:I. (make btree_set_{0} 1 l)
          , \\r:(btree_set_{0}, {1}, btree_set_{0}).
              if b
                then
                  make btree_set_{0} 1
                    ( __internal_btree_set_{0}_merge (not b) (make btree_set_{0} 1 l) r[0]
                    , r[1]
                    , r[2]
                    )
                else
                  make btree_set_{0} 1
                    ( l[0]
                    , l[1]
                    , __internal_btree_set_{0}_merge (not b) l[2] (make btree_set_{0} 1 r)
                    )
          )
    )
  in

letrec btree_set_{0}_find : {1} -> btree_set_{0} -> I = \\x:{1}.
  destruct
    ( \\_:I. 0
    , \\t:(btree_set_{0}, {1}, btree_set_{0}).
        if __internal_btree_set_{0}_eq x t[1]
          then 1
        else if __internal_btree_set_{0}_lte x t[1]
          then btree_set_{0}_find x t[0]
          else btree_set_{0}_find x t[2]
    )
  in

letrec btree_set_{0}_delete : {1} -> btree_set_{0} -> btree_set_{0} = \\x:{1}.
  destruct
    ( \\_:I. btree_set_{0}_empty
    , \\t:(btree_set_{0}, {1}, btree_set_{0}).
        if __internal_btree_set_{0}_eq x t[1]
          then __internal_btree_set_{0}_merge 1 t[0] t[2]
        else if __internal_btree_set_{0}_lte x t[1]
          then make btree_set_{0} 1 (btree_set_{0}_delete x t[0], t[1], t[2])
          else make btree_set_{0} 1 (t[0], t[1], btree_set_{0}_delete x t[2])
    )
  in

""".format(name, ty_key, eq, lte))

def make_btree_map(name, ty_key, eq, lte, ty_val):
  make_btree_set(name, ty_key, eq, lte)
  print(
"""
let __internal_btree_map_{0}_eq = {2} in
let __internal_btree_map_{0}_lte = {3} in
type btree_map_{0}_maybe = <I, {4}> in

type btree_map_{0} = <I, (?, {1}, {4}, ?)> in
let btree_map_{0}_empty = make btree_map_{0} 0 0 in
let btree_map_{0}_node = \\l:btree_map_{0}. \\x:{1}. \\v:{4}. \\r:btree_map_{0}. make btree_map_{0} 1 (l, x, v, r) in

letrec btree_map_{0}_find : {1} -> btree_map_{0} -> btree_map_{0}_maybe = \\x:{1}.
  destruct
    ( \\_:I. make btree_map_{0}_maybe 0 0
    , \\t:(btree_map_{0}, {1}, {4}, btree_map_{0}).
        if __internal_btree_map_{0}_eq x t[1]
          then make btree_map_{0}_maybe 1 t[2]
        else if __internal_btree_map_{0}_lte x t[1]
          then btree_map_{0}_find x t[0]
          else btree_map_{0}_find x t[3]
    )
  in

letrec btree_map_{0}_update : {1} -> ({4} -> {4}) -> btree_map_{0} -> btree_map_{0} = \\x:{1}. \\f:({4} -> {4}).
  destruct
    ( \\_:I. btree_map_{0}_empty
    , \\t:(btree_map_{0}, {1}, {4}, btree_map_{0}).
        if __internal_btree_map_{0}_eq x t[1]
          then btree_map_{0}_node t[0] t[1] (f t[2]) t[3]
        else if __internal_btree_map_{0}_lte x t[1]
          then btree_map_{0}_node (btree_map_{0}_update x f t[0]) t[1] t[2] t[3]
          else btree_map_{0}_node t[0] t[1] t[2] (btree_map_{0}_update x f t[3])
    )
  in

letrec btree_map_{0}_from_set : {4} -> btree_set_{0} -> btree_map_{0} = \\x:{4}.
  destruct
    ( \\_:I. btree_map_{0}_empty
    , \\t:(btree_set_{0}, {1}, btree_set_{0}).
        btree_map_{0}_node (btree_map_{0}_from_set x t[0]) t[1] x (btree_map_{0}_from_set x t[2])
    )
  in

letrec btree_map_{0}_from_set_generate : ({1} -> {4}) -> btree_set_{0} -> btree_map_{0} = \\f:({1}->{4}).
  destruct
    ( \\_:I. btree_map_{0}_empty
    , \\t:(btree_set_{0}, {1}, btree_set_{0}).
        btree_map_{0}_node (btree_map_{0}_from_set_generate f t[0]) t[1] (f t[1]) (btree_map_{0}_from_set_generate f t[2])
    )
  in

""".format(name, ty_key, eq, lte, ty_val))

make_btree_map("I", "I", "\\x:I. \\y:I. x == y", "\\x:I. \\y:I. x <= y", "I")
make_btree_map("II", "(I, I)"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0]"
  , "I")
make_btree_map("IIL", "(I, I)"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0]"
  , "[(I, I)]")
make_btree_map(
"V"
, "((I,I),(I,I))"
, """
let __cmpeq = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0 in
\\x:((I,I),(I,I)). \\y:((I,I),(I,I)). if __cmpeq x[0] y[0] then __cmpeq x[1] y[1] else 0
"""
, """
let __cmplte = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0] in
let __cmpeq = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0 in
\\x:((I,I),(I,I)). \\y:((I,I),(I,I)). if __cmpeq x[0] y[0] then __cmplte x[1] y[1] else __cmplte x[0] y[0]
"""
, "I")
