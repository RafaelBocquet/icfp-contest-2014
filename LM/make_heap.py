#!/usr/bin/env python

def make_heap(name, ty, eq, lte):
  print(
"""
let __internal_heap_min_{0}_eq = {2} in
let __internal_heap_min_{0}_lte = {3} in
type heap_min_{0}_maybe = <I, {1}> in

let heap_min_{0}_tag_complete = 0 in
let heap_min_{0}_tag_left = 1 in
let heap_min_{0}_tag_right = 2 in

type heap_min_{0} = <I, (I, ?, {1}, ?)> in

let heap_min_{0}_tag = destruct (\\_:I. heap_min_{0}_tag_complete, \\h:(I, heap_min_{0}, {1}, heap_min_{0}). h[0]) in

let heap_min_{0}_empty = make heap_min_{0} 0 0 in
let heap_min_{0}_node = \\l:heap_min_{0}. \\x:{1}. \\r:heap_min_{0}.
  make heap_min_{0} 1
    (if heap_min_{0}_tag l == heap_min_{0}_tag_complete
      then if heap_min_{0}_tag r == heap_min_{0}_tag_complete
        then heap_min_{0}_tag_complete
        else heap_min_{0}_tag_right
      else heap_min_{0}_tag_left
    , l
    , x
    , r
    )
  in
let heap_min_{0}_singleton = \\x:{1}. heap_min_{0}_node heap_min_{0}_empty x heap_min_{0}_empty in

let heap_min_{0}_min =
  destruct
    ( \\_:I. make heap_min_{0}_maybe 0 0
    , \\h:(I, heap_min_{0}, {1}, heap_min_{0}). make heap_min_{0}_maybe 1 h[2]
    )
  in

letrec __internal_heap_min_{0}_insert : {1} -> heap_min_{0} -> (heap_min_{0}, {1}, heap_min_{0}) = \\x:{1}.
  destruct
    ( \\_:I. (heap_min_{0}_empty, x, heap_min_{0}_empty)
    , \\h:(I, heap_min_{0}, {1}, heap_min_{0}).
        if h[0] == heap_min_{0}_tag_right
          then
            let r_ = __internal_heap_min_{0}_insert x h[3] in
            if __internal_heap_min_{0}_lte h[2] r_[1]
              then
                (h[1], h[2], heap_min_{0}_node r_[0] r_[1] r_[2])
              else
                (h[1], r_[1], heap_min_{0}_node r_[0] h[2] r_[2])
          else
            let l_ = __internal_heap_min_{0}_insert x h[1] in
            if __internal_heap_min_{0}_lte h[2] l_[1]
              then
                (heap_min_{0}_node l_[0] l_[1] l_[2], h[2], h[3])
              else
                (heap_min_{0}_node l_[0] h[2] l_[2], l_[1], h[3])
    )
  in

let heap_min_{0}_insert = \\x:{1}. \\h:heap_min_{0}.
  let h_ = __internal_heap_min_{0}_insert x h in
  heap_min_{0}_node h_[0] h_[1] h_[2]
  in

letrec __internal_heap_min_{0}_delete_2 : heap_min_{0} -> heap_min_{0} =
  destruct
    ( \\_:I. heap_min_{0}_empty
    , \\h:(I, heap_min_{0}, {1}, heap_min_{0}).
        (destruct
          ( \\__:I. make heap_min_{0} 1 h
          , \\l:(I, heap_min_{0}, {1}, heap_min_{0}).
            (destruct
              ( \\___:I.
                  if __internal_heap_min_{0}_lte l[2] h[2]
                    then make heap_min_{0} 1 (h[0], make heap_min_{0} 1 (l[0], l[1], h[2], l[3]), l[2], h[3])
                    else make heap_min_{0} 1 h
              , \\r:(I, heap_min_{0}, {1}, heap_min_{0}).
                  if and (__internal_heap_min_{0}_lte l[2] h[2]) (__internal_heap_min_{0}_lte l[2] r[2])
                    then make heap_min_{0} 1 (h[0], make heap_min_{0} 1 (l[0], l[1], h[2], l[3]), l[2], h[3])
                  else if __internal_heap_min_{0}_lte r[2] h[2]
                    then make heap_min_{0} 1 (h[0], h[1], r[2], make heap_min_{0} 1 (r[0], r[1], h[2], r[3]))
                  else make heap_min_{0} 1 h
              )
            ) h[3]
          )
        ) h[1]
    )
  in

letrec __internal_heap_min_{0}_delete_1 : (I, heap_min_{0}, {1}, heap_min_{0}) -> (heap_min_{0}, {1}) = \\h:(I, heap_min_{0}, {1}, heap_min_{0}).
  (destruct
    ( \\__:I. (heap_min_{0}_empty, h[2])
    , \\l:(I, heap_min_{0}, {1}, heap_min_{0}).
        (destruct
          ( \\___:I.
              let p = __internal_heap_min_{0}_delete_1 l in
              (heap_min_{0}_node p[0] h[2] h[3], p[1])
          , \\r:(I, heap_min_{0}, {1}, heap_min_{0}).
              if or
                  (h[0] == heap_min_{0}_tag_complete)
                  (and
                    (h[0] == heap_min_{0}_tag_right)
                    (r[0] == heap_min_{0}_tag_complete)
                  )
                then
                  let p = __internal_heap_min_{0}_delete_1 r in
                  (heap_min_{0}_node h[1] h[2] p[0], p[1])
                else
                  let p = __internal_heap_min_{0}_delete_1 l in
                  (heap_min_{0}_node p[0] h[2] h[3], p[1])
          ) 
        ) h[3]
    )
  ) h[1]
  in

let heap_min_{0}_delete = 
  destruct
    ( \\_:I. heap_min_{0}_empty
    , \\h:(I, heap_min_{0}, {1}, heap_min_{0}).
        let p = __internal_heap_min_{0}_delete_1 h in
        ( destruct
            ( \\_:I. heap_min_{0}_empty
            , \\p_:(I, heap_min_{0}, {1}, heap_min_{0}).
                __internal_heap_min_{0}_delete_2 (heap_min_{0}_node p_[1] p[1] p_[3])
            )
        ) p[0]
    )
  in

""".format(name, ty, eq, lte))

make_heap("I", "I", "\\x:I. \\y:I. x == y", "\\x:I. \\y:I. x <= y")
make_heap("II", "(I, I)"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0"
  , "\\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0]")
make_heap("V", "((I,I),(I,I))"
, """
let __cmpeq = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0 in
\\x:((I,I),(I,I)). \\y:((I,I),(I,I)). if __cmpeq x[0] y[0] then __cmpeq x[1] y[1] else 0
"""
, """
let __cmplte = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] <= y[1] else x[0] <= y[0] in
let __cmpeq = \\x:(I, I). \\y:(I, I). if x[0] == y[0] then x[1] == y[1] else 0 in
\\x:((I,I),(I,I)). \\y:((I,I),(I,I)). if __cmpeq x[0] y[0] then __cmplte x[1] y[1] else __cmplte x[0] y[0]
""")
