#!/usr/bin/env python

print("""
type qt1 = I in
let qt1_at_ = \\q:qt1. \\x_:I. \\y_:I. \\x:I. \\y:I. q in
let qt1_insert_ = \\q:qt1. \\x_:I. \\y_:I. \\x:I. \\y:I. \\v:I. v in
let qt1_empty = 0 in
""")
for i in range(1,9):
  q = 2**(i-1)
  p = 2**i
  print(
"""
type qt{1} = ((qt{0}, qt{0}), (qt{0}, qt{0})) in

let qt{1}_at_ = \\q:qt{1}. \\x_:I. \\y_:I. \\x:I. \\y:I.
  if x >= x_ + {0}
    then
      if y >= y_ + {0}
        then qt{0}_at_ q[1][1] (x_ + {0}) (y_ + {0}) x y
        else qt{0}_at_ q[1][0] (x_ + {0}) (y_)       x y
    else
      if y >= y_ + {0}
        then qt{0}_at_ q[0][1] (x_)       (y_ + {0}) x y
        else qt{0}_at_ q[0][0] (x_)       (y_)       x y
  in
let qt{1}_at = \\q:qt{1}. qt{1}_at_ q 0 0 in

let qt{1}_insert_ = \\q:qt{1}. \\x_:I. \\y_:I. \\x:I. \\y:I. \\v:I.
  if x >= x_ + {0}
    then
      if y >= y_ + {0}
        then
        ( ( q[0][0]                                          
          , q[0][1]
          )
        ,  ( q[1][0]
           , qt{0}_insert_ q[1][1] (x_ + {0}) (y_ + {0}) x y v
           )
        )
        else
        ( ( q[0][0]                                          
          , q[0][1]
          )
        , ( qt{0}_insert_ q[1][0] (x_ + {0}) y_ x y v
          , q[1][1]
          )
        )
    else
      if y >= y_ + {0}
        then
        ( ( q[0][0]                                          
          , qt{0}_insert_ q[0][1] x_ (y_ + {0}) x y v
          )
        ,  ( q[1][0]
           , q[1][1]
           )
        )
        else
        ( ( qt{0}_insert_ q[0][0] x_ y_ x y v
          , q[0][1]
          )
        ,  ( q[1][0]
           , q[1][1]
           )
        )
  in
let qt{1}_insert = \\q:qt{1}. qt{1}_insert_ q 0 0 in
let qt{1}_empty = ((qt{0}_empty, qt{0}_empty), (qt{0}_empty, qt{0}_empty)) in
""".format(str(q), str(p)))
