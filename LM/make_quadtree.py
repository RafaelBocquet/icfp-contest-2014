#!/usr/bin/env python

def make_quadtree(name, ty, empty):
  print("""
  type qt1_{2} = {3} in
  let qt1_{2}_at_ = \\q:qt1_{2}. \\x_:I. \\y_:I. \\x:I. \\y:I. q in
  let qt1_{2}_insert_ = \\q:qt1_{2}. \\x_:I. \\y_:I. \\x:I. \\y:I. \\v:qt1_{2}. v in
  let qt1_{2}_empty = {4} in
  """.format(0,0,name,ty, empty))
  for i in range(1,6):
    q = 2**(i-1)
    p = 2**i
    print(
  """
  type qt{1}_{2} = ((qt{0}_{2}, qt{0}_{2}), (qt{0}_{2}, qt{0}_{2})) in

  let qt{1}_{2}_at_ = \\q:qt{1}_{2}. \\x_:I. \\y_:I. \\x:I. \\y:I.
    if x >= x_ + {0}
      then
        if y >= y_ + {0}
          then qt{0}_{2}_at_ q[1][1] (x_ + {0}) (y_ + {0}) x y
          else qt{0}_{2}_at_ q[1][0] (x_ + {0}) (y_)       x y
      else
        if y >= y_ + {0}
          then qt{0}_{2}_at_ q[0][1] (x_)       (y_ + {0}) x y
          else qt{0}_{2}_at_ q[0][0] (x_)       (y_)       x y
    in
  let qt{1}_{2}_at = \\q:qt{1}_{2}. qt{1}_{2}_at_ q 0 0 in

  let qt{1}_{2}_insert_ = \\q:qt{1}_{2}. \\x_:I. \\y_:I. \\x:I. \\y:I. \\v:qt1_{2}.
    if x >= x_ + {0}
      then
        if y >= y_ + {0}
          then
          ( ( q[0][0]                                          
            , q[0][1]
            )
          ,  ( q[1][0]
             , qt{0}_{2}_insert_ q[1][1] (x_ + {0}) (y_ + {0}) x y v
             )
          )
          else
          ( ( q[0][0]                                          
            , q[0][1]
            )
          , ( qt{0}_{2}_insert_ q[1][0] (x_ + {0}) y_ x y v
            , q[1][1]
            )
          )
      else
        if y >= y_ + {0}
          then
          ( ( q[0][0]                                          
            , qt{0}_{2}_insert_ q[0][1] x_ (y_ + {0}) x y v
            )
          ,  ( q[1][0]
             , q[1][1]
             )
          )
          else
          ( ( qt{0}_{2}_insert_ q[0][0] x_ y_ x y v
            , q[0][1]
            )
          ,  ( q[1][0]
             , q[1][1]
             )
          )
    in
  let qt{1}_{2}_insert = \\q:qt{1}_{2}. qt{1}_{2}_insert_ q 0 0 in
  let qt{1}_{2}_empty = ((qt{0}_{2}_empty, qt{0}_{2}_empty), (qt{0}_{2}_empty, qt{0}_{2}_empty)) in
  """.format(str(q), str(p), name, ty, empty))

make_quadtree('I', 'I', '0')
make_quadtree('NOV', '<I,(I,I),((I,I),(I,I))>', 'make qt1_NOV 0 0')