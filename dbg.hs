
class S a where
  s :: a -> String

instance S Int where
  s = show

instance S a -> S ((a,a),(a,a)) where
  s ((x, y),(z, w)) = s x ++ s y ++ s z ++ s w

main = do
  s