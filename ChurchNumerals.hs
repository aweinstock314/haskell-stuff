{-# LANGUAGE RankNTypes #-}
import Data.Function (on)

-- RankNTypes for polymorphic Church numerals
newtype Church = Church { runChurch :: forall a. (a -> a) -> (a -> a) }

z = Church (\f x -> x)
s n = Church (\f x -> f (runChurch n f x))

toInt (Church f) = f (+1) 0
fromInt 0 = z
fromInt n = s (fromInt (n-1))

plus m n = Church (\f x -> (runChurch m f) ((runChurch n f) x))
plus' m n = runChurch m s n
mult m n = Church (\f x -> runChurch m (runChurch n f) x)
mult' m n = runChurch m (runChurch n s) z
expo b e = Church (runChurch e (runChurch b))

-- Rank1/unwrapped church nats: they still work, but they have messy types (e.g. they don't unify)
uz = \f x -> x
us n = \f x -> f (n f x)

uplus m n = m us n
umult m n = m (n us) uz
uexpo b e = e b

-- This typechecks, but that isn't what we need below
uwrap :: (forall a. a -> a -> a) -> (Church -> Church -> Church)
uwrap f (Church x) (Church y) = Church (f x y)

-- With the Church wrapper, we can make ([uplus', umult', uexpo'] :: [Church -> Church -> Church]), but their rank1 versions don't unify
uplus' (Church m) (Church n) = Church (uplus n m)
umult' (Church m) (Church n) = Church (umult n m)
uexpo' (Church b) (Church e) = Church (uexpo b e)

-- Exhaustive test cases on all pairs in the square (0, 0) to (6, 6)
fuzz f g = all (\(x, y) -> f x y == g x y) [(x,y) | x <- nums, y <- nums] where nums = take 7 (iterate s z)
fuzz' f g = fuzz ((toInt .) . f) (g `on` toInt)

tests = [fuzz' plus (+),
         fuzz' plus' (+),
         fuzz' mult (*),
         fuzz' mult' (*),
         fuzz' expo (^)
         ]

tests' = [fuzz' uplus' (+),
          fuzz' umult' (*),
          fuzz' uexpo' (^)
          ]

main = print (tests ++ tests')
