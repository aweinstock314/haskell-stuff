module EuclideanAlgorithm where

euclid a b = step (a, 1, 0) (b, 0, 1) where
    step x (0, _, _) = x
    step (r, s, t) (r', s', t') = let q = r `div` r' in
        step (r', s', t') (r - q*r', s - q*s', t - q*t')

prop_satisfies_bezout's_identity a b = (abs r == gcd a b) && (r == (a*s) + (b*t)) where
     (r, s, t) = euclid a b
