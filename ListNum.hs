import Data.List

instance Eq a => Num [a] where
    (+) = (++)
    list * count = concat $ replicate (length count) list
    (-) = (\\)
    abs = id
    signum = const []
    fromInteger = flip genericReplicate undefined
