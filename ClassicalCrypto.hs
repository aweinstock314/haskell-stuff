module ClassicalCrypto where
import Data.Char
import Data.List
import qualified Data.Map as M

(all_of, any_of) = ((all `of_`), (any `of_`)) where
    (f `of_` predicates) x = f id $ map ($ x) predicates

toLowercaseLetters = filter (all_of [isAlpha, isAscii]) . map toLower

caesarCipher shift = map (chr . (`mod` 256) . (+ shift) . ord)

occurrences s = M.fromList $ zip (nub $ concat clusters) (map length clusters) where
    clusters = group $ sort s

transposeMap :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
transposeMap = M.foldlWithKey (\acc k v -> M.insertWith (++) v k acc) M.empty . M.mapKeys return

lettersByFrequency = concatMap snd . M.toDescList . transposeMap . occurrences . toLowercaseLetters

-- http://upload.wikimedia.org/wikipedia/commons/b/b0/English_letter_frequency_%28frequency%29.svg
englishLettersByFrequency = "etaoinshrdlcumwfgypbvkjxqz"
