#!/usr/bin/env runhaskell
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Control.Monad
import System.IO
import Text.Blaze.Svg.Renderer.String
import Text.Blaze.Svg11 ((!))
import qualified Data.Text as T
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

tau = 2 * pi

blackOutline x = x ! A.fill "none" ! A.stroke "black" ! A.strokeWidth "1"

text :: (Double, Double) -> T.Text -> S.Markup
text (x, y) str = (S.text_ . S.text) str ! A.x (S.toValue x) ! A.y (S.toValue y) ! A.fontSize (S.toValue (50::Int))

rect :: (Double, Double) -> Double -> Double -> S.Markup
rect (x, y) w h = let [x', y', w', h'] = map S.toValue [x, y, w, h] in blackOutline $ S.rect !
    A.x x' ! A.y y' !
    A.width w' ! A.height h'
    

line :: (Double, Double) -> (Double, Double) -> S.Markup
line (x1, y1) (x2, y2) = let [x1', y1', x2', y2'] = map S.toValue [x1, y1, x2, y2] in
    blackOutline $ S.line ! A.x1 x1' ! A.y1 y1' ! A.x2 x2' ! A.y2 y2'

svgString :: Double -> Double -> String ->S.Markup -> String
svgString w h viewBox = renderSvg . (S.docTypeSvg ! A.viewbox (S.toValue viewBox) !
    A.width (S.toValue w) ! A.height (S.toValue h))

svgFile fname w h content viewBox = do
    svgFile <- openFile fname WriteMode
    hPutStr svgFile $ svgString w h content viewBox
    hClose svgFile

cells (x, y) = do
    rect (x, y) 50 50
    rect (x+50, y) 50 50

toText = foldr T.cons T.empty

arrow (x1, y1) (x2, y2) = do
    line (x1, y1) (x2, y2)
    let (dx, dy) = (x2-x1, y2-y1)
    --let mag = negate . (/ 10) . sqrt $ dx**2 + dy**2
    let mag = -10
    let ori = atan2 dy dx
    let backwardsPart op = let ori' = ori `op` (tau/10) in line (x2, y2) (x2 + (mag * cos ori'), y2 + (mag * sin ori'))
    mapM_ backwardsPart [(+),(-)]

drawFiniteList (x, y) [] = return ()
drawFiniteList (x, y) (e:[]) = do
    cells (x, y)
    text (x+10, y+40) (toText e)
drawFiniteList (x, y) (e:es) = do
    drawFiniteList (x, y) [e]
    arrow (x+75, y+25) (x+150, y+25)
    drawFiniteList (x+150, y) es

{-
ll123 ox oy = do
    forM_ [1..3] $ \x -> do
        text (ox+10+(x-1)*150) (oy+40) (toText . show $ floor x)
        cells (ox+(x-1)*150) oy
    forM_ [0,1] $ \x ->
        arrow (ox+75+(x*150)) (oy+25) (ox+150+(x*150)) (oy+25)
    line (ox+150*2+50) oy (ox+150*2+100) (oy+50)
-}

ll123 (x, y) = drawFiniteList (x, y) (map show [1,2,3]) >> line (x+350, y) (x+400, y+50)

jointedLine [] = return ()
jointedLine pts = zipWithM_ line pts (tail pts)

main = do
    let shared_examples = do
        text (40, 140) "x" >> ll123 (100, 100)
        text (40, 240) "x'" >> ll123 (100, 200)
        text (40, 340) "tail x'" >> arrow (175,325) (275, 250)

        text (40, 440) "y"
        drawFiniteList (100, 400) (map show [1,2])
        jointedLine [(325, 425), (325, 375), (125, 375)]
        arrow (125, 375) (125, 400)

        text (40, 740) "z"
        text (40, 840) "tail z"
        arrow (175,825) (275, 750)

    let f ((dx, y), ixs) = do
        let xs = map ((dx+) . (125+) . (150*)) ixs
        line (minimum xs, y) (maximum xs, y)
        text (head xs+50, y) "+"
        forM_ (init xs) $ \x -> do
            line (x, y) (x, 700)
        arrow (last xs, y) (last xs, 700)
    svgFile "LinkedListExamples1.svg" 900 700 "100 75 800 800" $ do
        shared_examples
        drawFiniteList (100, 700) (["1", "1"] ++ replicate 4 "...")
        mapM_ f [(((-20,675)),[0,1,2]), ((20,650), [1,2,3]), ((-10,625), [2,3,4]), ((10,600), [3,4,5])]
    svgFile "LinkedListExamples2.svg" 900 700 "100 75 800 800" $ do
        shared_examples
        drawFiniteList (100, 700) (["1", "1", "2"] ++ replicate 3 "...")
        mapM_ f [((20,650), [1,2,3]), ((-10,625), [2,3,4]), ((10,600), [3,4,5])]
    svgFile "LinkedListExamples3.svg" 900 700 "100 75 800 800" $ do
        shared_examples
        drawFiniteList (100, 700) (["1", "1", "2", "3"] ++ replicate 2 "...")
        mapM_ f [((-10,625), [2,3,4]), ((10,600), [3,4,5])]
    svgFile "LinkedListExamples4.svg" 900 700 "100 75 800 800" $ do
        shared_examples
        drawFiniteList (100, 700) ["1", "1", "2", "3", "5", "..."]
        f ((10,600), [3,4,5])
    svgFile "LinkedListExamples5.svg" 900 700 "100 75 800 800" $ do
        shared_examples
        drawFiniteList (100, 700) ["1", "1", "2", "3", "5", "8"]

