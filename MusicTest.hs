#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Arrow
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Writer
import Data.Binary.IEEE754
import Data.List
import Data.Monoid
import Data.String
import Language.Javascript.JMacro
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random
import Text.Blaze.Html.Renderer.Utf8
import WebUtils
import qualified Data.Binary.Put as BPut
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Debug.Trace

tau :: Floating a => a
tau = 2 * pi

floatsToLBS = BPut.runPut . sequence_ . map putFloat32le

sineWave sampleRate seconds frequency =
    map step [0..numFrames-1] where
        step i = sin (frequency * tau * i / sampleRate)
        numFrames = sampleRate * seconds

lfo sampleRate frequency floats = zipWith step [0..] floats where
    step i x = x * sin ((frequency (i / sampleRate)) * tau * i / sampleRate)

silence sampleRate seconds = replicate numFrames 0.0 where numFrames = floor $ sampleRate * seconds

scaleFromTo (a, b) (a', b') x = ((x-a)/(b-a))*(b'-a')+a'

noise :: (RandomGen r, RealFrac a) => r -> a -> a -> ([Float], r)
noise gen sampleRate seconds = first (take (floor $ sampleRate*seconds) . map (scaleFromTo (0, 1) (-1, 1)) . randoms) $ split gen

mapWithPrev f x0 = reverse . snd . foldl (\(prev, result) cur -> (cur, (f prev cur):result)) (x0, [])
mapWithPrevTwo f x0 x1 = reverse . (\(_,_,x)->x) . foldl (\(prev1, prev0, result) cur -> (prev0, cur, (f prev1 prev0 cur):result)) (x0, x1, [])

funcpow f i x = (iterate f x) !! i

rollingAggregate f seed size = result where
    seedLen = V.length seed
    deref xs i = if i < seedLen then seed V.! i else xs V.! (i-seedLen)
    result = let d = deref result in V.generate size (\i -> f i (d . (+i)))

karplusStrong b p sampleRate seconds = V.toList result where
    -- Implemented from description at https://ccrma.stanford.edu/~sdill/220A-project/drums.html
    numFrames = floor $ sampleRate * seconds
    (gen1, gen2) = split $ mkStdGen 0
    waveTable = V.fromList . fst $ noise gen1 (fromIntegral p) 1
    biasedCoinFlips = V.fromList . take numFrames $ map (< b) (randoms gen2 :: [Double])
    result = rollingAggregate (\i w -> (0.5*w 0)+((if biasedCoinFlips V.! i then id else negate)0.5*w 1)) waveTable numFrames

-- attack, decay, sustain, release are in [0,1], attack < decay < release
-- attack, decay, release are fractions into the sample
-- sustain is the volume between decay and release
adsr :: (Enum a, Floating a, Ord a) => a -> a -> a -> a -> [a] -> [a]
adsr attack decay sustain release sample =
    V.toList $ V.imap (step . fromIntegral) sample' where
    step i x | i < atk = (atkVolume i) * x
    step i x | i < dec = (decVolume i) * x
    step i x | i < rel =       sustain * x
    step i x           = (relVolume i) * x
    [atk, dec, rel] = map (*len) [attack, decay, release]
    atkVolume = scaleFromTo (0, atk) (0, 1)
    decVolume = scaleFromTo (atk, dec) (1, sustain)
    relVolume = scaleFromTo (rel, len) (sustain, 0)
    len = fromIntegral $ V.length sample'
    sample' = V.fromList sample
-- Eyeball test of envelope generator: zip [0..] $ adsr 0.1 0.3 0.5 0.9 $ replicate 20 1.0

tune1 sampleRate = execWriter $ do
    let env = adsr 0.0 0.6 0.25 0.9
    let freqs = [500,600..1000]
    let freqs' = freqs <> reverse freqs
    let freqs'' = freqs' <> map (+50) freqs'
    forM_ freqs'' $ \freq -> do
        tell . env $ sineWave sampleRate 0.2 freq
        tell $ silence sampleRate 0.05

linearCombination coeffs datums = foldl1 (zipWith (+)) $ zipWith (map . (*)) coeffs datums

-- Meant to approximate tetris music.
tune2 pitchShift sampleRate = execWriter $ do
    let env = map (*0.5) . adsr 0.0 0.8 0.25 0.95
    let rest = tell . silence sampleRate
    let (d1, d2, d3, d4) = (0.15/2, 0.20, 0.225, 0.25)
    let f (freq, delay, emphasize) = do
        let volumeMult = if emphasize then 1.5 else 0.5
        tell . map (*volumeMult) . env $ sineWave sampleRate (delay+0.15) freq
        rest delay
    let part1 = do
        mapM_ f $ map (\(x,y,z) -> (x+pitchShift,y,z)) [(850, d2, True),
                (700, d1, False), (750, d1, False), (800, d2, True), (750, d1, False), (700, d1, False), (650, d3, True),
                (700, d1, False), (750, d1, False), (850, d2, True), (800, d1, False), (750, d1, False), (750, d2, True),
                (700, d1, False), (750, d1, False), (800, d3, True), (850, d2, False), (800, d2, False),
                (725, d2, True), (725, d4, True)]
    part1

tune3 sampleRate = linearCombination [0.75, 0.25] [tune2 0 sampleRate, tune2 (-250) sampleRate]

tune4 gen sampleRate = execWriter $ do
    --let env = adsr 0.0 0.8 0.25 0.95
    let env = map (*0.5)
    --tell . env . fst $ noise gen sampleRate 0.5
    let sample b p = karplusStrong b p sampleRate 1
    forM_ [0.9, 0.91..1] $ \b -> do
        forM_ [200, 250..600] $ \p -> do
            tell . env $ sample b p
            tell $ silence sampleRate 0.1

tune5 sampleRate = (\lst -> lst <> reverse lst) . execWriter $ do
    let env = id --map (*0.5)
    forM_ [0.2, 0.3..0.9] $ \b -> do
        forM_ (take 6 $ cycle [600, 800]) $ \p -> do
            tell . env $ karplusStrong b p sampleRate 0.25

tune6 sampleRate = adsr 0.1 0.1 1 0.9 . (\lst -> lst <> reverse lst) $ linearCombination [0.1, 0.5] [cycle $ tune1 sampleRate, tune5 sampleRate]

tune7 sampleRate = execWriter $ do
    let env = map (*0.5)
    let note x = (\x -> x <> reverse x <> replicate (floor $ sampleRate*0.1) 0.0) $ karplusStrong 0.7 x sampleRate 0.1
    forM_ (take 10 $ cycle [800, 500]) $ \x -> do
        tell . env $ note x

tune8 sampleRate = result where
    base = map (0.5*) $ sineWave sampleRate 10 700
    result = lfo sampleRate (\x -> if even $ floor x then 2 else 10) base

tuneServer tune = withAllWebsocketConnections $ \sock -> do
    sampleRateString <- WS.receiveData sock
    case safeRead $ unlbs sampleRateString :: Maybe Float of
        Just sampleRate -> do
            putStrLn $ mconcat ["Received a websocket connection. sampleRate = ", show sampleRate]
            withTiming (WS.sendBinaryData sock . floatsToLBS $ tune sampleRate) (\dt -> putStrLn $ mconcat ["Rendered the tune in ", show dt, "."])
        Nothing -> return ()

jsDefinitions = [jmacro|
function playBuffer(buf) {
    // adapted from example at https://developer.mozilla.org/en-US/docs/Web/API/AudioContext.createBufferSource
    var ctx = new AudioContext();
    var audioBuffer = ctx.createBuffer(1, buf.length, ctx.sampleRate);
    audioBuffer.copyToChannel(buf, 0, 0);
    var source = ctx.createBufferSource();
    source.buffer = audioBuffer;
    source.connect(ctx.destination);
    source.start();
}

function canvasOfBuffer(buf, width, height, bgcolor, fgcolor) {
    var scaleFactor = width/buf.length;
    var canvas = document.createElement('canvas');
    canvas.width = width;
    canvas.height = height;
    if(!canvas.getContext) { return; }
    var ctx = canvas.getContext('2d');

    ctx.fillStyle = bgcolor;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    var mid = height/2;
    ctx.beginPath();
    ctx.strokeStyle = fgcolor;
    ctx.moveTo(0, mid);
    //for(var i=0; i<buf.length; i++) { ctx.lineTo(i*scaleFactor, buf[i]*mid + mid); }
    for(var i=0; i<canvas.width; i++) { ctx.lineTo(i, buf[Math.floor(i/scaleFactor)]*mid + mid); }
    ctx.stroke();

    return canvas;
}

function !playSineScale() {
    var TAU = 2 * Math.PI;
    var sampleRate = new AudioContext().sampleRate;
    var numSineFrames = 5 * sampleRate;
    var cyclesPerSecond = 500;
    var sineWave = new Float32Array(numSineFrames);
    for(var i=0; i<numSineFrames; i++) {
        whichSecond = Math.floor(i / sampleRate);
        sineWave[i] = 0.5 * Math.sin((cyclesPerSecond+(100*whichSecond))*TAU*i/sampleRate);
    }
    playBuffer(sineWave);
}

function !playBufferFromServer() {
    var sampleRate = new AudioContext().sampleRate;
    var sock = new WebSocket(document.URL.replace('8504', '8505').replace('http', 'ws'));
    sock.binaryType = 'arraybuffer';
    sock.onopen = function(event) {
        sock.send(String(sampleRate));
    };
    sock.onmessage = function(event) {
        var buf = new Float32Array(event.data);
        playBuffer(buf);
        document.body.appendChild(canvasOfBuffer(buf, 9973, 500, 'rgb(200,200,200)', 'black'));
    };
}
|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "Music Test"
        embedScriptMultiline jsDefinitions
    H.body `onloadDo` [jmacroE|playBufferFromServer()|] $ do
        "Audio should start playing once the graph appears:"

pageServer request respond = respond $ responseLBS status200 [] $ renderHtml $ page

main = do
    let portNumber = 8504
    gen <- getStdGen
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ tuneServer tune8 (portNumber+1)
    run portNumber pageServer
