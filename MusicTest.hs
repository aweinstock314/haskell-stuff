#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
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
import Text.Blaze.Html.Renderer.Utf8
import WebUtils
import qualified Data.Binary.Put as BPut
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

serverUrl :: IsString a => a
serverUrl = "ws://localhost:8505"

tau :: Floating a => a
tau = 2 * pi

floatsToLBS = BPut.runPut . sequence_ . map putFloat32le

sineWave sampleRate seconds frequency =
    map step [0..numFrames-1] where
        step i = sin (frequency * tau * i / sampleRate)
        numFrames = sampleRate * seconds

silence sampleRate seconds = replicate numFrames 0.0 where numFrames = floor $ sampleRate * seconds

-- attack, decay, sustain, release are in [0,1], attack < decay < release
-- attack, decay, release are fractions into the sample
-- sustain is the volume between decay and release
adsr :: (Enum a, Floating a, Ord a) => a -> a -> a -> a -> [a] -> [a]
adsr attack decay sustain release sample =
    map step $ zip [0..] sample where
    step (i, x) | i < atk = (atkVolume i) * x
    step (i, x) | i < dec = (decVolume i) * x
    step (i, x) | i < rel =       sustain * x
    step (i, x)           = (relVolume i) * x
    [atk, dec, rel] = map (*len) [attack, decay, release]
    atkVolume i = (i/atk)
    decVolume i = (1-sustain)*(1-((i-atk)/(dec-atk)))+sustain
    relVolume i = sustain * (1-((i-rel)/(len-rel)))
    len = genericLength sample
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

tuneServer tune = withAllWebsocketConnections $ \sock -> do
    sampleRateString <- WS.receiveData sock
    case safeRead $ unlbs sampleRateString :: Maybe Float of
        Just sampleRate -> do
            putStrLn $ mconcat ["Received a websocket connection. sampleRate = ", show sampleRate]
            WS.sendBinaryData sock . floatsToLBS $ tune sampleRate
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
    var sock = new WebSocket(`serverUrl::String`);
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
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ tuneServer (tune2 0) (portNumber+1)
    run portNumber pageServer
