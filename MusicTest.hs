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
adsr :: Float -> Float -> Float -> Float -> [Float] -> [Float]
adsr attack decay sustain release sample =
    map step $ zip [0..] sample where
    step (i, x) | i < atk = (atkVolume i) * x
    step (i, x) | i < dec = (decVolume i) * x
    step (i, x) | i < rel =       sustain * x
    step (i, x)           = (relVolume i) * x
    [atk, dec, rel] = map (*len) [attack, decay, release]
    atkVolume i = (i/atk)
    decVolume i = 1-((i-atk)/(dec-atk))
    relVolume i = sustain * (1-((i-rel)/(len-rel)))
    len = genericLength sample
-- Eyeball test of envelope generator: zip [0..] $ adsr 0.1 0.3 0.5 0.9 $ replicate 20 1.0

tune1 sampleRate = snd . runWriter $ do
    let env = adsr 0.0 0.6 0.25 0.9
    let freqs = [500,600..1000]
    let freqs' = freqs <> reverse freqs
    let freqs'' = freqs' <> map (+50) freqs'
    forM_ freqs'' $ \freq -> do
        tell . env $ sineWave sampleRate 0.2 freq
        tell $ silence sampleRate 0.05

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
    };
}

|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "Music Test"
        embedScriptMultiline jsDefinitions
    H.body `onloadDo` [jmacroE|playBufferFromServer()|] $ do
        "Audio should be playing."

pageServer request respond = respond $ responseLBS status200 [] $ renderHtml $ page

main = do
    let portNumber = 8504
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ tuneServer tune1 (portNumber+1)
    run portNumber pageServer
