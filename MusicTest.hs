{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Concurrent (forkIO, threadDelay)
import Data.Monoid
import Language.Javascript.JMacro
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Blaze.Html.Renderer.Utf8
import WebUtils
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as V
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

jsDefinitions = [jmacro|
function !playBuffer(buf) {
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
|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "Music Test"
        embedScriptMultiline jsDefinitions
    H.body `onloadDo` [jmacroE|playSineScale()|] $ do
        "Audio should be playing."

pageServer request respond = respond $ responseLBS status200 [] $ renderHtml $ page

main = do
    let portNumber = 8504
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    run portNumber pageServer
