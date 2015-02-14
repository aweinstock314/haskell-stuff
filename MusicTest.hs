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
function !runAudio() {
    // adapted from example at https://developer.mozilla.org/en-US/docs/Web/API/AudioContext.createBufferSource
    var TAU = 2 * Math.PI;
    var ctx = new AudioContext();
    var numSineFrames = 5 * ctx.sampleRate;
    var sineWave = ctx.createBuffer(1, numSineFrames, ctx.sampleRate);
    var cyclesPerSecond = 500;
    var buf = sineWave.getChannelData(0);
    for(var i=0; i<numSineFrames; i++) {
        whichSecond = i / ctx.sampleRate;
        buf[i] = 0.5 * Math.sin((cyclesPerSecond+(100*whichSecond))*TAU*i/ctx.sampleRate);
    }

    var source = ctx.createBufferSource();
    source.buffer = sineWave;
    source.connect(ctx.destination);
    source.start();
}
|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "Music Test"
        embedScriptMultiline jsDefinitions
    H.body `onloadDo` [jmacroE|runAudio()|] $ do
        "Audio should be playing."

pageServer request respond = respond $ responseLBS status200 [] $ renderHtml $ page

main = do
    let portNumber = 8504
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    run portNumber pageServer
