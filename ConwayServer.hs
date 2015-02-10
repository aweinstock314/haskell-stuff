{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import ConwayLife
import Data.Array.Unboxed
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String
import Data.IORef
import Language.Javascript.JMacro
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random
import Text.Blaze.Html.Renderer.Utf8
import WebUtils
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


cellStr True = "#"
cellStr False = " "

-- Uint8, ABGR (due to cons + reverse idiom)
mkPixel :: IsString a => Bool -> a
mkPixel True = "\xff\x00\x00\x00"
mkPixel False = "\xff\xc8\xc8\xc8"


serverUrl, cellCanvas :: IsString a => a
serverUrl = "ws://localhost:8503"
cellCanvas = "cellCanvas"

cellSize = 4 :: Int

simpleConwayServer gen (w, h) portNumber = WS.runServer "0.0.0.0" portNumber $ \pending -> do
    sock <- WS.acceptRequest pending
    putStrLn "Received a websocket connection."
    let initBoard = makeRandomBoard listArray gen (w, h) 0.3 :: UArray (Int, Int) Bool
    forM_ (iterate (evolveBoard $ wrapIdx (w, h)) initBoard) $ \board -> do
        responseRef <- newIORef L.empty
        dogrid (w, h) (\x y -> modifyIORef responseRef ((mkPixel $ board!(x, y))<>)) (return ())
        response <- readIORef responseRef
        WS.sendBinaryData sock $ L.reverse response
        delayMs 10

jsDefinitions (w, h) = [jmacro|
// http://stackoverflow.com/questions/3955229/remove-all-child-elements-of-a-dom-node-in-javascript
function removeAllChildren(node) {
    while(node.lastChild) {
        node.removeChild(node.lastChild);
    }
}

function !simpleConwayClient() {
    var sock = new WebSocket(`serverUrl::String`);
    sock.binaryType = 'arraybuffer';
    // https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_usage
    var canvas = document.getElementById(`cellCanvas::String`);
    if(!canvas.getContext) { return; }
    var ctx = canvas.getContext('2d');
    /*var buffer = document.createElement('canvas');
    buffer.width = `w*cellSize`;
    buffer.height = `h*cellSize`;
    bufCtx = buffer.getContext('2d');*/
    sock.onmessage = function(event) {
        var uintArr = new Uint8ClampedArray(event.data);
        var dataBuf = new ImageData(uintArr, `w`, `h`);
        ctx.putImageData(dataBuf, 0, 0);
        /*bufCtx.putImageData(dataBuf, 0, 0);
        for(var y=0; y < `h`; y++) {
            for(var x=0; x < `w`; x++) {
                var ch = event.data[y*`w`+x];
                bufCtx.fillStyle = (ch === '#') ? 'rgb(0,0,0)' : 'rgb(200, 200, 200)';
                bufCtx.fillRect(x*`cellSize`, y*`cellSize`, `cellSize`, `cellSize`);
            }
        }
        ctx.drawImage(buffer, 0, 0);*/
    }
}
|]

page (w, h) = H.docTypeHtml $ do
    H.head $ do
        H.title "Conway's game of life"
        embedScript $ jsDefinitions (w, h)
    H.body `onloadDo` [jmacroE|simpleConwayClient()|] $ do
        H.canvas "" H.! A.width (H.stringValue . show $ w*cellSize) H.! A.height (H.stringValue . show $ h*cellSize) H.! A.id cellCanvas

pageServer (w, h) request respond = respond $ responseLBS status200 [] $ renderHtml $ page (w, h)

main = do
    let portNumber = 8502
    let (width, height) = (200, 100)
    gen <- getStdGen
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ simpleConwayServer gen (width, height) (portNumber+1)
    run portNumber (pageServer (fromIntegral width, fromIntegral height))
