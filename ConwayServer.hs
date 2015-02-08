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

serverUrl, cellDiv :: IsString a => a
serverUrl = "ws://localhost:8503"
cellDiv = "cellDiv"

simpleConwayServer gen (w, h) portNumber = WS.runServer "0.0.0.0" portNumber $ \pending -> do
    sock <- WS.acceptRequest pending
    putStrLn "Received a websocket connection."
    let initBoard = makeRandomBoard listArray gen (w, h) 0.3 :: UArray (Int, Int) Bool
    forM_ (iterate (evolveBoard $ wrapIdx (w, h)) initBoard) $ \board -> do
        responseRef <- newIORef ""
        dogrid (w, h) (\x y -> modifyIORef responseRef ((cellStr $ board!(x, y))++)) (return ())
        response <- readIORef responseRef
        WS.sendTextData sock . lbs $ reverse response
        delayMs 250

jsDefinitions (w, h) = [jmacro|
// http://stackoverflow.com/questions/3955229/remove-all-child-elements-of-a-dom-node-in-javascript
function removeAllChildren(node) {
    while(node.lastChild) {
        node.removeChild(node.lastChild);
    }
}

function !simpleConwayClient() {
    var sock = new WebSocket(`serverUrl::String`);
    var cellDivVar = document.getElementById(`cellDiv::String`);
    sock.onmessage = function(event) {
        var tableRoot = document.createElement('table');
        var cells = `mkTable w h`(tableRoot, function(node, x, y) {
            var ch = event.data[y*`w`+x];
            node.textContent = ch;
            if(ch === '#') {
                node.style.color = '#ffffff';
                node.style.backgroundColor = '#000000';
            }
            else {
                node.style.color = '#000000';
                node.style.backgroundColor = '#ffffff';
            }
        });
        removeAllChildren(cellDivVar);
        cellDiv.appendChild(tableRoot);
    }
}
|]

page (w, h) = H.docTypeHtml $ do
    H.head $ do
        H.title "Conway's game of life"
        embedScript $ jsDefinitions (w, h)
    H.body `onloadDo` [jmacroE|simpleConwayClient()|] $ do
        H.pre $ H.div "" H.! A.id cellDiv

pageServer (w, h) request respond = respond $ responseLBS status200 [] $ renderHtml $ page (w, h)

main = do
    let portNumber = 8502
    let (width, height) = (50, 50)
    gen <- getStdGen
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ simpleConwayServer gen (width, height) (portNumber+1)
    run portNumber (pageServer (fromIntegral width, fromIntegral height))
