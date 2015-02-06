{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Char
import Data.Monoid
import Data.String
import Language.Javascript.JMacro
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal
import qualified Text.PrettyPrint.Leijen.Text as R

lbs = L.pack . map (fromIntegral.ord)
delayMs = threadDelay . (*) 1000

onloadDo :: (JsToDoc a, JMacro a, Text.Blaze.Internal.Attributable c) => c -> a -> c
onloadDo = flip $ flip (H.!) . A.onload . H.stringValue . show . R.renderOneLine . renderJs

embedScript, showScript :: (JsToDoc a, JMacro a) => a -> H.Markup
embedScript = H.script . H.string . show . R.renderOneLine . renderJs
showScript = H.pre . H.string . show . renderJs

factorialsDiv, escapingDiv, heartBeatDiv, heartBeatTable :: IsString a => a
factorialsDiv = "factorialsDiv"
escapingDiv = "escapingDiv"
heartBeatDiv = "heartBeatDiv"
heartBeatTable = "heartBeatTable"

defineFactorial = [jmacro|
function !factorial(n) {
    var res = 1;
    for(var i=n; i>0; i--) {
        res *= i;
    }
    return res;
}
|]

populateDivWithFactorials = [jmacro|
    for(var i=0; i<10; i++) {
        var str = "factorial(" + i + ") = " + factorial(i) + "\n";
        document.getElementById(`factorialsDiv::String`).innerHTML += str;
    }
|]

escapingTest = [jmacro|
var x = document.createTextNode("<script>alert('XSS2');</script>");
document.getElementById(`escapingDiv::String`).appendChild(x);
|]

heartBeatServerUrl :: IsString a => a
heartBeatServerUrl = "ws://localhost:8501"

heartBeatServer portNumber = WS.runServer "0.0.0.0" portNumber $ \ pending -> do
    sock <- WS.acceptRequest pending
    putStrLn "Received a websocket connection."
    forM_ [1..] $ \i -> do
        WS.sendTextData sock ("Pulse: " <> (lbs $ show i) <> "\n")
        delayMs 10

-- https://developer.mozilla.org/en-US/docs/WebSockets/Writing_WebSocket_client_applications
setupHeartBeatClient = [jmacro|
var sock = new WebSocket(`heartBeatServerUrl::String`);
var tableRoot = document.getElementById(`heartBeatTable::String`);
var WIDTH = 10;
var HEIGHT = 10;
var cells = new Array(HEIGHT);
for(var y=0; y<HEIGHT; y++) {
    var row = document.createElement('tr');
    cells[y] = new Array(WIDTH);
    tableRoot.appendChild(row)
    for(var x=0; x<WIDTH; x++) {
        cells[y][x] = document.createElement('td');
        cells[y][x].textContent = 'blank-ish';
        row.appendChild(cells[y][x]);
    }
}
var xIdx = 0;
var yIdx = 0;
sock.onmessage = function(event) {
    /*var hbDiv = document.getElementById(`heartBeatDiv::String`);
    var x = document.createTextNode(event.data);
    hbDiv.appendChild(x);*/
    var node = cells[yIdx][xIdx];
    node.textContent = event.data;
    if(++xIdx >= WIDTH) {
        xIdx = 0;
        if(++yIdx >= HEIGHT) {
            yIdx = 0;
        }
    }
}
|]

defineDoStuff =
    let stuff = populateDivWithFactorials <> [jmacro|alert("This is a message box!");|] <> escapingTest <> setupHeartBeatClient
    in [jmacro| function !doStuff(){`stuff`;}|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "The title of the web page."
        embedScript $ defineFactorial <> defineDoStuff
    H.body `onloadDo` [jmacroE|doStuff()|] $ do
        H.div "Hello, world!"
        showScript defineFactorial
        H.div H.! A.id escapingDiv $ do
            H.string "<p>testing escaping</p>" <> H.br
            H.string "<script>alert(\"XSS1\");</script>" <> H.br
        H.pre $ H.div "" H.! A.id factorialsDiv
        H.pre $ H.div H.! (A.id heartBeatDiv) $ H.table "" H.! (A.id heartBeatTable)

pageServer request respond =
    respond $ responseLBS status200 [] $ renderHtml page

main = do
    let portNumber = 8500
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    forkIO $ heartBeatServer (portNumber+1)
    run portNumber pageServer
