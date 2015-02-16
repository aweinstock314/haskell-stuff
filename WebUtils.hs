{-# LANGUAGE QuasiQuotes #-}
module WebUtils where
import Control.Concurrent (threadDelay)
import Data.Char
import Data.Maybe
import Language.Javascript.JMacro
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets as WS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal
import qualified Text.PrettyPrint.Leijen.Text as R

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads
lbs = L.pack . map (fromIntegral . ord)
unlbs = L.foldr ((:) . chr . fromIntegral) ""
delayMs = threadDelay . (*) 1000

onloadDo :: (JsToDoc a, JMacro a, Text.Blaze.Internal.Attributable c) => c -> a -> c
onloadDo = flip $ flip (H.!) . A.onload . H.stringValue . show . R.renderOneLine . renderJs

embedScript, embedScriptMultiline, showScript :: (JsToDoc a, JMacro a) => a -> H.Markup
embedScript = H.script . H.string . show . R.renderOneLine . renderJs
embedScriptMultiline = H.script . H.string . show . renderJs
showScript = H.pre . H.string . show . renderJs

withAllWebsocketConnections action portNumber = WS.runServer "0.0.0.0" portNumber $ \pending -> do
    sock <- WS.acceptRequest pending
    action sock

mkTable :: Integer -> Integer -> JExpr
mkTable width height = [jmacroE|
function(tableRoot, callback) {
    var cells = new Array(`height`);
    for(var y=0; y<`height`; y++) {
        var row = document.createElement('tr');
        cells[y] = new Array(`width`);
        tableRoot.appendChild(row)
        for(var x=0; x<`width`; x++) {
            cells[y][x] = document.createElement('td');
            callback(cells[y][x], x, y);
            row.appendChild(cells[y][x]);
        }
    }
    return cells;
}
|]
