{-# LANGUAGE QuasiQuotes #-}
module WebUtils where
import Control.Concurrent (threadDelay)
import Language.Javascript.JMacro
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal
import qualified Text.PrettyPrint.Leijen.Text as R

delayMs = threadDelay . (*) 1000

onloadDo :: (JsToDoc a, JMacro a, Text.Blaze.Internal.Attributable c) => c -> a -> c
onloadDo = flip $ flip (H.!) . A.onload . H.stringValue . show . R.renderOneLine . renderJs

embedScript, showScript :: (JsToDoc a, JMacro a) => a -> H.Markup
embedScript = H.script . H.string . show . R.renderOneLine . renderJs
showScript = H.pre . H.string . show . renderJs


mkTable :: Integer -> Integer -> JExpr
mkTable width height = [jmacroE|
function(tableRoot) {
    var cells = new Array(`height`);
    for(var y=0; y<`height`; y++) {
        var row = document.createElement('tr');
        cells[y] = new Array(`width`);
        tableRoot.appendChild(row)
        for(var x=0; x<`width`; x++) {
            cells[y][x] = document.createElement('td');
            cells[y][x].textContent = 'blank-ish';
            row.appendChild(cells[y][x]);
        }
    }
    return cells;
}
|]
