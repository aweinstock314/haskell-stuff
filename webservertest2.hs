{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Data.Monoid
import Language.Javascript.JMacro
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal
import qualified Text.PrettyPrint.Leijen.Text as R

defineFactorial = [jmacro|
function !factorial(n) {
    var res = 1;
    for(var i=n; i>0; i--) {
        res *= i;
    }
    return res;
}
|]

onloadDo :: (JsToDoc a, JMacro a, Text.Blaze.Internal.Attributable c) => c -> a -> c
onloadDo = flip $ flip (H.!) . A.onload . H.stringValue . show . R.renderOneLine . renderJs

embedScript, showScript :: (JsToDoc a, JMacro a) => a -> H.Markup
embedScript = H.script . H.string . show . R.renderOneLine . renderJs
showScript = H.pre . H.string . show . renderJs

populateDivWithFactorials = [jmacro|
    for(var i=0; i<10; i++) {
        var str = "factorial(" + i + ") = " + factorial(i) + "\n";
        document.getElementById("toBePopulated").innerHTML += str;
    }
|]

defineDoStuff =
    let stuff = populateDivWithFactorials <> [jmacro|alert("This is a message box!");|]
    in [jmacro| function !doStuff(){`stuff`;}|]

page = H.docTypeHtml $ do
    H.head $ do
        H.title "The title of the web page."
        embedScript $ defineFactorial <> defineDoStuff
    H.body `onloadDo` [jmacroE|doStuff()|] $ do
        H.div "Hello, world!"
        showScript defineFactorial
        H.string "<p>testing escaping</p>"
        H.string "<script>alert(\"XSS\");</script>"
        H.pre $ H.div "" H.! A.id "toBePopulated"

application request respond =
    respond $ responseLBS status200 [] $ renderHtml page

main = do
    let portNumber = 8500
    putStrLn $ mconcat ["Listening on port ", show portNumber, "."]
    run portNumber application
