import Test.Hspec
import Scraper
import Types
import Text.HTML.Scalpel

dataHtml :: String
dataHtml = "<h1 id=\"g:1\">Selectors</h1> \n<div class=\"top\"> \n    <p class=\"src\"><span class=\"keyword\">data</span> <a name=\"t:Selector\" class=\"def\">Selector</a> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#Selector\" class=\"link\">Source</a></p> \n    <div class=\"doc\"> \n        <p><code><a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></code> defines a selection of an HTML DOM tree to be operated on by a web scraper. The selection includes the opening tag that matches the selection, all of the inner tags, and the corresponding closing tag.</p> \n    </div> \n    <div class=\"subs instances\"> \n        <p id=\"control.i:Selector\" class=\"caption collapser\" onclick=\"toggleSection('i:Selector')\">Instances</p> \n        <div id=\"section.i:Selector\" class=\"show\"> \n            <table> \n                <tr> \n                    <td class=\"src clearfix\"><span class=\"inst-left\"><a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a> <a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#line-69\" class=\"link\">Source</a></td> \n                    <td class=\"doc empty\">&nbsp;</td> \n                </tr> \n            </table> \n        </div> \n    </div> \n</div> \n"

classHtml :: String
classHtml = "<div class=\"top\"> \n    <p class=\"src\"><span class=\"keyword\">class</span> <a name=\"t:Selectable\" class=\"def\">Selectable</a> s <span class=\"keyword\">where</span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#Selectable\" class=\"link\">Source</a></p> \n    <div class=\"doc\"> \n        <p>The <code><a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a></code> class defines a class of types that are capable of being cast into a <code><a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></code> which in turns describes a section of an HTML DOM tree. \n        </p> \n    </div> \n    <div class=\"subs methods\"> \n        <p class=\"caption\">Methods</p> \n        <p class=\"src\"><a name=\"v:toSelector\" class=\"def\">toSelector</a> :: s -&gt; <a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#toSelector\" class=\"link\">Source</a></p> \n    </div> \n    <div class=\"subs instances\"> \n        <p id=\"control.i:Selectable\" class=\"caption collapser\" onclick=\"toggleSection('i:Selectable')\">Instances</p> \n        <div id=\"section.i:Selectable\" class=\"show\"> \n            <table> \n                <tr> \n                    <td class=\"src clearfix\"><span class=\"inst-left\"><a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a> <a href=\"/package/base-4.8.2.0/docs/Data-String.html#t:String\">String</a></span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#line-72\" class=\"link\">Source</a></td> \n                    <td class=\"doc empty\">&nbsp;</td> \n                </tr> \n                <tr> \n                    <td class=\"src clearfix\"><span class=\"inst-left\"><a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a> <a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#line-69\" class=\"link\">Source</a></td> \n                    <td class=\"doc empty\">&nbsp;</td> \n                </tr> \n                <tr> \n                    <td class=\"src clearfix\"><span class=\"inst-left\"><a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a> <a href=\"Text-HTML-Scalpel.html#t:Any\">Any</a></span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Types.html#line-75\" class=\"link\">Source</a></td> \n                    <td class=\"doc empty\">&nbsp;</td> \n                </tr> \n            </table> \n        </div> \n    </div> \n</div>\n"

funcHtml :: String
funcHtml = "<div class=\"top\">\n  <p class=\"src\">\n    <a name=\"v:hasClass\" class=\"def\">hasClass</a> ::\n    <a href=\"/package/base-4.8.2.0/docs/Data-String.html#t:String\">String</a> -&gt;\n    <a href=\"Text-HTML-Scalpel.html#t:AttributePredicate\">AttributePredicate</a>\n    <a href=\"src/Text-HTML-Scalpel-Internal-Select-Combinators.html#hasClass\" class=\"link\">Source</a>\n  </p>\n  <div class=\"doc\">\n    <p>The classes of a tag are defined in HTML as a space separated list given by the <code>class</code> attribute. The <code><a href=\"Text-HTML-Scalpel.html#v:hasClass\">hasClass</a></code> function will match a <code>class</code> attribute if the given class appears anywhere in the space separated list of classes.</p>\n  </div>\n</div>\n"

operatorHtml :: String
operatorHtml = "<div class=\"top\">\n  <p class=\"src\">\n    <a name=\"v:-47--47-\" class=\"def\">\n      (//)\n    </a>\n    :: (<a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a>\n    a, <a href=\"Text-HTML-Scalpel.html#t:Selectable\">Selectable</a>\n    b) =&gt; a -&gt; b -&gt;\n    <a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a>\n    <span class=\"fixity\">infixl 5</span><span class=\"rightedge\"></span> <a href=\"src/Text-HTML-Scalpel-Internal-Select-Combinators.html#%2F%2F\" class=\"link\">Source</a></p>\n    <div class=\"doc\">\n        <p>The <code><a href=\"Text-HTML-Scalpel.html#v:-47--47-\">//</a></code> operator creates an <code><a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></code> by nesting one <code><a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></code> in another. For example, <code>&quot;div&quot; // &quot;a&quot;</code> will create a <code><a href=\"Text-HTML-Scalpel.html#t:Selector\">Selector</a></code> that matches anchor tags that are nested arbitrarily deep within a div tag.</p>\n    </div>\n</div>\n"

spec :: Spec
spec =
     describe "Scraper" $ do
         it "scrapes class" $
             scrapeStringLike classHtml items `shouldBe` Just [Class "Selectable" " \n        The Selectable class defines a class of types that are capable of being cast into a Selector which in turns describes a section of an HTML DOM tree. \n         \n    " [] [] "src/Text-HTML-Scalpel-Internal-Select-Types.html#Selectable"]
         it "scrapes func" $
             scrapeStringLike funcHtml items `shouldBe` Just [Func "hasClass" "\n    hasClass ::\n    String ->\n    AttributePredicate\n    Source\n  " "\n    The classes of a tag are defined in HTML as a space separated list given by the class attribute. The hasClass function will match a class attribute if the given class appears anywhere in the space separated list of classes.\n  " "src/Text-HTML-Scalpel-Internal-Select-Combinators.html#hasClass"]

         it "scrapes operator" $
             scrapeStringLike operatorHtml items `shouldBe` Just [Op "\n      (//)\n    " "\n    \n      (//)\n    \n    :: (Selectable\n    a, Selectable\n    b) => a -> b ->\n    Selector\n    infixl 5 Source" "\n        The // operator creates an Selector by nesting one Selector in another. For example, \"div\" // \"a\" will create a Selector that matches anchor tags that are nested arbitrarily deep within a div tag.\n    " "infixl 5" "src/Text-HTML-Scalpel-Internal-Select-Combinators.html#%2F%2F"]

         it "scrapes data" $
             scrapeStringLike dataHtml items `shouldBe` Just [Data "Selector" "\n        Selector defines a selection of an HTML DOM tree to be operated on by a web scraper. The selection includes the opening tag that matches the selection, all of the inner tags, and the corresponding closing tag.\n    " "src/Text-HTML-Scalpel-Internal-Select-Types.html#Selector"]
    -- scrapeURL `shouldBe`

