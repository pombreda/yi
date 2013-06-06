{
#define NO_ALEX_CONTEXTS
module Yi.Lexer.HTML (
 initState,
 alexScanToken,
 Token(..),
 HlState
 ) where

import Yi.Lexer.Alex
import Yi.Style
}

$ws = [\ \t\r\n]
$tagChar = [a-zA-Z0-9\:]
$attrChar = [a-zA-Z0-9_\:\-]
@string = [^\"]*


html :-
  <0> $ws+               ;

  <tagStarted> {
    $ws+ ;
    "/" { c Slash }
    [^\<\>]+ { ms (const tagNamed) TagName }
    "<"|">" { ms (const 0) ErrorToken }
  }

  <tagNamed> {
    $ws+           ;
    "=" { c Equals }
    "/" { c Slash }
    ">" { m (const 0) TagRight }
    \" @string \" { cs AttrString }
    $attrChar+ { cs AttrName }
    "<" { ms (const 0) ErrorToken }
  }

  <0> {
  "<!--" (\-[^\-]|\-\-[^\>]|[^\-])* "-->" { cs Comment }
  "<!" (doctype|DOCTYPE) [^\>]* ">" { cs Doctype }
  "<" { m (const tagStarted) TagLeft }
  "&" [^\;\ ]+ ";" { cs Special }
  "&" { c OpenSpecial }
  ([^\&\<\>]|$ws)+ { cs TextNode }
  ">" { cs ErrorToken }
  }
{

data Token = TagLeft
           | TagRight
           | Equals
           | Slash
           | Doctype !String
           | Special !String -- ^ HTML-encoded entity
           | OpenSpecial -- ^ an & without a matching ;
           | TagName !String
           | AttrString !String
           | AttrName !String
           | Comment !String
           | TextNode !String
           | ErrorToken !String
           deriving (Eq, Ord, Show)

type HlState = Int

initState :: HlState
initState = 0

stateToInit = id

#include "common.hsinc"
}
