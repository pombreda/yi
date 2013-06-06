module Yi.Mode.HTML ( htmlMode ) where

import Prelude ()

import Yi.Buffer
import Yi.Prelude
import Yi.Syntax
import Yi.Syntax.Tree
import qualified Yi.IncrementalParse as P
import qualified Yi.Lexer.Alex as Alex
import qualified Yi.Lexer.HTML as HTML
import qualified Yi.Syntax.HTML as HTML
import Yi.Syntax.OnlineTree as OT
import Yi.Modes ( anyExtension, fundamentalMode )
-- import qualified Yi.Syntax ( ExtHL(..), mkHighlighter ) -- .Driver as Driver

htmlLexer :: Scanner Point Char
          -> Scanner (Alex.AlexState HTML.HlState) (Alex.Tok HTML.Token)
htmlLexer = Alex.lexScanner HTML.alexScanToken HTML.initState

htmlMode :: Mode (HTML.Tree HTML.TT)
htmlMode = fundamentalMode
  { modeApplies = anyExtension ["html", "htm", "xhtml", "HTML", "HTM"]
  , modeToggleCommentSelection = toggleCommentSelectionB "<!-- " "-->"
  , modeName = "html"
  , modeHL = ExtHL $ mkHighlighter (P.scanner HTML.parse . htmlLexer)
  , modeGetStrokes = HTML.getStrokes
  }
