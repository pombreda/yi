{-# LANGUAGE DeriveFoldable #-}
module Yi.Syntax.HTML (
  Tree,
  TT,
  parse,
  getStrokes
  ) where

import Prelude ()

import Yi.IncrementalParse
import Yi.Lexer.Alex
import Yi.Lexer.HTML ( Token )
import qualified Yi.Lexer.HTML as HTML
import Yi.Style
import Yi.Syntax.Tree
import Yi.Syntax
import Yi.Prelude hiding ( (<>) )
import Data.Monoid

type TT = Tok Token
data Tree t = SelfTag t t [Attr t] t t
            | OpenTag t t [Attr t] t
            | CloseTag t t t t
            | ProperTag (Tree t) [Tree t] (Tree t)
            | MismatchedTag (Tree t) [Tree t] (Tree t)
            | Comment t
            | DocType t
            | TextNode t
            | Document [Tree t]
            | Error [t]
            deriving (Show, Foldable)

data Attr t = SimpleAttribute t
            | ValueAttribute t t t
            deriving (Show, Foldable)

instance IsTree Tree where
  emptyNode = Document []
  uniplate (Document ts) = (ts, Document)
  uniplate (ProperTag t0 items t1) =
    (t0 : t1 : items, \(t0':t1':items') -> ProperTag t0' items' t1')
  uniplate (MismatchedTag t0 items t1) =
    (t0 : t1 : items, \(t0':t1':items') -> MismatchedTag t0' items' t1')
  uniplate t = ([], const t)


parse :: P TT (Tree TT)
parse = pDocument <* eof

sym :: (Eq t) => t -> Parser (Tok t) (Tok t)
sym t = sym' (== t)
  where
    sym' p = symbol (p . tokT)

tagName, comment, doctype, textNode, special, attrName,
  attrString, errorTok :: Token -> Bool
tagName (HTML.TagName _) = True
tagName _ = False
doctype (HTML.Doctype _) = True
doctype _ = False
comment (HTML.Comment _) = True
comment _ = False
textNode (HTML.TextNode _) = True
textNode _ = False
special (HTML.Special _) = True
special _ = False
attrName (HTML.AttrName _) = True
attrName _ = False
attrString (HTML.AttrString _) = True
attrString _ = False
errorTok (HTML.ErrorToken _) = True
errorTok _ = False

pDocument :: P TT (Tree TT)
pDocument = Document <$> many pItem

pItem :: P TT (Tree TT)
pItem = pTagTree
    <|> pComment
    <|> pTextNode
    <|> pError

pError :: P TT (Tree TT)
pError = Error <$> some (symbol (errorTok .tokT))

pComment = Comment <$> symbol (comment . tokT)
       <|> DocType <$> symbol (doctype . tokT)

pSpecial, pTextContent, pTagName :: P TT TT
pSpecial = symbol (special . tokT)
pTextContent = symbol (textNode . tokT)
pTagName = symbol (tagName . tokT)

pTextNode :: P TT (Tree TT)
pTextNode = do
  tok <- anyToken
  case tokT tok of
    HTML.TextNode _ -> return $ TextNode tok
    HTML.Special _ -> return $ TextNode tok
    _ -> return $ Error [tok]

-- pProperTag = do
--   o@(OpenTag _ tno _ _) <- pOpenTag
--   t <- many pTree
--   c@(CloseTag _ _ tnc _) <- pCloseTag
--   case tno == tnc of
--     True -> return $ ProperTag o t c
--     False -> return $ MismatchedTag o t tnc

fromTT = tokT

anyToken = symbol (const True . tokT)

pTagTree :: P TT (Tree TT)
pTagTree = do
  t0 <- pTryTag
  case t0 of
    SelfTag _ _ _ _ _ -> return t0
    OpenTag _ targetTag _ _ -> do
      items <- many pItem
      mCloseTag <- pTryTag
      case mCloseTag of
        CloseTag _ _ tname _
          | tname == targetTag -> return $ ProperTag t0 items mCloseTag
          | otherwise -> return $ MismatchedTag t0 items mCloseTag
        _ -> fail "Mismatched tag 1"
    _ -> fail "Mismatched tag 2"

pTryTag :: P TT (Tree TT)
pTryTag = do
  openT <- sym HTML.TagLeft
  nxt <- anyToken
  case tokT nxt of
    HTML.Slash -> do
      tn <- pTagName
      tr <- sym HTML.TagRight
      return $ CloseTag openT nxt tn tr
    HTML.TagName _ -> do
      attrs <- many pTagAttr
      maybeClose <- anyToken
      case tokT maybeClose of
        HTML.TagRight -> return $ OpenTag openT nxt attrs maybeClose
        HTML.Slash -> do
          tr <- sym HTML.TagRight
          return $ SelfTag openT nxt attrs maybeClose tr
        _ -> return $ Error [openT, nxt, maybeClose]
    _ -> return $ Error [openT, nxt]

-- Check for a normal attribute first because it is a longer parse.
-- If it fails, fall back to a simple no-value attribute.
pTagAttr, pSimpleAttr, pAttr :: P TT (Attr TT)
pTagAttr = pAttr <|> pSimpleAttr
pSimpleAttr = SimpleAttribute <$> symbol (attrName . tokT)
pAttr = ValueAttribute <$> symbol (attrName . tokT) <*> sym HTML.Equals <*> symbol (attrString . tokT)

getStrokes :: Tree TT -> Point -> Point -> Point -> [Stroke]
getStrokes t0 point begin end = appEndo (go t0) []
  where
    go :: Tree TT -> Endo [Stroke]
    go (TextNode t) = ss t
    go (ProperTag o t c) = go o <> foldMap go t <> go c
    go (MismatchedTag o t c) = go o <> foldMap go t <> go c -- foldMap (Endo . modStroke errorStroke) (go c)
    go (SelfTag o n atts s c) = ss o <> ss n <> foldMap goAttr atts <> ss s <> ss c
    go (OpenTag o n atts c) = ss o <> ss n <> foldMap goAttr atts <> ss c
    go (CloseTag o s n c) = ss o <> ss s <> ss n <> ss c
    go (Comment c) = ss c
    go (DocType dt) = asComment dt
    go (Error es) = foldMap errorStroke es
    go (Document ts) = foldMap go ts

    goAttr (SimpleAttribute t) = ss t
    goAttr (ValueAttribute a e v) = ss a <> ss e <> ss v

asComment = one (modStroke commentStyle . tokenToStroke)

ss :: TT -> Endo [Stroke]
ss = toStroke id

errorStroke :: TT -> Endo [Stroke]
errorStroke x = one (modStroke commentStyle . tokenToStroke) x

one :: (t -> a) -> t -> Endo [a]
one f x = Endo (f x :)

modStroke :: StyleName -> Stroke -> Stroke
modStroke f = fmap (f <>)

toStroke :: (Stroke -> a) -> TT -> Endo [a]
toStroke f t
  | isErrorTok (tokT t) = mempty
  | otherwise = Endo (f (tokenToStroke t) :)

tokenToStroke :: TT -> Stroke
tokenToStroke = fmap tokenToStyle . tokToSpan

tokenToStyle :: Token -> StyleName
tokenToStyle t =
  case t of
    HTML.TagName _ -> keywordStyle
    HTML.Equals -> operatorStyle
    HTML.TagLeft -> operatorStyle
    HTML.TagRight -> operatorStyle
    HTML.Slash -> operatorStyle
    HTML.AttrName _ -> keywordStyle
    HTML.AttrString _ -> stringStyle
    HTML.Special _ -> preprocessorStyle
    HTML.OpenSpecial -> errorStyle
    HTML.Doctype _ -> commentStyle
    HTML.Comment _ -> commentStyle
    HTML.TextNode _ -> defaultStyle
    HTML.ErrorToken _ -> errorStyle

isErrorTok :: Token -> Bool
isErrorTok HTML.OpenSpecial = True
isErrorTok _ = False
