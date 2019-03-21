module PatMat.Core.Syntax where

import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Indent
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import PatMat.Core.Pat
import PatMat.Core.ADT

type Parser a = IndentParser String () a

-- A is the type.
data RawPat = RPAny | RPExact Sym [RawPat]
  deriving (Show, Eq)

-- pats, maybe cond, res
type RawCase = ([RawPat], Maybe Sym, Sym)
type RawADT = [RawVariant]

-- Variant name, [(typeName, fieldName?)]
type RawVariant = (Sym, [(Sym, Maybe Sym)])


data Stmt
  = STypeDef Sym RawADT
  | SMatch {- funcName -} String {- [(typeName, argName)] -} [(Sym, String)] [RawCase]
  deriving (Show, Eq)

-- Example:
-- data TypeName = (Ctor Fields)*;
--   where Fields = ident* | { ((ident,)+ :: ident,)+ }
-- FuncName (ident :: ident)+ = case
--   (Pat,)+ -> "asdf"
--   (Pat,)+ | "cond" -> "fdsa";

runParser :: String -> Parser a -> Either ParseError a
runParser src p = runIndentParser p () "PatMat.Syntax" src

pTopLavelStmts = whiteSpace *> many1 (pTopLavelStmt <* semi) <* eof

pTopLavelStmt :: Parser Stmt
pTopLavelStmt = topLevel *> (pTypeDef <|> pMatch)

pTypeDef :: Parser Stmt
pTypeDef = do
  reserved "data"
  typeName <- SType <$> ident
  reservedOp "="
  vs <- pVariant `sepBy` reservedOp "|"
  pure (STypeDef typeName vs)

pVariant :: Parser RawVariant
pVariant = do
  ctor <- SCtor <$> ident
  fs <- braces (concat <$> pNamed `sepBy1` comma) <|> many pUnnamed
  pure (ctor, fs)
 where
  pNamed = do
    names <- (Just . SField <$> ident) `sepBy1` comma
    reservedOp "::"
    ty <- SType <$> ident
    pure (map (ty,) names)
  pUnnamed = do
    ty <- SType <$> ident
    pure (ty, Nothing)

pMatch :: Parser Stmt
pMatch = withBlock mk pHead pCase
 where
  mk (funcName, args) cases = SMatch funcName args cases
  pHead = do
    funcName <- ident
    args <- concat <$> many1 pArg
    reservedOp "="
    reserved "case"
    pure (funcName, args)

  pArg = parens $ do
    argNames <- many1 ident
    reservedOp "::"
    tyName <- SType <$> ident
    pure (map (tyName,) argNames)

pCase :: Parser RawCase
pCase = (,,) <$> pPats <*> pCond <*> (reservedOp "->" *> pBody)
 where
  pPats = pPat `sepBy1` comma
  pCond = fmap SGuard <$> (reservedOp "|" *> (Just <$> stringLiteral) <|> pure Nothing)
  pBody = SResult <$> (stringLiteral <|> (show <$> intLiteral))

-- XXX fields might be too greedy.
pPat :: Parser RawPat
pPat = go True
 where
  go canHaveFields = do
    ctor <- underscore <|> ident
    if ctor == "_"
      then pure RPAny
      else if canHaveFields
        then do
          fields <- many (parens (go True) <|> go False)
          pure (RPExact (SCtor ctor) fields)
        else
          pure (RPExact (SCtor ctor) [])

-- Lexers

lexer = P.makeTokenParser haskellStyle

stringLiteral = P.stringLiteral lexer
whiteSpace = P.whiteSpace lexer
intLiteral = P.natural lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
ident = P.identifier lexer
parens = P.parens lexer
braces = P.braces lexer
comma = P.comma lexer
maybeParens x = parens x <|> x
lexeme = P.lexeme lexer
underscore = lexeme (string "_")
semi = P.semi lexer

-- Slightly modified from Parsec
haskellStyle :: P.GenLanguageDef String () (IndentT Identity)
haskellStyle = P.emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.nestedComments = True
                , P.identStart     = letter
                , P.identLetter    = alphaNum <|> oneOf "_'"
                , P.opStart        = P.opLetter haskellStyle
                , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.reservedOpNames= ["->", "=", "::", "|"]
                , P.reservedNames  = ["case", "of", "data"]
                , P.caseSensitive  = True
                }

-- Name resolving
data Sym
  = SType String
  | SCtor String
  | SField String
  | SGuard String
  | SResult String
  deriving (Eq, Ord)

instance Show Sym where
  show = showSym

showSym = \case
  SType x -> x
  SCtor x -> x
  SField x -> x
  SGuard x -> x
  SResult x -> x

-- Tying the knot.
grabADTs :: [Stmt] -> M.Map Sym (ADT Sym)
grabADTs ss = tyMap
 where
  tyMap = foldr go M.empty ss
  go s = case s of
    STypeDef name rty -> M.insert name (toADT name rty)
    _ -> id

  toADT :: Sym -> RawADT -> ADT Sym
  toADT name vs = ADT name (map toVariant vs)

  toVariant (ctor, fs) = Variant ctor (map resolve fs)

  resolve (tyName, fieldName) = Field fieldName (tyMap M.! tyName)

grabCases :: [Stmt] -> M.Map Sym (ADT Sym) -> [(String, [String], Cases Sym)]
grabCases ss tyMap = cs
 where
  cs = catMaybes (map go ss)

  go s = case s of
    SMatch funcName args rcs ->
      let typeNames = map fst args
          argNames = map snd args
      in Just (funcName, argNames, map (doCase typeNames) rcs)
    _ -> Nothing

  doCase :: [Sym] -> RawCase -> Case Sym
  doCase tyNames (rps, cond, res) =
    Case (zipWith doPat rps (map (tyMap M.!) tyNames)) cond res

  doPat :: RawPat -> ADT Sym -> Pat Sym
  doPat RPAny _ = PAny
  doPat (RPExact ctor fs) ty@(ADT {..}) =
    PExact ctor ty (zipWith doPat fs (map fType (vFields v)))
   where
    Just v = L.find ((ctor ==) . vCtor) adtVariants

