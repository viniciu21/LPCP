module Main (main) where

import Data.Binary.Get (remaining)
import Text.Parsec
import Tokens

----------------------------- Seções -----------------------------
declarationToken = tokenPrim show update_pos get_token -- declaration
  where
    get_token Declaration = Just Declaration
    get_token _ = Nothing

endDeclarationToken = tokenPrim show update_pos get_token -- end_declaration
  where
    get_token EndDeclaration = Just EndDeclaration
    get_token _ = Nothing

mainToken = tokenPrim show update_pos get_token -- main
  where
    get_token Main = Just Main
    get_token _ = Nothing

endMainToken = tokenPrim show update_pos get_token -- end_main
  where
    get_token EndMain = Just EndMain
    get_token _ = Nothing

----------------------------- Simbolos -----------------------------

-- :Colon
colonToken = tokenPrim show update_pos get_token
  where
    get_token Colon = Just Colon
    get_token _ = Nothing

-- ; SemiColon
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token SemiColon = Just SemiColon
    get_token _ = Nothing

-- , Comma
commaToken = tokenPrim show update_pos get_token
  where
    get_token Comma = Just Comma
    get_token _ = Nothing

-- ( LeftParenthesis
leftParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token LeftParenthesis = Just LeftParenthesis
    get_token _ = Nothing

-- ) RightParenthesis
rightParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token RightParenthesis = Just RightParenthesis
    get_token _ = Nothing

-- { LeftCurlyBrackets
leftCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token LeftCurlyBrackets = Just LeftCurlyBrackets
    get_token _ = Nothing

-- } RightCurlyBrackets
rightCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token RightCurlyBrackets = Just RightCurlyBrackets
    get_token _ = Nothing

-- -> To
toToken = tokenPrim show update_pos get_token
  where
    get_token To = Just To
    get_token _ = Nothing

-- = Assign
assignToken = tokenPrim show update_pos get_token
  where
    get_token Assign = Just Assign
    get_token _ = Nothing

-- + Plus
plusToken = tokenPrim show update_pos get_token
  where
    get_token Plus = Just Plus
    get_token _ = Nothing

-- - Minus
minusToken = tokenPrim show update_pos get_token
  where
    get_token Minus = Just Minus
    get_token _ = Nothing

-- * Times
timesToken = tokenPrim show update_pos get_token
  where
    get_token Times = Just Times
    get_token _ = Nothing

-- / Divider
dividerToken = tokenPrim show update_pos get_token
  where
    get_token Divider = Just Divider
    get_token _ = Nothing

-- // IntegerDivider
integerDividerToken = tokenPrim show update_pos get_token
  where
    get_token IntegerDivider = Just IntegerDivider
    get_token _ = Nothing

-- ** Exponent
exponentToken = tokenPrim show update_pos get_token
  where
    get_token Exponent = Just Exponent
    get_token _ = Nothing

-- && And
andToken = tokenPrim show update_pos get_token
  where
    get_token And = Just And
    get_token _ = Nothing

-- || Or
orToken = tokenPrim show update_pos get_token
  where
    get_token Or = Just Or
    get_token _ = Nothing

-- ^ Xor
xorToken = tokenPrim show update_pos get_token
  where
    get_token Xor = Just Xor
    get_token _ = Nothing

-- ! Not
notToken = tokenPrim show update_pos get_token
  where
    get_token Not = Just Not
    get_token _ = Nothing

-- < Less
lessToken = tokenPrim show update_pos get_token
  where
    get_token Less = Just Less
    get_token _ = Nothing

-- > Greater
greaterToken = tokenPrim show update_pos get_token
  where
    get_token Greater = Just Greater
    get_token _ = Nothing

-- == Equal
equalToken = tokenPrim show update_pos get_token
  where
    get_token Equal = Just Equal
    get_token _ = Nothing

-- != Different
differentToken = tokenPrim show update_pos get_token
  where
    get_token Different = Just Different
    get_token _ = Nothing

----------------------------- ID -----------------------------
idToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Id x) = Just (Id x)
    get_token _ = Nothing

----------------------------- Tipos -----------------------------
typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x) = Just (Type x)
    get_token _ = Nothing

----------------------------- Valores Literais -----------------------------

intValToken = tokenPrim show update_pos get_token
  where
    get_token (IntValue x) = Just (IntValue x)
    get_token _ = Nothing

floatValToken = tokenPrim show update_pos get_token
  where
    get_token (FloatValue x) = Just (FloatValue x)
    get_token _ = Nothing

charValToken = tokenPrim show update_pos get_token
  where
    get_token (CharValue x) = Just (CharValue x)
    get_token _ = Nothing

stringValToken = tokenPrim show update_pos get_token
  where
    get_token (StringValue x) = Just (StringValue x)
    get_token _ = Nothing

boolValToken = tokenPrim show update_pos get_token
  where
    get_token (BoolValue x) = Just (BoolValue x)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

-- parsers para os não-terminais

program :: Parsec [Token] st [Token]
program = do
  decl <- declarationToken
  colonD <- colonToken
  decls <- decls
  endDecl <- endDeclarationToken
  main <- mainToken
  colonM <- colonToken
  stmts <- stmts
  endMain <- endMainToken
  eof
  return ([decl] ++ [colonD] ++ decls ++ [endDecl] ++ [main] ++ [colonM] ++ stmts ++ [endMain])

decls :: Parsec [Token] st [Token]
decls = do
  first <- decl
  next <- remainingDecls
  return (first ++ next)

remainingDecls :: Parsec [Token] st [Token]
remainingDecls =
  ( do
      decls
  )
    <|> return []

decl :: Parsec [Token] st [Token]
decl = do
  id <- idToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

stmts :: Parsec [Token] st [Token]
stmts = do
  first <- stmt
  next <- remainingStmts
  return (first ++ next)

remainingStmts :: Parsec [Token] st [Token]
remainingStmts =
  ( do
      stmts -- Recursão em stms dando loop
  )
    <|> return []

stmt :: Parsec [Token] st [Token]
stmt = do
      assignTok <- assign
      semiCol <- semiColonToken
      return (assignTok ++ [semiCol])

assign :: Parsec [Token] st [Token]
assign = do
  id <- idToken
  assignSym <- assignToken
  value <- assignVal
  return (id : assignSym : value)

assignVal :: Parsec [Token] st [Token]
assignVal =
  do
    valLiteral <- assignValLiteral
    return [valLiteral]
    <|> assignValExpression

assignValLiteral :: Parsec [Token] st Token
assignValLiteral =
  do
    intValToken
    <|> floatValToken
    <|> charValToken
    <|> stringValToken
    <|> boolValToken

assignValExpression :: Parsec [Token] st [Token]
assignValExpression =
    do
        literalRight <- assignValLiteral
        plus <- plusToken
        literalLeft <- assignValLiteral
        return ([literalRight] ++ [plus] ++ [literalLeft])


-- invocação do parser para o símbolo de partida

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "exemplo_atribuicao_tipos_simples.txt") of
  Left err -> print err
  Right ans -> print ans