{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
module Main (main) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Binary.Get (remaining)
import Text.Parsec
import Tokens

----------------------------- Blocos -----------------------------
declarationToken = tokenPrim show update_pos get_token -- declaration
  where
    get_token Declaration = Just Declaration
    get_token _ = Nothing

endDeclarationToken :: Parsec [Token] st Token
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

ifToken = tokenPrim show update_pos get_token -- if
  where
    get_token If = Just If
    get_token _ = Nothing

endifToken = tokenPrim show update_pos get_token -- endIf
  where
    get_token EndIf = Just EndIf
    get_token _ = Nothing

elifToken :: Parsec [Token] st Token
elifToken = tokenPrim show update_pos get_token -- elif
  where
    get_token Elif = Just Elif
    get_token _ = Nothing

elseToken :: Parsec [Token] st Token
elseToken = tokenPrim show update_pos get_token -- else
  where
    get_token Else = Just Else
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
commaToken :: Parsec [Token] st Token
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
leftCurlyBracketsToken :: Parsec [Token] st Token
leftCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token LeftCurlyBrackets = Just LeftCurlyBrackets
    get_token _ = Nothing

-- } RightCurlyBrackets
rightCurlyBracketsToken :: Parsec [Token] st Token
rightCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token RightCurlyBrackets = Just RightCurlyBrackets
    get_token _ = Nothing

-- -> To
toToken :: Parsec [Token] st Token
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

-- | | Or
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

-- <= LessEqual
lessEqualToken = tokenPrim show update_pos get_token
  where
    get_token LessEqual = Just LessEqual
    get_token _ = Nothing

-- > Greater
greaterToken = tokenPrim show update_pos get_token
  where
    get_token Greater = Just Greater
    get_token _ = Nothing

-- >= GreaterEqual
greaterEqualToken = tokenPrim show update_pos get_token
  where
    get_token GreaterEqual = Just GreaterEqual
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

----------------------------- Declarações -----------------------------

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

----------------------------- Code -----------------------------

stmts :: Parsec [Token] st [Token]
stmts = do
  first <- stmt
  next <- remainingStmts
  return (first ++ next)

remainingStmts :: Parsec [Token] st [Token]
remainingStmts =
  ( do
      stmts
  )
    <|> return []

stmt :: Parsec [Token] st [Token]
stmt =
  try
    assignStmt
    <|> ifStmt

-- assignTok <- assign
-- semiCol <- semiColonToken
-- return (assignTok ++ [semiCol])

ifStmt :: Parsec [Token] st [Token]
ifStmt = do
  ifLiteral <- ifToken
  expression <- ifParenthesisExpression
  colonLiteral <- colonToken
  stmtsBlock <- stmts
  elifStmt' <- elifStmt
  elseStmt' <- elseStmt
  endIfLiteral <- endifToken
  semiCol <- semiColonToken
  return ([ifLiteral] ++ expression ++ [colonLiteral] ++ stmtsBlock ++ elifStmt' ++ elseStmt' ++ [endIfLiteral] ++ [semiCol])

elifStmt :: Parsec [Token] st [Token]
elifStmt =
  ( do
      elifLiteral <- elifToken
      expression <- ifParenthesisExpression
      colonLiteral <- colonToken
      stmtsBlock <- stmts
      return ([elifLiteral] ++ expression ++ [colonLiteral] ++ stmtsBlock)
  )
    <|> return []

elseStmt :: Parsec [Token] st [Token]
elseStmt =
  ( do
      elseLiteral <- elseToken
      colonLiteral <- colonToken
      stmtsBlock <- stmts
      return ([elseLiteral] ++ [colonLiteral] ++ stmtsBlock)
  )
    <|> return []

assignStmt :: Parsec [Token] st [Token]
assignStmt = do
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
  try
    assignValExpression
    <|> valueLiteralExpression

valueLiteral :: Parsec [Token] st Token
valueLiteral =
  do
    intValToken
    <|> floatValToken
    <|> charValToken
    <|> stringValToken
    <|> boolValToken

----------------------------- Expressões -----------------------------

assignValExpression :: Parsec [Token] st [Token]
assignValExpression =
  try
    relationalExpression
    <|> arithmeticExpression
    <|> logicalExpression
    <|> parenthesisExpression
    <|> idTokenExpression

-- <|> call

arithmeticExpression :: Parsec [Token] st [Token]
arithmeticExpression =
  try
    plusMinusExpression
    <|> term

plusMinusExpression :: Parsec [Token] st [Token]
plusMinusExpression = do
  term' <- term
  arithmeticOp <- binaryArithmeticOperatorLiteral
  expressionLeft <- arithmeticExpressionRemaining
  return (term' ++ [arithmeticOp] ++ expressionLeft)

arithmeticExpressionRemaining :: Parsec [Token] st [Token]
arithmeticExpressionRemaining =
  do
    arithmeticOp <- binaryArithmeticOperatorLiteral
    expressionLeft <- arithmeticExpressionRemaining
    return ([arithmeticOp] ++ expressionLeft)
    <|> term

relationalExpression :: Parsec [Token] st [Token]
relationalExpression = do
  arithmeticExpressionRight <- arithOrParentExpression
  relationalOp <- binaryRelationalOperatorLiteral
  arithmeticExpressionLeft <- arithOrParentExpression
  return (arithmeticExpressionRight ++ [relationalOp] ++ arithmeticExpressionLeft)

arithOrParentExpression :: Parsec [Token] st [Token]
arithOrParentExpression =
  try
    parenthesisExpression
    <|> arithmeticExpression

logicalExpression :: Parsec [Token] st [Token]
logicalExpression =
  try
    binaryLogicalExpression
    <|> notExpression

binaryLogicalExpression :: Parsec [Token] st [Token]
binaryLogicalExpression = do
  firstPar <- leftParenthesisToken
  relExpLeft <- relationalExpression
  secondPar <- rightParenthesisToken
  logicalLiteral <- binaryLogicalOperatorLiteral
  thirdPar <- leftParenthesisToken
  relExpRight <- relationalExpression
  forthPar <- rightParenthesisToken
  return ([firstPar] ++ relExpLeft ++ [secondPar] ++ [logicalLiteral] ++ [thirdPar] ++ relExpRight ++ [forthPar])

notExpression :: Parsec [Token] st [Token]
notExpression =
  try
    notBoolValExpression
    <|> notParenthesisExpression

notBoolValExpression :: Parsec [Token] st [Token]
notBoolValExpression =
  do
    notTok <- notToken
    boolValue <- boolValToken
    return ([notTok] ++ [boolValue])

notParenthesisExpression :: Parsec [Token] st [Token]
notParenthesisExpression =
  do
    notTok <- notToken
    expression <- parenthesisExpression
    return ([notTok] ++ expression)

relatOrLogicExpression :: Parsec [Token] st [Token]
relatOrLogicExpression =
  try
    relationalExpression
    <|> logicalExpression

parenthesisExpression :: Parsec [Token] st [Token]
parenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- assignValExpression
  rightPar <- rightParenthesisToken
  return ([leftPar] ++ expression ++ [rightPar])

ifParenthesisExpression :: Parsec [Token] st [Token]
ifParenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- relatOrLogicExpression
  rightPar <- rightParenthesisToken
  return ([leftPar] ++ expression ++ [rightPar])

idTokenExpression :: Parsec [Token] st [Token]
idTokenExpression = do
  idToken' <- idToken
  return [idToken']

valueLiteralExpression :: Parsec [Token] st [Token]
valueLiteralExpression = do
  valueLiteral' <- valueLiteral
  return [valueLiteral']

term :: Parsec [Token] st [Token]
term =
  try
    termExpression
    <|> factor

termExpression :: Parsec [Token] st [Token]
termExpression = do
  factor <- factor
  termOp <- termOperatorLiteral
  termAssoc <- termRemaining
  return (factor ++ [termOp] ++ termAssoc)

termRemaining :: Parsec [Token] st [Token]
termRemaining =
  do
    termOp <- termOperatorLiteral
    term <- termRemaining
    return ([termOp] ++ term)
    <|> factor

termOperatorLiteral :: Parsec [Token] st Token
termOperatorLiteral =
  do
    timesToken
    <|> dividerToken
    <|> integerDividerToken

factor :: Parsec [Token] st [Token]
factor =
  try
    factorExpression
    <|> exponential

factorExpression :: Parsec [Token] st [Token]
factorExpression = do
  exponential <- exponential
  factorOp <- exponentToken
  factor <- factorRemaining
  return (factor ++ [factorOp] ++ exponential)

factorRemaining :: Parsec [Token] st [Token]
factorRemaining =
  do
    factorOp <- exponentToken
    factor <- factorRemaining
    return ([factorOp] ++ factor)
    <|> exponential

exponential :: Parsec [Token] st [Token]
exponential =
  try
    valueLiteralExpression
    <|> idTokenExpression

-- <|> call

----------------------------- Parsec de literais -----------------------------

binaryArithmeticOperatorLiteral :: Parsec [Token] st Token
binaryArithmeticOperatorLiteral =
  try
    plusToken
    <|> minusToken

binaryRelationalOperatorLiteral :: Parsec [Token] st Token
binaryRelationalOperatorLiteral =
  try
    lessToken
    <|> lessEqualToken
    <|> greaterToken
    <|> greaterEqualToken
    <|> equalToken
    <|> differentToken

binaryLogicalOperatorLiteral :: Parsec [Token] st Token
binaryLogicalOperatorLiteral =
  try
    andToken
    <|> orToken
    <|> xorToken

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "exemplo_if_stmts.txt") of
  Left err -> print err
  Right ans -> print ans