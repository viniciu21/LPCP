module Expressions where

import Control.Monad.IO.Class
import LiteralTokens
import MemoryState
import Text.Parsec
import Tokens
import Utils

----------------------------- Expressões -----------------------------

assignValExpression :: ParsecT [Token] MemoryState IO Token
assignValExpression =
  try
    relationalExpression
    <|> arithmeticExpression
    <|> logicalExpression
    <|> parenthesisExpression
    <|> idTokenExpression

-- <|> call

arithmeticExpression :: ParsecT [Token] MemoryState IO Token
arithmeticExpression =
  try
    plusMinusExpression
    <|> term

-- + | -
plusMinusExpression :: ParsecT [Token] MemoryState IO Token
plusMinusExpression = do
  term' <- term
  result <- arithmeticExpressionRemaining term'
  return result

arithmeticExpressionRemaining :: Token -> ParsecT [Token] MemoryState IO Token
arithmeticExpressionRemaining termIn =
  do
    arithmeticOp <- binaryArithmeticOperatorLiteral
    term' <- term
    result <- arithmeticExpressionRemaining (binaryEval termIn arithmeticOp term')
    return result
    <|> return termIn

-- < | <= | == | > | >= | !=
relationalExpression :: ParsecT [Token] MemoryState IO Token
relationalExpression = do
  arithmeticExpressionRight <- arithOrParentExpression
  relationalOp <- binaryRelationalOperatorLiteral
  arithmeticExpressionLeft <- arithOrParentExpression

  let result = binaryEval arithmeticExpressionRight relationalOp arithmeticExpressionLeft
  return result

arithOrParentExpression :: ParsecT [Token] MemoryState IO Token
arithOrParentExpression =
  try
    parenthesisExpression
    <|> arithmeticExpression

logicalExpression :: ParsecT [Token] MemoryState IO Token
logicalExpression =
  try
    binaryLogicalExpression
    <|> notExpression

-- (<exp>) && (<exp>) | (<exp>) || (<exp>)
binaryLogicalExpression :: ParsecT [Token] MemoryState IO Token
binaryLogicalExpression = do
  firstPar <- leftParenthesisToken
  relExpLeft <- relationalExpression
  secondPar <- rightParenthesisToken
  logicalLiteral <- binaryLogicalOperatorLiteral
  thirdPar <- leftParenthesisToken
  relExpRight <- relationalExpression
  forthPar <- rightParenthesisToken
  let result = binaryEval relExpLeft logicalLiteral relExpRight
  return result

-- !(<exp>)
notExpression :: ParsecT [Token] MemoryState IO Token
notExpression =
  try
    notBoolValExpression
    <|> notParenthesisExpression

notBoolValExpression :: ParsecT [Token] MemoryState IO Token
notBoolValExpression =
  do
    notTok <- notToken
    boolValue <- boolValToken
    let result = unaryEval notTok boolValue
    return result

notParenthesisExpression :: ParsecT [Token] MemoryState IO Token
notParenthesisExpression =
  do
    notTok <- notToken
    expression <- parenthesisExpression
    let result = unaryEval notTok expression
    return result

relatOrLogicExpression :: ParsecT [Token] MemoryState IO Token
relatOrLogicExpression =
  try
    relationalExpression
    <|> logicalExpression

parenthesisExpression :: ParsecT [Token] MemoryState IO Token
parenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- assignValExpression
  rightPar <- rightParenthesisToken
  return expression

ifParenthesisExpression :: ParsecT [Token] MemoryState IO Token
ifParenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- relatOrLogicExpression
  rightPar <- rightParenthesisToken
  return expression

idTokenExpression :: ParsecT [Token] MemoryState IO Token
idTokenExpression = do
  -- liftIO $ liftIO (putStrLn $ "entrou aqui2")
  idToken' <- idToken
  -- liftIO $ print $ show idToken'
  symtable <- getState
  if isFuncFlagTrue symtable
    then do
      let idVal = getLocalSymtable idToken' symtable 
      return (fromTypeValuetoValue idVal)
  else 
    case symtableGet idToken' symtable of
      Just val -> return (fromTypeValuetoValue val)
      Nothing -> fail "Variable not found"

valueLiteralExpression :: ParsecT [Token] MemoryState IO Token
valueLiteralExpression = do
  valueLiteral

term :: ParsecT [Token] MemoryState IO Token
term =
  try
    termExpression
    <|> factor

termExpression :: ParsecT [Token] MemoryState IO Token
termExpression = do
  -- liftIO $ liftIO (putStrLn $ "entrou aqui5")
  factor' <- factor
  result <- termRemaining factor'
  return result

termRemaining :: Token -> ParsecT [Token] MemoryState IO Token
termRemaining factorIn =
  do
    termOp <- termOperatorLiteral
    factor' <- factor
    result <- termRemaining (binaryEval factorIn termOp factor')
    return result
    <|> return factorIn

termOperatorLiteral :: ParsecT [Token] MemoryState IO Token
termOperatorLiteral =
  do
    timesToken
    <|> dividerToken
    <|> integerDividerToken

factor :: ParsecT [Token] MemoryState IO Token
factor =
  try
    factorExpression
    <|> exponential

factorExpression :: ParsecT [Token] MemoryState IO Token
factorExpression = do
  exponential' <- exponential
  result <- factorRemaining exponential'
  return result

factorRemaining :: Token -> ParsecT [Token] MemoryState IO Token
factorRemaining exponentialIn =
  do
    factorOp <- exponentToken
    exponential' <- exponential
    result <- factorRemaining (binaryEval exponentialIn factorOp exponential')
    return result
    <|> return exponentialIn

exponential :: ParsecT [Token] MemoryState IO Token
exponential =
  try
    valueLiteralExpression
    <|> idTokenExpression

scanfExpression :: Token -> ParsecT [Token] MemoryState IO Token
scanfExpression idScan = do
  scanTok <- scanToken
  lParenthesisLiteral <- leftParenthesisToken
  scanString <- stringValToken
  rParenthesisLiteral <- rightParenthesisToken
  liftIO $ print scanString
  scanValue <- readValue idScan
  return scanValue

----------------------------- Parsec de literais -----------------------------

binaryArithmeticOperatorLiteral :: ParsecT [Token] MemoryState IO Token
binaryArithmeticOperatorLiteral =
  try
    plusToken
    <|> minusToken

binaryRelationalOperatorLiteral :: ParsecT [Token] MemoryState IO Token
binaryRelationalOperatorLiteral =
  try
    lessToken
    <|> lessEqualToken
    <|> greaterToken
    <|> greaterEqualToken
    <|> equalToken
    <|> differentToken

binaryLogicalOperatorLiteral :: ParsecT [Token] MemoryState IO Token
binaryLogicalOperatorLiteral =
  try
    andToken
    <|> orToken

-- <|> xorToken

valueLiteral :: ParsecT [Token] MemoryState IO Token
valueLiteral =
  do
    intValToken
    <|> floatValToken
    <|> charValToken
    <|> stringValToken
    <|> boolValToken