{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec
import Tokens

-- import qualified Lexer as requisitada

----------------------------- Blocos -----------------------------
declarationToken = tokenPrim show update_pos get_token -- declaration
  where
    get_token (Declaration position) = Just (Declaration position)
    get_token _ = Nothing

endDeclarationToken :: ParsecT [Token] MemoryState IO Token
endDeclarationToken = tokenPrim show update_pos get_token -- end_declaration
  where
    get_token (EndDeclaration position) = Just (EndDeclaration position)
    get_token _ = Nothing

mainToken = tokenPrim show update_pos get_token -- main
  where
    get_token (Main position) = Just (Main position)
    get_token _ = Nothing

endMainToken = tokenPrim show update_pos get_token -- end_main
  where
    get_token (EndMain position) = Just (EndMain position)
    get_token _ = Nothing

ifToken :: ParsecT [Token] MemoryState IO Token
ifToken = tokenPrim show update_pos get_token -- if
  where
    get_token (If position) = Just (If position)
    get_token _ = Nothing

endifToken :: ParsecT [Token] MemoryState IO Token
endifToken = tokenPrim show update_pos get_token -- endIf
  where
    get_token (EndIf position) = Just (EndIf position)
    get_token _ = Nothing

elifToken :: ParsecT [Token] MemoryState IO Token
elifToken = tokenPrim show update_pos get_token -- elif
  where
    get_token (Elif position) = Just (Elif position)
    get_token _ = Nothing

elseToken :: ParsecT [Token] MemoryState IO Token
elseToken = tokenPrim show update_pos get_token -- else
  where
    get_token (Else position) = Just (Else position)
    get_token _ = Nothing

whileToken :: ParsecT [Token] MemoryState IO Token
whileToken = tokenPrim show update_pos get_token -- if
  where
    get_token (While position) = Just (While position)
    get_token _ = Nothing

endWhileToken :: ParsecT [Token] MemoryState IO Token
endWhileToken = tokenPrim show update_pos get_token -- if
  where
    get_token (EndWhile position) = Just (EndWhile position)
    get_token _ = Nothing

forToken :: ParsecT [Token] MemoryState IO Token
forToken = tokenPrim show update_pos get_token -- if
  where
    get_token (For position) = Just (For position)
    get_token _ = Nothing

endForToken :: ParsecT [Token] MemoryState IO Token
endForToken = tokenPrim show update_pos get_token -- if
  where
    get_token (EndFor position) = Just (EndFor position)
    get_token _ = Nothing

----------------------------- Simbolos -----------------------------

-- :Colon
colonToken :: ParsecT [Token] MemoryState IO Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token (Colon position) = Just (Colon position)
    get_token _ = Nothing

-- ; SemiColon
semiColonToken :: ParsecT [Token] MemoryState IO Token
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token (SemiColon position) = Just (SemiColon position)
    get_token _ = Nothing

-- , Comma
commaToken :: ParsecT [Token] MemoryState IO Token
commaToken = tokenPrim show update_pos get_token
  where
    get_token (Comma position) = Just (Comma position)
    get_token _ = Nothing

-- ( LeftParenthesis
leftParenthesisToken :: ParsecT [Token] MemoryState IO Token
leftParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (LeftParenthesis position) = Just (LeftParenthesis position)
    get_token _ = Nothing

-- ) RightParenthesis
rightParenthesisToken :: ParsecT [Token] MemoryState IO Token
rightParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (RightParenthesis position) = Just (RightParenthesis position)
    get_token _ = Nothing

-- { LeftCurlyBrackets
leftCurlyBracketsToken :: ParsecT [Token] MemoryState IO Token
leftCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (LeftCurlyBrackets position) = Just (LeftCurlyBrackets position)
    get_token _ = Nothing

-- } RightCurlyBrackets
rightCurlyBracketsToken :: ParsecT [Token] MemoryState IO Token
rightCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (RightCurlyBrackets position) = Just (RightCurlyBrackets position)
    get_token _ = Nothing

-- -> To
toToken :: ParsecT [Token] MemoryState IO Token
toToken = tokenPrim show update_pos get_token
  where
    get_token (To position) = Just (To position)
    get_token _ = Nothing

-- = Assign
assignToken = tokenPrim show update_pos get_token
  where
    get_token (Assign position) = Just (Assign position)
    get_token _ = Nothing

-- + Plus
plusToken = tokenPrim show update_pos get_token
  where
    get_token (Plus position) = Just (Plus position)
    get_token _ = Nothing

-- - Minus
minusToken = tokenPrim show update_pos get_token
  where
    get_token (Minus position) = Just (Minus position)
    get_token _ = Nothing

-- * Times

timesToken = tokenPrim show update_pos get_token
  where
    get_token (Times position) = Just (Times position)
    get_token _ = Nothing

-- / Divider
dividerToken = tokenPrim show update_pos get_token
  where
    get_token (Divider position) = Just (Divider position)
    get_token _ = Nothing

-- // IntegerDivider
integerDividerToken = tokenPrim show update_pos get_token
  where
    get_token (IntegerDivider position) = Just (IntegerDivider position)
    get_token _ = Nothing

-- ** Exponent

exponentToken = tokenPrim show update_pos get_token
  where
    get_token (Exponent position) = Just (Exponent position)
    get_token _ = Nothing

-- && And
andToken = tokenPrim show update_pos get_token
  where
    get_token (And position) = Just (And position)
    get_token _ = Nothing

-- | | Or
orToken = tokenPrim show update_pos get_token
  where
    get_token (Or position) = Just (Or position)
    get_token _ = Nothing
-- ^ Xor

xorToken :: ParsecT [Token] MemoryState IO Token
xorToken = tokenPrim show update_pos get_token
  where
    get_token (Xor position) = Just (Xor position)
    get_token _ = Nothing

-- ! Not
notToken = tokenPrim show update_pos get_token
  where
    get_token (Not position) = Just (Not position)
    get_token _ = Nothing

-- < Less
lessToken = tokenPrim show update_pos get_token
  where
    get_token (Less position) = Just (Less position)
    get_token _ = Nothing

-- <= LessEqual
lessEqualToken = tokenPrim show update_pos get_token
  where
    get_token (LessEqual position) = Just (LessEqual position)
    get_token _ = Nothing

-- > Greater
greaterToken = tokenPrim show update_pos get_token
  where
    get_token (Greater position) = Just (Greater position)
    get_token _ = Nothing

-- >= GreaterEqual
greaterEqualToken = tokenPrim show update_pos get_token
  where
    get_token (GreaterEqual position) = Just (GreaterEqual position)
    get_token _ = Nothing

-- == Equal
equalToken = tokenPrim show update_pos get_token
  where
    get_token (Equal position) = Just (Equal position)
    get_token _ = Nothing

-- != Different
differentToken = tokenPrim show update_pos get_token
  where
    get_token (Different position) = Just (Different position)
    get_token _ = Nothing

----------------------------- ID -----------------------------
idToken :: ParsecT [Token] MemoryState IO Token
idToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Id x position) = Just (Id x position)
    get_token _ = Nothing

----------------------------- Tipos -----------------------------
typeToken :: ParsecT [Token] MemoryState IO Token
typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x position) = Just (Type x position)
    get_token _ = Nothing

----------------------------- Valores Literais -----------------------------

intValToken = tokenPrim show update_pos get_token
  where
    get_token (IntValue x position) = Just (IntValue x position)
    get_token _ = Nothing

floatValToken = tokenPrim show update_pos get_token
  where
    get_token (FloatValue x position) = Just (FloatValue x position)
    get_token _ = Nothing

charValToken = tokenPrim show update_pos get_token
  where
    get_token (CharValue x position) = Just (CharValue x position)
    get_token _ = Nothing

stringValToken = tokenPrim show update_pos get_token
  where
    get_token (StringValue x position) = Just (StringValue x position)
    get_token _ = Nothing

boolValToken :: ParsecT [Token] u IO Token
boolValToken = tokenPrim show update_pos get_token
  where
    get_token (BoolValue x position) = Just (BoolValue x position)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

------------------------------- Parsers para os não-terminais -----------------------------

program :: ParsecT [Token] MemoryState IO [Token]
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

decls :: ParsecT [Token] MemoryState IO [Token]
decls = do
  first <- decl
  next <- remainingDecls
  return (first ++ next)

remainingDecls :: ParsecT [Token] MemoryState IO [Token]
remainingDecls =
  ( do
      decls
  )
    <|> return []

decl :: ParsecT [Token] MemoryState IO ([Token])
decl = do
  id <- idToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken

  updateState (symtableInsert (id, getDefaultValue varType))
  state <- getState
  liftIO (putStrLn $ "Declaração de variável: " ++ show id ++ show state)
  -- liftIO (print state)

  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

----------------------------- Code -----------------------------

stmts :: ParsecT [Token] MemoryState IO [Token]
stmts = do
  first <- stmt
  next <- remainingStmts
  return (first ++ next)

remainingStmts :: ParsecT [Token] MemoryState IO [Token]
remainingStmts =
  ( do
      stmts
  )
    <|> return []

stmt :: ParsecT [Token] MemoryState IO [Token]
stmt =
  try
    assignStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt

---- IF-ELIF-ELSE
ifStmt :: ParsecT [Token] MemoryState IO [Token]
ifStmt = do
  ifLiteral <- ifToken
  expression <- ifParenthesisExpression
  colonLiteral <- colonToken

  -- Verifica expressão do if
  let result = evaluateCondition expression
  if result
    then do
      stmtsBlock <- stmts
      skip' <- manyTill anyToken (lookAhead endifToken)
      liftIO (putStrLn $ "Tokens pulados depois do if:" ++ show skip')
      endIfLiteral <- endifToken
      semiCol1 <- semiColonToken
      return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ stmtsBlock ++ [endIfLiteral] ++ [semiCol1])
    else do
      skip' <- manyTill anyToken (lookAhead elifToken <|> elseToken)
      liftIO (putStrLn $ "Tokens pulados antes de elif <|> else:" ++ show skip')
      elifStmt' <- elifStmt
      if null elifStmt'
        then do
          skip' <- manyTill anyToken (lookAhead elseToken)
          liftIO (putStrLn $ "Tokens pulados antes de else:" ++ show skip')
          elseStmt' <- elseStmt
          endIfLiteral <- endifToken
          semiCol <- semiColonToken
          return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elseStmt' ++ [endIfLiteral] ++ [semiCol])
        else do
          skip' <- manyTill anyToken (lookAhead endifToken)
          liftIO (putStrLn $ "Tokens pulados após elif:" ++ show skip')
          endIfLiteral <- endifToken
          semiCol <- semiColonToken
          return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elifStmt' ++ [endIfLiteral] ++ [semiCol])

elifStmt :: ParsecT [Token] MemoryState IO [Token]
elifStmt =
  ( do
      elifLiteral <- elifToken
      expression <- ifParenthesisExpression
      colonLiteral <- colonToken
      -- Verifica expressão do elif
      let result = evaluateCondition expression
      if result
        then do
          stmtsBlock <- stmts
          return ([elifLiteral] ++ [expression] ++ [colonLiteral] ++ stmtsBlock)
        else do
          skip' <- manyTill anyToken (lookAhead elifToken)
          liftIO (putStrLn $ "Tokens pulados antes de elif seguido:" ++ show skip')
          elifStmt' <- elifStmt
          return elifStmt'
  )
    <|> return []

elseStmt :: ParsecT [Token] MemoryState IO [Token]
elseStmt =
  ( do
      elseLiteral <- elseToken
      colonLiteral <- colonToken
      stmtsBlock <- stmts
      return ([elseLiteral] ++ [colonLiteral] ++ stmtsBlock)
  )
    <|> return []

---- While
whileStmt :: ParsecT [Token] MemoryState IO [Token]
whileStmt = do
  whileLiteral <- whileToken
  expression <- ifParenthesisExpression
  colonLiteral <- colonToken
  stmtsBlock <- stmts
  endWhileLiteral <- endWhileToken
  semiCol <- semiColonToken
  return ([whileLiteral] ++ [expression] ++ [colonLiteral] ++ stmtsBlock ++ [endWhileLiteral] ++ [semiCol])

---- For
forStmt :: ParsecT [Token] MemoryState IO [Token]
forStmt = do
  forLiteral <- forToken
  expression <- forExpression
  colon' <- colonToken
  stmts' <- stmts
  endFor <- endForToken
  semiCol <- semiColonToken
  return ([forLiteral] ++ expression ++ [colon'] ++ stmts' ++ [endFor] ++ [semiCol])

---- Assign
assignStmt :: ParsecT [Token] MemoryState IO [Token]
assignStmt = do
  assignTok <- assign
  semiCol <- semiColonToken
  return (assignTok ++ [semiCol])

assign :: ParsecT [Token] MemoryState IO [Token]
assign = do
  id <- idToken
  assignSym <- assignToken
  value <- assignVal
  state <- getState
  if not (compatible (getType id state) value)
    then fail "type mismatch"
    else do
      updateState (symtableUpdate (id, value))
      newState <- getState
      liftIO (putStrLn $ "Atualização de estado sobre a variável: " ++ show id ++ show newState)
      return (id : assignSym : [value])

assignVal :: ParsecT [Token] MemoryState IO Token
assignVal =
  try
    assignValExpression
    <|> valueLiteralExpression

valueLiteral :: ParsecT [Token] MemoryState IO Token
valueLiteral =
  do
    intValToken
    <|> floatValToken
    <|> charValToken
    <|> stringValToken
    <|> boolValToken

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
  -- liftIO (putStrLn $ "Termo plusMinus: " ++ show term')
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
  idToken' <- idToken
  symtable <- getState
  case symtableGet idToken' symtable of
    Just val -> return val
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

forExpression :: ParsecT [Token] MemoryState IO [Token]
forExpression = do
  leftParenthesis <- leftParenthesisToken
  assign' <- assign
  semiCol' <- semiColonToken
  expression <- relatOrLogicExpression
  semiCol'' <- semiColonToken
  assign'' <- assign
  rightParenthesis <- rightParenthesisToken
  return ([leftParenthesis] ++ assign' ++ [semiCol'] ++ [expression] ++ [semiCol''] ++ assign'' ++ [rightParenthesis])

-- <|> call

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

----------------------------- Funções de Tipo -----------------------------
{-
  getDefaultValue é utilizado na declaração de novas variáveis, para definir um valor básico para ela. Recebe como parâmetro um token referente a um tipo
-}
getDefaultValue :: Token -> Token
getDefaultValue (Type "int" (l, c)) = IntValue 0 (l, c)
getDefaultValue (Type "char" (l, c)) = CharValue "" (l, c)
getDefaultValue (Type "string" (l, c)) = StringValue "" (l, c)
getDefaultValue (Type "float" (l, c)) = FloatValue 0.0 (l, c)
getDefaultValue (Type "bool" (l, c)) = BoolValue False (l, c)
getDefaultValue (Type _ (_, _)) = error "This type doesn't exist"

{-
  Realiza a operação binária requisitada. Recebendo 3 parâmetros:
  param1: Token de "TypeValue"
  param2: Token de Operação
  param3: Token de "TypeValue"
-}
binaryEval :: Token -> Token -> Token -> Token
---- Aritméticas
-- Soma
binaryEval (IntValue x p) (Plus _) (IntValue y _) = IntValue (x + y) p
binaryEval (IntValue x p) (Plus _) (FloatValue y _) = FloatValue (fromIntegral x + y) p
binaryEval (FloatValue x p) (Plus _) (IntValue y _) = FloatValue (x + fromIntegral y) p
binaryEval (FloatValue x p) (Plus _) (FloatValue y _) = FloatValue (x + y) p
-- Subtração
binaryEval (IntValue x p) (Minus _) (IntValue y _) = IntValue (x - y) p
binaryEval (IntValue x p) (Minus _) (FloatValue y _) = FloatValue (fromIntegral x - y) p
binaryEval (FloatValue x p) (Minus _) (IntValue y _) = FloatValue (x - fromIntegral y) p
binaryEval (FloatValue x p) (Minus _) (FloatValue y _) = FloatValue (x - y) p
-- Multiplicação
binaryEval (IntValue x p) (Times _) (IntValue y _) = IntValue (x * y) p
binaryEval (IntValue x p) (Times _) (FloatValue y _) = FloatValue (fromIntegral x * y) p
binaryEval (FloatValue x p) (Times _) (IntValue y _) = FloatValue (x * fromIntegral y) p
binaryEval (FloatValue x p) (Times _) (FloatValue y _) = FloatValue (x * y) p
-- Exponenciação
binaryEval (IntValue x p) (Exponent _) (IntValue y _) = IntValue (x ^ y) p
binaryEval (IntValue x p) (Exponent _) (FloatValue y _) = FloatValue (fromIntegral x ** y) p
binaryEval (FloatValue x p) (Exponent _) (IntValue y _) = FloatValue (x ^ y) p
binaryEval (FloatValue x p) (Exponent _) (FloatValue y _) = FloatValue (x ** y) p
-- Divisão Iteira
binaryEval (IntValue x p) (IntegerDivider _) (IntValue y _) = IntValue (x `div` y) p
-- Divisão regular
binaryEval (IntValue x p) (Divider _) (IntValue y _) = FloatValue (fromIntegral x / fromIntegral y) p
binaryEval (IntValue x p) (Divider _) (FloatValue y _) = FloatValue (fromIntegral x / y) p
binaryEval (FloatValue x p) (Divider _) (IntValue y _) = FloatValue (x / fromIntegral y) p
binaryEval (FloatValue x p) (Divider _) (FloatValue y _) = FloatValue (x / y) p
---- Relacionais
-- <
binaryEval (IntValue x p) (Less _) (IntValue y _) = BoolValue (fromIntegral x < fromIntegral y) p
binaryEval (FloatValue x p) (Less _) (IntValue y _) = BoolValue (x < fromIntegral y) p
binaryEval (IntValue x p) (Less _) (FloatValue y _) = BoolValue (fromIntegral x < y) p
binaryEval (FloatValue x p) (Less _) (FloatValue y _) = BoolValue (x < y) p
-- <=
binaryEval (IntValue x p) (LessEqual _) (IntValue y _) = BoolValue (fromIntegral x <= fromIntegral y) p
binaryEval (FloatValue x p) (LessEqual _) (IntValue y _) = BoolValue (x <= fromIntegral y) p
binaryEval (IntValue x p) (LessEqual _) (FloatValue y _) = BoolValue (fromIntegral x <= y) p
binaryEval (FloatValue x p) (LessEqual _) (FloatValue y _) = BoolValue (x <= y) p
-- >
binaryEval (IntValue x p) (Greater _) (IntValue y _) = BoolValue (fromIntegral x > fromIntegral y) p
binaryEval (FloatValue x p) (Greater _) (IntValue y _) = BoolValue (x > fromIntegral y) p
binaryEval (IntValue x p) (Greater _) (FloatValue y _) = BoolValue (fromIntegral x > y) p
binaryEval (FloatValue x p) (Greater _) (FloatValue y _) = BoolValue (x > y) p
-- >=
binaryEval (IntValue x p) (GreaterEqual _) (IntValue y _) = BoolValue (fromIntegral x >= fromIntegral y) p
binaryEval (FloatValue x p) (GreaterEqual _) (IntValue y _) = BoolValue (x >= fromIntegral y) p
binaryEval (IntValue x p) (GreaterEqual _) (FloatValue y _) = BoolValue (fromIntegral x >= y) p
binaryEval (FloatValue x p) (GreaterEqual _) (FloatValue y _) = BoolValue (x >= y) p
-- ==
binaryEval (IntValue x p) (Equal _) (IntValue y _) = BoolValue (fromIntegral x == fromIntegral y) p
binaryEval (FloatValue x p) (Equal _) (IntValue y _) = BoolValue (x == fromIntegral y) p
binaryEval (IntValue x p) (Equal _) (FloatValue y _) = BoolValue (fromIntegral x == y) p
binaryEval (FloatValue x p) (Equal _) (FloatValue y _) = BoolValue (x == y) p
-- !=
binaryEval (IntValue x p) (Different _) (IntValue y _) = BoolValue (fromIntegral x /= fromIntegral y) p
binaryEval (FloatValue x p) (Different _) (IntValue y _) = BoolValue (x /= fromIntegral y) p
binaryEval (IntValue x p) (Different _) (FloatValue y _) = BoolValue (fromIntegral x /= y) p
binaryEval (FloatValue x p) (Different _) (FloatValue y _) = BoolValue (x /= y) p
---- Lógicos
binaryEval (BoolValue x p) (And _) (BoolValue y _) = BoolValue (x && y) p
binaryEval (BoolValue x p) (Or _) (BoolValue y _) = BoolValue (x || y) p

{-
  Realiza a operação unária requisitada. Recebendo 2 parâmetros:
  param2: Token de Operação
  param3: Token de "TypeValue"
-}

unaryEval :: Token -> Token -> Token
unaryEval notToken (BoolValue x p) = BoolValue (not x) p -- Not (!)

{-
  getType recebe um Token ID (Id String (l, c)) e a lista de símbolos atuais, e retornará o Token TypeValue pertencente à tupla deste ID, para posteriormente realizar uma comparação
-}
getType :: Token -> MemoryState -> Token
getType _ (_, [], _, _, _) = error "variable not found"
getType (Id idStr1 pos1) (_, (Id idStr2 _, value) : listTail, _, _, _) =
  if idStr1 == idStr2
    then value
    else getType (Id idStr1 pos1) (False, listTail, [], [], [])

{-
  Função utilizada para verificar se uma expressão é verdadeira ou falsa para poder entrar em um bloco de código. Utilizado para verificação de Ifs, elifs, whiles e for.
-}
evaluateCondition :: Token -> Bool
evaluateCondition (Id id1 _) = id1 /= "False"
evaluateCondition (BoolValue val _) = val
evaluateCondition _ = error "Cannot evaluate condition"

{-
  Recebe dois Tokens TypeValue e compara se são do mesmo tipo
-}
compatible :: Token -> Token -> Bool
compatible (IntValue _ _) (IntValue _ _) = True
compatible (FloatValue _ _) (FloatValue _ _) = True
compatible (StringValue _ _) (StringValue _ _) = True
compatible (CharValue _ _) (CharValue _ _) = True
compatible (BoolValue _ _) (BoolValue _ _) = True
compatible _ _ = False

----------------------------- Memória de execução -----------------------------

type MemoryState = (Bool, [(Token, Token)], [(Token, [(Token, Token)], [Token])], [[(Token, (Token, Token))]], [[(Token, [(Token, Token)], [Token])]])

----------------------------- Flag -----------------------------
{-
  Uma variável booliana que será acionada para fazer mudanças semânticas durante a análise sobre blocos de códigos, como subprogramas. Quando estiver falsa, o bloco de código será analisado apenas sintaticamente e, se necessário, guardado na memória.
-}
-- Function to set the flag to True
setFlagTrue :: MemoryState -> MemoryState
setFlagTrue (flag, vars, funcs, structs, callstack) = (True, vars, funcs, structs, callstack)

-- Function to set the flag to False
setFlagFalse :: MemoryState -> MemoryState
setFlagFalse (flag, vars, funcs, structs, callstack) = (False, vars, funcs, structs, callstack)


----------------------------- Tabela de símbolos -----------------------------
{-
  A tabela de simbolos é uma lista de tuplas, onde cada tupla possui dois Tokens, um identificando a variavel e outro identificando seu valor:
          symtable = [(IdToken1, val1), (IdToken12, val2), ... , (IdToken1n, valn)]
-}
{-
  symtableGet recebe um Token ID (Id String (l,c)) referente a uma variável, e verifica se ela existe na tabela de símbolos e, caso exista, retorna seu valor.
-}
symtableGet :: (Token) -> MemoryState -> Maybe Token
symtableGet _ (_, [], _, _, _) = fail "variable not found"
symtableGet (Id idStr1 pos1) (_, (Id idStr2 pos2, value2) : listTail, _, _, _) =
  if idStr1 == idStr2
    then Just value2
    else symtableGet (Id idStr1 pos1) (False, listTail, [], [], [])
symtableGet _ _ = Nothing

{-
  SymtableInsert recebe uma tupla (Token ID, Token TypeValue) e armazena na tabela de simbolos
-}
symtableInsert :: (Token, Token) -> MemoryState -> MemoryState
symtableInsert symbol (flag, symtable, funcs, structs, callstack) = (flag, symtable ++ [symbol], funcs, structs, callstack)

{-
  symtableUpdate recebe uma tupla (Token ID, Token Value) e atualiza o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableUpdate :: (Token, Token) -> MemoryState -> MemoryState
symtableUpdate _ (_, [], _, _, _) = error "variable not found"
symtableUpdate (Id idStr1 pos1, value1) (flag, (Id idStr2 pos2, value2) : listTail, funcs, structs, callstack)
  | idStr1 == idStr2 = (flag, (Id idStr1 pos2, value1) : listTail, funcs, structs, callstack)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack') = symtableUpdate (Id idStr1 pos1, value1) (flag, listTail, funcs, structs, callstack)
       in (flag', (Id idStr2 pos2, value2) : updatedSymtable, funcs', structs', callstack')

{-
  symtableRemove recebe uma tupla (Token ID, Token Value) e remove  ID e o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableRemove :: (Token, Token) -> MemoryState -> MemoryState
symtableRemove _ (_, [], _, _, _) = error "variable not found"
symtableRemove (id1, v1) (flag, (id2, v2) : listTail, funcs, structs, callstack)
  | id1 == id2 = (flag, listTail, funcs, structs, callstack)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack') = symtableRemove (id1, v1) (flag, listTail, funcs, structs, callstack)
       in (flag', (id2, v2) : updatedSymtable, funcs', structs', callstack')

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (False, [], [], [], []) "Error message" tokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      case unsafePerformIO (parser (getTokens fn)) of
        Left err -> print err
        Right ans -> do
          putStr "Tokens do programa: "
          print ans
    _ -> putStrLn "Please inform the input filename. Closing application..."