{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant return" #-}
module Main (main) where

import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Parsec
import Tokens

-- import qualified Lexer as requisitada

----------------------------- Blocos -----------------------------
declarationToken = tokenPrim show update_pos get_token -- declaration
  where
    get_token (Declaration position) = Just (Declaration position)
    get_token _ = Nothing

endDeclarationToken :: ParsecT [Token] [(Token, Token)] IO Token
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

ifToken :: ParsecT [Token] [(Token, Token)] IO Token
ifToken = tokenPrim show update_pos get_token -- if
  where
    get_token (If position) = Just (If position)
    get_token _ = Nothing

endifToken :: ParsecT [Token] [(Token, Token)] IO Token
endifToken = tokenPrim show update_pos get_token -- endIf
  where
    get_token (EndIf position) = Just (EndIf position)
    get_token _ = Nothing

elifToken :: ParsecT [Token] [(Token, Token)] IO Token
elifToken = tokenPrim show update_pos get_token -- elif
  where
    get_token (Elif position) = Just (Elif position)
    get_token _ = Nothing

elseToken :: ParsecT [Token] [(Token, Token)] IO Token
elseToken = tokenPrim show update_pos get_token -- else
  where
    get_token (Else position) = Just (Else position)
    get_token _ = Nothing

----------------------------- Simbolos -----------------------------

-- :Colon
colonToken :: ParsecT [Token] [(Token, Token)] IO Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token (Colon position) = Just (Colon position)
    get_token _ = Nothing

-- ; SemiColon
semiColonToken :: ParsecT [Token] [(Token, Token)] IO Token
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token (SemiColon position) = Just (SemiColon position)
    get_token _ = Nothing

-- , Comma
commaToken :: ParsecT [Token] [(Token, Token)] IO Token
commaToken = tokenPrim show update_pos get_token
  where
    get_token (Comma position) = Just (Comma position)
    get_token _ = Nothing

-- ( LeftParenthesis
leftParenthesisToken :: ParsecT [Token] [(Token, Token)] IO Token
leftParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (LeftParenthesis position) = Just (LeftParenthesis position)
    get_token _ = Nothing

-- ) RightParenthesis
rightParenthesisToken :: ParsecT [Token] [(Token, Token)] IO Token
rightParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (RightParenthesis position) = Just (RightParenthesis position)
    get_token _ = Nothing

-- { LeftCurlyBrackets
leftCurlyBracketsToken :: ParsecT [Token] [(Token, Token)] IO Token
leftCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (LeftCurlyBrackets position) = Just (LeftCurlyBrackets position)
    get_token _ = Nothing

-- } RightCurlyBrackets
rightCurlyBracketsToken :: ParsecT [Token] [(Token, Token)] IO Token
rightCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (RightCurlyBrackets position) = Just (RightCurlyBrackets position)
    get_token _ = Nothing

-- -> To
toToken :: ParsecT [Token] [(Token, Token)] IO Token
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

xorToken :: ParsecT [Token] [(Token, Token)] IO Token
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
idToken :: ParsecT [Token] [(Token, Token)] IO Token
idToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Id x position) = Just (Id x position)
    get_token _ = Nothing

----------------------------- Tipos -----------------------------
typeToken :: ParsecT [Token] [(Token, Token)] IO Token
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

boolValToken = tokenPrim show update_pos get_token
  where
    get_token (BoolValue x position) = Just (BoolValue x position)
    get_token _ = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token, Token)] IO [Token]
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

decls :: ParsecT [Token] [(Token, Token)] IO [Token]
decls = do
  first <- decl
  next <- remainingDecls
  return (first ++ next)

remainingDecls :: ParsecT [Token] [(Token, Token)] IO [Token]
remainingDecls =
  ( do
      decls
  )
    <|> return []

decl :: ParsecT [Token] [(Token, Token)] IO ([Token])
decl = do
  id <- idToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken

  updateState (symtableInsert (id, getDefaultValue varType))
  state <- getState
  liftIO (print state)

  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

----------------------------- Code -----------------------------

stmts :: ParsecT [Token] [(Token, Token)] IO [Token]
stmts = do
  first <- stmt
  next <- remainingStmts
  return (first ++ next)

remainingStmts :: ParsecT [Token] [(Token, Token)] IO [Token]
remainingStmts =
  ( do
      stmts
  )
    <|> return []

stmt :: ParsecT [Token] [(Token, Token)] IO [Token]
stmt =
  try
    assignStmt

-- <|> ifStmt

-- ifStmt :: ParsecT [Token] [(Token, Token)] IO [Token]
-- ifStmt = do
--   ifLiteral <- ifToken
--   expression <- ifParenthesisExpression
--   colonLiteral <- colonToken
--   stmtsBlock <- stmts
--   elifStmt' <- elifStmt
--   elseStmt' <- elseStmt
--   endIfLiteral <- endifToken
--   semiCol <- semiColonToken
--   return ([ifLiteral] ++ expression ++ [colonLiteral] ++ stmtsBlock ++ elifStmt' ++ elseStmt' ++ [endIfLiteral] ++ [semiCol])

-- elifStmt :: ParsecT [Token] [(Token, Token)] IO [Token]
-- elifStmt =
--   ( do
--       elifLiteral <- elifToken
--       expression <- ifParenthesisExpression
--       colonLiteral <- colonToken
--       stmtsBlock <- stmts
--       return ([elifLiteral] ++ expression ++ [colonLiteral] ++ stmtsBlock)
--   )
--     <|> return []

elseStmt :: ParsecT [Token] [(Token, Token)] IO [Token]
elseStmt =
  ( do
      elseLiteral <- elseToken
      colonLiteral <- colonToken
      stmtsBlock <- stmts
      return ([elseLiteral] ++ [colonLiteral] ++ stmtsBlock)
  )
    <|> return []

assignStmt :: ParsecT [Token] [(Token, Token)] IO [Token]
assignStmt = do
  assignTok <- assign
  semiCol <- semiColonToken
  return (assignTok ++ [semiCol])

assign :: ParsecT [Token] [(Token, Token)] IO [Token]
assign = do
  id <- idToken
  assignSym <- assignToken
  value <- assignVal
  state <- getState
  if not (compatible (getType id state) value)
    then fail "type mismatch"
    else do
      updateState (symtableUpdate (id, value))
      s <- getState
      liftIO (print s)
      return (id : assignSym : [value])

assignVal :: ParsecT [Token] [(Token, Token)] IO Token
assignVal =
  try
    assignValExpression
    <|> valueLiteralExpression

valueLiteral :: ParsecT [Token] [(Token, Token)] IO Token
valueLiteral =
  do
    intValToken
    <|> floatValToken
    <|> charValToken
    <|> stringValToken
    <|> boolValToken

----------------------------- Expressões -----------------------------

assignValExpression :: ParsecT [Token] [(Token, Token)] IO Token
assignValExpression =
  try
    relationalExpression
    <|> arithmeticExpression
    <|> logicalExpression
    <|> parenthesisExpression
    <|> idTokenExpression

-- <|> call

arithmeticExpression :: ParsecT [Token] [(Token, Token)] IO Token
arithmeticExpression =
  try
    plusMinusExpression
    <|> term

-- + | -
plusMinusExpression :: ParsecT [Token] [(Token, Token)] IO Token
plusMinusExpression = do
  term' <- term
  result <- arithmeticExpressionRemaining term'
  return result

arithmeticExpressionRemaining :: Token -> ParsecT [Token] [(Token, Token)] IO Token
arithmeticExpressionRemaining termIn =
  do
    arithmeticOp <- binaryArithmeticOperatorLiteral
    term' <- term
    result <- arithmeticExpressionRemaining (binaryEval termIn arithmeticOp term')
    return result
    <|> return termIn

-- < | <= | == | > | >= | !=
relationalExpression :: ParsecT [Token] [(Token, Token)] IO Token
relationalExpression = do
  arithmeticExpressionRight <- arithOrParentExpression
  relationalOp <- binaryRelationalOperatorLiteral
  arithmeticExpressionLeft <- arithOrParentExpression

  let result = binaryEval arithmeticExpressionRight relationalOp arithmeticExpressionLeft
  return result

arithOrParentExpression :: ParsecT [Token] [(Token, Token)] IO Token
arithOrParentExpression =
  try
    parenthesisExpression
    <|> arithmeticExpression

logicalExpression :: ParsecT [Token] [(Token, Token)] IO Token
logicalExpression =
  try
    binaryLogicalExpression
    <|> notExpression

-- (<exp>) && (<exp>) | (<exp>) || (<exp>)
binaryLogicalExpression :: ParsecT [Token] [(Token, Token)] IO Token
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
notExpression :: ParsecT [Token] [(Token, Token)] IO Token
notExpression =
  try
    notBoolValExpression
    <|> notParenthesisExpression

notBoolValExpression :: ParsecT [Token] [(Token, Token)] IO Token
notBoolValExpression =
  do
    notTok <- notToken
    boolValue <- boolValToken
    let result = unaryEval notTok boolValue
    return result

notParenthesisExpression :: ParsecT [Token] [(Token, Token)] IO Token
notParenthesisExpression =
  do
    notTok <- notToken
    expression <- parenthesisExpression
    let result = unaryEval notTok expression
    return result

relatOrLogicExpression :: ParsecT [Token] [(Token, Token)] IO Token
relatOrLogicExpression =
  try
    relationalExpression
    <|> logicalExpression

parenthesisExpression :: ParsecT [Token] [(Token, Token)] IO Token
parenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- assignValExpression
  rightPar <- rightParenthesisToken
  return expression

ifParenthesisExpression :: ParsecT [Token] [(Token, Token)] IO Token
ifParenthesisExpression = do
  leftPar <- leftParenthesisToken
  expression <- relatOrLogicExpression
  rightPar <- rightParenthesisToken
  return expression

idTokenExpression :: ParsecT [Token] [(Token, Token)] IO Token
idTokenExpression = do
  idToken' <- idToken
  return idToken'

valueLiteralExpression :: ParsecT [Token] [(Token, Token)] IO Token
valueLiteralExpression = do
  valueLiteral

term :: ParsecT [Token] [(Token, Token)] IO Token
term =
  try
    termExpression
    <|> factor

termExpression :: ParsecT [Token] [(Token, Token)] IO Token
termExpression = do
  factor' <- factor
  result <- termRemaining factor'
  return result

termRemaining :: Token -> ParsecT [Token] [(Token, Token)] IO Token
termRemaining factorIn =
  do
    termOp <- termOperatorLiteral
    factor' <- factor
    result <- termRemaining (binaryEval factorIn termOp factor')
    return result
    <|> return factorIn

termOperatorLiteral :: ParsecT [Token] [(Token, Token)] IO Token
termOperatorLiteral =
  do
    timesToken
    <|> dividerToken
    <|> integerDividerToken

factor :: ParsecT [Token] [(Token, Token)] IO Token
factor =
  try
    factorExpression
    <|> exponential

factorExpression :: ParsecT [Token] [(Token, Token)] IO Token
factorExpression = do
  exponential' <- exponential
  result <- factorRemaining exponential'
  return result

factorRemaining :: Token -> ParsecT [Token] [(Token, Token)] IO Token
factorRemaining exponentialIn =
  do
    factorOp <- exponentToken
    exponential' <- exponential
    result <- factorRemaining (binaryEval exponentialIn factorOp exponential')
    return result
    <|> return exponentialIn

exponential :: ParsecT [Token] [(Token, Token)] IO Token
exponential =
  try
    valueLiteralExpression
    <|> idTokenExpression

-- <|> call

----------------------------- Parsec de literais -----------------------------

binaryArithmeticOperatorLiteral :: ParsecT [Token] [(Token, Token)] IO Token
binaryArithmeticOperatorLiteral =
  try
    plusToken
    <|> minusToken

binaryRelationalOperatorLiteral :: ParsecT [Token] [(Token, Token)] IO Token
binaryRelationalOperatorLiteral =
  try
    lessToken
    <|> lessEqualToken
    <|> greaterToken
    <|> greaterEqualToken
    <|> equalToken
    <|> differentToken

binaryLogicalOperatorLiteral :: ParsecT [Token] [(Token, Token)] IO Token
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
binaryEval (IntValue x p) (Plus _) (IntValue y _) = IntValue (x + y) p -- Soma
binaryEval (IntValue x p) (Minus _) (IntValue y _) = IntValue (x - y) p -- Subtração
binaryEval (IntValue x p) (Times _) (IntValue y _) = IntValue (x * y) p
-- Divisão regular
binaryEval (IntValue x p) (Divider _) (IntValue y _) = FloatValue (fromIntegral x / fromIntegral y) p
binaryEval (IntValue x p) (Divider _) (FloatValue y _) = FloatValue (fromIntegral x / y) p
binaryEval (FloatValue x p) (Divider _) (IntValue y _) = FloatValue (x / fromIntegral y) p
binaryEval (FloatValue x p) (Divider _) (FloatValue y _) = FloatValue (x / y) p
-- Divisão Iteira
binaryEval (IntValue x p) (IntegerDivider _) (IntValue y _) = IntValue (x `div` y) p
-- Exponenciação
binaryEval (IntValue x p) (Exponent _) (IntValue y _) = IntValue (x ^ y) p

{-
  Realiza a operação unária requisitada. Recebendo 2 parâmetros:
  param2: Token de Operação
  param3: Token de "TypeValue"
-}
unaryEval :: Token -> Token -> Token
unaryEval notToken (BoolValue x p) = BoolValue (not x) p -- Not (!)

{-
  getType recebe um ID e a lista de símbolos atuais, e retornará o Token TypeValue pertencente à tupla deste ID, para posteriormente realizar uma comparação
-}
getType :: Token -> [(Token, Token)] -> Token
getType _ [] = error "variable not found"
getType (Id id1 p1) ((Id id2 _, value) : listTail) =
  if id1 == id2
    then value
    else getType (Id id1 p1) listTail

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

----------------------------- Tabela de símbolos -----------------------------
{-
  A tabela de simbolos é uma lista de tuplas, onde cada tupla possui dois Tokens, um identificando a variavel e outro identificando seu valor:
          symtable = [(IdToken1, val1), (IdToken12, val2), ... , (IdToken1n, valn)]
-}

{-
  SymtableInsert recebe uma tupla (Token ID, Token TypeValue) e armazena na tabela de simbolos
-}
symtableInsert :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableInsert symbol [] = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

{-
  symtableUpdate recebe uma tupla (Token ID, Token Value) e atualiza o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableUpdate :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableUpdate _ [] = fail "variable not found"
symtableUpdate (Id id1 p1, v1) ((Id id2 p2, v2) : t) =
  if id1 == id2
    then (Id id1 p2, v1) : t
    else (Id id2 p2, v2) : symtableUpdate (Id id1 p1, v1) t

{-
  symtableRemove recebe uma tupla (Token ID, Token Value) e remove  ID e o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableRemove :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableRemove _ [] = fail "variable not found"
symtableRemove (id1, v1) ((id2, v2) : t) =
  if id1 == id2
    then t
    else (id2, v2) : symtableRemove (id1, v1) t

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "exemplo5_atribuicoes_por_expressoes.txt")) of
  Left err -> print err
  Right ans -> print ans