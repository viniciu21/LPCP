-- {-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State (modify)
import Debug.Trace (trace)
import Expressions
import LiteralTokens
import MemoryState
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Tokens
import Utils
import Control.Arrow (ArrowChoice(right))

-- import qualified Lexer as requisitada

------------------------------- Parsers para os não-terminais -----------------------------

program :: ParsecT [Token] MemoryState IO [Token]
program = do
  modifyState setFlagTrue
  declBlock' <- declBlock

  modifyState setFlagFalse
  funcBlock' <- funcs
  modifyState setFlagTrue

  main <- mainToken
  colonM <- colonToken

  -- Verifica se existe algum stmt no programa principal
  endMainAhead <- lookAhead (endMainToken >> return True) <|> return False
  stmts' <-
    if endMainAhead
      then return []
      else stmts

  endMain <- endMainToken
  eof
  return (declBlock' ++ [main] ++ [colonM] ++ stmts' ++ [endMain])

----------------------------- Bloco de Declarações -----------------------------

declBlock :: ParsecT [Token] MemoryState IO [Token]
declBlock =
  try
    declBlockWithVars
    <|> declBlockWithoutVars

declBlockWithVars :: ParsecT [Token] MemoryState IO [Token]
declBlockWithVars = do
  decl <- declarationToken
  colonD <- colonToken
  decls <- decls
  endDecl <- endDeclarationToken
  return ([decl] ++ [colonD] ++ decls ++ [endDecl])

declBlockWithoutVars :: ParsecT [Token] MemoryState IO [Token]
declBlockWithoutVars = do
  decl <- declarationToken
  colonD <- colonToken
  endDecl <- endDeclarationToken
  return ([decl] ++ [colonD] ++ [endDecl])

decls :: ParsecT [Token] MemoryState IO [Token]
decls =
  do
    first <- declStmt
    next <- remainingDecls
    return (first ++ next)

remainingDecls :: ParsecT [Token] MemoryState IO [Token]
remainingDecls =
  ( do
      decls
  )
    <|> return []

declStmt :: ParsecT [Token] MemoryState IO ([Token])
declStmt =
  try
    varDeclStmt
    <|> funcDeclStmt

varDeclStmt :: ParsecT [Token] MemoryState IO ([Token])
varDeclStmt = do
  id <- idToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState
  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      updateState (symtableInsert (id, getDefaultValue varType))
      updatedState <- getState
      liftIO (putStrLn $ "Declaracao de variavel: " ++ show id ++ show updatedState)
    else
      liftIO (putStrLn "Flag is false, skipping variable declaration")
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

funcDeclStmt :: ParsecT [Token] MemoryState IO [Token]
funcDeclStmt = do
  id <- idToken
  colon <- colonToken
  parameters <- parametersTypeBlock
  to <- toToken
  returnType <- typeToken
  semiCol <- semiColonToken

  -- Gera uma lista de parametros default de Tokens ID
  let parameters' = parametersDefaultDecl parameters
  updateState (funcTableInsert id parameters' [])
  updatedState <- getState
  liftIO (putStrLn $ "Declaracao de função: " ++ show id ++ show updatedState)

  return ([id] ++ [colon] ++ parameters ++ [to] ++ [returnType] ++ [semiCol])

parametersTypeBlock :: ParsecT [Token] MemoryState IO [Token]
parametersTypeBlock =
  try
    nparameterType
    <|> return []

nparameterType :: ParsecT [Token] MemoryState IO [Token]
nparameterType = do
  parameter <- typeToken
  remainingParameters' <- remainingParametersType
  return (parameter : remainingParameters')

remainingParametersType :: ParsecT [Token] MemoryState IO [Token]
remainingParametersType =
  ( do
      comma <- commaToken
      parameters <- nparameterType
      return parameters
  )
    <|> return []

----------------------------- Blocos de Funções -----------------------------
funcs :: ParsecT [Token] MemoryState IO [Token]
funcs = do
  func <- funcBlock
  remainingFuncs' <- remainingFuncs
  return (func ++ remainingFuncs')

remainingFuncs :: ParsecT [Token] MemoryState IO [Token]
remainingFuncs =
  ( do
      funcs
  )
    <|> return []

funcBlock :: ParsecT [Token] MemoryState IO [Token]
funcBlock = do
  funcLiteral <- funcToken
  name <- idToken
  leftPar <- leftParenthesisToken
  parameters <- parametersIdsBlock
  rightPar <- rightParenthesisToken
  colon <- colonToken
  stmts <- manyTill anyToken (lookAhead endFuncToken)
  endFor <- endFuncToken
  updateState (funcTableUpdateParamStmts name parameters stmts)
  updatedState <- getState
  liftIO (putStrLn $ "Implementação de função: " ++ show name ++ show updatedState)

  return [funcLiteral]

parametersIdsBlock :: ParsecT [Token] MemoryState IO [Token]
parametersIdsBlock =
  try
    nparameterId
    <|> return []

nparameterId :: ParsecT [Token] MemoryState IO [Token]
nparameterId = do
  parameter <- idToken
  remainingParameters' <- remainingParametersId
  return (parameter : remainingParameters')

remainingParametersId :: ParsecT [Token] MemoryState IO [Token]
remainingParametersId =
  ( do
      comma <- commaToken
      parameters <- nparameterId
      return parameters
  )
    <|> return []

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
    <|> funcStmt
    <|> decls
    <|> printStmt

-- Parser para a instrução print
printStmt :: ParsecT [Token] MemoryState IO [Token]
printStmt = do
  printToken <- printToken
  lParenthesisLiteral <- leftParenthesisToken
  value <- printStringStmt <|> printExp2
  rParenthesisLiteral <- rightParenthesisToken
  semiCol <- semiColonToken
  liftIO $ printTypeValue value
  return ([printToken] ++ [lParenthesisLiteral] ++ [value] ++ [rParenthesisLiteral] ++ [semiCol])

printStringStmt :: ParsecT [Token] MemoryState IO Token
printStringStmt = do
  stringTok <- stringValToken
  return stringTok

printExp2 :: ParsecT [Token] MemoryState IO Token
printExp2 = do
  valueToken <- assignValExpression
  return valueToken

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
  expressionTokens <- manyTill anyToken (lookAhead colonToken) -- Armazena sintaticamente a condição
  colonLiteral <- colonToken
  stmtsBlock <- manyTill anyToken (lookAhead endWhileToken) -- Armazena sintaticamente o bloco de stmts
  endWhileLiteral <- endWhileToken
  semiCol <- semiColonToken

  -- Armazena a lista de tokens a serem consumidas após o for
  memoryState <- getState
  input <- getInput

  let loop = do
        memoryState <- getState
        -- Consome e avalia a condição
        setInput expressionTokens
        expressionValue <- ifParenthesisExpression
        let condition = evaluateCondition expressionValue
        if condition
          then do
            setInput stmtsBlock
            _ <- many stmts
            loop
          else setInput input

  loop

  return ([whileLiteral] ++ expressionTokens ++ [colonLiteral] ++ stmtsBlock ++ [endWhileLiteral] ++ [semiCol])

---- For
forStmt :: ParsecT [Token] MemoryState IO [Token]
forStmt = do
  forLiteral <- forToken
  leftParenthesis <- leftParenthesisToken
  assign' <- assign
  semiCol' <- semiColonToken
  expressionTokens <- manyTill anyToken (lookAhead semiColonToken) -- Armazena sintaticamente a condição
  semiCol'' <- semiColonToken
  updateAssign <- manyTill anyToken (lookAhead rightParenthesisToken) -- Armazena sintaticamente a atualização de valor da iteração
  rightParenthesis <- rightParenthesisToken
  colon' <- colonToken
  stmtsBlock <- manyTill anyToken (lookAhead endForToken) -- Armazena sintaticamente o bloco de stmts
  endFor <- endForToken
  semiCol <- semiColonToken

  -- Armazena a lista de tokens a serem consumidas após o for
  memoryState <- getState
  input <- getInput

  let loop = do
        memoryState <- getState
        -- Consome e avalia a condição
        setInput expressionTokens
        expressionValue <- relatOrLogicExpression
        let condition = evaluateCondition expressionValue
        if condition
          then do
            setInput stmtsBlock
            _ <- many stmts
            setInput updateAssign
            assign'' <- assign
            loop
          else setInput input

  loop

  return ([forLiteral] ++ [colon'] ++ stmtsBlock ++ [endFor] ++ [semiCol])

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
  value <- assignVal id
  state <- getState
  if not (compatible (getType id state) value)
    then fail "type mismatch"
    else do
      -- A atribuição só ocorre quando a flag estiver ativa
      if isFlagTrue state
        then do
          updateState (symtableUpdate (id, fromValuetoTypeValue value))
          newState <- getState
          liftIO (putStrLn $ "Atualizacao de estado sobre a variavel: " ++ show id ++ show newState)
          return (id : assignSym : [value])
        else
          return (id : assignSym : [value])

assignVal :: Token -> ParsecT [Token] MemoryState IO Token
assignVal idScan =
  try
    assignValExpression
    <|> valueLiteralExpression
    <|> scanfExpression idScan

-- Chamada de função
funcStmt :: ParsecT [Token] MemoryState IO [Token]
funcStmt = do
  id <- idToken
  leftpar <- leftParenthesisToken
  parameters' <- parametersExprBlock
  rightPar <- rightParenthesisToken
  state <- getState
  semiCol <- semiColonToken

  liftIO (putStrLn $ "FuncStmt: " ++ show parameters')

  let funcMemory = checkFunctionParameters id parameters' state
  
  liftIO (putStrLn $ "funcMemory: " ++ show funcMemory)
  
  return [leftpar]

parametersExprBlock :: ParsecT [Token] MemoryState IO [Token]
parametersExprBlock =
  try
    nparameterExpr
    <|> return []

nparameterExpr :: ParsecT [Token] MemoryState IO [Token]
nparameterExpr = do
  parameter <- assignValExpression
  remainingParameters' <- remainingParametersExpr
  return (parameter : remainingParameters')

remainingParametersExpr :: ParsecT [Token] MemoryState IO [Token]
remainingParametersExpr =
  ( do
      comma <- commaToken
      parameters <- nparameterExpr
      return parameters
  )
    <|> return []

----------------------------- Main -----------------------------

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