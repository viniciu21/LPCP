-- {-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Arrow (ArrowChoice (right))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.State (modify, MonadTrans (lift))
import Debug.Trace (trace)
import LiteralTokens
import MemoryState
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Tokens
import Utils

-- import qualified Lexer as requisitada

------------------------------- Parsers para os não-terminais -----------------------------

program :: ParsecT [Token] MemoryState IO [Token]
program = do
  updateState setFlagTrue
  declBlock' <- declBlock

  updateState setFlagFalse
  funcAhead <- lookAhead (funcToken >> return True) <|> return False
  funcBlock' <-
    if funcAhead
      then funcs
      else return []
  updateState setFlagTrue

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
  try
    decls
    <|> return []

declStmt :: ParsecT [Token] MemoryState IO ([Token])
declStmt =
  try
    varDeclStmt
    <|> try matrixDeclStmt
    <|> try listDeclStmt
    <|> try funcDeclStmt

-- typeDeclStmt

varDeclStmt :: ParsecT [Token] MemoryState IO ([Token])
varDeclStmt = do
  id <- idToken
  -- liftIO (putStrLn $ "varDeclStmt: " ++ show id)
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState
  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      if isFuncFlagTrue state
        then do
          updateState (insertLocalSymtable id varType)
          updatedState <- getState
          return ([id] ++ [colon] ++ [varType] ++ [semiCol])
        -- liftIO (putStrLn $ "Declaracao de variavel em função local: " ++ show id)
        -- liftIO $ printMemoryState updatedState
        else do
          updateState (symtableInsert (id, getDefaultValue varType))
          updatedState <- getState
          -- liftIO (putStrLn $ "Declaracao de variavel no programa principal: " ++ show id)
          return ([id] ++ [colon] ++ [varType] ++ [semiCol])
    else do
      liftIO (putStrLn "Flag is false, skipping variable declaration")
      return ([id] ++ [colon] ++ [varType] ++ [semiCol])
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

listDeclStmt :: ParsecT [Token] MemoryState IO ([Token])
listDeclStmt = do
  id@(Id name pos) <- idToken
  leftBrack <- leftBracketToken
  valList <- intValToken
  rightBrack <- rightBracketToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState

  let listType = getDefaultValueDataTypes valList (Type "list" pos) varType

  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      updateState (symtableInsert (id, listType)) -- primeiro argumento de getDefaultValue não é usado
      updatedState <- getState
      liftIO (putStrLn $ "Declaracao de variavel: " ++ show id ++ show updatedState)
    else
      liftIO (putStrLn "Flag is false, skipping variable declaration")
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

matrixDeclStmt :: ParsecT [Token] MemoryState IO ([Token])
matrixDeclStmt = do
  id@(Id name pos) <- idToken
  leftBrack1 <- leftBracketToken
  rows@(IntValue rowsVal (row, col)) <- intValToken
  rightBrack1 <- rightBracketToken
  leftBrack2 <- leftBracketToken
  cols@(IntValue colsVal _) <- intValToken
  rightBrack2 <- rightBracketToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState

  let defaultMatrix = getDefaultValueMatrix rows cols (Type "matrix" pos) varType

  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      updateState (symtableInsert (id, defaultMatrix)) -- primeiro argumento de getDefaultValue não é usado
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
  -- liftIO (putStrLn $ "Declaracao de função: " ++ show id ++ show updatedState)

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
  -- liftIO (putStrLn $ "Implementação de função salvo na memória: " ++ show name)
  -- liftIO $ printMemoryState updatedState

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

-- Struct
-- structDeclStmt :: Parsec [Token] MemoryState IO ([Token])
-- structDeclStmt = do
--   typedef <- typedefToken
--   struct  <- structToken
--   leftCurlyBrackets <- leftCurlyBracketsToken
--   decls <-
--   id <- idToken

----------------------------- Code -----------------------------

stmts :: ParsecT [Token] MemoryState IO [Token]
stmts = do
  first <- stmt
  next <- remainingStmts
  return (first ++ next)

remainingStmts :: ParsecT [Token] MemoryState IO [Token]
remainingStmts =
  try
    stmts
    <|> return []

stmt :: ParsecT [Token] MemoryState IO [Token]
stmt =
  try
    assignStmt
    <|> returnStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> try (lookAhead decls *> decls)
    <|> funcStmt
    <|> listStmt
    <|> printStmt

listStmt :: ParsecT [Token] MemoryState IO ([Token])
listStmt = do
  id@(Id name pos) <- idToken
  leftBrack <- leftBracketToken
  valList <- exponential
  rightBrack <- rightBracketToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState

  let listType = getDefaultValueDataTypes valList (Type "list" pos) varType

  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      updateState (symtableInsert (id, listType)) -- primeiro argumento de getDefaultValue não é usado
      updatedState <- getState
      liftIO (putStrLn $ "Declaracao de variavel: " ++ show id ++ show updatedState)
    else
      liftIO (putStrLn "Flag is false, skipping variable declaration")
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

returnStmt :: ParsecT [Token] MemoryState IO [Token]
returnStmt = do
  returnLit <- returnToken
  -- liftIO(putStrLn $ "Entrei no Return: " ++ show returnLit)
  expr <- assignValExpression
  -- liftIO(putStrLn $ "O Return: " ++ show returnLit ++ " Avaliou: " ++ show expr)
  semiCol <- semiColonToken
  updateState setFuncFlagFalse
  updateState setFlagFalse
  return ([returnLit] ++ [expr] ++ [semiCol])

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
  -- liftIO (putStrLn $ "Entramos no ifStmt:" ++ show ifLiteral)
  expression <- ifParenthesisExpression
  -- liftIO (putStrLn $ "Avaliamos a expressão no ifStmt:" ++ show expression)
  colonLiteral <- colonToken
  state <- getState
  -- Verifica expressão do if
  if isFlagTrue state
    then do
      if isFuncFlagTrue state
        then do
          let nLocalVar = getLocalSymtableLength state
          let result = evaluateCondition expression
          if result
            then do
              -- Entrou no IF
              stmtsBlock <- stmts
              skip' <- manyTill anyToken (lookAhead endifToken)
              -- liftIO (putStrLn $ "Tokens pulados depois do if:" ++ show skip')
              endIfLiteral <- endifToken
              semiCol1 <- semiColonToken
              updateState (removeLocalSymtableUntilLenght nLocalVar)
              return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ stmtsBlock)
            else do
              skip' <- manyTill anyToken (lookAhead elifToken <|> lookAhead elseToken <|> lookAhead endifToken)
              -- liftIO (putStrLn $ "Tokens pulados antes de elif <|> else <|> endIf:" ++ show skip')
              elifStmt' <- elifStmt
              -- liftIO (putStrLn $ "elifStmt':" ++ show elifStmt')
              if null elifStmt'
                then do
                  -- Entrou no else
                  skip' <- manyTill anyToken (lookAhead elseToken <|> lookAhead endifToken)
                  -- liftIO (putStrLn $ "Tokens pulados antes de else:" ++ show skip')
                  elseStmt' <- elseStmt
                  endIfLiteral <- endifToken
                  semiCol <- semiColonToken
                  updateState (removeLocalSymtableUntilLenght nLocalVar)
                  return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elseStmt')
                else do
                  -- Entrou no elif
                  skip' <- manyTill anyToken (lookAhead endifToken <|> lookAhead endifToken)
                  -- liftIO (putStrLn $ "Tokens pulados após elif:" ++ show skip')
                  endIfLiteral <- endifToken
                  semiCol <- semiColonToken
                  updateState (removeLocalSymtableUntilLenght nLocalVar)
                  return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elifStmt')
        else do
          let nVar = getSymtableLenght state
          let result = evaluateCondition expression
          if result
            then do
              -- Entrou no IF
              stmtsBlock <- stmts
              skip' <- manyTill anyToken (lookAhead endifToken)
              -- liftIO (putStrLn $ "Tokens pulados depois do if:" ++ show skip')
              endIfLiteral <- endifToken
              semiCol1 <- semiColonToken
              updateState (symtableRemoveUntilLenght nVar)
              return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ stmtsBlock)
            else do
              skip' <- manyTill anyToken (lookAhead elifToken <|> lookAhead elseToken <|> lookAhead endifToken)
              -- liftIO (putStrLn $ "Tokens pulados antes de elif <|> else:" ++ show skip')
              elifStmt' <- elifStmt
              if null elifStmt'
                then do
                  -- Entrou no else
                  skip' <- manyTill anyToken (lookAhead elseToken <|> lookAhead endifToken)
                  -- liftIO (putStrLn $ "Tokens pulados antes de else:" ++ show skip')
                  elseStmt' <- elseStmt
                  endIfLiteral <- endifToken
                  semiCol <- semiColonToken
                  updateState (symtableRemoveUntilLenght nVar)
                  return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elseStmt')
                else do
                  -- Entrou no elif
                  skip' <- manyTill anyToken (lookAhead endifToken <|> lookAhead endifToken)
                  -- liftIO (putStrLn $ "Tokens pulados após elif:" ++ show skip')
                  endIfLiteral <- endifToken
                  semiCol <- semiColonToken
                  updateState (symtableRemoveUntilLenght nVar)
                  return ([ifLiteral] ++ [expression] ++ [colonLiteral] ++ elifStmt')
    else
      return []

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
          -- liftIO (putStrLn $ "Tokens pulados antes de elif seguido:" ++ show skip')
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
  stmtsBlock <- nestedWhileTokens 0 -- Armazena sintaticamente o bloco de stmts
  endWhileLiteral <- endWhileToken
  semiCol <- semiColonToken

  -- Armazena a lista de tokens a serem consumidas após o for
  memoryState <- getState
  input <- getInput

  let loop = do
        memoryState <- getState
        if isFlagTrue memoryState
          then do
            if isFuncFlagTrue memoryState
              then do
                let nLocalVar = getLocalSymtableLength memoryState
                -- Consome e avalia a condição
                setInput expressionTokens
                expressionValue <- ifParenthesisExpression
                let condition = evaluateCondition expressionValue
                if condition
                  then do
                    setInput stmtsBlock
                    _ <- many stmts
                    -- Remove as novas variáveis do bloco
                    updateState (removeLocalSymtableUntilLenght nLocalVar)
                    loop
                  else setInput input
              else do
                let nVar = getSymtableLenght memoryState
                -- Consome e avalia a condição
                setInput expressionTokens
                expressionValue <- ifParenthesisExpression
                let condition = evaluateCondition expressionValue
                if condition
                  then do
                    setInput stmtsBlock
                    _ <- many stmts
                    -- Remove as novas variáveis do bloco
                    updateState (symtableRemoveUntilLenght nVar)
                    loop
                  else setInput input
          else setInput input

  loop

  return ([whileLiteral] ++ expressionTokens ++ [colonLiteral] ++ stmtsBlock ++ [endWhileLiteral] ++ [semiCol])

nestedWhileTokens :: Int -> ParsecT [Token] MemoryState IO [Token]
nestedWhileTokens nestDepth = do
  tokens <- manyTill anyToken (lookAhead endWhileToken <|> lookAhead whileToken)
  -- liftIO (putStrLn $ "nestedTokens until:" ++ show tokens)
  next <- lookAhead anyToken
  -- liftIO (putStrLn $ "nestedTokens next:" ++ show next)
  case next of
    While _ -> do
      whileToken' <- whileToken
      nestedFor <- nestedWhileTokens (nestDepth + 1)
      -- liftIO (putStrLn $ "nestedTokens recursed:" ++ show nestedFor)
      -- endFor <- endForToken
      -- liftIO (putStrLn $ "nestedTokens endFor:" ++ show endFor)
      tokens' <- manyTill anyToken (lookAhead endWhileToken)
      -- liftIO (putStrLn $ "nestedTokens tokens':" ++ show tokens')
      return (tokens ++ [whileToken'] ++ nestedFor ++ tokens')
    _ -> do
      if nestDepth == 0
        then do
          return (tokens)
        else do
          endWhile' <- endWhileToken
          return (tokens ++ [endWhile'])

---- For
forStmt :: ParsecT [Token] MemoryState IO [Token]
forStmt = do
  forLiteral <- forToken
  leftParenthesis <- leftParenthesisToken
  assign' <- assignToken
  semiCol' <- semiColonToken
  expressionTokens <- manyTill anyToken (lookAhead semiColonToken) -- Armazena sintaticamente a condição
  semiCol'' <- semiColonToken
  updateAssign <- manyTill anyToken (lookAhead rightParenthesisToken) -- Armazena sintaticamente a atualização de valor da iteração
  rightParenthesis <- rightParenthesisToken
  colon' <- colonToken
  stmtsBlock <- nestedForTokens 0
  -- liftIO (putStrLn $ "forStmt Stmts Block" ++ show stmtsBlock)
  endFor <- endForToken
  semiCol <- semiColonToken

  -- Armazena a lista de tokens a serem consumidas após o for
  memoryState <- getState
  input <- getInput

  let loop = do
        memoryState <- getState
        if isFlagTrue memoryState
          then do
            if isFuncFlagTrue memoryState
              then do
                let nLocalVar = getLocalSymtableLength memoryState
                setInput expressionTokens
                expressionValue <- relatOrLogicExpression
                let condition = evaluateCondition expressionValue
                if condition
                  then do
                    -- liftIO (putStrLn $ "forStmt Stmts Block" ++ show stmtsBlock)
                    setInput stmtsBlock
                    _ <- many stmts
                    setInput updateAssign
                    assign'' <- assignVar
                    updateState (removeLocalSymtableUntilLenght nLocalVar)
                    loop
                  else setInput input
              else do
                let nVar = getSymtableLenght memoryState
                -- Consome e avalia a condição
                setInput expressionTokens
                expressionValue <- relatOrLogicExpression
                let condition = evaluateCondition expressionValue
                if condition
                  then do
                    -- liftIO (putStrLn $ "forStmt Stmts Block" ++ show stmtsBlock)
                    setInput stmtsBlock
                    _ <- many stmts
                    setInput updateAssign
                    assign'' <- assignVar
                    updateState (symtableRemoveUntilLenght nVar)
                    loop
                  else setInput input
          else setInput input

  loop

  return ([forLiteral] ++ [colon'] ++ stmtsBlock ++ [endFor] ++ [semiCol])

nestedForTokens :: Int -> ParsecT [Token] MemoryState IO [Token]
nestedForTokens nestDepth = do
  tokens <- manyTill anyToken (lookAhead endForToken <|> lookAhead forToken)
  -- liftIO (putStrLn $ "nestedTokens until:" ++ show tokens)
  next <- lookAhead anyToken
  -- liftIO (putStrLn $ "nestedTokens next:" ++ show next)
  case next of
    For _ -> do
      forToken' <- forToken
      nestedFor <- nestedForTokens (nestDepth + 1)
      -- liftIO (putStrLn $ "nestedTokens recursed:" ++ show nestedFor)
      -- endFor <- endForToken
      -- liftIO (putStrLn $ "nestedTokens endFor:" ++ show endFor)
      tokens' <- manyTill anyToken (lookAhead endForToken)
      -- liftIO (putStrLn $ "nestedTokens tokens':" ++ show tokens')
      return (tokens ++ [forToken'] ++ nestedFor ++ tokens')
    _ -> do
      if nestDepth == 0
        then do
          return (tokens)
        else do
          endFor' <- endForToken
          return (tokens ++ [endFor'])

---- Assign
assignStmt :: ParsecT [Token] MemoryState IO [Token]
assignStmt = do
  assignTok <- try assignVar  <|> try assignList <|> try assignMatrix
  semiCol <- semiColonToken
  return (assignTok ++ [semiCol])

assignVar :: ParsecT [Token] MemoryState IO [Token]
assignVar = do
  id@(Id nameVar (l, c)) <- idToken
  assignSym <- assignToken
  value <- assignVal id
  -- liftIO (putStrLn $ "Assign: " ++ show id ++ show assignSym ++ show value)
  state <- getState
  -- liftIO $ printMemoryState state
  -- A atribuição só ocorre quando a flag estiver ativa
  if isFlagTrue state
    then do
      -- Atribuição para escopo local da função no topo da pilha
      if isFuncFlagTrue state
        then do
          if not (compatible (getLocalType id state) value)
            then error ("type mismatch trying to assign to variable " ++ show nameVar ++ "in line " ++ show l)
            else do
              updateState (updateLocalSymtable id (fromValuetoTypeValue value))
              -- liftIO (putStrLn $ "Atualizacao de estado sobre a variavel local: " ++ show id)
              newState <- getState
              -- liftIO $ printMemoryState newState
              return (id : assignSym : [value])
        else do
          if not (compatible (getType id state) value)
            then error ("type mismatch trying to assign to variable " ++ show nameVar ++ "in line " ++ show l)
            else do
              updateState (symtableUpdate (id, fromValuetoTypeValue value))
              newState <- getState
              -- liftIO (putStrLn $ "Atualizacao de estado sobre a variavel: " ++ show id)
              -- liftIO $ printMemoryState newState
              return (id : assignSym : [value])
    else
      return []

assignList :: ParsecT [Token] MemoryState IO [Token]
assignList = do
  id <- idToken
  leftBracl <- leftBracketToken
  valList@(Id acval pos) <- exponential
  rightBrack <- rightBracketToken
  assignSym <- assignToken
  value <- assignVal id
  state <- getState

  return (id : assignSym : [value])

assignMatrixToOtherMatrix :: ParsecT [Token] MemoryState IO [Token]
assignMatrixToOtherMatrix = do
  id <- idToken
  leftBrack1 <- leftBracketToken
  rows <- exponential
  rightBrack1 <- rightBracketToken
  leftBrack2 <- leftBracketToken
  cols <- exponential
  rightBrack2 <- rightBracketToken
  assignSym <- assignToken
  idAssign <- idToken
  leftBrack3 <- leftBracketToken
  rows1 <- exponential
  rightBrack3 <- rightBracketToken
  leftBrack4 <- leftBracketToken
  cols1 <- exponential
  rightBrack4 <- rightBracketToken
  state <- getState


  updateState (assignMatrixValueTypeValue id rows cols idToken rows1 cols1 state)  

  newstate <- getState

  liftIO $ printMemoryState newstate

  return (id : assignSym : [])


assignMatrix :: ParsecT [Token] MemoryState IO [Token]
assignMatrix = do
  id <- idToken
  leftBrack1 <- leftBracketToken
  rows <- exponential
  rightBrack1 <- rightBracketToken
  leftBrack2 <- leftBracketToken
  cols <- exponential
  rightBrack2 <- rightBracketToken
  assignSym <- assignToken
  value <- assignVal id
  state <- getState

  updateState (assignMatrixValue id rows cols value)

  newstate <- getState

  liftIO $ printMemoryState newstate

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
  -- liftIO (putStrLn $ "parameters': " ++ show parameters')
  rightPar <- rightParenthesisToken
  semiCol <- semiColonToken
  state <- getState
  input <- getInput

  -- Verifica se existe função com id, com o mesmo número e tipo de parametros que parameters'
  let parametersValues = parametersValuesFromIDs parameters' state
  let funcMemoryInstance@(idFunc, funcMemory, funcStmts) = checkFunctionParameters id parametersValues state

  updateState (callStackPush funcMemoryInstance)
  state' <- getState
  -- liftIO (putStrLn $ "State after callStackPush: ")
  -- liftIO $ printMemoryState state'

  -- Executa a função
  updateState setFuncFlagTrue
  setInput funcStmts
  stmts' <- stmts
  -- liftIO (putStrLn $ "Stmts': " ++ show stmts')
  setInput input

  -- Pega a instância da função que acabou de ser finalizada
  newState <- getState
  -- liftIO (putStrLn $ "State before callStackPop: ")
  -- liftIO $ printMemoryState newState
  let topStack = callStackGet newState
  -- liftIO (putStrLn $ "Saved instance: " ++ show topStack)

  -- Remove a função do topo da pilha de ativação
  updateState (const (callStackPop newState))
  removedState <- getState

  -- liftIO (putStrLn $ "State after callStackPop: ")
  -- liftIO $ printMemoryState removedState

  -- liftIO (putStrLn $ "Parametros que receberão os valores: " ++ show parameters')
  updateState (passResultValue parameters' topStack)
  updatedState <- getState
  -- liftIO (putStrLn $ "State after passResultValue: ")
  -- liftIO $ printMemoryState updatedState

  -- Update the flags before removing the callStack to check if it is empty
  updateState setFuncFlagFalse
  updateState setFlagTrue
  -- liftIO (putStrLn $ "State after poping stack: ")
  -- liftIO $ printMemoryState removedState
  -- liftIO (putStrLn $ "FuncStmt Stmts': " ++ show stmts')
  -- liftIO (putStrLn $ "Second-to-last Stmts': " ++ show (stmts' !! (length stmts' - 2)))
  return stmts'

funcExpr :: ParsecT [Token] MemoryState IO Token
funcExpr = do
  id <- idToken
  -- liftIO (putStrLn $ "Entrei no funcExpr com id: " ++ show id)
  leftpar <- leftParenthesisToken
  parameters' <- parametersExprBlock
  rightPar <- rightParenthesisToken
  state <- getState
  input <- getInput

  -- Verifica se existe função com id, com o mesmo número e tipo de parametros que parameters'
  let parametersValues = parametersValuesFromIDs parameters' state
  let funcMemoryInstance@(idFunc, funcMemory, funcStmts) = checkFunctionParameters id parametersValues state

  updateState (callStackPush funcMemoryInstance)
  state' <- getState
  -- liftIO (putStrLn $ "State after update for id: " ++ show id)
  -- liftIO $ printMemoryState state'

  -- Executa a função
  updateState setFuncFlagTrue
  setInput funcStmts
  stmts' <- stmts
  -- liftIO (putStrLn $ "Stmts': " ++ show stmts')
  setInput input

  -- Pega a instância da função que acabou de ser finalizada
  -- liftIO (putStrLn $ "Antes de passResultValue")
  newState <- getState
  -- liftIO (putStrLn $ "State before callStackPop: ")
  -- liftIO $ printMemoryState newState
  let topStack = callStackGet newState

  -- Remove a função do topo da pilha de ativação
  updateState (const (callStackPop newState))
  removedState <- getState

  -- liftIO (putStrLn $ "State after callStackPop: ")
  -- liftIO $ printMemoryState removedState

  -- liftIO (putStrLn $ "Parametros que receberão os valores: " ++ show parameters')
  updateState (passResultValue parameters' topStack)
  updatedState <- getState
  -- liftIO (putStrLn $ "State after update: ")
  -- liftIO $ printMemoryState updatedState

  -- Update the flags before removing the callStack to check if it is empty
  updateState setFuncFlagFalse
  updateState setFlagTrue

  -- liftIO (putStrLn $ "FuncExpr Stmts': " ++ show stmts')
  -- liftIO (putStrLn $ "Second-to-last Stmts': " ++ show (stmts' !! (length stmts' - 2)))
  return (stmts' !! (length stmts' - 2))

----------------------------- Expressões -----------------------------

assignValExpression :: ParsecT [Token] MemoryState IO Token
assignValExpression =
  do
    -- liftIO (putStrLn $ "Entrei em assignValExpression")
    try
      relationalExpression
      <|> arithmeticExpression
      -- <|> try (lookAhead funcExpr *> funcExpr)
      <|> logicalExpression

-- <|> parenthesisExpression
-- <|> idTokenExpression

-- Olhar essa merda de aritmetica
arithmeticExpression :: ParsecT [Token] MemoryState IO Token
arithmeticExpression =
  do
    -- liftIO (putStrLn $ "Entrei em arithmeticExpression")
    try plusMinusExpression
    <|> term

-- + | -
plusMinusExpression :: ParsecT [Token] MemoryState IO Token
plusMinusExpression = do
  term' <- term
  -- liftIO (putStrLn $ "Entrei no PlusMinus: " ++ show term')
  result <- arithmeticExpressionRemaining term'
  return result

arithmeticExpressionRemaining :: Token -> ParsecT [Token] MemoryState IO Token
arithmeticExpressionRemaining termIn =
  do
    arithmeticOp <- binaryArithmeticOperatorLiteral
    -- liftIO (putStrLn $ "PlusMinus com termo: " ++ show termIn ++ " com simbolo: " ++ show arithmeticOp)
    term' <- term
    -- liftIO (putStrLn $ "PlusMinus com segundo termo: " ++ show term')
    result <- arithmeticExpressionRemaining (binaryEval termIn arithmeticOp term')
    return result
    <|> return termIn

-- < | <= | == | > | >= | !=
relationalExpression :: ParsecT [Token] MemoryState IO Token
relationalExpression = do
  -- liftIO (putStrLn $ "Entrei em relationalExpression")
  arithmeticExpressionRight <- arithOrParentExpression
  relationalOp <- binaryRelationalOperatorLiteral
  arithmeticExpressionLeft <- arithOrParentExpression

  let result = binaryEval arithmeticExpressionRight relationalOp arithmeticExpressionLeft
  -- liftIO (putStrLn $ "Avaliando o relationalExpression: " ++ show arithmeticExpressionRight ++ show relationalOp ++ show arithmeticExpressionLeft ++ " = " ++ show result)
  return result

arithOrParentExpression :: ParsecT [Token] MemoryState IO Token
arithOrParentExpression =
  try
    arithmeticExpression
    <|> parenthesisExpression

logicalExpression :: ParsecT [Token] MemoryState IO Token
logicalExpression =
  do
    -- liftIO (putStrLn $ "Entrei em logicalExpression")
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

  -- liftIO (putStrLn $ "Avaliando o binaryLogicalExpression: " ++ show relExpLeft ++ show logicalLiteral ++ show relExpRight)

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
  -- liftIO (putStrLn $ "Entrei no parenthesisExpression:" ++ show leftPar)
  expression <- assignValExpression
  -- liftIO (putStrLn $ "Verifiquei a expressão no parenthesisExpression:" ++ show expression)
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
  -- liftIO $ liftIO (putStrLn $ "Entrei no idToken: " ++ show idToken')
  -- liftIO $ liftIO (putStrLn $ "State no idToken: ")
  -- state <- getState
  -- liftIO $ printMemoryState state
  -- liftIO $ print $ show idToken'
  symtable <- getState

  let valor =  getLocalSymtable idToken' symtable
  --liftIO $ print valor

  if isFuncFlagTrue symtable
    then case getLocalSymtable idToken' symtable of
      Just idVal -> 
        return (fromTypeValuetoValue idVal)
      Nothing -> fail "Variable not found in local symtable"
    else case symtableGet idToken' symtable of
      Just val -> 
        return (fromTypeValuetoValue val)
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
    -- liftIO (putStrLn $ "termRemaining: " ++ show factorIn ++ show termOp ++ show factor')
    result <- termRemaining (binaryEval factorIn termOp factor')
    return result
    <|> return factorIn

termOperatorLiteral :: ParsecT [Token] MemoryState IO Token
termOperatorLiteral =
  do
    timesToken
    <|> dividerToken
    <|> integerDividerToken
    <|> modToken

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
  do
    -- liftIO (putStrLn $ "Estou em exponential")
    try
      valueLiteralExpression
      <|> try (lookAhead funcExpr *> funcExpr)
      <|> idTokenExpression

scanfExpression :: Token -> ParsecT [Token] MemoryState IO Token
scanfExpression idScan = do
  scanTok <- scanToken
  lParenthesisLiteral <- leftParenthesisToken
  scanString <- stringValToken
  rParenthesisLiteral <- rightParenthesisToken
  liftIO $ printTypeValue scanString
  scanValue <- readValue idScan
  return scanValue

parametersExprBlock :: ParsecT [Token] MemoryState IO [Token]
parametersExprBlock =
  try
    nparameterExpr
    <|> return []

nparameterExpr :: ParsecT [Token] MemoryState IO [Token]
nparameterExpr = do
  (nextToken, nextNextToken) <- lookAheadTwoTokens
  input <- getInput

  -- liftIO (putStrLn $ "nexToken e nextnext: " ++ show nextToken ++ show nextNextToken ++ " input: " ++ show input)
  parameter <- case (nextToken, nextNextToken) of
    (Id _ _, Comma _) -> idToken
    (Id _ _, RightParenthesis _) -> idToken
    _ -> assignValExpression

  -- liftIO (putStrLn $ "parameter: " ++ show parameter)
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

----------------------------- Main -----------------------------

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (False, [], [], [], [], False, False) "Error message" tokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      case unsafePerformIO (parser (getTokens fn)) of
        Left err -> print err
        Right ans -> do
          putStr ""
    -- print ans
    _ -> putStrLn "Please inform the input filename. Closing application..."