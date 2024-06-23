-- {-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Debug.Trace (trace)
import LiteralTokens
import MemoryState
import System.Environment
import System.IO.Unsafe ( unsafePerformIO )
import Text.Parsec
import Tokens
import Utils
import Expressions
import Control.Monad.State (modify)

-- import qualified Lexer as requisitada

------------------------------- Parsers para os não-terminais -----------------------------

program :: ParsecT [Token] MemoryState IO [Token]
program = do
  modifyState setFlagTrue
  declBlock' <- declBlock
  modifyState setFlagFalse
  -- Functions
  modifyState setFlagTrue
  main <- mainToken
  colonM <- colonToken
  stmts <- stmts
  endMain <- endMainToken
  eof
  return (declBlock' ++ [main] ++ [colonM] ++ stmts ++ [endMain])

----------------------------- Declarações -----------------------------

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

-- decls :: ParsecT [Token] MemoryState IO [Token]
-- decls =
--   do
--     first <- declStmt
--     next <- remainingDecls
--     return (first ++ next)


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
    -- typeDeclStmt

varDeclStmt :: ParsecT [Token] MemoryState IO ([Token])
varDeclStmt = do
  id <- idToken
  colon <- colonToken
  varType <- typeToken
  semiCol <- semiColonToken
  state <- getState
  -- A declaração só ocorre quando a flag estiver ativa
  if isFlagTrue state then do
    updateState (symtableInsert (id, getDefaultValue id varType)) -- primeiro argumento de getDefaultValue não é usado
    updatedState <- getState
    liftIO (putStrLn $ "Declaracao de variavel: " ++ show id ++ show updatedState)
  else
    liftIO (putStrLn "Flag is false, skipping variable declaration")
  return ([id] ++ [colon] ++ [varType] ++ [semiCol])

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
  -- liftIO (putStrLn $ "Tokens pulados depois do if:" ++ show first)
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
      if isFlagTrue state then do
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

scanfExpression :: Token -> ParsecT [Token] MemoryState IO Token
scanfExpression idScan = do
  scanTok <- scanToken
  lParenthesisLiteral <- leftParenthesisToken
  scanString <- stringValToken
  rParenthesisLiteral <- rightParenthesisToken
  liftIO $ print scanString
  scanValue <- readValue idScan
  return scanValue

readValue :: Token -> ParsecT [Token] MemoryState IO Token
readValue (Id idStr position) = do
  state <- getState
  let typeStr = getTypeStr (getType (Id idStr position) state)
  inputTerminal <- liftIO getLine
  case typeStr of
    "int" -> return $ IntValue (read inputTerminal) position
    "float" -> return $ FloatValue (read inputTerminal) position
    "bool" -> return $ BoolValue (read inputTerminal == "true") position
    "string" -> return $ StringValue inputTerminal position
    "char" ->
      if length inputTerminal == 1
        then return $ CharValue (head inputTerminal) position
        else fail "Input for char must be a single character"
    _ -> error "Unsupported type"

----------------------------- Main -----------------------------

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (False, [], [], [], [], False) "Error message" tokens

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