module Main (main) where

import Data.Binary.Get (remaining)
import Text.Parsec
import Tokens

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

colonToken = tokenPrim show update_pos get_token -- :
  where
    get_token Colon = Just Colon
    get_token _ = Nothing

semiColonToken = tokenPrim show update_pos get_token -- ;
  where
    get_token SemiColon = Just SemiColon
    get_token _ = Nothing

assignToken = tokenPrim show update_pos get_token -- =
  where
    get_token Assign = Just Assign
    get_token _ = Nothing

idToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Id x) = Just (Id x)
    get_token _ = Nothing

typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x) = Just (Type x)
    get_token _ = Nothing

intValToken = tokenPrim show update_pos get_token
  where
    get_token (IntValue x) = Just (IntValue x)
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
      semiCol <- semiColonToken
      stmt <- stmt
      return ([semiCol] ++ stmt)
  )
    <|> return []

stmt :: Parsec [Token] st [Token]
stmt =
  do
    assign
    <|> return []

assign :: Parsec [Token] st [Token]
assign = do
  id <- idToken
  assignSym <- assignToken
  intVal <- intValToken
  return (id : assignSym : [intVal])

-- invocação do parser para o símbolo de partida

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "exemplo_uma_atribuicao.txt") of
  Left err -> print err
  Right ans -> print ans