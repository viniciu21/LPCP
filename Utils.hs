{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use foldr" #-}
module Utils where

import Control.Monad (when)
import Control.Monad.IO.Class
import MemoryState
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Tokens

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

----------------------------- Funções de Tipo -----------------------------

getDefaultValueList :: Token -> Token -> TypeValue
getDefaultValueList (IntValue val _) (Type "list" (l, c)) = ListType (val, []) (l, c)

fromValuetoTypeValue :: Token -> TypeValue
fromValuetoTypeValue (IntValue value pos) = IntType value pos
fromValuetoTypeValue (FloatValue value pos) = FloatType value pos
fromValuetoTypeValue (CharValue value pos) = CharType value pos
fromValuetoTypeValue (StringValue value pos) = StringType value pos
fromValuetoTypeValue (BoolValue value pos) = BoolType value pos

fromTypeValuetoValue :: TypeValue -> Token
fromTypeValuetoValue (IntType value pos) = IntValue value pos
fromTypeValuetoValue (FloatType value pos) = FloatValue value pos
fromTypeValuetoValue (CharType value pos) = CharValue value pos
fromTypeValuetoValue (StringType value pos) = StringValue value pos
fromTypeValuetoValue (BoolType value pos) = BoolValue value pos

{-
  Realiza a operação binária requisitada. Recebendo 3 parâmetros:
  param1: Token de "TypeValue"
  param2: Token de Operação
  param3: Token de "TypeValue"
  Retorna um Token TypeValue
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
getType _ (_, [], _, _, _, _, _) = error "variable not found"
getType (Id idStr1 pos1) (_, (Id idStr2 _, value) : listTail, _, _, _, _, _) =
  if idStr1 == idStr2
    then fromTypeValuetoValue value
    else getType (Id idStr1 pos1) (False, listTail, [], [], [], False, False)

{-
  Função para pegar o tipo de uma variável local do escopo da função que esteja no topo da pilha de ativação
-}
getLocalType :: Token -> MemoryState -> Token
getLocalType varId@(Id name1 pos1) memoryState =
  let (_, locals, _) = callStackGet memoryState
  in findVarType locals
  where
    findVarType :: [(Token, TypeValue)] -> Token
    findVarType [] = error "variable not found"
    findVarType ((localVarId@(Id name2 pos2), localVarType):rest)
      | name2 == name1 = fromTypeValuetoValue localVarType
      | otherwise = findVarType rest

getTypeStr :: Token -> String
getTypeStr (IntValue _ _) = "int"
getTypeStr (FloatValue _ _) = "float"
getTypeStr (BoolValue _ _) = "bool"
getTypeStr (StringValue _ _) = "string"
getTypeStr (CharValue _ _) = "char"
getTypeStr _ = error "deu ruim"

printTypeValue :: Token -> IO String
printTypeValue (IntValue b _) = do
  liftIO $ putStrLn $ show b
  return $ show b
printTypeValue (StringValue b _) = do
  putStrLn b
  return b
printTypeValue (FloatValue b _) = do
  liftIO $ putStrLn $ show b
  return $ show b
printTypeValue (BoolValue b _) = do
  liftIO $ putStrLn $ show b
  return $ show b
printTypeValue (CharValue b _) = do
  putStrLn [b] -- Print the character directly
  return [b] -- Return the character as a String
printTypeValue _ = error "nao funfou"

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

matchTypeValues :: TypeValue -> TypeValue -> Bool
matchTypeValues (IntType _ _) (IntType _ _) = True
matchTypeValues (FloatType _ _) (FloatType _ _) = True
matchTypeValues (StringType _ _) (StringType _ _) = True
matchTypeValues (CharType _ _) (CharType _ _) = True
matchTypeValues (BoolType _ _) (BoolType _ _) = True
matchTypeValues _ _ = False

--------------------------- Funções ---------------------------

{-
  parametersDefaultDecl é chamado quando ocorre uma declaração de função na seção de declarações. Como os paramêtros ainda não possuem nome e valor, é atribuido nome "default" e defaultValue.
-}
parametersDefaultDecl :: [Token] -> [(Token, TypeValue)]
parametersDefaultDecl [] = []
parametersDefaultDecl (parameter : parametersTail) =
  [(Id "default" (0, 0), getDefaultValue parameter)] ++ parametersDefaultDecl parametersTail

parametersValuesFromIDs :: [Token] -> MemoryState -> [Token]
parametersValuesFromIDs [] _ = []
parametersValuesFromIDs (parameter : parametersTail) symtable =
  case symtableGet parameter symtable of
        Just val -> fromTypeValuetoValue val : parametersValuesFromIDs parametersTail symtable
        Nothing -> fail "Variable not found"

{-
  checkFunctionParameters é chamada para verificar se uma função com o ID fornecido existe no MemoryState e se o número de parâmetros fornecidos corresponde ao número de parâmetros definidos na função.

  Parâmetros:
  - funcId: Token que representa o ID da função a ser verificada.
  - params: Lista de Tokens que representam os parâmetros fornecidos para a função.
  - memoryState: O estado atual da memória (MemoryState), que inclui informações sobre funções, variáveis, etc.

  Retorno:
  - Retorna a tupla (Token, [(Token, TypeValue)], [Token]) se a função for encontrada e o número de parâmetros fornecidos corresponder ao número de parâmetros definidos.
  - Lança um erro "function not found" se a função não for encontrada.
  - Lança um erro "parameter count mismatch" se o número de parâmetros fornecidos não corresponder ao número de parâmetros definidos.
-}
checkFunctionParameters :: Token -> [Token] -> MemoryState -> (Token, [(Token, TypeValue)], [Token])
checkFunctionParameters funcId params (_, _, funcs, _, _, _, _) =
  case findFunction funcId funcs of
    Nothing -> error "function not found"
    Just func@(fid, definedParams, stmts) ->
      if length params == length definedParams
        then if allParameterTypesMatch params definedParams
          then (fid, passParametersValues (map fromValuetoTypeValue params) definedParams, stmts)
          else error "parameter type mismatch"
        else error "parameter count mismatch"

allParameterTypesMatch :: [Token] -> [(Token, TypeValue)] -> Bool
allParameterTypesMatch [] [] = True
allParameterTypesMatch (param:paramsTail) ((_, expectedType):definedParamsTail) =
  matchTypeValues (fromValuetoTypeValue param) expectedType && allParameterTypesMatch paramsTail definedParamsTail
allParameterTypesMatch _ _ = False

passParametersValues :: [TypeValue] -> [(Token, TypeValue)] -> [(Token, TypeValue)]
passParametersValues newValues oldParams = zip (map fst oldParams) newValues

passResultValue :: [Token] -> (Token, [(Token, TypeValue)], [Token]) -> MemoryState -> MemoryState
passResultValue [] _ state = state
passResultValue (param:paramsTail) (funcName, (realParamId, realParamVal):realParamsTail, stmts) state =
  let updatedValue = getLocalSymtable realParamId state
      updatedState = symtableUpdate (param, updatedValue) state
  in passResultValue paramsTail (funcName, realParamsTail, stmts) updatedState

{-
  findFunction é uma função auxiliar usada para procurar uma função específica na lista de funções dentro do MemoryState.

  Parâmetros:
  - funcId: Token que representa o ID da função a ser procurada.
  - funcs: Lista de tuplas, onde cada tupla contém:
      - Um Token representando o ID da função.
      - Uma lista de tuplas (Token, TypeValue) representando os parâmetros da função.
      - Uma lista de Tokens representando as declarações da função.

  Retorno:
  - Retorna `Just (fid, params, stmts)` se a função com o ID fornecido for encontrada.
  - Retorna `Nothing` se a função com o ID fornecido não for encontrada.
-}
findFunction :: Token -> [(Token, [(Token, TypeValue)], [Token])] -> Maybe (Token, [(Token, TypeValue)], [Token])
findFunction _ [] = Nothing
findFunction funcId@(Id name1 pos1) ((fid@(Id name2 pos2), params, stmts):funcsTail)
  | name1 == name2 = Just (fid, params, stmts)
  | otherwise = findFunction funcId funcsTail

------------------------------------------------------

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

printMemoryState :: MemoryState -> IO ()
printMemoryState (flag, symtable, funcs, structs, callstack, structflag, funcFlag) =
  liftIO (putStrLn $ "Flag: " ++ show flag ++
  "\nSymtable: "++ show symtable ++
  "\nFuncs: "++ show funcs ++
  "\nStructs: "++ show structs ++
  "\nCallstack: "++ show callstack ++
  "\nStructflag: "++ show structflag ++
  "\nFuncFlag: "++ show funcFlag)