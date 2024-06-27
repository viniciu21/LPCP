{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use foldr" #-}
module Utils where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Data.List (intercalate)
import MemoryState
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Tokens

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

----------------------------- Funções de Tipo -----------------------------
{-
  getDefaultValue é utilizado na declaração de novas variáveis, para definir um valor básico para ela.
  Recebe como parâmetros um Token ID, um Token IntValue e um Token Type
-}
-- getDefaultValue :: Token -> TypeValue
-- getDefaultValue (Type "int" (l, c)) = IntType 0 (l, c)
-- getDefaultValue (Type "char" (l, c)) = CharType '\0' (l, c) -- Using null character as default
-- getDefaultValue (Type "string" (l, c)) = StringType "" (l, c)
-- getDefaultValue (Type "float" (l, c)) = FloatType 0.0 (l, c)
-- getDefaultValue (Type "bool" (l, c)) = BoolType False (l, c)
-- getDefaultValue (Type _ (_, _)) = error "This type doesn't exist"

getDefaultValueDataTypes :: Token -> Token -> Token -> TypeValue
getDefaultValueDataTypes (IntValue val _) (Type "list" (l, c)) varType = ListType (val, returnDefaultList val [] varType) (l, c)
getDefaultValueDataTypes _ _ _ = error "Unexpected type for number of elements"

getDefaultValueMatrix :: Token -> Token -> Token -> Token -> TypeValue
getDefaultValueMatrix (IntValue row _) (IntValue col _) (Type "matrix" (l, c)) varType = MatrixType (row, col, returnDefaultMatrix row col [[]] varType) (l, c)
getDefaultValueMatrix _ _ _ _ = error "Unexpected type for number of elements"

returnDefaultList :: Int -> [TypeValue] -> Token -> [TypeValue]
returnDefaultList 0 voidList _ = voidList
returnDefaultList val voidList varType =
  let defaultValue = getDefaultValue varType
   in returnDefaultList (val - 1) (defaultValue : voidList) varType

returnDefaultMatrix :: Int -> Int -> [[TypeValue]] -> Token -> [[TypeValue]]
returnDefaultMatrix 0 _ _ _ = []
returnDefaultMatrix rows cols voidList varType =
  let defaultRow = returnDefaultList cols [] varType
      remainingRows = returnDefaultMatrix (rows - 1) cols voidList varType
   in defaultRow : remainingRows

getDefaultValueList :: Token -> Token -> TypeValue
getDefaultValueList (IntValue val _) (Type "list" (l, c)) = ListType (val, []) (l, c)

fromValuetoTypeValue :: Token -> TypeValue
fromValuetoTypeValue (IntValue value pos) = IntType value pos
fromValuetoTypeValue (FloatValue value pos) = FloatType value pos
fromValuetoTypeValue (CharValue value pos) = CharType value pos
fromValuetoTypeValue (StringValue value pos) = StringType value pos
fromValuetoTypeValue (BoolValue value pos) = BoolType value pos
fromValuetoTypeValue (MatrixValue (rows, cols, tokens) pos) =
  MatrixType (rows, cols, map (map fromValuetoTypeValue) tokens) pos

fromTypeValuetoValue :: TypeValue -> Token
fromTypeValuetoValue (IntType value pos) = IntValue value pos
fromTypeValuetoValue (FloatType value pos) = FloatValue value pos
fromTypeValuetoValue (CharType value pos) = CharValue value pos
fromTypeValuetoValue (StringType value pos) = StringValue value pos
fromTypeValuetoValue (BoolType value pos) = BoolValue value pos
fromTypeValuetoValue (MatrixType (rows, cols, values) pos) =
  MatrixValue (rows, cols, map (map fromTypeValuetoValue) values) pos

-- (Int, Int, [[TypeValue]]) (Int, Int)

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
-- Mod
binaryEval (IntValue x p) (Mod _) (IntValue y _) = IntValue (x `mod` y) p
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
binaryEval (IntValue x p) (Equal _) (IntValue y _) = BoolValue (x == y) p
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

-- binaryEvalTypes :: TypeValue -> Token -> TypeValue -> TypeValue
-- binaryEvalTypes (MatrixType (l1, c1, values1) pos1) (Plus _) (MatrixType (l2, c2, values2) pos2)
--   | not (compatible (fromTypeValuetoValue (head (head values1))) (fromTypeValuetoValue (head (head values2)))) =
--       error "Incompatible matrix element types"
--   | l1 /= l2 || c1 /= c2 = error "Matrix dimensions must be the same"
--   | otherwise =
--       let resultValues = zipWith (zipWith addTypeValues) values1 values2
--       in MatrixType (l1, c1, resultValues) pos1
--   where
--     addTypeValues :: TypeValue -> TypeValue -> TypeValue
--     addTypeValues (IntType v1 pos1) (IntType v2 _) = IntType (v1 + v2) pos1
--     addTypeValues (FloatType v1 pos1) (FloatType v2 _) = FloatType (v1 + v2) pos1
--     -- Add more type cases as needed
--     addTypeValues _ _ = error "Unsupported matrix element types for addition"


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
    findVarType ((localVarId@(Id name2 pos2), localVarType) : rest)
      | name2 == name1 = fromTypeValuetoValue localVarType
      | otherwise = findVarType rest

getElementType :: TypeValue -> TypeValue
getElementType (ListType (n, elements) pos) = head elements

getTypeStr :: Token -> String
getTypeStr (IntValue _ _) = "int"
getTypeStr (FloatValue _ _) = "float"
getTypeStr (BoolValue _ _) = "bool"
getTypeStr (StringValue _ _) = "string"
getTypeStr (CharValue _ _) = "char"
getTypeStr _ = error "deu ruim"

printTypeValue :: Token -> IO String
printTypeValue (IntValue b _) = do
  liftIO $ print b
  return $ show b
printTypeValue (StringValue b _) = do
  putStrLn b
  return b
printTypeValue (FloatValue b _) = do
  liftIO $ print b
  return $ show b
printTypeValue (BoolValue b _) = do
  liftIO $ print b
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
parametersDefaultDecl parametersTail =
  foldr
    ( \parameter ->
        (++) [(Id "default" (0, 0), getDefaultValue parameter)]
    )
    []
    parametersTail

parametersValuesFromIDs :: [Token] -> MemoryState -> [Token]
parametersValuesFromIDs [] _ = []
parametersValuesFromIDs (parameter@(Id _ _) : parametersTail) symtable =
  case getValue parameter symtable of
    Just val -> fromTypeValuetoValue val : parametersValuesFromIDs parametersTail symtable
    Nothing -> error "Variable not found"
parametersValuesFromIDs (literal : parametersTail) symtable =
  literal : parametersValuesFromIDs parametersTail symtable

getValue :: Token -> MemoryState -> Maybe TypeValue
getValue varId memoryState@(flag, locals, _, _, _, _, _) =
  if isFuncFlagTrue memoryState
    then getLocalSymtable varId memoryState
    else symtableGet varId memoryState

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
        then
          if allParameterTypesMatch params definedParams
            then (fid, passParametersValues (map fromValuetoTypeValue params) definedParams, stmts)
            else error "parameter type mismatch"
        else error "parameter count mismatch"

allParameterTypesMatch :: [Token] -> [(Token, TypeValue)] -> Bool
allParameterTypesMatch [] [] = True
allParameterTypesMatch (param : paramsTail) ((_, expectedType) : definedParamsTail) =
  matchTypeValues (fromValuetoTypeValue param) expectedType && allParameterTypesMatch paramsTail definedParamsTail
allParameterTypesMatch _ _ = False

passParametersValues :: [TypeValue] -> [(Token, TypeValue)] -> [(Token, TypeValue)]
passParametersValues newValues oldParams = zip (map fst oldParams) newValues

passResultValue :: [Token] -> (Token, [(Token, TypeValue)], [Token]) -> MemoryState -> MemoryState
passResultValue [] _ state = state
passResultValue (param@(Id _ _) : paramsTail) (funcName, (realParamId, realParamVal) : realParamsTail, stmts) state =
  let updatedState =
        if isCallStackEmpty state
          then symtableUpdate (param, realParamVal) state
          else updateLocalSymtable param realParamVal state
   in passResultValue paramsTail (funcName, realParamsTail, stmts) updatedState
passResultValue (_ : paramsTail) (funcName, _ : realParamsTail, stmts) state =
  passResultValue paramsTail (funcName, realParamsTail, stmts) state
passResultValue _ _ state = state

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
findFunction funcId@(Id name1 pos1) ((fid@(Id name2 pos2), params, stmts) : funcsTail)
  | name1 == name2 = Just (fid, params, stmts)
  | otherwise = findFunction funcId funcsTail

------------------------------------------------------

readValue :: Token -> ParsecT [Token] MemoryState IO Token
readValue (Id idStr position) = do
  -- liftIO (putStrLn $ "Estou na função readValue com o Id: " ++ show idStr)
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
printMemoryState (flag, symtable, funcs, structs, callstack, structflag, funcFlag) = do
  let formattedCallStack = formatCallStack callstack
  liftIO $
    putStrLn $
      "Flag: "
        ++ show flag
        ++ "\nSymtable: "
        ++ show symtable
        ++ "\nFuncs: "
        ++ show funcs
        ++ "\nStructs: "
        ++ show structs
        ++ "\nCallstack: "
        ++ formattedCallStack
        ++ "\nStructflag: "
        ++ show structflag
        ++ "\nFuncFlag: "
        ++ show funcFlag

formatCallStack :: [(Token, [(Token, TypeValue)], [Token])] -> String
formatCallStack cs = "[\n" ++ intercalate "\n" (map showCallStackItem cs) ++ "\n]"

showCallStackItem :: (Token, [(Token, TypeValue)], [Token]) -> String
showCallStackItem (token, locals, stmts) =
  "(\n  Function: " ++ show token ++ ",\n  Locals: " ++ show locals ++ ",\n  Statements: " ++ show stmts ++ "\n)"

lookAheadTwoTokens :: ParsecT [Token] MemoryState IO (Token, Token)
lookAheadTwoTokens = try $ do
  lookAhead
    ( do
        t1 <- anyToken
        t2 <- anyToken
        return (t1, t2)
    )

updateMatrix :: [[TypeValue]] -> Int -> Int -> TypeValue -> [[TypeValue]]
updateMatrix matrix row col newVal =
  take row matrix
    ++ [take col (matrix !! row) ++ [newVal] ++ drop (col + 1) (matrix !! row)]
    ++ drop (row + 1) matrix

{-
  Params: MatrixElements, Row, Col
  Return: Element at that position
-}
getMatrixElement :: TypeValue -> Token -> Token -> TypeValue
getMatrixElement matrix@(MatrixType (l, c, elements) pos) (IntValue row _) (IntValue col _) = (elements !! row) !! col

assignMatrixValue :: Token -> Token -> Token -> Token -> MemoryState -> MemoryState
assignMatrixValue id (IntValue row _) (IntValue col _) newValue state =
  case symtableGet id state of
    Nothing -> error $ "Variable does not exist: " ++ show id
    Just (MatrixType (linha, coluna, matrix) pos) ->
      if row > linha - 1 || col > coluna - 1
        then
          error $ "Out of bounds " ++ show id
        else
          let updatedMatrix = updateMatrix matrix row col (fromValuetoTypeValue newValue)
              updatedState = symtableUpdate (id, MatrixType (linha, coluna, updatedMatrix) pos) state
           in updatedState
    Just _ -> error $ "Variable " ++ show id ++ " is not a matrix"

handleTypeValue :: TypeValue -> ParsecT [Token] MemoryState IO Token
handleTypeValue val =
  case val of
    MatrixType typeStruct pos -> return (fromTypeValuetoValue (MatrixType typeStruct pos))
    _ -> return (fromTypeValuetoValue val)

checkTypeValue :: TypeValue -> Bool
checkTypeValue (MatrixType _ _) = True
checkTypeValue _ = False

-- handleMatrixType :: TypeValue -> ParsecT [Token] MemoryState IO Token
-- handleMatrixType val = do
--   -- Suponha que `val` seja do tipo MatrixType
--   let MatrixType (_, _, matrixValues) pos = val

--   -- Iterar sobre os valores da matriz e realizar a atribuição elemento por elemento
--   forM_ (zip [(row, col) | row <- [0 ..], col <- [0 ..]] (concat matrixValues)) $ \((row, col), elemValue) -> do
--     -- Recuperar o valor atual em mat1[row][col]
--     let currentValue = matrixValues !! row !! col

--     -- Realizar a atribuição somente se o valor for diferente para evitar redundâncias
--     when (currentValue /= elemValue) $ do
--       -- Atualizar mat2[row][col] com elemValue
--       let updatedMatrix = updateMatrixElement matrixValues (row, col) elemValue

--       -- Aqui você deve atualizar a matriz mat2 na sua estrutura de estado (MemoryState)
--       -- Suponha que `updatedMatrix` é a nova versão de mat2
--       -- Você deve implementar a lógica adequada para atualizar a matriz na sua estrutura de estado

--       -- Exemplo hipotético de atualização na estrutura de estado:
--       -- updateState $ \st -> st { mat2 = updatedMatrix }

--       -- Retornar o token correspondente à atribuição em mat2[row][col]
--       return (fromTypeValuetoValue elemValue)

--   -- Neste exemplo hipotético, retornamos um token para indicar sucesso na atribuição
--   -- Seu código real deve retornar o token apropriado para representar a atribuição na gramática da sua linguagem
--   return (AssignmentSuccessToken pos)

updateMatrixElement :: [[TypeValue]] -> (Int, Int) -> TypeValue -> [[TypeValue]]
updateMatrixElement matrix (row, col) value =
  let (left, r : rs) = splitAt row matrix
      (left', _ : cs) = splitAt col r
   in left ++ [left' ++ [value] ++ cs] ++ rs