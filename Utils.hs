module Utils where
import Tokens
import Text.Parsec
import MemoryState
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad (when)
import Control.Monad.IO.Class

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok : _) = pos -- necessita melhoria
update_pos pos _ [] = pos

----------------------------- Funções de Tipo -----------------------------
{-
  getDefaultValue é utilizado na declaração de novas variáveis, para definir um valor básico para ela. 
  Recebe como parâmetros um Token ID, um Token IntValue e um Token Type
-}
getDefaultValue :: Token -> Token -> Token -> TypeValue
getDefaultValue _ _ (Type "int" (l, c)) = IntType 0 (l, c)
getDefaultValue _ _ (Type "char" (l, c)) = CharType '\0' (l, c) -- Using null character as default
getDefaultValue _ _ (Type "string" (l, c)) = StringType "" (l, c)
getDefaultValue _ _ (Type "float" (l, c)) = FloatType 0.0 (l, c)
getDefaultValue _ _ (Type "bool" (l, c)) = BoolType False (l, c)
getDefaultValue (Id name _) (IntValue val _) (Type "list" (l, c)) = ListType (name, val, []) (l, c)
getDefaultValue _  _ (Type _ (_, _)) = error "This type doesn't exist"

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
getType _ (_, [], _, _, _) = error "variable not found"
getType (Id idStr1 pos1) (_, (Id idStr2 _, value) : listTail, _, _, _) =
  if idStr1 == idStr2
    then fromTypeValuetoValue value
    else getType (Id idStr1 pos1) (False, listTail, [], [], [])

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