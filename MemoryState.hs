module MemoryState where
import Tokens
import Debug.Trace (trace)


----------------------------- Memória de execução -----------------------------

type MemoryState = (
                    Bool, -- Flag
                    [(Token, TypeValue)], -- Symtable
                    [(Token, [(Token, TypeValue)], [Token])], -- Funcs
                    [[(Token, TypeValue)]], -- Structs
                    [(Token, [(Token, TypeValue)], [Token])], -- Callstack
                    Bool, -- StructFlag
                    Bool -- FuncFlag
                    )

----------------------------- Flag -----------------------------
{-
  Uma variável booliana que será acionada para fazer mudanças semânticas durante a análise sobre blocos de códigos, como subprogramas. Quando estiver falsa, o bloco de código será analisado apenas sintaticamente e, se necessário, guardado na memória.
-}
-- Function to set the flag to True
setFlagTrue :: MemoryState -> MemoryState
setFlagTrue (flag, vars, funcs, structs, callstack, structflag, funcFlag) = (True, vars, funcs, structs, callstack, structflag, funcFlag)

-- Function to set the flag to False
setFlagFalse :: MemoryState -> MemoryState
setFlagFalse (flag, vars, funcs, structs, callstack, structflag, funcFlag) = (False, vars, funcs, structs, callstack, structflag, funcFlag)

isFlagTrue :: MemoryState -> Bool
isFlagTrue (flag, _, _, _, _, _, _) = flag


----------------------------- Tabela de símbolos -----------------------------
{-
  A tabela de simbolos é uma lista de tuplas, onde cada tupla possui dois Tokens, um identificando a variavel e outro identificando seu valor:
          symtable = [(IdToken1, val1), (IdToken12, val2), ... , (IdToken1n, valn)]
-}
{-
  symtableGet recebe um Token ID (Id String (l,c)) referente a uma variável, e verifica se ela existe na tabela de símbolos e, caso exista, retorna seu valor.
-}
symtableGet :: Token -> MemoryState -> Maybe TypeValue
symtableGet _ (_, [], _, _, _, _, _) = fail "variable not found"
symtableGet (Id idStr1 pos1) (_, (Id idStr2 pos2, value2) : listTail, _, _, _, _, _) =
  if idStr1 == idStr2
    then Just value2
    else symtableGet (Id idStr1 pos1) (False, listTail, [], [], [], False, False)
symtableGet _ _ = Nothing

{-
  SymtableInsert recebe uma tupla (Token ID, Token TypeValue) e armazena na tabela de simbolos
-}
symtableInsert :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableInsert newSymbol (flag, symtable, funcs, structs, callstack, structflag, funcFlag) = (flag, symtable ++ [newSymbol], funcs, structs, callstack, structflag, funcFlag)

{-
  symtableUpdate recebe uma tupla (Token ID, Token Value) e atualiza o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableUpdate :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableUpdate _ (_, [], _, _, _, _, _) = error "variable not found"
symtableUpdate (Id idStr1 pos1, value1) (flag, (Id idStr2 pos2, value2) : listTail, funcs, structs, callstack, structflag, funcFlag)
  | idStr1 == idStr2 = (flag, (Id idStr1 pos2, value1) : listTail, funcs, structs, callstack, structflag, funcFlag)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack', structflag', funcFlag') = symtableUpdate (Id idStr1 pos1, value1) (flag, listTail, funcs, structs, callstack, structflag, funcFlag)
       in (flag', (Id idStr2 pos2, value2) : updatedSymtable, funcs', structs', callstack', structflag', funcFlag')

{-
  symtableRemove recebe uma tupla (Token ID, Token Value) e remove  ID e o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableRemove :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableRemove _ (_, [], _, _, _, _, _) = error "variable not found"
symtableRemove (id1, v1) (flag, (id2, v2) : listTail, funcs, structs, callstack, structflag, funcFlag)
  | id1 == id2 = (flag, listTail, funcs, structs, callstack, structflag, funcFlag)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack', structflag', funcFlag') = symtableRemove (id1, v1) (flag, listTail, funcs, structs, callstack, structflag, funcFlag)
       in (flag', (id2, v2) : updatedSymtable, funcs', structs', callstack', structflag', funcFlag')

----------------------------- Tabela de Funções -----------------------------

{-
  funcTableInsert recebe um Token ID, uma lista de paramêtros ID TypeValue e uma lista de Stmts e insere na memória
-}
funcTableInsert :: Token -> [(Token, TypeValue)] -> [Token] -> MemoryState -> MemoryState
funcTableInsert name parameters stmts (flag, symtable, funcs, structs, callstack, structflag, funcFlag) = (flag, symtable, funcs ++ [(name, parameters, stmts)], structs, callstack, structflag, funcFlag)

{-
  funcTableUpdateParamStmts é uma função utilizada nas implementações das funções para atualizar o nome das variáveis default, e pra popular a lista de stmts vazia da declaração da função.
-}
funcTableUpdateParamStmts :: Token -> [Token] -> [Token] -> MemoryState -> MemoryState
funcTableUpdateParamStmts _ _ _ (_, _, [], _, _, _, _) = error "function not found"
funcTableUpdateParamStmts (Id name pos1) parameters newStmts (flag, symtable, (Id name2 pos2, param2, stmts2) : funcsTail, structs, callstack, structflag, funcFlag)
  | name == name2 = (flag, symtable, (Id name2 pos2, updateParametersNames parameters param2, newStmts) : funcsTail, structs, callstack, structflag, funcFlag)
  | otherwise =
      let (flag', symtable', funcsTail', structs', callstack', structflag', funcFlag') = funcTableUpdateParamStmts (Id name pos1) parameters newStmts (flag, symtable, funcsTail, structs, callstack, structflag, funcFlag)
       in (flag', symtable', (Id name2 pos2, param2, stmts2) : funcsTail', structs', callstack', structflag', funcFlag')

updateParametersNames :: [Token] -> [(Token, TypeValue)] -> [(Token, TypeValue)]
updateParametersNames newNames oldParams = zip newNames (map snd oldParams)


----------------------------- Pilha de ativação -----------------------------

callStackGet :: MemoryState -> (Token, [(Token, TypeValue)], [Token])
callStackGet (_, _, _, _, [], _, _) = error "call stack is empty"
callStackGet (_, _, _, _, callstack, _, _) = last callstack

callStackPush :: (Token, [(Token, TypeValue)], [Token]) -> MemoryState -> MemoryState
callStackPush newCallstackFunc (flag, symtable, funcs, structs, callstack, structflag, funcFlag) = (flag, symtable, funcs, structs, callstack ++ [newCallstackFunc], structflag, funcFlag)

callStackPop :: MemoryState -> MemoryState
callStackPop (_, _, _, _, [], _, _) = error "call stack is empty"
callStackPop (flag, symtable, funcs, structs, callstack, structflag, funcFlag) =
  (flag, symtable, funcs, structs, init callstack, structflag, funcFlag)

callStackUpdateTop :: (Token, [(Token, TypeValue)], [Token]) -> MemoryState -> MemoryState
callStackUpdateTop newFunc (flag, symTable, funcs, structs, callStack, structFlag, funcFlag) =
  if null callStack
    then error "call stack is empty"
    else (flag, symTable, funcs, structs, init callStack ++ [newFunc], structFlag, funcFlag)

isCallStackEmpty :: MemoryState -> Bool
isCallStackEmpty (flag, symTable, funcs, structs, callStack, structFlag, funcFlag) =
  null callStack

{-
  Recebe um Token ID e um Token Type e insere nas variáveis locais da função que está no topo da pilha de ativação.
-}
insertLocalSymtable :: Token -> Token -> MemoryState -> MemoryState
insertLocalSymtable newId newVarType memoryState =
  let (funcId, locals, stmts) = callStackGet memoryState
      newVar = (newId, getDefaultValue newVarType)
      updatedLocals = locals ++ [newVar]
      updatedFunc = (funcId, updatedLocals, stmts)
  in callStackUpdateTop updatedFunc memoryState

{-
  Recebe um Token ID e um TypeValue que serão atualizados no escopo local da função do topo da pilha de ativação.
-}
updateLocalSymtable :: Token -> TypeValue -> MemoryState -> MemoryState
updateLocalSymtable varId@(Id name1 pos1) newValue memoryState =
  let (funcId, locals, stmts) = callStackGet memoryState
      updatedLocals = updateVar locals
      updatedFunc = (funcId, updatedLocals, stmts)
  in callStackUpdateTop updatedFunc memoryState
  where
    updateVar :: [(Token, TypeValue)] -> [(Token, TypeValue)]
    updateVar [] = error "Variable not found"
    updateVar ((oldVarId@(Id name2 pos2), oldValue):rest)
      | name2 == name1 = (oldVarId, newValue) : rest
      | otherwise = (oldVarId, oldValue) : updateVar rest

{-
  Recebe um Token ID e verifica se ele existe no escopo local da função do topo da pilha de ativação. Se existir, retorna o seu TypeValue
-}
getLocalSymtable :: Token -> MemoryState -> TypeValue
getLocalSymtable varId@(Id name1 _) memoryState =
  let (_, locals, _) = callStackGet memoryState
  in findVarType locals
  where
    findVarType :: [(Token, TypeValue)] -> TypeValue
    findVarType [] = error "variable not found"
    findVarType ((localVarId@(Id name2 _), localVarType):rest)
      | name1 == name2 = localVarType
      | otherwise = findVarType rest



----------------------------- Structs -----------------------------
-- insertStruct :: Token -> [Token] -> MemoryState -> MemoryState
-- insertStruct name decls 

----------------------------- StructFlag -----------------------------
{-
  Uma variável booliana que será acionada para fazer o parseramento de declarações de struct, para que seja armazenado as variáveis dentro da struct, e não na memória principal.
-}
-- Function to set the flag to True
setStructFlagTrue :: MemoryState -> MemoryState
setStructFlagTrue (flag, vars, funcs, structs, callstack, structflag, funcFlag) = (True, vars, funcs, structs, callstack, structflag, funcFlag)

-- Function to set the flag to False
setStructFlagFalse :: MemoryState -> MemoryState
setStructFlagFalse (flag, vars, funcs, structs, callstack, structflag, funcFlag) = (False, vars, funcs, structs, callstack, structflag, funcFlag)

isStructFlagTrue :: MemoryState -> Bool
isStructFlagTrue (_, _, _, _, _, structflag, _) = structflag

----------------------------- StructFlag -----------------------------
{-
  Uma variável booliana que será acionada para fazer o parseramento de declarações de struct, para que seja armazenado as variáveis dentro da struct, e não na memória principal.
-}
-- Function to set the flag to True
setFuncFlagTrue :: MemoryState -> MemoryState
setFuncFlagTrue (flag, vars, funcs, structs, callstack, structflag, funcFlag) = (flag, vars, funcs, structs, callstack, structflag, True)

-- Function to set the flag to False
setFuncFlagFalse :: MemoryState -> MemoryState
setFuncFlagFalse (flag, vars, funcs, structs, callStack, structFlag, funcFlag) =
  let newFuncFlag = (not (null callStack) && funcFlag)
  in (flag, vars, funcs, structs, callStack, structFlag, newFuncFlag)


isFuncFlagTrue :: MemoryState -> Bool
isFuncFlagTrue (_, _, _, _, _, _, funcFlag) = funcFlag


----------------------------- Utils -----------------------------
{-
  getDefaultValue é utilizado na declaração de novas variáveis, para definir um valor básico para ela.
  Recebe como parâmetros um Token IntValue e um Token Type
-}
getDefaultValue :: Token -> TypeValue
getDefaultValue (Type "int" (l, c)) = IntType 0 (l, c)
getDefaultValue (Type "char" (l, c)) = CharType '\0' (l, c) -- Using null character as default
getDefaultValue (Type "string" (l, c)) = StringType "" (l, c)
getDefaultValue (Type "float" (l, c)) = FloatType 0.0 (l, c)
getDefaultValue (Type "bool" (l, c)) = BoolType False (l, c)
getDefaultValue (Type _ (_, _)) = error "This type doesn't exist"