module MemoryState where
import Tokens

----------------------------- Memória de execução -----------------------------

type MemoryState = (Bool, [(Token, TypeValue)], [(Token, [(Token, Token)], [Token])], [[(Token, (Token, Token))]], [[(Token, [(Token, Token)], [Token])]])

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

isFlagTrue :: MemoryState -> Bool
isFlagTrue (flag, _, _, _, _) = flag

----------------------------- Tabela de símbolos -----------------------------
{-
  A tabela de simbolos é uma lista de tuplas, onde cada tupla possui dois Tokens, um identificando a variavel e outro identificando seu valor:
          symtable = [(IdToken1, val1), (IdToken12, val2), ... , (IdToken1n, valn)]
-}
{-
  symtableGet recebe um Token ID (Id String (l,c)) referente a uma variável, e verifica se ela existe na tabela de símbolos e, caso exista, retorna seu valor.
-}
symtableGet :: (Token) -> MemoryState -> Maybe TypeValue
symtableGet _ (_, [], _, _, _) = fail "variable not found"
symtableGet (Id idStr1 pos1) (_, (Id idStr2 pos2, value2) : listTail, _, _, _) =
  if idStr1 == idStr2
    then Just value2
    else symtableGet (Id idStr1 pos1) (False, listTail, [], [], [])
symtableGet _ _ = Nothing

{-
  SymtableInsert recebe uma tupla (Token ID, Token TypeValue) e armazena na tabela de simbolos
-}
symtableInsert :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableInsert newSymbol (flag, symtable, funcs, structs, callstack) = (flag, symtable ++ [newSymbol], funcs, structs, callstack)

{-
  symtableUpdate recebe uma tupla (Token ID, Token Value) e atualiza o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableUpdate :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableUpdate _ (_, [], _, _, _) = error "variable not found"
symtableUpdate (Id idStr1 pos1, value1) (flag, (Id idStr2 pos2, value2) : listTail, funcs, structs, callstack)
  | idStr1 == idStr2 = (flag, (Id idStr1 pos2, value1) : listTail, funcs, structs, callstack)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack') = symtableUpdate (Id idStr1 pos1, value1) (flag, listTail, funcs, structs, callstack)
       in (flag', (Id idStr2 pos2, value2) : updatedSymtable, funcs', structs', callstack')

{-
  symtableRemove recebe uma tupla (Token ID, Token Value) e remove  ID e o valor na tabela de símbolos, se o Token ID já estiver na tabela
-}
symtableRemove :: (Token, TypeValue) -> MemoryState -> MemoryState
symtableRemove _ (_, [], _, _, _) = error "variable not found"
symtableRemove (id1, v1) (flag, (id2, v2) : listTail, funcs, structs, callstack)
  | id1 == id2 = (flag, listTail, funcs, structs, callstack)
  | otherwise =
      let (flag', updatedSymtable, funcs', structs', callstack') = symtableRemove (id1, v1) (flag, listTail, funcs, structs, callstack)
       in (flag', (id2, v2) : updatedSymtable, funcs', structs', callstack')