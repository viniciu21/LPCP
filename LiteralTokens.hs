module LiteralTokens where

import MemoryState
import Text.Parsec ( ParsecT, tokenPrim )
import Tokens
import Utils

----------------------------- Blocos -----------------------------
declarationToken :: ParsecT [Token] MemoryState IO Token
declarationToken = tokenPrim show update_pos get_token -- declaration
  where
    get_token (Declaration position) = Just (Declaration position)
    get_token _ = Nothing

endDeclarationToken :: ParsecT [Token] MemoryState IO Token
endDeclarationToken = tokenPrim show update_pos get_token -- end_declaration
  where
    get_token (EndDeclaration position) = Just (EndDeclaration position)
    get_token _ = Nothing

mainToken :: ParsecT [Token] MemoryState IO Token
mainToken = tokenPrim show update_pos get_token -- main
  where
    get_token (Main position) = Just (Main position)
    get_token _ = Nothing

endMainToken :: ParsecT [Token] MemoryState IO Token
endMainToken = tokenPrim show update_pos get_token -- end_main
  where
    get_token (EndMain position) = Just (EndMain position)
    get_token _ = Nothing

funcToken :: ParsecT [Token] MemoryState IO Token
funcToken = tokenPrim show update_pos get_token -- end_main
  where
    get_token (Func position) = Just (Func position)
    get_token _ = Nothing

endFuncToken :: ParsecT [Token] MemoryState IO Token
endFuncToken = tokenPrim show update_pos get_token -- end_main
  where
    get_token (EndFunc position) = Just (EndFunc position)
    get_token _ = Nothing    

ifToken :: ParsecT [Token] MemoryState IO Token
ifToken = tokenPrim show update_pos get_token -- if
  where
    get_token (If position) = Just (If position)
    get_token _ = Nothing

endifToken :: ParsecT [Token] MemoryState IO Token
endifToken = tokenPrim show update_pos get_token -- endIf
  where
    get_token (EndIf position) = Just (EndIf position)
    get_token _ = Nothing

elifToken :: ParsecT [Token] MemoryState IO Token
elifToken = tokenPrim show update_pos get_token -- elif
  where
    get_token (Elif position) = Just (Elif position)
    get_token _ = Nothing

elseToken :: ParsecT [Token] MemoryState IO Token
elseToken = tokenPrim show update_pos get_token -- else
  where
    get_token (Else position) = Just (Else position)
    get_token _ = Nothing

whileToken :: ParsecT [Token] MemoryState IO Token
whileToken = tokenPrim show update_pos get_token -- if
  where
    get_token (While position) = Just (While position)
    get_token _ = Nothing

endWhileToken :: ParsecT [Token] MemoryState IO Token
endWhileToken = tokenPrim show update_pos get_token -- if
  where
    get_token (EndWhile position) = Just (EndWhile position)
    get_token _ = Nothing

forToken :: ParsecT [Token] MemoryState IO Token
forToken = tokenPrim show update_pos get_token -- if
  where
    get_token (For position) = Just (For position)
    get_token _ = Nothing

endForToken :: ParsecT [Token] MemoryState IO Token
endForToken = tokenPrim show update_pos get_token -- if
  where
    get_token (EndFor position) = Just (EndFor position)
    get_token _ = Nothing

returnToken :: ParsecT [Token] MemoryState IO Token
returnToken = tokenPrim show update_pos get_token -- if
  where
    get_token (Return position) = Just (Return position)
    get_token _ = Nothing

structToken :: ParsecT [Token] MemoryState IO Token
structToken= tokenPrim show update_pos get_token -- if
  where
    get_token (Struct position) = Just (Struct position)
    get_token _ = Nothing

typedefToken :: ParsecT [Token] MemoryState IO Token
typedefToken= tokenPrim show update_pos get_token -- if
  where
    get_token (Typedef position) = Just (Typedef position)
    get_token _ = Nothing

----------------------------- Simbolos -----------------------------

-- :Colon
colonToken :: ParsecT [Token] MemoryState IO Token
colonToken = tokenPrim show update_pos get_token
  where
    get_token (Colon position) = Just (Colon position)
    get_token _ = Nothing

-- ; SemiColon
semiColonToken :: ParsecT [Token] MemoryState IO Token
semiColonToken = tokenPrim show update_pos get_token
  where
    get_token (SemiColon position) = Just (SemiColon position)
    get_token _ = Nothing

-- , Comma
commaToken :: ParsecT [Token] MemoryState IO Token
commaToken = tokenPrim show update_pos get_token
  where
    get_token (Comma position) = Just (Comma position)
    get_token _ = Nothing

-- ( LeftParenthesis
leftParenthesisToken :: ParsecT [Token] MemoryState IO Token
leftParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (LeftParenthesis position) = Just (LeftParenthesis position)
    get_token _ = Nothing

-- ) RightParenthesis
rightParenthesisToken :: ParsecT [Token] MemoryState IO Token
rightParenthesisToken = tokenPrim show update_pos get_token
  where
    get_token (RightParenthesis position) = Just (RightParenthesis position)
    get_token _ = Nothing

-- { LeftCurlyBrackets
leftCurlyBracketsToken :: ParsecT [Token] MemoryState IO Token
leftCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (LeftCurlyBrackets position) = Just (LeftCurlyBrackets position)
    get_token _ = Nothing

-- } RightCurlyBrackets
rightCurlyBracketsToken :: ParsecT [Token] MemoryState IO Token
rightCurlyBracketsToken = tokenPrim show update_pos get_token
  where
    get_token (RightCurlyBrackets position) = Just (RightCurlyBrackets position)
    get_token _ = Nothing

-- -> To
toToken :: ParsecT [Token] MemoryState IO Token
toToken = tokenPrim show update_pos get_token
  where
    get_token (To position) = Just (To position)
    get_token _ = Nothing

-- = Assign
assignToken :: ParsecT [Token] MemoryState IO Token
assignToken = tokenPrim show update_pos get_token
  where
    get_token (Assign position) = Just (Assign position)
    get_token _ = Nothing

-- + Plus
plusToken :: ParsecT [Token] MemoryState IO Token
plusToken = tokenPrim show update_pos get_token
  where
    get_token (Plus position) = Just (Plus position)
    get_token _ = Nothing

-- - Minus
minusToken :: ParsecT [Token] MemoryState IO Token
minusToken = tokenPrim show update_pos get_token
  where
    get_token (Minus position) = Just (Minus position)
    get_token _ = Nothing

-- * Times
timesToken :: ParsecT [Token] MemoryState IO Token
timesToken = tokenPrim show update_pos get_token
  where
    get_token (Times position) = Just (Times position)
    get_token _ = Nothing

-- / Divider
dividerToken :: ParsecT [Token] MemoryState IO Token
dividerToken = tokenPrim show update_pos get_token
  where
    get_token (Divider position) = Just (Divider position)
    get_token _ = Nothing

-- // IntegerDivider
integerDividerToken :: ParsecT [Token] MemoryState IO Token
integerDividerToken = tokenPrim show update_pos get_token
  where
    get_token (IntegerDivider position) = Just (IntegerDivider position)
    get_token _ = Nothing

-- ** Exponent
exponentToken :: ParsecT [Token] MemoryState IO Token
exponentToken = tokenPrim show update_pos get_token
  where
    get_token (Exponent position) = Just (Exponent position)
    get_token _ = Nothing

-- && And
andToken :: ParsecT [Token] MemoryState IO Token
andToken = tokenPrim show update_pos get_token
  where
    get_token (And position) = Just (And position)
    get_token _ = Nothing

-- | | Or
orToken :: ParsecT [Token] MemoryState IO Token
orToken = tokenPrim show update_pos get_token
  where
    get_token (Or position) = Just (Or position)
    get_token _ = Nothing
-- ^ Xor

xorToken :: ParsecT [Token] MemoryState IO Token
xorToken = tokenPrim show update_pos get_token
  where
    get_token (Xor position) = Just (Xor position)
    get_token _ = Nothing

-- ! Not
notToken :: ParsecT [Token] MemoryState IO Token
notToken = tokenPrim show update_pos get_token
  where
    get_token (Not position) = Just (Not position)
    get_token _ = Nothing

-- < Less
lessToken :: ParsecT [Token] MemoryState IO Token
lessToken = tokenPrim show update_pos get_token
  where
    get_token (Less position) = Just (Less position)
    get_token _ = Nothing

-- <= LessEqual
lessEqualToken :: ParsecT [Token] MemoryState IO Token
lessEqualToken = tokenPrim show update_pos get_token
  where
    get_token (LessEqual position) = Just (LessEqual position)
    get_token _ = Nothing

-- > Greater
greaterToken :: ParsecT [Token] MemoryState IO Token
greaterToken = tokenPrim show update_pos get_token
  where
    get_token (Greater position) = Just (Greater position)
    get_token _ = Nothing

-- >= GreaterEqual
greaterEqualToken :: ParsecT [Token] MemoryState IO Token
greaterEqualToken = tokenPrim show update_pos get_token
  where
    get_token (GreaterEqual position) = Just (GreaterEqual position)
    get_token _ = Nothing

-- == Equal
equalToken :: ParsecT [Token] MemoryState IO Token
equalToken = tokenPrim show update_pos get_token
  where
    get_token (Equal position) = Just (Equal position)
    get_token _ = Nothing

-- != Different
differentToken :: ParsecT [Token] MemoryState IO Token
differentToken = tokenPrim show update_pos get_token
  where
    get_token (Different position) = Just (Different position)
    get_token _ = Nothing

----------------------------- ID -----------------------------
idToken :: ParsecT [Token] MemoryState IO Token
idToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Id x position) = Just (Id x position)
    get_token _ = Nothing

scanToken :: ParsecT [Token] MemoryState IO Token
scanToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Scan x position) = Just (Scan x position)
    get_token _ = Nothing

printToken :: ParsecT [Token] MemoryState IO Token
printToken = tokenPrim show update_pos get_token -- ID
  where
    get_token (Print x position) = Just (Print x position)
    get_token _ = Nothing

----------------------------- Tipos -----------------------------
typeToken :: ParsecT [Token] MemoryState IO Token
typeToken = tokenPrim show update_pos get_token
  where
    get_token (Type x position) = Just (Type x position)
    get_token _ = Nothing

----------------------------- Valores Literais -----------------------------

intValToken :: ParsecT [Token] MemoryState IO Token
intValToken = tokenPrim show update_pos get_token
  where
    get_token (IntValue x position) = Just (IntValue x position)
    get_token _ = Nothing

floatValToken :: ParsecT [Token] MemoryState IO Token
floatValToken = tokenPrim show update_pos get_token
  where
    get_token (FloatValue x position) = Just (FloatValue x position)
    get_token _ = Nothing

charValToken :: ParsecT [Token] MemoryState IO Token
charValToken = tokenPrim show update_pos get_token
  where
    get_token (CharValue x position) = Just (CharValue x position)
    get_token _ = Nothing

stringValToken :: ParsecT [Token] MemoryState IO Token
stringValToken = tokenPrim show update_pos get_token
  where
    get_token (StringValue x position) = Just (StringValue x position)
    get_token _ = Nothing

boolValToken :: ParsecT [Token] u IO Token
boolValToken = tokenPrim show update_pos get_token
  where
    get_token (BoolValue x position) = Just (BoolValue x position)
    get_token _ = Nothing
