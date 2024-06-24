{
module Tokens where
    
import System.IO
import System.IO.Unsafe
import CleanString (cleanString, cleanChar)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                                   ;
    "#".*                                     ;
    ":"                                       { \p s -> Colon (getLC p) } 
    ";"                                       { \p s -> SemiColon (getLC p) } 
    ","                                       { \p s -> Comma (getLC p) } 
    "("                                       { \p s -> LeftParenthesis (getLC p) } 
    ")"                                       { \p s -> RightParenthesis (getLC p) } 
    "{"                                       { \p s -> LeftCurlyBrackets (getLC p) } 
    "}"                                       { \p s -> RightCurlyBrackets (getLC p) } 
    "->"                                      { \p s -> To (getLC p) } 
    "="                                       { \p s -> Assign (getLC p) }
    "+"                                       { \p s -> Plus (getLC p) } 
    "-"                                       { \p s -> Minus (getLC p) } 
    "*"                                       { \p s -> Times (getLC p) } 
    "/"                                       { \p s -> Divider (getLC p) } 
    "//"                                      { \p s -> IntegerDivider (getLC p) } 
    "**"                                      { \p s -> Exponent (getLC p) } 
    "&&"                                      { \p s -> And (getLC p) } 
    "||"                                      { \p s -> Or (getLC p) } 
    "^"                                       { \p s -> Xor (getLC p) } 
    "!"                                       { \p s -> Not (getLC p) } 
    "<"                                       { \p s -> Less (getLC p) } 
    "<="                                       { \p s -> LessEqual (getLC p) } 
    ">"                                       { \p s -> Greater (getLC p) }
    ">="                                       { \p s -> GreaterEqual (getLC p) } 
    "=="                                      { \p s -> Equal (getLC p) } 
    "!="                                      { \p s -> Different (getLC p) } 
    "int"                                     { \p s -> Type s (getLC p) } 
    "float"                                   { \p s -> Type s (getLC p) } 
    "char"                                    { \p s -> Type s (getLC p) } 
    "string"                                  { \p s -> Type s (getLC p) } 
    "bool"                                    { \p s -> Type s (getLC p) } 
    "list"                                    { \p s -> Type s (getLC p) } 
    "stack"                                   { \p s -> Type s (getLC p) } 
    "queue"                                   { \p s -> Type s (getLC p) } 
    "matrix"                                  { \p s -> Type s (getLC p) } 
    "graph"                                   { \p s -> Type s (getLC p) } 
    "tree"                                    { \p s -> Type s (getLC p) } 
    "n-array"                                 { \p s -> Type s (getLC p) } 
    "if"                                      { \p s -> If (getLC p) } 
    "end_if"                                  { \p s -> EndIf (getLC p) }
    "elif"                                    { \p s -> Elif (getLC p) } 
    "else"                                    { \p s -> Else (getLC p) } 
    "for"                                     { \p s -> For (getLC p) } 
    "end_for"                                 { \p s -> EndFor (getLC p) } 
    "while"                                   { \p s -> While (getLC p) } 
    "end_while"                               { \p s -> EndWhile (getLC p) } 
    "return"                                  { \p s -> Return (getLC p) } 
    "declaration"                             { \p s -> Declaration (getLC p) } 
    "end_declaration"                         { \p s -> EndDeclaration (getLC p) } 
    "func"                                    { \p s -> Func (getLC p) } 
    "end_func"                                { \p s -> EndFunc (getLC p) } 
    "main"                                    { \p s -> Main (getLC p) } 
    "end_main"                                { \p s -> EndMain (getLC p) } 
    "true"                                    { \p s -> BoolValue True(getLC p) }
    "false"                                   { \p s -> BoolValue False(getLC p) }
    "typedef"                                 { \p s -> Typedef (getLC p)}
    "struct"                                  { \p s -> Struct (getLC p)}
    "scan"                                    { \p s -> Scan s (getLC p) }
    "print"                                   { \p s -> Print s (getLC p) }
    $digit+ \. $digit+                        { \p s -> FloatValue (read s) (getLC p) }
    $digit+                                   { \p s -> IntValue (read s) (getLC p) }
    \" [$white $alpha $digit ! \_ \']* \"     { \p s -> StringValue (cleanString s) (getLC p) }
    \' $printable \'                          { \p s -> CharValue (cleanChar s) (getLC p) } 
    $alpha [$alpha $digit \_ \']*             { \p s -> Id s (getLC p) }


{
data TypeValue = 
    IntType Int (Int, Int) |
    FloatType Float (Int, Int) |
    StringType String (Int, Int) |
    CharType Char (Int, Int) |
    BoolType Bool (Int, Int) |
    ListType (Int, [TypeValue]) (Int, Int)| -- Tamanho, lista de valores, posição
    StructType [(String, TypeValue)] (Int, Int) 
    deriving (Eq)

instance Show TypeValue where
    show (IntType val pos) = show val
    show (FloatType val pos) = show val
    show (StringType val pos) = val
    show (CharType val pos) = show val
    show (BoolType val pos) = show val
    show (ListType (len, val) pos) = show val
    
data Token = 
    Id String (Int, Int)                               |
    Colon (Int, Int)                                   |
    SemiColon (Int, Int)                               |
    Comma (Int, Int)                                   |
    LeftParenthesis (Int, Int)                         |
    RightParenthesis (Int, Int)                        |
    LeftCurlyBrackets (Int, Int)                       |
    RightCurlyBrackets (Int, Int)                      |
    To (Int, Int)                                      |
    Assign (Int, Int)                                  |
    Plus (Int, Int)                                    |
    Minus (Int, Int)                                   |
    Times (Int, Int)                                   |
    Divider (Int, Int)                                 |
    IntegerDivider (Int, Int)                          |
    Exponent (Int, Int)                                |
    And (Int, Int)                                     |
    Or (Int, Int)                                      |
    Xor (Int, Int)                                     |
    Not (Int, Int)                                     |
    Less (Int, Int)                                    |
    LessEqual (Int, Int)                               |
    Greater (Int, Int)                                 |
    GreaterEqual (Int, Int)                            |
    Equal (Int, Int)                                   |
    Different (Int, Int)                               |
    Type String (Int, Int)                             |
    If (Int, Int)                                      |
    EndIf (Int, Int)                                   |  
    Elif (Int, Int)                                    |
    Else (Int, Int)                                    |
    For (Int, Int)                                     |
    EndFor (Int, Int)                                  |
    While (Int, Int)                                   |
    EndWhile (Int, Int)                                |
    Return (Int, Int)                                  |
    Declaration (Int, Int)                             |
    EndDeclaration (Int, Int)                          |
    Func (Int, Int)                                    |
    EndFunc (Int, Int)                                 |
    Main (Int, Int)                                    |
    EndMain (Int, Int)                                 |
    IntValue Int (Int, Int)                            |
    FloatValue Float (Int, Int)                        |
    StringValue String (Int, Int)                      |
    CharValue Char (Int, Int)                          |
    BoolValue Bool (Int, Int)                          |
    Typedef   (Int, Int)                               |
    Struct    (Int, Int)                               |
    Scan String (Int, Int)                             |
    Print String (Int, Int)                            
            
    deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}