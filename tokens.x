{
module Tokens where
    
import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                                   ;
    "#".*                                     ;
    ":"                                       { \s -> Colon } 
    ";"                                       { \s -> SemiColon } 
    ","                                       { \s -> Comma } 
    "("                                       { \s -> LeftParenthesis } 
    ")"                                       { \s -> RightParenthesis } 
    "{"                                       { \s -> LeftCurlyBrackets } 
    "}"                                       { \s -> RightCurlyBrackets } 
    "->"                                      { \s -> To } 
    "="                                       { \s -> Assign }
    "+"                                       { \s -> Plus } 
    "-"                                       { \s -> Minus } 
    "*"                                       { \s -> Times } 
    "/"                                       { \s -> Divider } 
    "//"                                      { \s -> IntegerDivider } 
    "**"                                      { \s -> Exponent } 
    "&&"                                      { \s -> And } 
    "||"                                      { \s -> Or } 
    "^"                                       { \s -> Xor } 
    "!"                                       { \s -> Not } 
    "<"                                       { \s -> Less } 
    ">"                                       { \s -> Greater } 
    "=="                                      { \s -> Equal } 
    "!="                                      { \s -> Different } 
    "int"                                     { \s -> Type s } 
    "float"                                   { \s -> Type s } 
    "char"                                    { \s -> Type s } 
    "string"                                  { \s -> Type s } 
    "bool"                                    { \s -> Type s } 
    "list"                                    { \s -> Type s } 
    "stack"                                   { \s -> Type s } 
    "queue"                                   { \s -> Type s } 
    "matrix"                                  { \s -> Type s } 
    "graph"                                   { \s -> Type s } 
    "tree"                                    { \s -> Type s } 
    "n-array"                                 { \s -> Type s } 
    "if"                                      { \s -> If } 
    "elif"                                    { \s -> Elif } 
    "else"                                    { \s -> Else } 
    "for"                                     { \s -> For } 
    "while"                                   { \s -> While } 
    "return"                                  { \s -> Return } 
    "declaration"                             { \s -> Declaration } 
    "end_declaration"                         { \s -> EndDeclaration } 
    "func"                                    { \s -> Func } 
    "end_func"                                { \s -> EndFunc } 
    "main"                                    { \s -> Main } 
    "end_main"                                { \s -> EndMain } 
    "true"                                    { \s -> BoolValue True}
    "false"                                   { \s -> BoolValue False}
    $digit+ \. $digit+                        { \s -> FloatValue (read s) }
    $digit+                                   { \s -> IntValue (read s) }
    \" $alpha [$alpha $digit ! \_ \']* \"     { \s -> StringValue s}
    \' $printable \'                          { \s -> CharValue s} 
    $alpha [$alpha $digit \_ \']*             { \s -> Id s }


{
data Token = 
    Id String                       |
    Colon                           |
    SemiColon                       |
    Comma                           |
    LeftParenthesis                 |
    RightParenthesis                |
    LeftCurlyBrackets               |
    RightCurlyBrackets              |
    To                              |
    Assign                          |
    Plus                            |
    Minus                           |
    Times                           |
    Divider                         |
    IntegerDivider                  |
    Exponent                        |
    And                             |
    Or                              |
    Xor                             |
    Not                             |
    Less                            |
    Greater                         |
    Equal                           |
    Different                       |
    Type String                     |
    If                              |
    Elif                            |
    Else                            |
    For                             |
    While                           |
    Return                          |
    Declaration                     |
    EndDeclaration                  |
    Func                            |
    EndFunc                         |
    Main                            |
    EndMain                         |
    IntValue Int                         |
    FloatValue Float                     |
    StringValue String                   |
    CharValue String                     |
    BoolValue Bool                  
    deriving (Eq,Show)


getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}