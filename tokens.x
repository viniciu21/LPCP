{
    module Main (main) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                                 ;
    "#".*                                   ;
    :                                       { \s -> Colon } 
    ;                                       { \s -> SemiColon } 
    ,                                       { \s -> Comma } 
    (                                       { \s -> LeftParenthesis } 
    )                                       { \s -> RightParenthesis } 
    {                                       { \s -> LeftCurlyBrackets } 
    }                                       { \s -> RightCurlyBrackets } 
    ->                                      { \s -> To } 
    +                                       { \s -> Plus } 
    -                                       { \s -> Minus } 
    *                                       { \s -> Times } 
    /                                       { \s -> Divider } 
    //                                      { \s -> IntegerDivider } 
    **                                      { \s -> Exponent } 
    &&                                      { \s -> And } 
    ||                                      { \s -> Or } 
    ^                                       { \s -> Xor } 
    !                                       { \s -> Not } 
    <                                       { \s -> Less } 
    >                                       { \s -> Greater } 
    ==                                      { \s -> Equal } 
    !=                                      { \s -> Different } 
    int                                     { \s -> Type s } 
    char                                    { \s -> Type s } 
    string                                  { \s -> Type s } 
    bool                                    { \s -> Type s } 
    list                                    { \s -> Type s } 
    stack                                   { \s -> Type s } 
    queue                                   { \s -> Type s } 
    matrix                                  { \s -> Type s } 
    graph                                   { \s -> Type s } 
    tree                                    { \s -> Type s } 
    n-array                                 { \s -> Type s } 
    if                                      { \s -> If } 
    elif                                    { \s -> Elif } 
    else                                    { \s -> Else } 
    for                                     { \s -> For } 
    while                                   { \s -> While } 
    return                                  { \s -> Return } 
    declaration                             { \s -> Declaration } 
    end_declaration                         { \s -> EndDeclaration } 
    func                                    { \s -> Func } 
    end_func                                { \s -> EndFunc } 
    main                                    { \s -> Main } 
    end_main                                { \s -> EndMain } 
    $alpha [$alpha $digit \_ \']*           { \s -> Id s }
    $digit+                                 { \s -> Int (read s) }
    $digit+ \. $digit+                      { \s -> Float (read s) }
    \" $alpha [$alpha $digit ! \_ \']* \"   { \s -> String s}
    \' $printable \'                        { \s -> Char s} 

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
    Int Int                         |
    Float Float                     |
    String String                   |
    Char Char                       
    deriving (Eq,Show)


main = do
    s <- getContents
    print(alexScanTokens s)
}