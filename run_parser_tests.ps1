# Defina o caminho do executável e a lista de arquivos
$parserPath = ".\parser.exe"
$files = @(
    ".\exemplos\exemplo2_uma_atribuicao.txt",
    ".\exemplos\exemplo3_atribuicao_tipos_simples.txt",
    ".\exemplos\exemplo4_atribuicao_por_expressao.txt",
    ".\exemplos\exemplo5_atribuicoes_por_expressoes.txt",
    ".\exemplos\exemplo5.1_problema_1.txt",
    ".\exemplos\exemplo6_if_stmts.txt",
    ".\exemplos\exemplo7_while_stmts.txt",
    ".\exemplos\exemplo8_for_stmts.txt",
    # ".\exemplo9_scan.txt",
    ".\exemplos\exemplo10_problema_2.txt"
    ".\exemplos\exemplo11_print.txt"
)

# Itere sobre cada arquivo e execute o parser.exe com o arquivo atual
foreach ($file in $files) {
    Write-Host "Running $file"
    & $parserPath $file
}
