# Defina o caminho do executável e a lista de arquivos
$parserPath = ".\parser.exe"
$files = @(
    ".\exemplo2_uma_atribuicao.txt",
    ".\exemplo3_atribuicao_tipos_simples.txt",
    ".\exemplo4_atribuicao_por_expressao.txt",
    ".\exemplo5_atribuicoes_por_expressoes.txt",
    ".\exemplo5.1_problema_1.txt",
    ".\exemplo6_if_stmts.txt",
    ".\exemplo7_while_stmts.txt",
    ".\exemplo8_for_stmts.txt",
    # ".\exemplo9_scan.txt",
    ".\exemplo10_problema_2.txt"
    ".\exemplo11_print.txt"
)

# Itere sobre cada arquivo e execute o parser.exe com o arquivo atual
foreach ($file in $files) {
    Write-Host "Running $file"
    & $parserPath $file
}
