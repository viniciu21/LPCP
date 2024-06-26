# Defina o caminho do executável e a lista de arquivos
$parserPath = ".\Parser.exe"
$files = @(
    ".\exemplos\exemplo1_declarations.txt",
    ".\exemplos\exemplo2_uma_atribuicao.txt",
    ".\exemplos\exemplo3_atribuicao_tipos_simples.txt",
    ".\exemplos\exemplo4_atribuicao_por_expressao.txt",
    ".\exemplos\exemplo5_atribuicoes_por_expressoes.txt",
    ".\exemplos\exemplo5.1_problema_1.txt",
    ".\exemplos\exemplo6_if_stmts.txt",
    ".\exemplos\exemplo7_while_stmts.txt",
    ".\exemplos\exemplo8_for_stmts.txt",
    # ".\exemplo9_scan.txt",
    ".\exemplos\exemplo10_problema_2.txt",
    ".\exemplos\exemplo11_print.txt",
    ".\exemplos\exemplo12_funcoes.txt",
    ".\exemplos\exemplo13_func_expr.txt"
    ".\exemplos\exemplo14_fibonnaci.txt"
)

# Itere sobre cada arquivo e execute o parser.exe com o arquivo atual
foreach ($file in $files) {
    Write-Host "Running $file"
    
    if ($file -eq ".\exemplos\exemplo10_problema_2.txt") {
        # Cria um arquivo temporário com o input necessário
        $tempInputFile = [System.IO.Path]::GetTempFileName()
        Set-Content -Path $tempInputFile -Value "-1"
        
        # Executa o parser com o arquivo atual e redireciona o input
        Start-Process -FilePath $parserPath -ArgumentList $file -RedirectStandardInput $tempInputFile -NoNewWindow -Wait
        
        # Remove o arquivo temporário
        Remove-Item $tempInputFile
    }
    elseif ($file -eq ".\exemplos\exemplo14_fibonnaci.txt") {
        # Cria um arquivo temporário com o input necessário
        $tempInputFile = [System.IO.Path]::GetTempFileName()
        Set-Content -Path $tempInputFile -Value "4"
        
        # Executa o parser com o arquivo atual e redireciona o input
        Start-Process -FilePath $parserPath -ArgumentList $file -RedirectStandardInput $tempInputFile -NoNewWindow -Wait
        
        # Remove o arquivo temporário
        Remove-Item $tempInputFile
    }
    else {
        # Executa o parser com o arquivo atual
        & $parserPath $file
    }
}
