# Defina o caminho do executável e a lista de arquivos
$parserPath = ".\Parser.exe"
$expectedOutputPath = ".\outputs" # Directory containing the expected output files
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
    ".\exemplos\exemplo10_problema_2.txt",
    ".\exemplos\exemplo11_print.txt"
)

# Itere sobre cada arquivo e execute o parser.exe com o arquivo atual
foreach ($file in $files) {
    Write-Host "Running $file"
    
    $tempOutputFile = [System.IO.Path]::GetTempFileName()
    if ($file -eq ".\exemplos\exemplo10_problema_2.txt") {
        # Cria um arquivo temporário com o input necessário
        $tempInputFile = [System.IO.Path]::GetTempFileName()
        Set-Content -Path $tempInputFile -Value "-1" -Encoding UTF8
        
        # Executa o parser com o arquivo atual e redireciona o input e output
        Start-Process -FilePath $parserPath -ArgumentList $file -RedirectStandardInput $tempInputFile -RedirectStandardOutput $tempOutputFile -NoNewWindow -Wait
        
        # Remove o arquivo temporário
        Remove-Item $tempInputFile
    } else {
        # Executa o parser com o arquivo atual e redireciona o output
        Start-Process -FilePath $parserPath -ArgumentList $file -RedirectStandardOutput $tempOutputFile -NoNewWindow -Wait
    }
    
    # Caminho para o arquivo de saída esperado
    $expectedOutputFileName = [System.IO.Path]::GetFileNameWithoutExtension($file) + "_expected.txt"
    $expectedOutputFile = Join-Path -Path $expectedOutputPath -ChildPath $expectedOutputFileName
    
    # Comparar o arquivo de saída temporário com o arquivo de saída esperado
    $tempOutput = Get-Content $tempOutputFile -Raw -Encoding UTF8
    $expectedOutput = Get-Content $expectedOutputFile -Raw -Encoding UTF8
    
    $diff = Compare-Object -ReferenceObject $expectedOutput -DifferenceObject $tempOutput -IncludeEqual -ExcludeDifferent
    
    if ($diff) {
        Write-Host "Diferenças encontradas para ${file}:"
        $diff | ForEach-Object { Write-Host $_ }
    } else {
        Write-Host "Testes passados para ${file}."
    }
    
    # Remove o arquivo de saída temporário
    Remove-Item $tempOutputFile
}
