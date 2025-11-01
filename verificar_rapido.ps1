# Verificação Rápida e Direta de Conteúdo XML

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  VERIFICAÇÃO RÁPIDA DE ARQUIVOS XML  " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Exemplo 1: ZCTE_TRANS -> ZESCTE_TRANS
Write-Host "=== EXEMPLO 1: ZCTE_TRANS ===" -ForegroundColor Yellow
Write-Host "Objeto Original: ZCTE_TRANS" -ForegroundColor White
Write-Host "Objeto Novo: ZESCTE_TRANS" -ForegroundColor White
Write-Host ""

$arquivo1Original = Get-ChildItem "src" -Recurse -File -Filter "zcte_trans.tabl.xml" | Select-Object -First 1
if ($arquivo1Original) {
    Write-Host "Arquivo original encontrado: $($arquivo1Original.Name)" -ForegroundColor Green
    $conteudo1Original = Get-Content $arquivo1Original.FullName -Raw
    
    # Mostrar primeiras linhas com ZCTE_TRANS
    $linhas = $conteudo1Original -split "`n" | Where-Object { $_ -match "ZCTE_TRANS" } | Select-Object -First 3
    Write-Host "Conteúdo original (primeiras referências):" -ForegroundColor Cyan
    foreach ($linha in $linhas) {
        Write-Host "  $($linha.Trim())" -ForegroundColor Gray
    }
}
Write-Host ""

$arquivo1Novo = Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File -Filter "zescte_trans.tabl.xml" | Select-Object -First 1
if ($arquivo1Novo) {
    Write-Host "Arquivo refatorado encontrado: $($arquivo1Novo.Name)" -ForegroundColor Green
    $conteudo1Novo = Get-Content $arquivo1Novo.FullName -Raw
    
    # Mostrar primeiras linhas com ZESCTE_TRANS
    $linhas = $conteudo1Novo -split "`n" | Where-Object { $_ -match "ZESCTE_TRANS" } | Select-Object -First 3
    Write-Host "Conteúdo refatorado (primeiras referências):" -ForegroundColor Cyan
    foreach ($linha in $linhas) {
        Write-Host "  $($linha.Trim())" -ForegroundColor Gray
    }
    
    # Verificar se ainda tem referência antiga
    if ($conteudo1Novo -match "ZCTE_TRANS" -and $conteudo1Novo -notmatch "ZESCTE_TRANS") {
        Write-Host "  ✗ ERRO: Ainda contém referência antiga!" -ForegroundColor Red
    }
    elseif ($conteudo1Novo -match "ZESCTE_TRANS") {
        Write-Host "  ✓ CORRETO: Referências atualizadas para ZESCTE_TRANS" -ForegroundColor Green
    }
}
Write-Host ""
Write-Host "----------------------------------------" -ForegroundColor Gray
Write-Host ""

# Exemplo 2: Pegar um objeto aleatório do mapeamento
if (Test-Path "mapeamento_objetos.csv") {
    $mapeamento = Import-Csv "mapeamento_objetos.csv"
    $exemplo2 = $mapeamento | Get-Random -Count 1
    
    Write-Host "=== EXEMPLO 2: $($exemplo2.ObjetoOriginal) ===" -ForegroundColor Yellow
    Write-Host "Objeto Original: $($exemplo2.ObjetoOriginal)" -ForegroundColor White
    Write-Host "Objeto Novo: $($exemplo2.ObjetoNovo)" -ForegroundColor White
    Write-Host ""
    
    # Procurar arquivo original
    $arquivoOriginal2 = Get-ChildItem "src" -Recurse -File | Where-Object { $_.Name -match "^$($exemplo2.ObjetoOriginal)\." } | Select-Object -First 1
    
    if ($arquivoOriginal2) {
        Write-Host "Arquivo original: $($arquivoOriginal2.Name)" -ForegroundColor Green
        
        # Procurar arquivo refatorado
        $nomeNovoArquivo = $arquivoOriginal2.Name -replace $exemplo2.ObjetoOriginal, $exemplo2.ObjetoNovo
        $arquivoNovo2 = Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File -Filter $nomeNovoArquivo | Select-Object -First 1
        
        if ($arquivoNovo2) {
            Write-Host "Arquivo refatorado: $($arquivoNovo2.Name)" -ForegroundColor Green
            
            $conteudoNovo2 = Get-Content $arquivoNovo2.FullName -Raw
            
            # Verificar
            $temNovo = $conteudoNovo2 -match $exemplo2.ObjetoNovo
            $temAntigo = $conteudoNovo2 -match $exemplo2.ObjetoOriginal
            
            if ($temNovo -and -not $temAntigo) {
                Write-Host "  ✓ CORRETO: Referências atualizadas" -ForegroundColor Green
            }
            elseif ($temNovo -and $temAntigo) {
                Write-Host "  ⚠ PARCIAL: Contém ambas as referências" -ForegroundColor Yellow
            }
            else {
                Write-Host "  ✗ ERRO: Referências não atualizadas corretamente" -ForegroundColor Red
            }
        }
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Estatísticas gerais
Write-Host "=== ESTATÍSTICAS GERAIS ===" -ForegroundColor Yellow
$totalOriginal = (Get-ChildItem "src" -Recurse -File).Count
$totalNovo = (Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File -ErrorAction SilentlyContinue).Count
$totalZES = (Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File -ErrorAction SilentlyContinue | Where-Object { $_.Name -match "^ZES" }).Count

Write-Host "Arquivos originais: $totalOriginal" -ForegroundColor White
Write-Host "Arquivos refatorados: $totalNovo" -ForegroundColor White
Write-Host "Arquivos com prefixo ZES: $totalZES" -ForegroundColor White

Write-Host ""
Write-Host "Verificação concluída!" -ForegroundColor Green
Write-Host ""

