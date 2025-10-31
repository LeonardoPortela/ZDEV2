# Script de Verificação da Refatoração EASYSAP
# Verifica se os arquivos foram criados corretamente

$ErrorActionPreference = "Continue"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  VERIFICAÇÃO DA REFATORAÇÃO EASYSAP   " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Verificar se a pasta existe
if (-not (Test-Path "EASYSAP_REFATORADO")) {
    Write-Host "ERRO: Pasta EASYSAP_REFATORADO não encontrada!" -ForegroundColor Red
    exit 1
}

Write-Host "✓ Pasta EASYSAP_REFATORADO encontrada" -ForegroundColor Green

# Verificar pasta src
if (-not (Test-Path "EASYSAP_REFATORADO\src")) {
    Write-Host "ERRO: Pasta EASYSAP_REFATORADO\src não encontrada!" -ForegroundColor Red
    exit 1
}

Write-Host "✓ Pasta EASYSAP_REFATORADO\src encontrada" -ForegroundColor Green
Write-Host ""

# Contar arquivos
Write-Host "Contando arquivos..." -ForegroundColor Yellow
$arquivos = Get-ChildItem -Path "EASYSAP_REFATORADO\src" -Recurse -File
$totalArquivos = $arquivos.Count
Write-Host "✓ Total de arquivos: $totalArquivos" -ForegroundColor Green

# Contar arquivos XML
$arquivosXML = $arquivos | Where-Object { $_.Extension -eq ".xml" }
$totalXML = $arquivosXML.Count
Write-Host "✓ Arquivos XML: $totalXML" -ForegroundColor Green

# Contar arquivos ABAP
$arquivosABAP = $arquivos | Where-Object { $_.Extension -eq ".abap" }
$totalABAP = $arquivosABAP.Count
Write-Host "✓ Arquivos ABAP: $totalABAP" -ForegroundColor Green

Write-Host ""

# Verificar alguns arquivos renomeados
Write-Host "Verificando arquivos renomeados com prefixo ZES..." -ForegroundColor Yellow
$arquivosZES = $arquivos | Where-Object { $_.Name -match "^ZES" }
$totalZES = $arquivosZES.Count
Write-Host "✓ Arquivos com prefixo ZES: $totalZES" -ForegroundColor Green

# Mostrar exemplos
Write-Host ""
Write-Host "Exemplos de arquivos renomeados (primeiros 10):" -ForegroundColor Cyan
$arquivosZES | Select-Object -First 10 | ForEach-Object {
    Write-Host "  - $($_.Name)" -ForegroundColor White
}

Write-Host ""

# Verificar mapeamento
if (Test-Path "mapeamento_objetos.csv") {
    Write-Host "✓ Arquivo de mapeamento encontrado" -ForegroundColor Green
    $mapeamento = Import-Csv "mapeamento_objetos.csv"
    Write-Host "  Total de objetos mapeados: $($mapeamento.Count)" -ForegroundColor White
}

# Verificar log
if (Test-Path "refatoracao_easysap.log") {
    Write-Host "✓ Arquivo de log encontrado" -ForegroundColor Green
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "    VERIFICAÇÃO CONCLUÍDA COM SUCESSO   " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Próximo passo: Execute .\git_easysap.ps1 para criar a branch" -ForegroundColor Yellow
Write-Host ""

