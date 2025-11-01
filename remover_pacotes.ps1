# Script para Remover Pacotes Específicos da Refatoração EASYSAP
# Remove pacotes e seus objetos

$ErrorActionPreference = "Continue"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  REMOÇÃO DE PACOTES EASYSAP" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$logFile = "remocao_pacotes.log"
if (Test-Path $logFile) { Remove-Item $logFile }

function Write-Log {
    param($Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "$timestamp - $Message"
    Write-Host $logMessage
    Add-Content -Path $logFile -Value $logMessage
}

# Pacotes a remover (com prefixo ZES após refatoração)
$pacotesRemover = @(
    "zapp_aprovacao",
    "zesapp_aprovacao",  # variação com ZES
    "zeszapp_aprovacao",  # variação com ZES duplicado
    "zapp_aprovacao_adia_for",
    "zeszapp_aprovacao_adia_for",
    "zequi_patrimonial",
    "zeszequi_patrimonial",
    "zsd_rel_contas_rec",
    "zeszsd_rel_contas_rec",
    "zca"  # zca também mencionado na estrutura
)

Write-Log "=== INICIANDO REMOÇÃO DE PACOTES ==="
Write-Log "Pacotes a remover: $($pacotesRemover -join ', ')"
Write-Host ""

$pastaBase = "EASYSAP_REFATORADO\src"

if (-not (Test-Path $pastaBase)) {
    Write-Host "ERRO: Pasta EASYSAP_REFATORADO\src não encontrada!" -ForegroundColor Red
    exit 1
}

Write-Log "Pasta base: $pastaBase"
Write-Host ""

# Contar arquivos antes
$arquivosAntes = (Get-ChildItem -Path $pastaBase -Recurse -File).Count
Write-Log "Total de arquivos antes: $arquivosAntes"
Write-Host ""

# Procurar e remover cada pacote
$pacotesRemovidos = @()
$arquivosRemovidos = 0

foreach ($pacote in $pacotesRemover) {
    Write-Host "Procurando pacote: $pacote" -ForegroundColor Yellow
    
    # Procurar diretórios com esse nome
    $diretorios = Get-ChildItem -Path $pastaBase -Recurse -Directory | Where-Object { $_.Name -match "^$pacote$" -or $_.Name -match "^$pacote`$" }
    
    foreach ($dir in $diretorios) {
        Write-Log "Encontrado: $($dir.FullName)"
        
        # Contar arquivos no diretório
        $arquivosNoDiretorio = (Get-ChildItem -Path $dir.FullName -Recurse -File).Count
        
        Write-Host "  Removendo diretório: $($dir.Name)" -ForegroundColor Red
        Write-Host "    Caminho: $($dir.FullName)" -ForegroundColor Gray
        Write-Host "    Arquivos: $arquivosNoDiretorio" -ForegroundColor Gray
        
        # Remover diretório
        Remove-Item -Path $dir.FullName -Recurse -Force
        
        $pacotesRemovidos += $dir.Name
        $arquivosRemovidos += $arquivosNoDiretorio
        
        Write-Host "  ✓ Removido com sucesso!" -ForegroundColor Green
        Write-Log "Removido: $($dir.FullName) ($arquivosNoDiretorio arquivos)"
    }
    
    # Procurar arquivos soltos (não em diretórios de pacotes)
    $arquivosSoltos = Get-ChildItem -Path $pastaBase -File | Where-Object { $_.Name -match "^$pacote\." }
    
    foreach ($arquivo in $arquivosSoltos) {
        Write-Host "  Removendo arquivo: $($arquivo.Name)" -ForegroundColor Red
        Remove-Item -Path $arquivo.FullName -Force
        $arquivosRemovidos++
        Write-Log "Removido arquivo: $($arquivo.Name)"
    }
    
    Write-Host ""
}

# Contar arquivos depois
$arquivosDepois = (Get-ChildItem -Path $pastaBase -Recurse -File).Count

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "    REMOÇÃO CONCLUÍDA" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Resumo:" -ForegroundColor Yellow
Write-Host "  Pacotes removidos: $($pacotesRemovidos.Count)" -ForegroundColor White
Write-Host "  Arquivos antes: $arquivosAntes" -ForegroundColor White
Write-Host "  Arquivos depois: $arquivosDepois" -ForegroundColor White
Write-Host "  Arquivos removidos: $($arquivosAntes - $arquivosDepois)" -ForegroundColor Red
Write-Host ""

if ($pacotesRemovidos.Count -gt 0) {
    Write-Host "Pacotes removidos:" -ForegroundColor Yellow
    foreach ($pkg in $pacotesRemovidos) {
        Write-Host "  - $pkg" -ForegroundColor Gray
    }
}
Write-Host ""

Write-Log "=== REMOÇÃO CONCLUÍDA ==="
Write-Log "Total removido: $($arquivosAntes - $arquivosDepois) arquivos"

Write-Host "✓ Processo concluído!" -ForegroundColor Green
Write-Host "Log salvo em: $logFile" -ForegroundColor Cyan
Write-Host ""

