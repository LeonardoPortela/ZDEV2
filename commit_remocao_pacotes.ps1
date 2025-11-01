# Script para Commit e Push das Alterações na Branch EASYSAP
# Após remover pacotes

$ErrorActionPreference = "Continue"

$logFile = "commit_remocao_pacotes.log"
if (Test-Path $logFile) { Remove-Item $logFile }

function Write-Log {
    param($Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "$timestamp - $Message"
    Write-Host $logMessage
    Add-Content -Path $logFile -Value $logMessage
}

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  COMMIT - REMOÇÃO DE PACOTES" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

Write-Log "=== INICIANDO PROCESSO DE COMMIT ==="

# Verificar branch atual
$branchAtual = git branch --show-current 2>&1
Write-Log "Branch atual: $branchAtual"

if ($branchAtual -ne "EASYSAP") {
    Write-Host "ATENÇÃO: Não está na branch EASYSAP!" -ForegroundColor Yellow
    Write-Host "Mudando para branch EASYSAP..." -ForegroundColor Yellow
    git checkout EASYSAP 2>&1 | Out-Null
    Write-Log "Mudou para branch EASYSAP"
}

Write-Host "✓ Branch EASYSAP ativa" -ForegroundColor Green
Write-Host ""

# Verificar status
Write-Host "Verificando alterações..." -ForegroundColor Yellow
$status = git status --short 2>&1
Write-Log "Status: $status"

# Adicionar alterações
Write-Host "Adicionando alterações ao stage..." -ForegroundColor Yellow
git add EASYSAP_REFATORADO/ 2>&1 | Out-Null
git add remocao_pacotes.log 2>&1 | Out-Null
git add remover_pacotes.ps1 2>&1 | Out-Null
Write-Log "Alterações adicionadas ao stage"
Write-Host "✓ Alterações staged" -ForegroundColor Green
Write-Host ""

# Fazer commit
Write-Host "Fazendo commit..." -ForegroundColor Yellow
$mensagemCommit = "Remover pacotes desnecessários: ZAPP_APROVACAO, ZEQUI_PATRIMONIAL, ZSD_REL_CONTAS_REC e objetos relacionados"
$output = git commit -m $mensagemCommit 2>&1
Write-Log $output
Write-Host "✓ Commit realizado" -ForegroundColor Green
Write-Host ""

# Push
Write-Host "Fazendo push para repositório remoto..." -ForegroundColor Yellow
Write-Host "Aguarde, isso pode demorar alguns segundos..." -ForegroundColor Gray
$output = git push origin EASYSAP 2>&1
Write-Log $output
Write-Host "✓ Push concluído" -ForegroundColor Green
Write-Host ""

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "    PROCESSO CONCLUÍDO COM SUCESSO!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Resumo:" -ForegroundColor Yellow
Write-Host "  Branch: EASYSAP" -ForegroundColor White
Write-Host "  Commit: $mensagemCommit" -ForegroundColor White
Write-Host "  Push: origin/EASYSAP" -ForegroundColor White
Write-Host ""
Write-Host "Log salvo em: $logFile" -ForegroundColor Cyan
Write-Host ""

Write-Log "=== PROCESSO CONCLUÍDO ==="

