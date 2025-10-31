# Script Master - Execução Completa da Refatoração EASYSAP
# Autor: Sistema de Refatoração Automática
# Data: 2025-10-31

$ErrorActionPreference = "Stop"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  REFATORAÇÃO EASYSAP - SCRIPT MASTER  " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Verificar se a pasta origem existe
if (-not (Test-Path "src")) {
    Write-Host "ERRO: Pasta src não encontrada!" -ForegroundColor Red
    exit 1
}

Write-Host "Pasta origem encontrada: src" -ForegroundColor Green
Write-Host ""

# Confirmação do usuário
Write-Host "Este script irá:" -ForegroundColor Yellow
Write-Host "  1. Copiar todos os arquivos de ZDEV_REFATORADO\src" -ForegroundColor Yellow
Write-Host "  2. Adicionar prefixo ZES a todos os objetos SAP" -ForegroundColor Yellow
Write-Host "  3. Renomear arquivos e substituir referências internas" -ForegroundColor Yellow
Write-Host "  4. Criar pasta EASYSAP_REFATORADO com o resultado" -ForegroundColor Yellow
Write-Host "  5. Criar branch EASYSAP e fazer push" -ForegroundColor Yellow
Write-Host ""
Write-Host "ATENÇÃO: Nenhum arquivo em ZDEV_REFATORADO será alterado!" -ForegroundColor Green
Write-Host ""

$confirmacao = Read-Host "Deseja continuar? (S/N)"
if ($confirmacao -ne "S" -and $confirmacao -ne "s") {
    Write-Host "Operação cancelada pelo usuário." -ForegroundColor Yellow
    exit 0
}

Write-Host ""
Write-Host "=== ETAPA 1/2: REFATORAÇÃO DOS ARQUIVOS ===" -ForegroundColor Cyan
Write-Host "Executando refatoracao_easysap.ps1..." -ForegroundColor White
Write-Host ""

try {
    & ".\refatoracao_easysap.ps1"
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne $null) {
        throw "Erro na execução do script de refatoração"
    }
}
catch {
    Write-Host "ERRO na refatoração: $_" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "Refatoração concluída com sucesso!" -ForegroundColor Green
Write-Host ""

# Perguntar se deseja fazer o commit e push
Write-Host "=== ETAPA 2/2: COMMIT E PUSH ===" -ForegroundColor Cyan
Write-Host ""
$fazerCommit = Read-Host "Deseja criar a branch EASYSAP e fazer push? (S/N)"

if ($fazerCommit -eq "S" -or $fazerCommit -eq "s") {
    Write-Host ""
    Write-Host "Executando git_easysap.ps1..." -ForegroundColor White
    Write-Host ""
    
    try {
        & ".\git_easysap.ps1"
        if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne $null) {
            throw "Erro na execução do script git"
        }
    }
    catch {
        Write-Host "ERRO no processo git: $_" -ForegroundColor Red
        Write-Host "Os arquivos refatorados estão em EASYSAP_REFATORADO" -ForegroundColor Yellow
        exit 1
    }
    
    Write-Host ""
    Write-Host "Processo git concluído com sucesso!" -ForegroundColor Green
}
else {
    Write-Host ""
    Write-Host "Commit/Push pulado. Os arquivos refatorados estão em EASYSAP_REFATORADO" -ForegroundColor Yellow
    Write-Host "Para fazer o commit depois, execute: .\git_easysap.ps1" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "    PROCESSO CONCLUÍDO COM SUCESSO!    " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Arquivos gerados:" -ForegroundColor White
Write-Host "  - EASYSAP_REFATORADO\src\ (arquivos refatorados)" -ForegroundColor White
Write-Host "  - refatoracao_easysap.log (log detalhado)" -ForegroundColor White
Write-Host "  - mapeamento_objetos.csv (mapeamento de objetos)" -ForegroundColor White
if ($fazerCommit -eq "S" -or $fazerCommit -eq "s") {
    Write-Host "  - git_easysap.log (log do git)" -ForegroundColor White
}
Write-Host ""

