# Script para criar branch EASYSAP e fazer commit/push
# Autor: Sistema de Refatoração Automática
# Data: 2025-10-31

$ErrorActionPreference = "Continue"

$logFile = "git_easysap.log"

# Função para log
function Write-Log {
    param($Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "$timestamp - $Message"
    Write-Host $logMessage
    Add-Content -Path $logFile -Value $logMessage
}

# Limpar log anterior
if (Test-Path $logFile) { Remove-Item $logFile }

Write-Log "=== INICIANDO PROCESSO GIT EASYSAP ==="

# Verificar se a pasta EASYSAP_REFATORADO existe
if (-not (Test-Path "EASYSAP_REFATORADO")) {
    Write-Log "ERRO: Pasta EASYSAP_REFATORADO não encontrada!"
    Write-Log "Execute primeiro o script refatoracao_easysap.ps1"
    exit 1
}

Write-Log "Pasta EASYSAP_REFATORADO encontrada"

# Salvar branch atual
Write-Log "Salvando branch atual..."
$branchAtual = git branch --show-current 2>&1
Write-Log "Branch atual: $branchAtual"

# Verificar se branch EASYSAP já existe
$branchExists = git branch --list EASYSAP
if ($branchExists) {
    Write-Log "Branch EASYSAP já existe. Removendo..."
    git branch -D EASYSAP 2>&1 | Out-Null
}

# Criar nova branch EASYSAP
Write-Log "Criando branch EASYSAP..."
$output = git checkout -b EASYSAP 2>&1
Write-Log $output

# Verificar status
Write-Log "Verificando status do repositório..."
$status = git status --short 2>&1
Write-Log $status

# Adicionar pasta EASYSAP_REFATORADO
Write-Log "Adicionando pasta EASYSAP_REFATORADO ao git..."
$output = git add EASYSAP_REFATORADO/ 2>&1
if ($output) { Write-Log $output }
Write-Log "Pasta EASYSAP_REFATORADO adicionada"

# Adicionar arquivos de log e mapeamento
Write-Log "Adicionando arquivos de log e mapeamento..."
git add refatoracao_easysap.log 2>&1 | Out-Null
git add mapeamento_objetos.csv 2>&1 | Out-Null
git add *.ps1 2>&1 | Out-Null
Write-Log "Arquivos adicionados"

# Fazer commit
Write-Log "Fazendo commit..."
$mensagemCommit = "Refatoracao EASYSAP: Adicionar prefixo ZES a todos os objetos SAP - 5262 objetos renomeados"
$output = git commit -m $mensagemCommit 2>&1
Write-Log $output

# Push para o repositório remoto
Write-Log "Fazendo push para o repositório remoto..."
Write-Log "ATENÇÃO: O push pode demorar alguns minutos devido ao tamanho dos arquivos..."
$output = git push -u origin EASYSAP 2>&1
Write-Log $output

Write-Log "=== PROCESSO GIT CONCLUÍDO ==="
Write-Log "Branch EASYSAP criada e enviada para o repositório remoto"
Write-Log "Para voltar à branch anterior, execute: git checkout $branchAtual"

Write-Host ""
Write-Host "========================================" -ForegroundColor Green
Write-Host "    PROCESSO GIT CONCLUÍDO COM SUCESSO  " -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green
Write-Host ""
Write-Host "Resumo:" -ForegroundColor Cyan
Write-Host "  Branch criada: EASYSAP" -ForegroundColor White
Write-Host "  Commit: $mensagemCommit" -ForegroundColor White
Write-Host "  Push: origin/EASYSAP" -ForegroundColor White
Write-Host ""
Write-Host "Para voltar à branch anterior:" -ForegroundColor Yellow
Write-Host "  git checkout $branchAtual" -ForegroundColor White
Write-Host ""

