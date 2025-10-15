# Script para excluir arquivos não utilizados
param(
    [string]$FileListPath = ".\files-to-delete.txt",
    [switch]$WhatIf = $false
)

Write-Host "=== Exclusão de Arquivos Não Utilizados ===" -ForegroundColor Green

# Ler lista de arquivos a excluir
if (-not (Test-Path $FileListPath)) {
    Write-Host "Arquivo de lista não encontrado: $FileListPath" -ForegroundColor Red
    exit 1
}

$filesToDelete = Get-Content $FileListPath
$totalFiles = $filesToDelete.Count

Write-Host "Total de arquivos a excluir: $totalFiles" -ForegroundColor Yellow

if ($WhatIf) {
    Write-Host "Modo SIMULAÇÃO ativado - nenhum arquivo será realmente excluído" -ForegroundColor Cyan
}

$deletedCount = 0
$errorCount = 0
$notFoundCount = 0

foreach ($file in $filesToDelete) {
    $deletedCount++
    
    if ($deletedCount % 1000 -eq 0) {
        $percent = [math]::Round(($deletedCount / $totalFiles) * 100, 2)
        Write-Host "Progresso: $deletedCount de $totalFiles ($percent%) - Erros: $errorCount - Não encontrados: $notFoundCount" -ForegroundColor Gray
    }
    
    if (Test-Path $file) {
        try {
            if (-not $WhatIf) {
                Remove-Item -Path $file -Force -ErrorAction Stop
            }
        }
        catch {
            Write-Host "Erro ao excluir: $file - $_" -ForegroundColor Red
            $errorCount++
        }
    }
    else {
        $notFoundCount++
    }
}

Write-Host "`n=== Exclusão Completa ===" -ForegroundColor Green
Write-Host "Arquivos processados: $deletedCount" -ForegroundColor Cyan
Write-Host "Arquivos não encontrados: $notFoundCount" -ForegroundColor Yellow
Write-Host "Erros: $errorCount" -ForegroundColor $(if ($errorCount -gt 0) { "Red" } else { "Green" })

if (-not $WhatIf) {
    Write-Host "`nLimpando diretórios vazios..." -ForegroundColor Yellow
    
    # Remover diretórios vazios (recursivo, de baixo para cima)
    $emptyDirs = Get-ChildItem -Path ".\src" -Recurse -Directory | 
        Where-Object { (Get-ChildItem $_.FullName -Force | Measure-Object).Count -eq 0 } |
        Sort-Object -Property FullName -Descending
    
    $emptyDirCount = $emptyDirs.Count
    Write-Host "Diretórios vazios encontrados: $emptyDirCount" -ForegroundColor Cyan
    
    foreach ($dir in $emptyDirs) {
        try {
            Remove-Item -Path $dir.FullName -Force -ErrorAction Stop
            Write-Host "  Removido: $($dir.FullName)" -ForegroundColor DarkGray
        }
        catch {
            Write-Host "  Erro ao remover diretório: $($dir.FullName) - $_" -ForegroundColor Red
        }
    }
}

Write-Host "`nConcluído!" -ForegroundColor Green

