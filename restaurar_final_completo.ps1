# Script para restaurar TODOS os arquivos ABAP com diferenças
# Classes (.clas.abap) e Interfaces (.intf.abap)

Write-Host "====================================================" -ForegroundColor Cyan
Write-Host "Restauracao Completa - Classes e Interfaces ABAP" -ForegroundColor Cyan
Write-Host "====================================================" -ForegroundColor Cyan
Write-Host ""

# Obter lista de TODOS os arquivos .clas.abap e .intf.abap com diferenças
Write-Host "Identificando TODOS os arquivos .clas.abap com diferencas..." -ForegroundColor Yellow
$clas_output = git diff --name-only main ZDEV_REFATORADO -- "src/**/*.clas.abap" 2>&1
$clas_files = $clas_output | Where-Object { $_ -and $_ -match '\.clas\.abap$' }

Write-Host "Identificando TODOS os arquivos .intf.abap com diferencas..." -ForegroundColor Yellow
$intf_output = git diff --name-only main ZDEV_REFATORADO -- "src/**/*.intf.abap" 2>&1
$intf_files = $intf_output | Where-Object { $_ -and $_ -match '\.intf\.abap$' }

# Combinar ambas as listas
$todos_arquivos = @()
if ($clas_files) { $todos_arquivos += $clas_files }
if ($intf_files) { $todos_arquivos += $intf_files }

# Remover duplicatas
$todos_arquivos = $todos_arquivos | Select-Object -Unique | Where-Object { $_ -and $_.Trim() -ne "" }

Write-Host ""
Write-Host "Total de arquivos encontrados: $($todos_arquivos.Count)" -ForegroundColor Cyan
Write-Host "  - Classes (.clas.abap): $($clas_files.Count)" -ForegroundColor White
Write-Host "  - Interfaces (.intf.abap): $($intf_files.Count)" -ForegroundColor White
Write-Host ""

if ($todos_arquivos.Count -eq 0) {
    Write-Host "Nenhum arquivo com diferencas encontrado!" -ForegroundColor Green
    exit 0
}

# Perguntar confirmação
Write-Host "ATENCAO: Serao restaurados $($todos_arquivos.Count) arquivos da branch MAIN" -ForegroundColor Yellow
Write-Host "Pressione ENTER para continuar ou CTRL+C para cancelar..."
# Read-Host

$contador = 0
$sucesso = 0
$erro = 0

foreach ($arquivo in $todos_arquivos) {
    $contador++
    $percentual = [math]::Round(($contador / $todos_arquivos.Count) * 100, 1)
    
    if ($contador % 10 -eq 0) {
        Write-Host "[$contador/$($todos_arquivos.Count) - $percentual%] Restaurando..." -ForegroundColor Cyan
    }
    
    Write-Host "  $arquivo" -ForegroundColor Gray
    
    try {
        $resultado = git checkout main -- $arquivo 2>&1
        
        if ($LASTEXITCODE -eq 0) {
            $sucesso++
        } else {
            Write-Host "    ERRO: $resultado" -ForegroundColor Red
            $erro++
        }
    }
    catch {
        Write-Host "    EXCECAO: $_" -ForegroundColor Red
        $erro++
    }
}

Write-Host ""
Write-Host "====================================================" -ForegroundColor Cyan
Write-Host "Resumo da Restauracao" -ForegroundColor Cyan
Write-Host "====================================================" -ForegroundColor Cyan
Write-Host "Total processados: $contador" -ForegroundColor White
Write-Host "Sucesso: $sucesso" -ForegroundColor Green
Write-Host "Erros: $erro" -ForegroundColor Red
Write-Host ""

Write-Host "Verificando arquivos modificados..." -ForegroundColor Cyan
$modified = git status --short | Where-Object { $_ -match '^M ' }
Write-Host "Total de arquivos modificados: $($modified.Count)" -ForegroundColor White
Write-Host ""
Write-Host "Restauracao completa!" -ForegroundColor Green

