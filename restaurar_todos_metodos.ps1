# Script para restaurar TODOS os métodos faltantes em classes de exceção e interfaces
# Restaurando arquivos da branch main para ZDEV_REFATORADO

Write-Host "====================================================" -ForegroundColor Cyan
Write-Host "Restauracao Completa de Metodos Faltantes" -ForegroundColor Cyan
Write-Host "====================================================" -ForegroundColor Cyan
Write-Host ""

# Obter lista de arquivos com diferenças nas classes de exceção
Write-Host "Identificando classes de excecao (zcx_*.clas.abap)..." -ForegroundColor Yellow
$zcx_files = git diff --name-only main ZDEV_REFATORADO -- "src/**/zcx_*.clas.abap" 2>&1 | Where-Object { $_ -notlike "*fatal*" -and $_ -notlike "*warning*" }

# Obter lista de arquivos com diferenças nas interfaces
Write-Host "Identificando interfaces (zif_*.intf.abap)..." -ForegroundColor Yellow
$zif_files = git diff --name-only main ZDEV_REFATORADO -- "src/**/zif_*.intf.abap" 2>&1 | Where-Object { $_ -notlike "*fatal*" -and $_ -notlike "*warning*" }

# Combinar ambas as listas
$todos_arquivos = @()
$todos_arquivos += $zcx_files
$todos_arquivos += $zif_files

# Remover linhas vazias
$todos_arquivos = $todos_arquivos | Where-Object { $_ -and $_.Trim() -ne "" }

Write-Host ""
Write-Host "Total de arquivos encontrados: $($todos_arquivos.Count)" -ForegroundColor Cyan
Write-Host ""

if ($todos_arquivos.Count -eq 0) {
    Write-Host "Nenhum arquivo com diferencas encontrado!" -ForegroundColor Green
    exit 0
}

$contador = 0
$sucesso = 0
$erro = 0

foreach ($arquivo in $todos_arquivos) {
    $contador++
    $percentual = [math]::Round(($contador / $todos_arquivos.Count) * 100, 1)
    Write-Host "[$contador/$($todos_arquivos.Count) - $percentual%] Restaurando: $arquivo" -ForegroundColor Yellow
    
    try {
        # Restaurar arquivo da branch main
        $resultado = git checkout main -- $arquivo 2>&1
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  -> OK" -ForegroundColor Green
            $sucesso++
        } else {
            Write-Host "  -> ERRO: $resultado" -ForegroundColor Red
            $erro++
        }
    }
    catch {
        Write-Host "  -> EXCECAO: $_" -ForegroundColor Red
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

Write-Host "Verificando status do repositorio..." -ForegroundColor Cyan
git status --short | Select-Object -First 20
Write-Host ""
Write-Host "Restauracao completa!" -ForegroundColor Green

