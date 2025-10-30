# Script para corrigir BOM UTF-8 e verificar interfaces

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "Corrigindo BOM UTF-8 e verificando interfaces" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

# PARTE 1: Corrigir BOM UTF-8 em arquivos ABAP
Write-Host "PARTE 1: Verificando e corrigindo BOM UTF-8..." -ForegroundColor Yellow
Write-Host ""

$arquivosAbap = Get-ChildItem -Path "src" -Recurse -Filter "*.abap"
$arquivosComBom = 0
$arquivosCorrigidos = 0

foreach ($arquivo in $arquivosAbap) {
    $bytes = [System.IO.File]::ReadAllBytes($arquivo.FullName)
    
    # Verificar se tem BOM UTF-8 (EF BB BF)
    if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
        Write-Host "  BOM encontrado em: $($arquivo.Name)" -ForegroundColor Red
        $arquivosComBom++
        
        try {
            # Ler conteúdo sem BOM
            $conteudo = [System.IO.File]::ReadAllText($arquivo.FullName, [System.Text.UTF8Encoding]::new($false))
            
            # Salvar sem BOM
            [System.IO.File]::WriteAllText($arquivo.FullName, $conteudo, [System.Text.UTF8Encoding]::new($false))
            
            Write-Host "    -> CORRIGIDO!" -ForegroundColor Green
            $arquivosCorrigidos++
        }
        catch {
            Write-Host "    -> ERRO ao corrigir: $_" -ForegroundColor Red
        }
    }
}

Write-Host ""
Write-Host "Resumo BOM:" -ForegroundColor Cyan
Write-Host "  Arquivos com BOM: $arquivosComBom" -ForegroundColor White
Write-Host "  Arquivos corrigidos: $arquivosCorrigidos" -ForegroundColor Green
Write-Host ""

# PARTE 2: Verificar interfaces ZIF*
Write-Host "PARTE 2: Verificando interfaces ZIF*..." -ForegroundColor Yellow
Write-Host ""

$interfaces = Get-ChildItem -Path "src" -Recurse -Filter "zif*.intf.abap"

foreach ($intf in $interfaces) {
    $conteudo = Get-Content $intf.FullName -Raw
    
    # Contar linhas com "methods " (definições de métodos)
    $metodos = ([regex]::Matches($conteudo, '\n\s+(class-)?methods\s+', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)).Count
    
    $tamanho = (Get-Content $intf.FullName | Measure-Object -Line).Lines
    
    Write-Host "Interface: $($intf.Name)" -ForegroundColor White
    Write-Host "  Linhas: $tamanho" -ForegroundColor Gray
    Write-Host "  Métodos: $metodos" -ForegroundColor $(if ($metodos -gt 0) { "Green" } else { "Red" })
    
    if ($metodos -eq 0 -and $tamanho -lt 20) {
        Write-Host "  STATUS: VAZIA - Precisa ser restaurada!" -ForegroundColor Red
    }
    elseif ($metodos -eq 0 -and $tamanho -ge 20) {
        Write-Host "  AVISO: Tem conteúdo mas sem métodos detectados" -ForegroundColor Yellow
    }
    else {
        Write-Host "  STATUS: OK" -ForegroundColor Green
    }
    Write-Host ""
}

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "Verificação completa!" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan

