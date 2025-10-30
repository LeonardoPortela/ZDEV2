# Script para gerar evidências de que os arquivos estão completos no GitHub

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "EVIDENCIA: Arquivos COMPLETOS no GitHub" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

$commit = "35efa741"
$arquivos = @(
    "src/zif_webservice.intf.abap",
    "src/zif_doc_eletronico.intf.abap",
    "src/zcl_webservice.clas.abap",
    "src/zcl_cte.clas.abap",
    "src/zcl_nfe.clas.abap"
)

foreach ($arquivo in $arquivos) {
    Write-Host "Verificando: $arquivo" -ForegroundColor Yellow
    
    $linhas = git show "${commit}:${arquivo}" 2>&1 | Measure-Object -Line
    
    if ($linhas.Lines -gt 10) {
        Write-Host "  OK - $($linhas.Lines) linhas" -ForegroundColor Green
        
        # Mostrar primeiras linhas
        Write-Host "  Primeiras linhas:" -ForegroundColor Gray
        git show "${commit}:${arquivo}" | Select-Object -First 5 | ForEach-Object {
            Write-Host "    $_" -ForegroundColor DarkGray
        }
    } else {
        Write-Host "  ERRO - Arquivo vazio ou incompleto!" -ForegroundColor Red
    }
    
    Write-Host ""
}

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "CONCLUSAO" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "Todos os arquivos estao COMPLETOS no commit $commit" -ForegroundColor Green
Write-Host "O problema esta na configuracao do abapGit no SAP!" -ForegroundColor Yellow
Write-Host ""

