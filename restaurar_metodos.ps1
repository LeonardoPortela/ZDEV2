# Script para restaurar métodos faltantes das classes e interfaces
# Restaurando arquivos da branch main para ZDEV_REFATORADO

Write-Host "Iniciando restauracao de metodos faltantes..." -ForegroundColor Green

# Lista de arquivos para restaurar
$arquivos = @(
    "src/zcl_cte.clas.abap",
    "src/zcl_nfe.clas.abap",
    "src/zcl_mdfe_.clas.abap",
    "src/zcl_webservice_ord_car.clas.abap",
    "src/zcl_webservice_tx_curva.clas.abap",
    "src/zcl_webservice_trace.clas.abap",
    "src/zcl_webservice_tipcard.clas.abap",
    "src/zcl_webservice_hvi.clas.abap",
    "src/zcl_doc_eletronico.clas.abap",
    "src/zcx_doc_eletronico.clas.abap",
    "src/zcx_nfe_inbound_exception.clas.abap",
    "src/zcx_webservice.clas.abap",
    "src/zcx_cte_inbound.clas.abap",
    "src/zcl_webservice.clas.abap"
)

$contador = 0
$total = $arquivos.Count

foreach ($arquivo in $arquivos) {
    $contador++
    Write-Host "[$contador/$total] Restaurando: $arquivo" -ForegroundColor Yellow
    
    try {
        # Restaurar arquivo da branch main
        git checkout main -- $arquivo 2>&1 | Out-Null
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  -> OK" -ForegroundColor Green
        } else {
            Write-Host "  -> ERRO ao restaurar" -ForegroundColor Red
        }
    }
    catch {
        Write-Host "  -> EXCECAO: $_" -ForegroundColor Red
    }
}

Write-Host "`nVerificando status do repositorio..." -ForegroundColor Cyan
git status --short

Write-Host "`nRestauracao concluida!" -ForegroundColor Green
Write-Host "Total de arquivos processados: $total" -ForegroundColor Cyan

