# Script de Analise de Objetos ABAP
Write-Host "Iniciando analise de objetos..." -ForegroundColor Cyan

$ObjetosPrincipais = @('ZCL_DOC_ELETRONICO', 'ZIF_DOC_ELETRONICO', 'ZCL_WEBSERVICE', 'ZIF_WEBSERVICE', 'ZBRNFE_DANFE', 'ZGRC', 'ZBR_GPF_NFE_DANFE')

Write-Host ""
Write-Host "Objetos Principais a Manter:" -ForegroundColor Green
foreach ($obj in $ObjetosPrincipais) {
    $objLower = $obj.ToLower()
    $arquivos = Get-ChildItem -Path src -Filter "*$objLower*" -Recurse -File
    if ($arquivos) {
        Write-Host "  $obj : $($arquivos.Count) arquivos" -ForegroundColor Cyan
    }
}
Write-Host ""
Write-Host "Analise concluida!" -ForegroundColor Green
