# Script para identificar objetos faltantes
Write-Output "=== IDENTIFICANDO OBJETOS FALTANTES ===`n"

$listaInicial = @(
    'ZCL_DOC_ELETRONICO', 'ZIF_DOC_ELETRONICO', 'ZCL_WEBSERVICE', 'ZIF_WEBSERVICE',
    'ZBRNFE_DANFE', 'ZFSD_BUSCA_DANFE', 'ZDEQUEUE_ALL', 'ZGRC_LIMPA_REF_MIRO_FISCAL',
    'Z_DETALHAMENTO_CTE', 'Z_DETALHAMENTO_CTE_IN_MASSA', 'Z_DETALHAMENTO_CTE_XML',
    'Z_DETALHAMENTO_NFE', 'Z_GRC_AJUSTA_TP_EMISSAO', 'Z_GRC_ARQUIVO_DOC',
    'Z_GRC_ARQUIVO_DOC_XML', 'Z_GRC_DOWNLOAD_XML_PDF', 'Z_GRC_ENVIA_LEGADO',
    'Z_GRC_GET_STATUS_DOC', 'Z_GRC_MDFE_AVULSA', 'Z_GRC_MDFE_LOAD',
    'Z_GRC_MONTA_LINK', 'Z_GRC_NEW_NFE', 'Z_GRC_REGISTRA_INF_ZIB_NFE',
    'Z_GRC_REGISTRA_LOG_DOC', 'Z_GRC_SEND_EMAIL_AUTO', 'Z_J_1BMFE_CANCEL_EVENT_SEND',
    'Z_J_1B_EVENT_CANCEL_NF_CTE', 'Z_J_1B_MDFE_CANCEL', 'Z_J_1B_MDFE_CLOSE',
    'Z_J_1B_MDFE_XML_OUT', 'Z_J_1B_NF_OBJECT_ADD', 'Z_SHOW_DETALHAMENTO_CTE',
    'Z_SHOW_DETALHAMENTO_NFE'
)

# Verificar quais objetos da lista inicial não existem
Write-Output "Verificando lista inicial..."
$faltantesListaInicial = @()

foreach ($obj in $listaInicial) {
    $objLower = $obj.ToLower()
    $arquivos = Get-ChildItem -Path "src\" -Recurse -File | Where-Object {
        $_.Name -match "^$objLower\."
    }
    
    if ($arquivos.Count -eq 0) {
        $faltantesListaInicial += $obj
        Write-Output "  ❌ $obj - NÃO ENCONTRADO"
    }
}

Write-Output "`nFaltantes da lista inicial: $($faltantesListaInicial.Count)"

if ($faltantesListaInicial.Count -gt 0) {
    Write-Output "`nObjetos da lista inicial que não existem no repositório:"
    $faltantesListaInicial | ForEach-Object { Write-Output "  - $_" }
    $faltantesListaInicial | Out-File "objetos_lista_inicial_faltantes.txt" -Encoding UTF8
}

Write-Output "`n=== FIM ==="

