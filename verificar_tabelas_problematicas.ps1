# Verificar se as tabelas problematicas sao realmente necessarias
Write-Output "=== VERIFICANDO TABELAS PROBLEMATICAS ===`n"

$tabelasProblematicas = @(
    'ZLEST0061',
    'ZSDT_RETLOTE'
)

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

foreach ($tabela in $tabelasProblematicas) {
    Write-Output "=== $tabela ===`n"
    
    $referencias = Get-ChildItem -Path "src\" -Recurse -File | Select-String -Pattern "\b$tabela\b" -CaseSensitive:$false
    
    if ($referencias) {
        $arquivosReferenciadores = $referencias | ForEach-Object { $_.Filename } | Select-Object -Unique | Where-Object { $_ -notmatch "$($tabela.ToLower())\.tabl\.xml" }
        
        if ($arquivosReferenciadores.Count -gt 0) {
            Write-Output "Referenciado em $($arquivosReferenciadores.Count) arquivos (alem do proprio):"
            $arquivosReferenciadores | ForEach-Object {
                Write-Output "  - $_"
            }
        }
        else {
            Write-Output "NAO referenciado (apenas auto-referencia)"
        }
    }
    else {
        Write-Output "NAO encontrado"
    }
    Write-Output ""
}

Write-Output "=== FIM ==="

