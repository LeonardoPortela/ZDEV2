# Auditar objetos especificos que estao dando erro
Write-Output "=== AUDITANDO OBJETOS PROBLEMATICOS ===`n"

$objetosProblematicos = @(
    'ZLEST0061', 'ZSDT0237', 'ZSDT_RETLOTE', 'ZMMT0072', 'ZSDT0001',
    'ZLEST0056', 'ZLEST0060', 'ZLEST0073', 'ZSDT0102'
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

Write-Output "Procurando referencias aos objetos problematicos...`n"

$objetosParaRemover = @()

foreach ($obj in $objetosProblematicos) {
    $objLower = $obj.ToLower()
    Write-Output "Analisando: $obj"
    
    # Verificar se esta na lista inicial
    if ($listaInicial -contains $obj) {
        Write-Output "  -> LISTA INICIAL - MANTER`n"
        continue
    }
    
    # Procurar referencias
    $referencias = Get-ChildItem -Path "src\" -Recurse -File | Select-String -Pattern "\b$obj\b" -CaseSensitive:$false
    
    if ($referencias) {
        $arquivosReferenciadores = $referencias | ForEach-Object { $_.Filename } | Select-Object -Unique
        Write-Output "  Referenciado em $($arquivosReferenciadores.Count) arquivos:"
        
        $arquivosReferenciadores | Select-Object -First 5 | ForEach-Object {
            Write-Output "    - $_"
        }
        
        if ($arquivosReferenciadores.Count -gt 5) {
            Write-Output "    ... e mais $($arquivosReferenciadores.Count - 5)"
        }
        Write-Output ""
    }
    else {
        Write-Output "  NAO REFERENCIADO - CANDIDATO A REMOCAO`n"
        $objetosParaRemover += $obj
    }
}

Write-Output "=== RESULTADO ===`n"

if ($objetosParaRemover.Count -gt 0) {
    Write-Output "Objetos NAO referenciados (candidatos a remocao):"
    $objetosParaRemover | ForEach-Object { Write-Output "  - $_" }
    $objetosParaRemover | Out-File "objetos_nao_referenciados.txt" -Encoding UTF8
}
else {
    Write-Output "Todos os objetos problematicos SAO referenciados"
}

Write-Output "`n=== FIM ==="

