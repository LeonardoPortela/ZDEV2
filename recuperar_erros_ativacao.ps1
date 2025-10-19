# Recuperar objetos especificos dos erros de ativacao

$objetosFaltantes = @(
    # Tabelas de valores nao ativas
    "ZIMP_CAD_DEPTO", "ZWST0001", "ZFIWRT0001", "ZCTE_CIOT",
    
    # Search helps nao ativos
    "ZSDAJ005", "ZSDAJ006", "ZDM_OPER_AQUAV", "ZLESH0003", 
    "ZPSQ_LOCAL_ESTOQUE", "ZPSQ_TIPO_ENTRADA", "ZSH_DEP_RESP",
    
    # Views/tabelas transparentes
    "ZI_LES_F4_OPERACAO", "ZLEST0025", "ZSDV0001",
    
    # Estruturas e includes faltantes
    "ZDE_CTE_DIST_C57", "ZDE_CTE_DIST_N01", "ZDE_INF_CTE_NORM_INF_DOC",
    "ZDE_INF_CARGA_DACTE", "ZDE_INF_CTE_VPREST", "ZDE_INF_CTE_NORM",
    "ZDE_NFE_ENTRADA_ITEM", "ZCA_CAMPOS_CREATE", "ZINTG_COMP", "ZINTG_INFQ",
    "ZDE_RET_EVENT_INB", "ZDE_S_CHAVE_NFE", "ZDE_INTEGRACAO_HTTP_CONFIG",
    "ZIB_CTE_DIST_ANT", "ZIB_CTE_DIST_C57", "ZIB_CTE_DIST_CVL",
    "ZIB_CTE_DIST_DUP", "ZIB_CTE_DIST_001", "ZIB_CTE_DIST_D01",
    "ZIB_CTE_DIST_D55", "ZIB_CTE_DIST_MOT", "ZIB_CTE_DIST_N01",
    "ZIB_CTE_DIST_N55", "ZIB_CTE_DIST_VEI", "ZIB_CTE_DIST_VGA",
    "ZLEST0053", "ZLEST0062", "ZPFE_LOTE", "ZPFE_LOTE_ALV",
    "ZPFE_LOTE_ITEM", "ZPFE_LOTE_ITEM_ALV", "ZSDT0104", "ZSDT0118",
    "ZSDT0133", "ZSDT0243", "ZSDT0422", "ZSDT_RETLOTE", "ZSTRUCT_DADOS_EUDR",
    "ZDE_INF_MODAL_DACTE", "ZIB_NFE_DIST_ITM", "ZIB_NFE_DIST_AVI",
    "ZIB_NFE_FORN", "ZSDT0001LE", "ZSDT0001TETX",
    
    # Estruturas XML/Eventos
    "ZDE_EVENTO", "ZDE_INF_NFE", "ZDE_NFE_XML", "ZDE_PROC_EVENTO_NFE",
    "ZDE_INF_CTE", "ZDE_INF_CTE_AJ", "ZDE_INF_CTE_OS", "ZDE_INF_CTE_NORM_AJ",
    "ZDE_CTE_XML", "ZDE_CTE_XML_AJ", "ZDE_CTE_OS_XML",
    "ZCTE_XML_SEFAZ", "ZCTE_XML_SEFAZ_AJ", "ZCTE_XML_SEFAZ_AUTH",
    "ZCTE_XML_SEFAZ_AUTH_AJ", "ZCTE_OS_XML_SEFAZ", "ZCTE_OS_XML_SEFAZ_AUTH",
    "ZNFE_XML_SEFAZ", "ZAVERB_XML_SEFAZ_AUTH", "ZDE_XML_CTE",
    
    # Data elements faltantes (campos especificos)
    "C57_CST_ICMS", "C57_PREDU_BICMS", "C57_VALR_CREDITO", 
    "C57_VL_AICMS", "C57_VL_BICMS", "C57_VL_ICMS", "C57_VL_SERVICO",
    "N01_CODG_CFOP", "N01_VL_BASE_ICMS", "N01_VL_BICMS_ST",
    "N01_VL_ICMS", "N01_VL_ICMS_ST", "N01_VL_NOTA", "N01_VL_PESO", "N01_VL_PRODUTOS",
    "CHVID", "C57_CHAVE_ACESSO", "N55_CHAVE_ACESSO",
    "PROP_TP_DOC", "VEIC_CAPACIDADE_KG", "VEIC_CAPACIDADE_M3", 
    "VEIC_RENAVAM", "VEIC_TARA_KG", "COD_TP_EMB_MIN",
    "HR_BELNR", "HR_LEITURA", "HR_CONF_ADM", "CUNID", "ARMAZEM_ORG",
    "PC_VEICULO", "PRAZO", "EUDR", "FERROV", "RODO",
    "COBR", "COMPRA", "SENHA_INICIAL", "DS_CONTENT_TYPE",
    "ZPERDA", "ZPESO_DIFERENCA", "ZQUEBRA", "ZVLR_LIQ_PAGAR", 
    "ZVLR_PERDA", "ZVLR_QUEBRA",
    
    # Table types
    "ZINTG_COMP_T", "ZINTG_INFQ_T", "ZCTMM_CHAVE_NFE", "ZCTMM_XML_CTE",
    
    # Outros
    "ZHCMT0007", "ZSDT0241"
)

$recuperados = 0
$jaExistem = 0
$naoEncontrados = 0

Write-Output "=== RECUPERANDO OBJETOS DOS ERROS DE ATIVACAO ===" | Tee-Object -FilePath "recuperacao_erros.log"
Write-Output "Total de objetos a verificar: $($objetosFaltantes.Count)" | Tee-Object -FilePath "recuperacao_erros.log" -Append

foreach ($obj in $objetosFaltantes) {
    $nome = $obj.ToLower()
    $recuperadoAlgo = $false
    
    # Tentar varios tipos
    $extensoes = @(".tabl.xml", ".dtel.xml", ".doma.xml", ".ttyp.xml", ".shlp.xml", 
                   ".fugr.xml", ".clas.xml", ".prog.xml", ".intf.xml")
    
    foreach ($ext in $extensoes) {
        $arquivo = "src\$nome$ext"
        
        # Se ja existe, pular
        if (Test-Path $arquivo) {
            if (-not $recuperadoAlgo) {
                $jaExistem++
                $recuperadoAlgo = $true
            }
            continue
        }
        
        # Tentar recuperar
        git checkout main -- "src/$nome$ext" 2>&1 | Out-Null
        
        if ($LASTEXITCODE -eq 0 -and (Test-Path $arquivo)) {
            Write-Output "  [+] $nome$ext" | Tee-Object -FilePath "recuperacao_erros.log" -Append
            $recuperados++
            $recuperadoAlgo = $true
        }
    }
    
    if (-not $recuperadoAlgo) {
        Write-Output "  [!] NAO ENCONTRADO: $obj" | Tee-Object -FilePath "recuperacao_erros.log" -Append
        $naoEncontrados++
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "recuperacao_erros.log" -Append
Write-Output "RESULTADO:" | Tee-Object -FilePath "recuperacao_erros.log" -Append
Write-Output "  Ja existentes: $jaExistem" | Tee-Object -FilePath "recuperacao_erros.log" -Append
Write-Output "  Recuperados: $recuperados" | Tee-Object -FilePath "recuperacao_erros.log" -Append
Write-Output "  Nao encontrados: $naoEncontrados" | Tee-Object -FilePath "recuperacao_erros.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "recuperacao_erros.log" -Append

$totalArquivos = (Get-ChildItem -Path "src" -Recurse -File | Measure-Object).Count
Write-Output "Total de arquivos no src: $totalArquivos" | Tee-Object -FilePath "recuperacao_erros.log" -Append

