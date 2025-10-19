# Recuperar objetos faltantes baseado na lista de erros

$faltantes = @(
    "ZDM_APLIC_EM", "ZDO_AUT_EMBARQUE", "ZDM_CFOP", "ZDM_CHAVE_DOC_E",
    "ZDM_CST_ICMS", "ZDM_CTE_DIST_ST", "ZDM_DOC_PARC", "ZDM_FAGL_CHECK",
    "ZDM_FINA_EMISSAO", "ZDM_FORMA_PAG", "Z_INSC_E", "ZDM_ID_SIMETRYA",
    "ZDM_MDFE_TPCARGA", "ZDM_MDFE_XPROD", "ZDM_NR_BOLETO", "ZDM_NR_FASE",
    "ZMENG15", "ZDM_PROCESSO_CTE", "ZDM_QT_CARGA_CTE", "ZDM_SENHA",
    "ZDM_SE_CODE_WORKFLOW", "ZDM_SE_DETAIL_WORKFLOW", "ZDM_SE_RECORDID_WORKFLOW",
    "ZDM_SE_RECORDKEY_WORKFLOW", "ZDM_SE_STATUS_WORKFLOW", "ZDM_STATUS_DOC_ELETRONICO",
    "ZDM_ST_NFE_ARMAZEM", "ZDM_ST_NFE_DOCUMENTO", "ZDM_ST_NFE_FISCAL", "ZDM_ST_NFE_FISICO",
    "ZDM_TIPO_DOC", "ZDM_TIPO_EMISSAO", "ZDM_TP_COMPRA_FUTURA", "ZDM_TP_ENC_MDFE",
    "ZDM_TRANS_NF_PROPRI", "ZDM_USUARIO", "ZDM_VLR15_02", "ZDM_WEB_SERV_CTX",
    "ZDE_LATITUDE", "ZDE_LONGITUDE", "ZDE_ID_TOKEN", "ZDE_PROCESSO_TOKEN",
    "ZDE_ADD01", "ZDE_ADD02", "ZDE_METHOD_REQUEST", "ZDE_CONTENT_TYPE", "ZZTOKEN",
    "ZDE_ROMANEIO", "ZDE_DS_DEPARTAMENTO", "ZDE_CK_SEM_RET_GRUPO", "ZDE_CK_SEM_RET_PEDIDO",
    "ZHELP_ZMMT0072"
)

$exts = @(".doma.xml", ".dtel.xml", ".tabl.xml", ".shlp.xml")

$recuperados = 0
$naoEncontrados = 0

Write-Output "Recuperando objetos faltantes..."

foreach ($obj in $faltantes) {
    $nome = $obj.ToLower()
    $found = $false
    
    foreach ($ext in $exts) {
        $arquivo = "src\$nome$ext"
        
        # Pular se já existe
        if (Test-Path $arquivo) {
            continue
        }
        
        # Tentar recuperar
        git checkout main -- "src/$nome$ext" 2>&1 | Out-Null
        
        if ($LASTEXITCODE -eq 0 -and (Test-Path $arquivo)) {
            Write-Output "  [+] $nome$ext"
            $recuperados++
            $found = $true
        }
    }
    
    if (-not $found) {
        Write-Output "  [!] $obj NAO ENCONTRADO"
        $naoEncontrados++
    }
}

Write-Output "`n===================================="
Write-Output "Recuperados: $recuperados"
Write-Output "Nao encontrados: $naoEncontrados"
Write-Output "===================================="

