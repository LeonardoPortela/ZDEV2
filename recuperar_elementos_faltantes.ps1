# Recuperar elementos de dados faltantes
Write-Output "=== RECUPERANDO ELEMENTOS DE DADOS FALTANTES ===`n"

$elementosFaltantes = @(
    'ZXNFE_TPEMIT',
    'ZXNFE_TP_TRANSP',
    'ZXNFE_INDCANALVERDE',
    'ZXNFE_QCARGA',
    'ZXNFE_VCARGA',
    'ZDE_DS_DEPARTAMENTO',
    'ZDE_CK_SEM_RET_GRUPO',
    'ZDE_CK_SEM_RET_PEDIDO',
    'ZCHAR02',
    'ZE_PRAZO'
)

Write-Output "Procurando $($elementosFaltantes.Count) elementos...`n"

$recuperados = 0
$naoEncontrados = @()

foreach ($elem in $elementosFaltantes) {
    $elemLower = $elem.ToLower()
    Write-Output "Procurando: $elem"
    
    $arquivos = git ls-tree -r main --name-only | Select-String -Pattern "/$elemLower\."
    
    if ($arquivos) {
        foreach ($arquivo in $arquivos) {
            $arquivoStr = $arquivo.ToString().Trim()
            Write-Output "  Recuperando: $arquivoStr"
            git checkout main -- $arquivoStr
            $recuperados++
        }
    }
    else {
        Write-Output "  NAO ENCONTRADO na main"
        $naoEncontrados += $elem
    }
}

Write-Output "`n=== RESULTADO ===`n"
Write-Output "Arquivos recuperados: $recuperados"
Write-Output "Objetos nao encontrados: $($naoEncontrados.Count)"

if ($naoEncontrados.Count -gt 0) {
    Write-Output "`nNao encontrados:"
    $naoEncontrados | ForEach-Object { Write-Output "  - $_" }
}

Write-Output "`n=== FIM ==="

