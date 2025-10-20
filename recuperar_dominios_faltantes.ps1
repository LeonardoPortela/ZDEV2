# Recuperar dominios faltantes
Write-Output "=== RECUPERANDO DOMINIOS FALTANTES ===`n"

$dominiosFaltantes = @(
    'ZDM_CK_SEM_RET_GRUPO',
    'ZDM_CK_SEM_RET_PEDIDO',
    'ZDM_DS_DEPARTAMENTO'
)

Write-Output "Procurando $($dominiosFaltantes.Count) dominios...`n"

$recuperados = 0
$naoEncontrados = @()

foreach ($dom in $dominiosFaltantes) {
    $domLower = $dom.ToLower()
    Write-Output "Procurando: $dom"
    
    $arquivos = git ls-tree -r main --name-only | Select-String -Pattern "/$domLower\."
    
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
        $naoEncontrados += $dom
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

