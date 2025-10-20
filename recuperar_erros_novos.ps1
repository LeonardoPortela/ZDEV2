# Script para recuperar objetos dos erros de ativacao
Write-Output "=== RECUPERANDO OBJETOS DOS ERROS DE ATIVACAO ===`n"

$objetosFaltantes = @(
    'ZTPCLASS',
    'ZCUNID',
    'ZMODAL',
    'ZPRAZO',
    'ZCIOT',
    'ZMMT0072',
    'ZPSQ_LOCAL_ESTOQUE',
    'ZPSQ_TIPO_ENTRADA',
    'ZHELP_ZMMT0072'
)

Write-Output "Objetos a recuperar: $($objetosFaltantes.Count)`n"

$recuperados = 0
$naoEncontrados = @()

foreach ($obj in $objetosFaltantes) {
    $objLower = $obj.ToLower()
    Write-Output "Procurando: $obj..."
    
    $arquivos = git ls-tree -r main --name-only | Select-String -Pattern "/$objLower\."
    
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
        $naoEncontrados += $obj
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
