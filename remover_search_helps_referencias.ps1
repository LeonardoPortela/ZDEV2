# Script para remover referencias a search helps dos XMLs
Write-Output "=== REMOVENDO REFERENCIAS A SEARCH HELPS ===`n"

$arquivosComShlp = @(
    'src\zde_forn_cnpj.dtel.xml',
    'src\zde_forn_insc_es.dtel.xml',
    'src\zde_nfe_entrada.tabl.xml',
    'src\zib_cte_dist_ter.tabl.xml',
    'src\zmmt0072.tabl.xml',
    'src\zsdt0001.tabl.xml',
    'src\zsdt_export.tabl.xml'
)

$totalModificados = 0

foreach ($arquivo in $arquivosComShlp) {
    if (Test-Path $arquivo) {
        Write-Output "Processando: $arquivo"
        
        $conteudo = Get-Content $arquivo -Raw
        $conteudoOriginal = $conteudo
        
        # Remover blocos DD08V completos (search help definition)
        $conteudo = $conteudo -replace '(?s)<DD08V>.*?</DD08V>', ''
        
        # Remover linhas vazias extras
        $conteudo = $conteudo -replace '(\r?\n\s*){3,}', "`r`n`r`n"
        
        if ($conteudo -ne $conteudoOriginal) {
            Set-Content -Path $arquivo -Value $conteudo -NoNewline -Encoding UTF8
            Write-Output "  Modificado!"
            $totalModificados++
        }
        else {
            Write-Output "  Sem alteracoes"
        }
    }
    else {
        Write-Output "  Arquivo nao encontrado"
    }
}

Write-Output "`n=== RESULTADO ===`n"
Write-Output "Arquivos modificados: $totalModificados"
Write-Output "`n=== FIM ==="

