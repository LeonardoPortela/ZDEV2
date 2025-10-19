# Script para remover TODOS os indices de TODAS as tabelas

Write-Output "=== REMOVENDO INDICES DE TABELAS ===" | Tee-Object -FilePath "remocao_indices.log"

$arquivosTabelas = Get-ChildItem -Path "src" -Recurse -Filter "*.tabl.xml"
$tabelasModificadas = 0
$indicesRemovidos = 0

Write-Output "`nTotal de tabelas a processar: $($arquivosTabelas.Count)" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "Processando..." | Tee-Object -FilePath "remocao_indices.log" -Append

foreach ($arquivo in $arquivosTabelas) {
    try {
        $conteudo = Get-Content $arquivo.FullName -Raw -Encoding UTF8
        $conteudoOriginal = $conteudo
        
        # Contar indices antes da remocao
        $indicesAntes = ([regex]::Matches($conteudo, '<DD12V>|<abapsource:DD12V>')).Count
        
        if ($indicesAntes -gt 0) {
            # Remover blocos DD12V (definicoes de indices)
            # Pattern: de <DD12V> ate </DD12V> (incluindo namespaces)
            $conteudo = $conteudo -replace '<(abapsource:)?DD12V>[\s\S]*?</(abapsource:)?DD12V>', ''
            
            # Remover blocos DD12VK (campos de indices)
            # Pattern: de <DD12VK> ate </DD12VK>
            $conteudo = $conteudo -replace '<(abapsource:)?DD12VK>[\s\S]*?</(abapsource:)?DD12VK>', ''
            
            # Remover linhas vazias extras deixadas pela remocao
            $conteudo = $conteudo -replace '(\r?\n){3,}', "`r`n`r`n"
            
            # Verificar se realmente houve mudanca
            if ($conteudo -ne $conteudoOriginal) {
                Set-Content -Path $arquivo.FullName -Value $conteudo -Encoding UTF8 -NoNewline
                $tabelasModificadas++
                $indicesRemovidos += $indicesAntes
                
                $nomeTabela = $arquivo.BaseName -replace '\.tabl$', ''
                Write-Output "  [MODIFICADO] $($nomeTabela.ToUpper()) - $indicesAntes indice(s) removido(s)" | Tee-Object -FilePath "remocao_indices.log" -Append
            }
        }
    } catch {
        Write-Output "  [ERRO] $($arquivo.Name): $_" | Tee-Object -FilePath "remocao_indices.log" -Append
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "RESULTADO:" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "  Tabelas processadas: $($arquivosTabelas.Count)" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "  Tabelas modificadas: $tabelasModificadas" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "  Total de indices removidos: $indicesRemovidos" | Tee-Object -FilePath "remocao_indices.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "remocao_indices.log" -Append

Write-Output "`nConcluido! Execute 'git status' para ver as mudancas."

