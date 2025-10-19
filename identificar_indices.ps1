# Script para identificar tabelas com indices

Write-Output "=== IDENTIFICANDO TABELAS COM INDICES ===" | Tee-Object -FilePath "indices_tabelas.log"

$tabelasComIndices = @()
$todosIndices = @()

# Buscar todos os arquivos .tabl.xml
$arquivosTabelas = Get-ChildItem -Path "src" -Recurse -Filter "*.tabl.xml"

Write-Output "`nTotal de tabelas encontradas: $($arquivosTabelas.Count)" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "Analisando..." | Tee-Object -FilePath "indices_tabelas.log" -Append

foreach ($arquivo in $arquivosTabelas) {
    try {
        $conteudo = Get-Content $arquivo.FullName -Raw -Encoding UTF8
        
        # Verificar se tem definicao de indice (tag DD12V ou DD12VK)
        if ($conteudo -match '<DD12V>|<abapsource:DD12V>') {
            $nomeTabela = $arquivo.BaseName -replace '\.tabl$', ''
            
            # Extrair nomes de indices
            $indicesEncontrados = [regex]::Matches($conteudo, '<INDEXNAME>([^<]+)</INDEXNAME>')
            
            if ($indicesEncontrados.Count -gt 0) {
                $nomeIndices = @()
                foreach ($match in $indicesEncontrados) {
                    $nomeIndice = $match.Groups[1].Value
                    if ($nomeIndice -and $nomeIndice -ne "") {
                        $nomeIndices += $nomeIndice
                        
                        # Extrair campos do indice
                        $inicio = $conteudo.IndexOf("<INDEXNAME>$nomeIndice</INDEXNAME>")
                        if ($inicio -gt 0) {
                            $bloco = $conteudo.Substring([Math]::Max(0, $inicio - 500), [Math]::Min(1000, $conteudo.Length - $inicio + 500))
                            $campos = [regex]::Matches($bloco, '<FIELDNAME>([^<]+)</FIELDNAME>')
                            $listaCampos = ($campos | ForEach-Object { $_.Groups[1].Value }) -join ", "
                            
                            $todosIndices += [PSCustomObject]@{
                                Tabela = $nomeTabela.ToUpper()
                                Indice = $nomeIndice
                                Campos = $listaCampos
                            }
                        }
                    }
                }
                
                $tabelasComIndices += [PSCustomObject]@{
                    Tabela = $nomeTabela.ToUpper()
                    Arquivo = $arquivo.Name
                    NumIndices = $nomeIndices.Count
                    Indices = ($nomeIndices -join ", ")
                }
                
                Write-Output "  [+] $($nomeTabela.ToUpper()) - $($nomeIndices.Count) indice(s): $($nomeIndices -join ', ')" | Tee-Object -FilePath "indices_tabelas.log" -Append
            }
        }
    } catch {
        Write-Output "  [ERRO] $($arquivo.Name): $_" | Tee-Object -FilePath "indices_tabelas.log" -Append
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "RESUMO:" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "  Total de tabelas analisadas: $($arquivosTabelas.Count)" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "  Tabelas com indices: $($tabelasComIndices.Count)" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "  Total de indices: $($todosIndices.Count)" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "indices_tabelas.log" -Append

# Salvar relatorio detalhado
Write-Output "`n=== RELATORIO DETALHADO DE INDICES ===" | Out-File "relatorio_indices.txt" -Encoding UTF8
Write-Output "Gerado em: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')`n" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append

if ($tabelasComIndices.Count -gt 0) {
    Write-Output "TABELAS COM INDICES ($($tabelasComIndices.Count)):" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
    Write-Output "=" * 80 | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
    
    foreach ($tabela in ($tabelasComIndices | Sort-Object Tabela)) {
        Write-Output "`nTABELA: $($tabela.Tabela)" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
        Write-Output "  Arquivo: $($tabela.Arquivo)" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
        Write-Output "  Numero de indices: $($tabela.NumIndices)" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
        Write-Output "  Indices: $($tabela.Indices)" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
        
        # Detalhar cada indice
        $indicesDaTabela = $todosIndices | Where-Object { $_.Tabela -eq $tabela.Tabela }
        foreach ($idx in $indicesDaTabela) {
            if ($idx.Campos) {
                Write-Output "    -> $($idx.Indice): $($idx.Campos)" | Out-File "relatorio_indices.txt" -Encoding UTF8 -Append
            }
        }
    }
}

Write-Output "`nRelatorio salvo em: relatorio_indices.txt" | Tee-Object -FilePath "indices_tabelas.log" -Append
Write-Output "Log completo em: indices_tabelas.log" | Tee-Object -FilePath "indices_tabelas.log" -Append

# Mostrar estatisticas
Write-Output "`n=== TOP 10 TABELAS COM MAIS INDICES ===" | Tee-Object -FilePath "indices_tabelas.log" -Append
$tabelasComIndices | Sort-Object NumIndices -Descending | Select-Object -First 10 | ForEach-Object {
    Write-Output "  $($_.Tabela): $($_.NumIndices) indices" | Tee-Object -FilePath "indices_tabelas.log" -Append
}

