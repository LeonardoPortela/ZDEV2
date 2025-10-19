# Script para corrigir XMLs quebrados apos remocao de indices

Write-Output "=== CORRIGINDO XMLs QUEBRADOS ===" | Tee-Object -FilePath "correcao_xml.log"

$tabelasComErro = @(
    'zact_tmp', 'zcarta_correcao', 'zcte_ciot', 'zcte_identifica', 'zcte_seguro',
    'zevent_tmp', 'zhcmt0007', 'zib_autorit_grc', 'zib_cancel_grc', 'zib_cte_dist_001',
    'zib_cte_dist_c57', 'zib_cte_dist_n01', 'zib_cte_dist_n55', 'zib_cte_dist_ter',
    'zib_dfe_erro', 'zib_nfe_dist_itm', 'zib_nfe_dist_ter', 'zib_nfe_forn',
    'zlest0056', 'zlest0060', 'zlest0061', 'zlest0073', 'zlest0104', 'zlest0143',
    'zpfe_lote', 'zpfe_lote_item', 'zsdt0001', 'zsdt0051', 'zsdt0102', 'zsdt0105',
    'zsdt0118', 'zsdt0127', 'zsdt0133', 'zsdt0231', 'zsdt_retlote'
)

$corrigidos = 0

foreach ($tabela in $tabelasComErro) {
    $arquivo = "src\$tabela.tabl.xml"
    
    if (Test-Path $arquivo) {
        try {
            Write-Output "Processando: $tabela" | Tee-Object -FilePath "correcao_xml.log" -Append
            
            $conteudo = Get-Content $arquivo -Raw -Encoding UTF8
            
            # Verificar se tem tag DD12V orphan
            if ($conteudo -match '</DD12V>') {
                Write-Output "  [!] Encontrada tag </DD12V> orphan" | Tee-Object -FilePath "correcao_xml.log" -Append
                
                # Remover tags </DD12V> orphans (fechamento sem abertura)
                $conteudo = $conteudo -replace '</DD12V>\s*', ''
                $conteudo = $conteudo -replace '</abapsource:DD12V>\s*', ''
                
                # Limpar linhas vazias extras
                $conteudo = $conteudo -replace '(\r?\n){3,}', "`r`n`r`n"
                
                Set-Content -Path $arquivo -Value $conteudo -Encoding UTF8 -NoNewline
                $corrigidos++
                Write-Output "  [OK] Corrigido!" | Tee-Object -FilePath "correcao_xml.log" -Append
            } else {
                Write-Output "  [INFO] Sem tags orphan" | Tee-Object -FilePath "correcao_xml.log" -Append
            }
            
        } catch {
            Write-Output "  [ERRO] $_" | Tee-Object -FilePath "correcao_xml.log" -Append
        }
    } else {
        Write-Output "  [!] Arquivo nao encontrado: $arquivo" | Tee-Object -FilePath "correcao_xml.log" -Append
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "correcao_xml.log" -Append
Write-Output "Total de arquivos corrigidos: $corrigidos" | Tee-Object -FilePath "correcao_xml.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "correcao_xml.log" -Append

