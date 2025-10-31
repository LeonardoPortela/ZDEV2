# Script de Monitoramento da Refatoração
# Monitora o arquivo de log e exibe o status

$logFile = "refatoracao_easysap.log"
$ultimaLinha = ""

Write-Host "Monitorando refatoração..." -ForegroundColor Cyan
Write-Host "Pressione Ctrl+C para sair" -ForegroundColor Yellow
Write-Host ""

while ($true) {
    if (Test-Path $logFile) {
        $linhas = Get-Content $logFile -Tail 5
        $ultimaLinhaAtual = $linhas[-1]
        
        if ($ultimaLinhaAtual -ne $ultimaLinha) {
            Clear-Host
            Write-Host "=== MONITORAMENTO REFATORAÇÃO EASYSAP ===" -ForegroundColor Cyan
            Write-Host ""
            
            # Exibir últimas 10 linhas
            $ultimasLinhas = Get-Content $logFile -Tail 10
            foreach ($linha in $ultimasLinhas) {
                if ($linha -match "ERRO") {
                    Write-Host $linha -ForegroundColor Red
                }
                elseif ($linha -match "CONCLUÍDA" -or $linha -match "sucesso") {
                    Write-Host $linha -ForegroundColor Green
                }
                elseif ($linha -match "ETAPA") {
                    Write-Host $linha -ForegroundColor Yellow
                }
                else {
                    Write-Host $linha
                }
            }
            
            # Verificar se concluiu
            if ($ultimaLinhaAtual -match "REFATORAÇÃO CONCLUÍDA") {
                Write-Host ""
                Write-Host "=== REFATORAÇÃO CONCLUÍDA! ===" -ForegroundColor Green
                Write-Host ""
                
                # Exibir resumo
                $conteudo = Get-Content $logFile
                $objetosRenomeados = ($conteudo | Select-String "Total de objetos renomeados: (\d+)").Matches[0].Groups[1].Value
                $arquivosProcessados = ($conteudo | Select-String "Total de arquivos processados: (\d+)").Matches[0].Groups[1].Value
                
                Write-Host "Resumo:" -ForegroundColor Cyan
                Write-Host "  Objetos renomeados: $objetosRenomeados" -ForegroundColor White
                Write-Host "  Arquivos processados: $arquivosProcessados" -ForegroundColor White
                Write-Host ""
                Write-Host "Para continuar com o Git, execute: .\git_easysap.ps1" -ForegroundColor Yellow
                
                break
            }
            
            $ultimaLinha = $ultimaLinhaAtual
        }
    }
    else {
        Write-Host "Aguardando início da refatoração..." -ForegroundColor Yellow
    }
    
    Start-Sleep -Seconds 5
}

