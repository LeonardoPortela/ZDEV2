# Script para renomear objetos com conflito de pacote e atualizar todas referencias

Write-Output "=== RENOMEANDO OBJETOS COM CONFLITO DE PACOTE ===" | Tee-Object -FilePath "renomear.log"

# Mapeamento: NomeAntigo -> NomeNovo
$renomeacoes = @{
    "ZDE_CHAVE" = "ZESAP_DE_CHAVE"
    "ZDE_CONTADOR" = "ZESAP_CONTADOR"
    "ZPLACA" = "ZESAP_PLACA"
    "ZPROD" = "ZESAP_PROD"
}

# FASE 1: Renomear os arquivos dos objetos
Write-Output "`n=== FASE 1: Renomeando arquivos dos objetos ===" | Tee-Object -FilePath "renomear.log" -Append

foreach ($antigo in $renomeacoes.Keys) {
    $novo = $renomeacoes[$antigo]
    $antigoLower = $antigo.ToLower()
    $novoLower = $novo.ToLower()
    
    Write-Output "`nProcessando: $antigo -> $novo" | Tee-Object -FilePath "renomear.log" -Append
    
    # Procurar todos os arquivos do objeto antigo
    $arquivos = Get-ChildItem -Path "src" -Recurse -File | Where-Object { $_.Name -match "^$antigoLower\." }
    
    foreach ($arquivo in $arquivos) {
        $nomeArquivoNovo = $arquivo.Name -replace "^$antigoLower\.", "$novoLower."
        $caminhoNovo = Join-Path $arquivo.Directory.FullName $nomeArquivoNovo
        
        Write-Output "  Renomeando arquivo: $($arquivo.Name) -> $nomeArquivoNovo" | Tee-Object -FilePath "renomear.log" -Append
        
        # Renomear arquivo
        Move-Item -Path $arquivo.FullName -Destination $caminhoNovo -Force
        
        # Atualizar conteudo interno do arquivo
        if ($caminhoNovo -match '\.(xml|abap)$') {
            $conteudo = Get-Content $caminhoNovo -Raw -Encoding UTF8
            $conteudoNovo = $conteudo -replace "\b$antigo\b", $novo
            Set-Content -Path $caminhoNovo -Value $conteudoNovo -Encoding UTF8 -NoNewline
            Write-Output "    Conteudo interno atualizado" | Tee-Object -FilePath "renomear.log" -Append
        }
    }
}

# FASE 2: Atualizar referencias em TODOS os arquivos do repositorio
Write-Output "`n=== FASE 2: Atualizando referencias em todos os arquivos ===" | Tee-Object -FilePath "renomear.log" -Append

$todosArquivos = Get-ChildItem -Path "src" -Recurse -File -Include "*.xml","*.abap"
$totalArquivos = $todosArquivos.Count
$contador = 0
$arquivosModificados = 0

foreach ($arquivo in $todosArquivos) {
    $contador++
    
    if ($contador % 100 -eq 0) {
        Write-Output "  Processados: $contador/$totalArquivos arquivos..." | Tee-Object -FilePath "renomear.log" -Append
    }
    
    try {
        $conteudo = Get-Content $arquivo.FullName -Raw -Encoding UTF8
        $conteudoOriginal = $conteudo
        
        # Substituir cada referencia
        foreach ($antigo in $renomeacoes.Keys) {
            $novo = $renomeacoes[$antigo]
            
            # Substituir com word boundary para evitar substituicoes parciais
            $conteudo = $conteudo -replace "\b$antigo\b", $novo
        }
        
        # Se houve mudanca, salvar
        if ($conteudo -ne $conteudoOriginal) {
            Set-Content -Path $arquivo.FullName -Value $conteudo -Encoding UTF8 -NoNewline
            $arquivosModificados++
            Write-Output "  [MODIFICADO] $($arquivo.FullName.Replace((Get-Location).Path, ''))" | Tee-Object -FilePath "renomear.log" -Append
        }
    } catch {
        Write-Output "  [ERRO] $($arquivo.FullName): $_" | Tee-Object -FilePath "renomear.log" -Append
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "renomear.log" -Append
Write-Output "RESULTADO:" | Tee-Object -FilePath "renomear.log" -Append
Write-Output "  Total de arquivos analisados: $totalArquivos" | Tee-Object -FilePath "renomear.log" -Append
Write-Output "  Arquivos modificados: $arquivosModificados" | Tee-Object -FilePath "renomear.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "renomear.log" -Append

Write-Output "`nRenomeacoes realizadas:" | Tee-Object -FilePath "renomear.log" -Append
foreach ($antigo in $renomeacoes.Keys) {
    Write-Output "  $antigo -> $($renomeacoes[$antigo])" | Tee-Object -FilePath "renomear.log" -Append
}

Write-Output "`nConcluido! Execute 'git status' para ver as mudancas." | Tee-Object -FilePath "renomear.log" -Append

