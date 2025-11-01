# Script de Verificação do Conteúdo XML - Refatoração EASYSAP
# Verifica se os arquivos foram renomeados E se as referências internas foram atualizadas

$ErrorActionPreference = "Continue"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  VERIFICAÇÃO DE CONTEÚDO XML EASYSAP  " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Carregar mapeamento
if (-not (Test-Path "mapeamento_objetos.csv")) {
    Write-Host "ERRO: Arquivo de mapeamento não encontrado!" -ForegroundColor Red
    exit 1
}

$mapeamento = Import-Csv "mapeamento_objetos.csv"
Write-Host "✓ Mapeamento carregado: $($mapeamento.Count) objetos" -ForegroundColor Green
Write-Host ""

# Selecionar 20 objetos aleatórios para verificação detalhada
$amostra = $mapeamento | Get-Random -Count 20

Write-Host "=== VERIFICAÇÃO DETALHADA DE AMOSTRA ===" -ForegroundColor Yellow
Write-Host "Verificando 20 objetos aleatórios..." -ForegroundColor Yellow
Write-Host ""

$sucessos = 0
$erros = 0

foreach ($item in $amostra) {
    $nomeOriginal = $item.ObjetoOriginal
    $nomeNovo = $item.ObjetoNovo
    
    # Procurar arquivo original
    $arquivoOriginal = Get-ChildItem -Path "src" -Recurse -File | Where-Object { $_.Name -match "^$nomeOriginal\." } | Select-Object -First 1
    
    if (-not $arquivoOriginal) {
        continue
    }
    
    # Construir nome do arquivo novo
    $nomeArquivoNovo = $arquivoOriginal.Name -replace $nomeOriginal, $nomeNovo
    
    # Procurar arquivo refatorado
    $arquivoNovo = Get-ChildItem -Path "EASYSAP_REFATORADO\src" -Recurse -File | Where-Object { $_.Name -eq $nomeArquivoNovo } | Select-Object -First 1
    
    if (-not $arquivoNovo) {
        Write-Host "  ✗ Arquivo não encontrado: $nomeArquivoNovo" -ForegroundColor Red
        $erros++
        continue
    }
    
    # Ler conteúdo dos arquivos
    $conteudoOriginal = Get-Content -Path $arquivoOriginal.FullName -Raw -Encoding UTF8
    $conteudoNovo = Get-Content -Path $arquivoNovo.FullName -Raw -Encoding UTF8
    
    # Verificações
    $conteudoAtualizado = $conteudoNovo -match $nomeNovo
    $referenciaRemovida = $conteudoNovo -notmatch "\b$nomeOriginal\b"
    
    if ($conteudoAtualizado -and $referenciaRemovida) {
        Write-Host "  ✓ $nomeArquivoNovo" -ForegroundColor Green
        Write-Host "    - Arquivo renomeado: ✓" -ForegroundColor Gray
        Write-Host "    - Conteúdo atualizado: ✓" -ForegroundColor Gray
        Write-Host "    - Referências antigas removidas: ✓" -ForegroundColor Gray
        $sucessos++
    }
    else {
        Write-Host "  ✗ $nomeArquivoNovo" -ForegroundColor Red
        Write-Host "    - Arquivo renomeado: ✓" -ForegroundColor Gray
        Write-Host "    - Conteúdo atualizado: $conteudoAtualizado" -ForegroundColor Yellow
        Write-Host "    - Referências antigas removidas: $referenciaRemovida" -ForegroundColor Yellow
        $erros++
    }
    
    Write-Host ""
}

Write-Host ""
Write-Host "=== VERIFICAÇÃO DE REFERÊNCIAS CRUZADAS ===" -ForegroundColor Yellow
Write-Host "Verificando se objetos renomeados são referenciados corretamente em outros arquivos..." -ForegroundColor Yellow
Write-Host ""

# Pegar 5 objetos para verificar referências cruzadas
$amostraReferencias = $mapeamento | Get-Random -Count 5

foreach ($item in $amostraReferencias) {
    $nomeOriginal = $item.ObjetoOriginal
    $nomeNovo = $item.ObjetoNovo
    
    Write-Host "Objeto: $nomeOriginal → $nomeNovo" -ForegroundColor Cyan
    
    # Buscar em amostra de 100 arquivos
    $arquivosComReferencia = Get-ChildItem -Path "EASYSAP_REFATORADO\src" -Recurse -File -Filter "*.xml" | 
        Select-Object -First 100 | 
        Where-Object { 
            $conteudo = Get-Content $_.FullName -Raw -Encoding UTF8 -ErrorAction SilentlyContinue
            $conteudo -match "\b$nomeNovo\b"
        }
    
    if ($arquivosComReferencia) {
        Write-Host "  ✓ Encontrado em $($arquivosComReferencia.Count) arquivo(s)" -ForegroundColor Green
        
        # Verificar se ainda existem referências ao nome antigo
        $arquivosComReferenciaAntiga = $arquivosComReferencia | 
            Where-Object { 
                $conteudo = Get-Content $_.FullName -Raw -Encoding UTF8 -ErrorAction SilentlyContinue
                $conteudo -match "\b$nomeOriginal\b"
            }
        
        if ($arquivosComReferenciaAntiga) {
            Write-Host "  ⚠ ATENÇÃO: $($arquivosComReferenciaAntiga.Count) arquivo(s) ainda contém referência ao nome antigo!" -ForegroundColor Yellow
        }
        else {
            Write-Host "  ✓ Nenhuma referência ao nome antigo encontrada" -ForegroundColor Green
        }
    }
    Write-Host ""
}

Write-Host ""
Write-Host "=== ESTATÍSTICAS GERAIS ===" -ForegroundColor Yellow

# Contar arquivos com prefixo ZES
$arquivosZES = Get-ChildItem -Path "EASYSAP_REFATORADO\src" -Recurse -File | Where-Object { $_.Name -match "^ZES" }
$totalArquivosZES = $arquivosZES.Count

Write-Host "Total de arquivos com prefixo ZES: $totalArquivosZES" -ForegroundColor White

# Verificar alguns arquivos XML específicos para exemplos
Write-Host ""
Write-Host "=== EXEMPLOS DE CONTEÚDO XML ===" -ForegroundColor Yellow

# Pegar 3 arquivos XML aleatórios e mostrar snippet
$exemplosXML = $arquivosZES | Where-Object { $_.Extension -eq ".xml" } | Get-Random -Count 3

foreach ($exemplo in $exemplosXML) {
    Write-Host ""
    Write-Host "Arquivo: $($exemplo.Name)" -ForegroundColor Cyan
    
    $conteudo = Get-Content -Path $exemplo.FullName -Raw -Encoding UTF8
    
    # Extrair as primeiras tags relevantes usando [xml]
    try {
        $xmlDoc = [xml]$conteudo
        
        # Tentar encontrar diferentes tipos de tags
        if ($xmlDoc.SelectSingleNode("//TABNAME")) {
            Write-Host "  TABNAME: $($xmlDoc.SelectSingleNode('//TABNAME').'#text')" -ForegroundColor White
        }
        if ($xmlDoc.SelectSingleNode("//ROLLNAME")) {
            Write-Host "  ROLLNAME: $($xmlDoc.SelectSingleNode('//ROLLNAME').'#text')" -ForegroundColor White
        }
        if ($xmlDoc.SelectSingleNode("//DOMNAME")) {
            Write-Host "  DOMNAME: $($xmlDoc.SelectSingleNode('//DOMNAME').'#text')" -ForegroundColor White
        }
        if ($xmlDoc.SelectSingleNode("//CLASSNAME")) {
            Write-Host "  CLASSNAME: $($xmlDoc.SelectSingleNode('//CLASSNAME').'#text')" -ForegroundColor White
        }
        if ($xmlDoc.SelectSingleNode("//CLAS_NAME")) {
            Write-Host "  CLAS_NAME: $($xmlDoc.SelectSingleNode('//CLAS_NAME').'#text')" -ForegroundColor White
        }
    }
    catch {
        Write-Host "  (Não foi possível parsear como XML)" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "    VERIFICAÇÃO CONCLUÍDA              " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Resumo da Amostra Verificada:" -ForegroundColor White
Write-Host "  ✓ Sucessos: $sucessos" -ForegroundColor Green
Write-Host "  ✗ Erros: $erros" -ForegroundColor $(if($erros -gt 0){"Red"}else{"Green"})
Write-Host "  Total verificado: $($sucessos + $erros)" -ForegroundColor White
Write-Host ""

if ($erros -eq 0) {
    Write-Host "✓ TODOS OS ARQUIVOS VERIFICADOS ESTÃO CORRETOS!" -ForegroundColor Green
}
else {
    Write-Host "⚠ Alguns arquivos podem precisar de revisão." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Arquivos totais com prefixo ZES: $totalArquivosZES" -ForegroundColor Cyan
Write-Host ""
