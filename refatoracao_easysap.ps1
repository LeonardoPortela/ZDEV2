# Script de Refatoração EASYSAP - Adicionar prefixo ZES a todos os objetos SAP
# Autor: Sistema de Refatoração Automática
# Data: 2025-10-31

$ErrorActionPreference = "Continue"
$ProgressPreference = "SilentlyContinue"

# Configurações
$pastaOrigem = "src"
$pastaDestino = "EASYSAP_REFATORADO\src"
$logFile = "refatoracao_easysap.log"
$mappingFile = "mapeamento_objetos.csv"

# Função para log
function Write-Log {
    param($Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "$timestamp - $Message"
    Write-Host $logMessage
    Add-Content -Path $logFile -Value $logMessage
}

# Limpar log anterior
if (Test-Path $logFile) { Remove-Item $logFile }

Write-Log "=== INICIANDO REFATORAÇÃO EASYSAP ==="
Write-Log "Origem: $pastaOrigem"
Write-Log "Destino: $pastaDestino"

# Criar pasta destino
if (Test-Path $pastaDestino) {
    Write-Log "Removendo pasta destino existente..."
    Remove-Item -Path $pastaDestino -Recurse -Force
}
Write-Log "Criando pasta destino..."
New-Item -ItemType Directory -Path $pastaDestino -Force | Out-Null
New-Item -ItemType Directory -Path "EASYSAP_REFATORADO" -Force | Out-Null

# Etapa 1: Identificar todos os objetos SAP
Write-Log "=== ETAPA 1: IDENTIFICANDO OBJETOS SAP ==="

# Obter caminho completo da pasta origem
$pastaOrigemCompleta = (Get-Item $pastaOrigem).FullName
Write-Log "Caminho origem completo: $pastaOrigemCompleta"

$arquivos = Get-ChildItem -Path $pastaOrigem -Recurse -File
$totalArquivos = $arquivos.Count
Write-Log "Total de arquivos encontrados: $totalArquivos"

# Criar mapeamento de objetos
$mapeamento = @{}
$contador = 0

foreach ($arquivo in $arquivos) {
    $contador++
    if ($contador % 100 -eq 0) {
        Write-Log "Processando arquivo $contador de $totalArquivos..."
    }
    
    $nomeArquivo = $arquivo.Name
    
    # Extrair nome do objeto (parte antes da primeira extensão)
    if ($nomeArquivo -match '^([a-zA-Z0-9_]+)\.') {
        $nomeObjeto = $matches[1].ToUpper()
        
        # Se começa com Z, adicionar prefixo ZES
        if ($nomeObjeto -match '^Z[A-Z0-9_]+$') {
            $novoNome = "ZES" + $nomeObjeto
            
            if (-not $mapeamento.ContainsKey($nomeObjeto)) {
                $mapeamento[$nomeObjeto] = $novoNome
            }
        }
        # Se começa com EZ, substituir por ZES
        elseif ($nomeObjeto -match '^EZ[A-Z0-9_]+$') {
            $novoNome = "ZES" + $nomeObjeto.Substring(1)
            
            if (-not $mapeamento.ContainsKey($nomeObjeto)) {
                $mapeamento[$nomeObjeto] = $novoNome
            }
        }
    }
}

Write-Log "Total de objetos identificados para renomear: $($mapeamento.Count)"

# Salvar mapeamento em CSV
$mapeamento.GetEnumerator() | 
    Select-Object @{Name='ObjetoOriginal';Expression={$_.Key}}, @{Name='ObjetoNovo';Expression={$_.Value}} |
    Export-Csv -Path $mappingFile -NoTypeInformation -Encoding UTF8

Write-Log "Mapeamento salvo em: $mappingFile"

# Etapa 2: Copiar e renomear arquivos
Write-Log "=== ETAPA 2: COPIANDO E RENOMEANDO ARQUIVOS ==="
$contador = 0

foreach ($arquivo in $arquivos) {
    $contador++
    if ($contador % 100 -eq 0) {
        Write-Log "Copiando arquivo $contador de $totalArquivos..."
    }
    
    # Calcular caminho relativo corretamente
    $caminhoRelativo = $arquivo.FullName.Substring($pastaOrigemCompleta.Length + 1)
    $caminhoDestino = Join-Path $pastaDestino $caminhoRelativo
    
    # Criar diretório destino se não existir
    $diretorioDestino = Split-Path $caminhoDestino -Parent
    if (-not (Test-Path $diretorioDestino)) {
        New-Item -ItemType Directory -Path $diretorioDestino -Force | Out-Null
    }
    
    # Renomear arquivo se necessário
    $nomeArquivoNovo = $arquivo.Name
    foreach ($obj in $mapeamento.GetEnumerator()) {
        if ($nomeArquivoNovo -match "^$($obj.Key)\." -or $nomeArquivoNovo -match "\.$($obj.Key)\.") {
            $nomeArquivoNovo = $nomeArquivoNovo -replace $obj.Key, $obj.Value
        }
    }
    
    $caminhoDestinoFinal = Join-Path (Split-Path $caminhoDestino -Parent) $nomeArquivoNovo
    
    # Copiar arquivo
    Copy-Item -Path $arquivo.FullName -Destination $caminhoDestinoFinal -Force
}

Write-Log "=== ETAPA 3: SUBSTITUINDO REFERÊNCIAS NOS ARQUIVOS ==="
Write-Log "Processando substituições em todos os arquivos..."

# Criar lista de objetos ordenada por tamanho (maior para menor) para evitar substituições parciais
$objetosOrdenados = $mapeamento.GetEnumerator() | Sort-Object { $_.Key.Length } -Descending

$arquivosDestino = Get-ChildItem -Path $pastaDestino -Recurse -File
$totalArquivosDestino = $arquivosDestino.Count
$contador = 0

foreach ($arquivo in $arquivosDestino) {
    $contador++
    if ($contador % 50 -eq 0) {
        Write-Log "Substituindo em arquivo $contador de $totalArquivosDestino..."
    }
    
    try {
        # Ler conteúdo do arquivo
        $conteudo = Get-Content -Path $arquivo.FullName -Raw -Encoding UTF8
        $modificado = $false
        
        # Substituir cada referência de objeto
        foreach ($obj in $objetosOrdenados) {
            $pattern = $obj.Key
            $replacement = $obj.Value
            
            if ($conteudo -match $pattern) {
                $conteudo = $conteudo -replace "\b$pattern\b", $replacement
                $modificado = $true
            }
        }
        
        # Salvar se foi modificado
        if ($modificado) {
            Set-Content -Path $arquivo.FullName -Value $conteudo -Encoding UTF8 -NoNewline
        }
    }
    catch {
        Write-Log "ERRO ao processar arquivo $($arquivo.FullName): $_"
    }
}

Write-Log "=== REFATORAÇÃO CONCLUÍDA ==="
Write-Log "Total de objetos renomeados: $($mapeamento.Count)"
Write-Log "Total de arquivos processados: $totalArquivosDestino"
Write-Log "Pasta destino: $pastaDestino"
Write-Log "Log completo: $logFile"
Write-Log "Mapeamento: $mappingFile"

Write-Host "`n=== RESUMO ==="
Write-Host "Objetos renomeados: $($mapeamento.Count)"
Write-Host "Arquivos processados: $totalArquivosDestino"
Write-Host "Destino: $pastaDestino"
Write-Host "`nRefatoração concluída com sucesso!"

