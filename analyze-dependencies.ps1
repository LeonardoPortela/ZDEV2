# Script para mapear dependências de objetos ABAP
param(
    [string]$SourcePath = ".\src"
)

# Lista de objetos base (conforme fornecido)
$baseObjects = @(
    "ZCL_DOC_ELETRONICO",
    "ZIF_DOC_ELETRONICO",
    "ZCL_WEBSERVICE",
    "ZIF_WEBSERVICE",
    "ZBRNFE_DANFE",
    "ZFSD_BUSCA_DANFE",
    "ZDEQUEUE_ALL",
    "ZGRC_LIMPA_REF_MIRO_FISCAL",
    "Z_DETALHAMENTO_CTE",
    "Z_DETALHAMENTO_CTE_IN_MASSA",
    "Z_DETALHAMENTO_CTE_XML",
    "Z_DETALHAMENTO_NFE",
    "Z_GRC_AJUSTA_TP_EMISSAO",
    "Z_GRC_ARQUIVO_DOC",
    "Z_GRC_ARQUIVO_DOC_XML",
    "Z_GRC_DOWNLOAD_XML_PDF",
    "Z_GRC_ENVIA_LEGADO",
    "Z_GRC_GET_STATUS_DOC",
    "Z_GRC_MDFE_AVULSA",
    "Z_GRC_MDFE_LOAD",
    "Z_GRC_MONTA_LINK",
    "Z_GRC_NEW_NFE",
    "Z_GRC_REGISTRA_INF_ZIB_NFE",
    "Z_GRC_REGISTRA_LOG_DOC",
    "Z_GRC_SEND_EMAIL_AUTO",
    "Z_J_1BMFE_CANCEL_EVENT_SEND",
    "Z_J_1B_EVENT_CANCEL_NF_CTE",
    "Z_J_1B_MDFE_CANCEL",
    "Z_J_1B_MDFE_CLOSE",
    "Z_J_1B_MDFE_XML_OUT",
    "Z_J_1B_NF_OBJECT_ADD",
    "Z_SHOW_DETALHAMENTO_CTE",
    "Z_SHOW_DETALHAMENTO_NFE"
)

Write-Host "=== Iniciando Análise de Dependências ===" -ForegroundColor Green
Write-Host "Objetos base: $($baseObjects.Count)" -ForegroundColor Cyan

# Dicionário para armazenar todos os objetos utilizados
$usedObjects = @{}
$processedObjects = @{}
$objectFiles = @{}

# Função para normalizar nome de objeto (remover prefixos de namespace, etc)
function Get-NormalizedObjectName {
    param([string]$name)
    $name = $name.ToUpper().Trim()
    # Remove caracteres especiais comuns
    $name = $name -replace '[<>()"\[\]]', ''
    $name = $name -replace '^\/', ''
    return $name
}

# Criar índice de todos os arquivos por nome de objeto
Write-Host "Indexando todos os arquivos..." -ForegroundColor Yellow
$allFiles = Get-ChildItem -Path $SourcePath -Recurse -File
$totalFiles = $allFiles.Count
$fileIndex = 0

foreach ($file in $allFiles) {
    $fileIndex++
    if ($fileIndex % 5000 -eq 0) {
        Write-Host "  Indexado $fileIndex de $totalFiles arquivos..." -ForegroundColor DarkGray
    }
    
    # Extrair nome do objeto do nome do arquivo
    $fileName = $file.Name
    # Padrão: nome_objeto.tipo.extensão ou nome_objeto.tipo.hash.extensão
    if ($fileName -match '^([a-z0-9_]+)\.(clas|intf|prog|fugr|tabl|dtel|doma|enho|ssfo|ssst|ttyp|shlp|msag|ddls|ddlx|sicf|smim|sush|pdts)') {
        $objectName = Get-NormalizedObjectName $matches[1]
        if (-not $objectFiles.ContainsKey($objectName)) {
            $objectFiles[$objectName] = @()
        }
        $objectFiles[$objectName] += $file.FullName
    }
}

Write-Host "Total de objetos únicos encontrados: $($objectFiles.Count)" -ForegroundColor Cyan

# Função para extrair referências de um arquivo
function Get-ObjectReferences {
    param(
        [string]$filePath
    )
    
    $references = @{}
    
    try {
        $content = Get-Content -Path $filePath -Raw -ErrorAction SilentlyContinue
        
        if ([string]::IsNullOrEmpty($content)) {
            return $references
        }
        
        # Padrões comuns em ABAP para identificar referências
        $patterns = @(
            # Classes e interfaces
            'TYPE\s+REF\s+TO\s+([a-z0-9_]+)',
            'CLASS\s+([a-z0-9_]+)\s+DEFINITION',
            'CLASS\s+([a-z0-9_]+)\s+IMPLEMENTATION',
            'INTERFACE\s+([a-z0-9_]+)',
            'CALL\s+METHOD\s+([a-z0-9_]+)',
            '([a-z0-9_]+)=>',
            # Tabelas e estruturas
            'TABLES?\s*:?\s*([a-z0-9_]+)',
            'FROM\s+([a-z0-9_]+)',
            'INTO\s+TABLE\s+([a-z0-9_]+)',
            'TYPE\s+([a-z0-9_]+)',
            'LIKE\s+([a-z0-9_]+)',
            # Function modules
            'CALL\s+FUNCTION\s+''([A-Z0-9_]+)''',
            'FUNCTION\s+([a-z0-9_]+)',
            # Programas
            'SUBMIT\s+([a-z0-9_]+)',
            'INCLUDE\s+([a-z0-9_]+)',
            # Domínios e elementos de dados
            '<abapName>([a-z0-9_]+)</abapName>',
            '<STRUCOBJN>([a-z0-9_]+)</STRUCOBJN>',
            '<ROLLNAME>([a-z0-9_]+)</ROLLNAME>',
            '<DOMNAME>([a-z0-9_]+)</DOMNAME>',
            '<DATATYPE>([a-z0-9_]+)</DATATYPE>',
            '<REFTYPE>([a-z0-9_]+)</REFTYPE>',
            # XML references
            'name="([a-z0-9_]+)"',
            '<ddlname>([a-z0-9_]+)</ddlname>'
        )
        
        foreach ($pattern in $patterns) {
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
            foreach ($match in $matches) {
                if ($match.Groups.Count -gt 1) {
                    $refName = Get-NormalizedObjectName $match.Groups[1].Value
                    # Filtrar objetos que começam com Z (customizados) ou objetos SAP comuns
                    if ($refName -match '^Z' -or $refName -match '^Y') {
                        $references[$refName] = $true
                    }
                }
            }
        }
    }
    catch {
        Write-Host "  Erro ao processar arquivo $filePath : $_" -ForegroundColor Red
    }
    
    return $references
}

# Função recursiva para processar dependências
function Process-ObjectDependencies {
    param(
        [string]$objectName,
        [int]$level = 0
    )
    
    $normalizedName = Get-NormalizedObjectName $objectName
    
    # Evitar processamento circular
    if ($processedObjects.ContainsKey($normalizedName)) {
        return
    }
    
    $processedObjects[$normalizedName] = $true
    $usedObjects[$normalizedName] = $true
    
    $indent = "  " * $level
    Write-Host "$indent Processando: $normalizedName (Nível $level)" -ForegroundColor Gray
    
    # Encontrar todos os arquivos relacionados a este objeto
    if ($objectFiles.ContainsKey($normalizedName)) {
        foreach ($file in $objectFiles[$normalizedName]) {
            $references = Get-ObjectReferences -filePath $file
            
            foreach ($refName in $references.Keys) {
                if (-not $usedObjects.ContainsKey($refName)) {
                    Write-Host "$indent   -> Encontrada referência: $refName" -ForegroundColor DarkCyan
                    Process-ObjectDependencies -objectName $refName -level ($level + 1)
                }
            }
        }
    }
}

# Processar todos os objetos base
Write-Host "`n=== Processando Objetos Base ===" -ForegroundColor Green
foreach ($baseObj in $baseObjects) {
    Write-Host "`nProcessando objeto base: $baseObj" -ForegroundColor Yellow
    Process-ObjectDependencies -objectName $baseObj -level 0
}

Write-Host "`n=== Análise Completa ===" -ForegroundColor Green
Write-Host "Total de objetos únicos utilizados: $($usedObjects.Count)" -ForegroundColor Cyan

# Identificar objetos não utilizados
$unusedObjects = @{}
foreach ($objName in $objectFiles.Keys) {
    if (-not $usedObjects.ContainsKey($objName)) {
        $unusedObjects[$objName] = $objectFiles[$objName]
    }
}

Write-Host "Total de objetos NÃO utilizados: $($unusedObjects.Count)" -ForegroundColor Yellow

# Salvar resultados
$resultsPath = ".\dependency-analysis-results.txt"
$usedObjectsList = $usedObjects.Keys | Sort-Object
$unusedObjectsList = $unusedObjects.Keys | Sort-Object

@"
=== ANÁLISE DE DEPENDÊNCIAS - ZESAP3 ===
Data: $(Get-Date)

OBJETOS BASE: $($baseObjects.Count)
$($baseObjects | Sort-Object | ForEach-Object { "  - $_" } | Out-String)

OBJETOS UTILIZADOS (direta ou indiretamente): $($usedObjects.Count)
$($usedObjectsList | ForEach-Object { "  - $_" } | Out-String)

OBJETOS NÃO UTILIZADOS: $($unusedObjects.Count)
$($unusedObjectsList | ForEach-Object { "  - $_" } | Out-String)

=== ARQUIVOS PARA EXCLUSÃO ===
Total de arquivos a excluir: $(($unusedObjects.Values | ForEach-Object { $_.Count } | Measure-Object -Sum).Sum)

"@ | Out-File -FilePath $resultsPath -Encoding UTF8

Write-Host "`nResultados salvos em: $resultsPath" -ForegroundColor Green

# Salvar lista de arquivos para exclusão
$deleteListPath = ".\files-to-delete.txt"
$unusedObjects.Values | ForEach-Object { $_ } | Sort-Object | Out-File -FilePath $deleteListPath -Encoding UTF8

Write-Host "Lista de arquivos para exclusão salva em: $deleteListPath" -ForegroundColor Green

# Retornar estatísticas
return @{
    BaseObjects = $baseObjects.Count
    UsedObjects = $usedObjects.Count
    UnusedObjects = $unusedObjects.Count
    TotalFilesToDelete = ($unusedObjects.Values | ForEach-Object { $_.Count } | Measure-Object -Sum).Sum
}

