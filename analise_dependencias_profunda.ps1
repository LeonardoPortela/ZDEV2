# Script de analise profunda e recursiva de dependencias Z

Write-Output "=== INICIANDO ANALISE PROFUNDA DE DEPENDENCIAS ===" | Tee-Object -FilePath "analise_profunda.log"

# Objetos principais (ponto de partida)
$ObjetosPrincipais = @(
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

# Conjunto de todos os objetos Z necessarios (usando HashSet para performance)
$TodosObjetosZ = New-Object System.Collections.Generic.HashSet[string]

# Adicionar objetos principais
foreach ($obj in $ObjetosPrincipais) {
    [void]$TodosObjetosZ.Add($obj.ToUpper())
}

Write-Output "Objetos principais: $($ObjetosPrincipais.Count)" | Tee-Object -FilePath "analise_profunda.log" -Append

# Funcao para extrair referencias Z de arquivo ABAP
function Get-ZReferencesFromABAP {
    param([string]$FilePath)
    
    $referencias = New-Object System.Collections.Generic.HashSet[string]
    
    if (-not (Test-Path $FilePath)) { return $referencias }
    
    $content = Get-Content $FilePath -Raw -ErrorAction SilentlyContinue
    if (-not $content) { return $referencias }
    
    # Patterns para ABAP
    $patterns = @(
        "(?i)\bTYPE\s+(Z[A-Z0-9_]+)",
        "(?i)\bLIKE\s+(Z[A-Z0-9_]+)",
        "(?i)\bTABLES?\s*:\s*(Z[A-Z0-9_]+)",
        "(?i)\bDATA\([A-Z0-9_]+\)\s+TYPE\s+(Z[A-Z0-9_]+)",
        "(?i)\bCONSTANTS?\s+[A-Z0-9_]+\s+TYPE\s+(Z[A-Z0-9_]+)",
        "(?i)\bSTRUCTURE\s+(Z[A-Z0-9_]+)",
        "(?i)\bINCLUDE\s+STRUCTURE\s+(Z[A-Z0-9_]+)",
        "(?i)'(Z[A-Z0-9_]{3,30})'",
        "(?i)FROM\s+(Z[A-Z0-9_]+)",
        "(?i)INTO\s+TABLE\s+(Z[A-Z0-9_]+)",
        "(?i)SELECT\s+.*\s+FROM\s+(Z[A-Z0-9_]+)"
    )
    
    foreach ($pattern in $patterns) {
        $matches = [regex]::Matches($content, $pattern)
        foreach ($match in $matches) {
            if ($match.Groups.Count -gt 1) {
                $ref = $match.Groups[1].Value.ToUpper()
                if ($ref -match "^Z[A-Z0-9_]{2,29}$") {
                    [void]$referencias.Add($ref)
                }
            }
        }
    }
    
    return $referencias
}

# Funcao para extrair referencias Z de arquivo XML (dicionario)
function Get-ZReferencesFromXML {
    param([string]$FilePath)
    
    $referencias = New-Object System.Collections.Generic.HashSet[string]
    
    if (-not (Test-Path $FilePath)) { return $referencias }
    
    try {
        [xml]$xml = Get-Content $FilePath -Raw -ErrorAction Stop
        
        # Extrair de varios campos XML
        $camposParaBuscar = @(
            "//ROLLNAME",
            "//DATATYPE", 
            "//DOMNAME",
            "//REFTYPE",
            "//PRECFIELD",
            "//REFFIELD",
            "//CHECKTABLE",
            "//INCLUDES",
            "//TABNAME",
            "//TYPENAME",
            "//ROWTYPE"
        )
        
        foreach ($campo in $camposParaBuscar) {
            $nodes = $xml.SelectNodes($campo)
            foreach ($node in $nodes) {
                $valor = $node.InnerText.Trim().ToUpper()
                if ($valor -match "^Z[A-Z0-9_]{2,29}$") {
                    [void]$referencias.Add($valor)
                }
            }
        }
        
        # Buscar em atributos tambem
        $todosNos = $xml.SelectNodes("//*")
        foreach ($no in $todosNos) {
            if ($no.HasAttributes) {
                foreach ($attr in $no.Attributes) {
                    $valor = $attr.Value.Trim().ToUpper()
                    if ($valor -match "^Z[A-Z0-9_]{2,29}$") {
                        [void]$referencias.Add($valor)
                    }
                }
            }
        }
        
    } catch {
        # Se nao for XML valido, tentar regex simples
        $content = Get-Content $FilePath -Raw -ErrorAction SilentlyContinue
        if ($content) {
            $matches = [regex]::Matches($content, "(?i)(Z[A-Z0-9_]{3,30})")
            foreach ($match in $matches) {
                $ref = $match.Groups[1].Value.ToUpper()
                if ($ref -match "^Z[A-Z0-9_]{2,29}$") {
                    [void]$referencias.Add($ref)
                }
            }
        }
    }
    
    return $referencias
}

# Funcao para encontrar arquivos de um objeto Z
function Get-FilesForObject {
    param([string]$ObjectName)
    
    $arquivos = @()
    $name = $ObjectName.ToLower()
    
    # Procurar todos os arquivos relacionados
    $patterns = @(
        "$name.clas.abap",
        "$name.clas.xml",
        "$name.clas.locals*",
        "$name.clas.testclasses.abap",
        "$name.intf.abap",
        "$name.intf.xml",
        "$name.prog.abap",
        "$name.prog.xml",
        "$name.fugr.*",
        "$name.func.abap",
        "$name.func.xml",
        "$name.tabl.xml",
        "$name.dtel.xml",
        "$name.doma.xml",
        "$name.ttyp.xml",
        "$name.shlp.xml",
        "$name.msag.xml",
        "$name.enqu.xml",
        "$name.ssfo.xml",
        "$name.ssst.xml"
    )
    
    foreach ($pattern in $patterns) {
        $found = Get-ChildItem -Path "src" -Recurse -File -Filter $pattern -ErrorAction SilentlyContinue
        if ($found) {
            $arquivos += $found
        }
    }
    
    return $arquivos
}

Write-Output "`n=== FASE 1: ANALISE RECURSIVA DE DEPENDENCIAS ===" | Tee-Object -FilePath "analise_profunda.log" -Append

$iteracao = 0
$continuar = $true

while ($continuar) {
    $iteracao++
    $tamanhoAnterior = $TodosObjetosZ.Count
    
    Write-Output "`nIteracao $iteracao - Objetos atuais: $tamanhoAnterior" | Tee-Object -FilePath "analise_profunda.log" -Append
    
    $objetosParaAnalisar = @($TodosObjetosZ)
    
    foreach ($objeto in $objetosParaAnalisar) {
        # Encontrar arquivos do objeto
        $arquivos = Get-FilesForObject -ObjectName $objeto
        
        foreach ($arquivo in $arquivos) {
            $ext = $arquivo.Extension
            $referencias = $null
            
            if ($ext -eq ".abap") {
                $referencias = Get-ZReferencesFromABAP -FilePath $arquivo.FullName
            } elseif ($ext -eq ".xml") {
                $referencias = Get-ZReferencesFromXML -FilePath $arquivo.FullName
            }
            
            if ($referencias) {
                foreach ($ref in $referencias) {
                    if ($TodosObjetosZ.Add($ref)) {
                        Write-Output "  [+] Nova dependencia encontrada: $ref (de $($arquivo.Name))" | Tee-Object -FilePath "analise_profunda.log" -Append
                    }
                }
            }
        }
    }
    
    $tamanhoNovo = $TodosObjetosZ.Count
    
    if ($tamanhoNovo -eq $tamanhoAnterior) {
        Write-Output "`nNenhuma nova dependencia encontrada. Analise completa!" | Tee-Object -FilePath "analise_profunda.log" -Append
        $continuar = $false
    } else {
        Write-Output "Encontrados $($tamanhoNovo - $tamanhoAnterior) novos objetos" | Tee-Object -FilePath "analise_profunda.log" -Append
    }
    
    if ($iteracao -gt 20) {
        Write-Output "`nLimite de iteracoes atingido (20)" | Tee-Object -FilePath "analise_profunda.log" -Append
        $continuar = $false
    }
}

Write-Output "`n=== RESULTADO FINAL ===" | Tee-Object -FilePath "analise_profunda.log" -Append
Write-Output "Total de objetos Z necessarios: $($TodosObjetosZ.Count)" | Tee-Object -FilePath "analise_profunda.log" -Append

# Salvar lista completa
$TodosObjetosZ | Sort-Object | Out-File "objetos_z_necessarios.txt" -Encoding UTF8

Write-Output "`nLista salva em: objetos_z_necessarios.txt" | Tee-Object -FilePath "analise_profunda.log" -Append
Write-Output "`nConcluido!"


