# Analise recursiva COMPLETA - incluindo dominios, search helps e includes

Write-Output "=== ANALISE RECURSIVA COMPLETA V2 ===" | Tee-Object -FilePath "analise_v2.log"

$ObjetosPrincipais = @(
    "ZCL_DOC_ELETRONICO", "ZIF_DOC_ELETRONICO", "ZCL_WEBSERVICE", "ZIF_WEBSERVICE",
    "ZBRNFE_DANFE", "ZFSD_BUSCA_DANFE", "ZDEQUEUE_ALL", "ZGRC_LIMPA_REF_MIRO_FISCAL",
    "Z_DETALHAMENTO_CTE", "Z_DETALHAMENTO_CTE_IN_MASSA", "Z_DETALHAMENTO_CTE_XML",
    "Z_DETALHAMENTO_NFE", "Z_GRC_AJUSTA_TP_EMISSAO", "Z_GRC_ARQUIVO_DOC",
    "Z_GRC_ARQUIVO_DOC_XML", "Z_GRC_DOWNLOAD_XML_PDF", "Z_GRC_ENVIA_LEGADO",
    "Z_GRC_GET_STATUS_DOC", "Z_GRC_MDFE_AVULSA", "Z_GRC_MDFE_LOAD",
    "Z_GRC_MONTA_LINK", "Z_GRC_NEW_NFE", "Z_GRC_REGISTRA_INF_ZIB_NFE",
    "Z_GRC_REGISTRA_LOG_DOC", "Z_GRC_SEND_EMAIL_AUTO", "Z_J_1BMFE_CANCEL_EVENT_SEND",
    "Z_J_1B_EVENT_CANCEL_NF_CTE", "Z_J_1B_MDFE_CANCEL", "Z_J_1B_MDFE_CLOSE",
    "Z_J_1B_MDFE_XML_OUT", "Z_J_1B_NF_OBJECT_ADD", "Z_SHOW_DETALHAMENTO_CTE",
    "Z_SHOW_DETALHAMENTO_NFE"
)

$TodosObjetos = New-Object System.Collections.Generic.HashSet[string]
foreach ($obj in $ObjetosPrincipais) { [void]$TodosObjetos.Add($obj.ToUpper()) }

function Get-ZRefsFromXML {
    param([string]$File)
    $refs = New-Object System.Collections.Generic.HashSet[string]
    if (-not (Test-Path $File)) { return $refs }
    
    try {
        [xml]$xml = Get-Content $File -Raw -ErrorAction Stop
        
        # Tags importantes
        $tags = @("ROLLNAME", "DATATYPE", "DOMNAME", "REFTYPE", "PRECFIELD", 
                  "REFFIELD", "CHECKTABLE", "INCLUDES", "TABNAME", "TYPENAME", 
                  "ROWTYPE", "SHLPNAME", "INTTYPE")
        
        foreach ($tag in $tags) {
            $nodes = $xml.SelectNodes("//$tag")
            foreach ($node in $nodes) {
                $val = $node.InnerText.Trim().ToUpper()
                if ($val -match "^Z[A-Z0-9_]{2,29}$") {
                    [void]$refs.Add($val)
                }
            }
        }
    } catch {
        # Fallback regex
        $content = Get-Content $File -Raw -ErrorAction SilentlyContinue
        if ($content) {
            $matches = [regex]::Matches($content, "(?i)(Z[A-Z0-9_]{3,30})")
            foreach ($m in $matches) {
                $r = $m.Groups[1].Value.ToUpper()
                if ($r -match "^Z[A-Z0-9_]{2,29}$") { [void]$refs.Add($r) }
            }
        }
    }
    return $refs
}

function Get-ZRefsFromABAP {
    param([string]$File)
    $refs = New-Object System.Collections.Generic.HashSet[string]
    if (-not (Test-Path $File)) { return $refs }
    
    $content = Get-Content $File -Raw -ErrorAction SilentlyContinue
    if (-not $content) { return $refs }
    
    $patterns = @(
        "(?i)\bTYPE\s+(Z[A-Z0-9_]+)", "(?i)\bLIKE\s+(Z[A-Z0-9_]+)",
        "(?i)\bTABLES?\s*:\s*(Z[A-Z0-9_]+)", "(?i)\bSTRUCTURE\s+(Z[A-Z0-9_]+)",
        "(?i)\bINCLUDE\s+STRUCTURE\s+(Z[A-Z0-9_]+)", "(?i)'(Z[A-Z0-9_]{3,30})'",
        "(?i)FROM\s+(Z[A-Z0-9_]+)", "(?i)INTO\s+TABLE\s+(Z[A-Z0-9_]+)"
    )
    
    foreach ($p in $patterns) {
        $ms = [regex]::Matches($content, $p)
        foreach ($m in $ms) {
            if ($m.Groups.Count -gt 1) {
                $r = $m.Groups[1].Value.ToUpper()
                if ($r -match "^Z[A-Z0-9_]{2,29}$") { [void]$refs.Add($r) }
            }
        }
    }
    return $refs
}

function Find-Files {
    param([string]$Obj)
    $files = @()
    $n = $Obj.ToLower()
    
    $patterns = @("$n.*.abap", "$n.*.xml")
    foreach ($p in $patterns) {
        $f = Get-ChildItem -Path "src" -Recurse -Filter $p -File -ErrorAction SilentlyContinue
        if ($f) { $files += $f }
    }
    return $files
}

Write-Output "Objetos iniciais: $($TodosObjetos.Count)" | Tee-Object -FilePath "analise_v2.log" -Append

$iter = 0
$continuar = $true

while ($continuar) {
    $iter++
    $antes = $TodosObjetos.Count
    Write-Output "`nIteracao $iter - Total: $antes" | Tee-Object -FilePath "analise_v2.log" -Append
    
    $lista = @($TodosObjetos)
    
    foreach ($obj in $lista) {
        $arquivos = Find-Files -Obj $obj
        
        foreach ($arq in $arquivos) {
            $refs = $null
            if ($arq.Extension -eq ".abap") {
                $refs = Get-ZRefsFromABAP -File $arq.FullName
            } elseif ($arq.Extension -eq ".xml") {
                $refs = Get-ZRefsFromXML -File $arq.FullName
            }
            
            if ($refs) {
                foreach ($r in $refs) {
                    if ($TodosObjetos.Add($r)) {
                        Write-Output "  [+] $r (de $($arq.Name))" | Tee-Object -FilePath "analise_v2.log" -Append
                    }
                }
            }
        }
    }
    
    $depois = $TodosObjetos.Count
    if ($depois -eq $antes) {
        Write-Output "`nConvergencia atingida!" | Tee-Object -FilePath "analise_v2.log" -Append
        $continuar = $false
    } else {
        Write-Output "Novos: $($depois - $antes)" | Tee-Object -FilePath "analise_v2.log" -Append
    }
    
    if ($iter -gt 30) {
        Write-Output "`nLimite de iteracoes!" | Tee-Object -FilePath "analise_v2.log" -Append
        $continuar = $false
    }
}

Write-Output "`n=== TOTAL: $($TodosObjetos.Count) objetos ===" | Tee-Object -FilePath "analise_v2.log" -Append
$TodosObjetos | Sort-Object | Out-File "objetos_completos.txt" -Encoding UTF8
Write-Output "Salvo em: objetos_completos.txt"

