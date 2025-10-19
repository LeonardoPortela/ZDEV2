# Script DEFINITIVO - Analisa TODOS os arquivos existentes e recupera TODAS as dependencias

Write-Output "=== RECUPERACAO COMPLETA DE DEPENDENCIAS ===" | Tee-Object -FilePath "recuperacao_completa.log"

function Get-AllZRefsFromXML {
    param([string]$FilePath)
    $refs = New-Object System.Collections.Generic.HashSet[string]
    
    if (-not (Test-Path $FilePath)) { return $refs }
    
    try {
        $content = Get-Content $FilePath -Raw -ErrorAction Stop
        
        # Extrair de tags XML especificas (mais confiavel que SelectNodes)
        $xmlTags = @(
            '<DOMNAME>([^<]+)</DOMNAME>',
            '<ROLLNAME>([^<]+)</ROLLNAME>',
            '<DATATYPE>([^<]+)</DATATYPE>',
            '<REFTYPE>([^<]+)</REFTYPE>',
            '<PRECFIELD>([^<]+)</PRECFIELD>',
            '<REFFIELD>([^<]+)</REFFIELD>',
            '<CHECKTABLE>([^<]+)</CHECKTABLE>',
            '<TABNAME>([^<]+)</TABNAME>',
            '<TYPENAME>([^<]+)</TYPENAME>',
            '<ROWTYPE>([^<]+)</ROWTYPE>',
            '<SHLPNAME>([^<]+)</SHLPNAME>',
            '<INCLUDES>([^<]+)</INCLUDES>',
            '<STRUCOBJN>([^<]+)</STRUCOBJN>',
            '<INTTYPE>([^<]+)</INTTYPE>'
        )
        
        foreach ($pattern in $xmlTags) {
            $matches = [regex]::Matches($content, $pattern)
            foreach ($m in $matches) {
                if ($m.Groups.Count -gt 1) {
                    $val = $m.Groups[1].Value.Trim().ToUpper()
                    if ($val -match '^Z[A-Z0-9_]{2,29}$') {
                        [void]$refs.Add($val)
                    }
                }
            }
        }
        
        # Fallback: buscar qualquer string que pareca um objeto Z
        $allMatches = [regex]::Matches($content, '\b(Z[A-Z0-9_]{3,30})\b')
        foreach ($m in $allMatches) {
            $val = $m.Groups[1].Value.ToUpper()
            if ($val -match '^Z[A-Z0-9_]{2,29}$') {
                [void]$refs.Add($val)
            }
        }
        
    } catch {
        Write-Output "  [!] Erro ao ler $FilePath" | Tee-Object -FilePath "recuperacao_completa.log" -Append
    }
    
    return $refs
}

function Get-AllZRefsFromABAP {
    param([string]$FilePath)
    $refs = New-Object System.Collections.Generic.HashSet[string]
    
    if (-not (Test-Path $FilePath)) { return $refs }
    
    try {
        $content = Get-Content $FilePath -Raw -ErrorAction Stop
        
        $patterns = @(
            "(?i)\bTYPE\s+(Z[A-Z0-9_]+)",
            "(?i)\bLIKE\s+(Z[A-Z0-9_]+)",
            "(?i)\bTABLES?\s*:\s*(Z[A-Z0-9_]+)",
            "(?i)\bSTRUCTURE\s+(Z[A-Z0-9_]+)",
            "(?i)\bINCLUDE\s+STRUCTURE\s+(Z[A-Z0-9_]+)",
            "(?i)'(Z[A-Z0-9_]{3,30})'",
            "(?i)FROM\s+(Z[A-Z0-9_]+)",
            "(?i)INTO\s+TABLE\s+(Z[A-Z0-9_]+)"
        )
        
        foreach ($p in $patterns) {
            $ms = [regex]::Matches($content, $p)
            foreach ($m in $ms) {
                if ($m.Groups.Count -gt 1) {
                    $r = $m.Groups[1].Value.ToUpper()
                    if ($r -match '^Z[A-Z0-9_]{2,29}$') {
                        [void]$refs.Add($r)
                    }
                }
            }
        }
    } catch {
        Write-Output "  [!] Erro ao ler $FilePath" | Tee-Object -FilePath "recuperacao_completa.log" -Append
    }
    
    return $refs
}

function Try-RecoverObject {
    param([string]$ObjName)
    
    $nome = $ObjName.ToLower()
    $extensoes = @(".doma.xml", ".dtel.xml", ".tabl.xml", ".ttyp.xml", ".shlp.xml", 
                   ".clas.abap", ".clas.xml", ".intf.abap", ".intf.xml",
                   ".prog.abap", ".prog.xml", ".fugr.xml", ".msag.xml", ".enqu.xml")
    
    $recuperado = $false
    
    foreach ($ext in $extensoes) {
        $arquivo = "src\$nome$ext"
        
        # Pular se já existe
        if (Test-Path $arquivo) {
            continue
        }
        
        # Tentar recuperar da main
        git checkout main -- "src/$nome$ext" 2>&1 | Out-Null
        
        if ($LASTEXITCODE -eq 0 -and (Test-Path $arquivo)) {
            Write-Output "    [+] Recuperado: $nome$ext" | Tee-Object -FilePath "recuperacao_completa.log" -Append
            $recuperado = $true
        }
    }
    
    return $recuperado
}

# FASE 1: Coletar TODAS as referencias Z de TODOS os arquivos existentes
Write-Output "`nFASE 1: Analisando arquivos existentes..." | Tee-Object -FilePath "recuperacao_completa.log" -Append

$todosArquivos = Get-ChildItem -Path "src" -Recurse -File
$todasRefs = New-Object System.Collections.Generic.HashSet[string]

$contadorArquivos = 0
foreach ($arquivo in $todosArquivos) {
    $contadorArquivos++
    
    if ($contadorArquivos % 50 -eq 0) {
        Write-Output "  Analisados: $contadorArquivos/$($todosArquivos.Count) arquivos..." | Tee-Object -FilePath "recuperacao_completa.log" -Append
    }
    
    $refs = $null
    
    if ($arquivo.Extension -eq ".xml") {
        $refs = Get-AllZRefsFromXML -FilePath $arquivo.FullName
    } elseif ($arquivo.Extension -eq ".abap") {
        $refs = Get-AllZRefsFromABAP -FilePath $arquivo.FullName
    }
    
    if ($refs) {
        foreach ($r in $refs) {
            [void]$todasRefs.Add($r)
        }
    }
}

Write-Output "`nTotal de referencias Z encontradas: $($todasRefs.Count)" | Tee-Object -FilePath "recuperacao_completa.log" -Append

# FASE 2: Tentar recuperar objetos faltantes
Write-Output "`nFASE 2: Recuperando objetos faltantes..." | Tee-Object -FilePath "recuperacao_completa.log" -Append

$listaRefs = @($todasRefs | Sort-Object)
$recuperados = 0
$jaExistem = 0

foreach ($ref in $listaRefs) {
    $nome = $ref.ToLower()
    
    # Verificar se pelo menos um arquivo existe
    $existeAlgum = $false
    $exts = @(".doma.xml", ".dtel.xml", ".tabl.xml", ".ttyp.xml", ".shlp.xml", ".clas.xml", ".intf.xml", ".prog.xml", ".fugr.xml")
    
    foreach ($e in $exts) {
        if (Test-Path "src\$nome$e") {
            $existeAlgum = $true
            break
        }
    }
    
    if ($existeAlgum) {
        $jaExistem++
    } else {
        Write-Output "  Tentando recuperar: $ref" | Tee-Object -FilePath "recuperacao_completa.log" -Append
        if (Try-RecoverObject -ObjName $ref) {
            $recuperados++
        }
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "RESULTADO:" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "  Referencias encontradas: $($todasRefs.Count)" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "  Objetos ja existentes: $jaExistem" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "  Objetos recuperados: $recuperados" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "  Objetos faltantes: $($todasRefs.Count - $jaExistem - $recuperados)" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "recuperacao_completa.log" -Append

# FASE 3: ITERACAO - Analisar arquivos novos recuperados
Write-Output "`nFASE 3: Iterando sobre novos arquivos recuperados..." | Tee-Object -FilePath "recuperacao_completa.log" -Append

$iteracao = 0
$continuar = $true

while ($continuar -and $iteracao -lt 10) {
    $iteracao++
    $antes = $recuperados
    
    Write-Output "`n  Iteracao $iteracao..." | Tee-Object -FilePath "recuperacao_completa.log" -Append
    
    # Re-analisar TODOS os arquivos (incluindo novos)
    $todosArquivos = Get-ChildItem -Path "src" -Recurse -File
    $novasRefs = New-Object System.Collections.Generic.HashSet[string]
    
    foreach ($arquivo in $todosArquivos) {
        $refs = $null
        
        if ($arquivo.Extension -eq ".xml") {
            $refs = Get-AllZRefsFromXML -FilePath $arquivo.FullName
        } elseif ($arquivo.Extension -eq ".abap") {
            $refs = Get-AllZRefsFromABAP -FilePath $arquivo.FullName
        }
        
        if ($refs) {
            foreach ($r in $refs) {
                if (-not $todasRefs.Contains($r)) {
                    [void]$novasRefs.Add($r)
                }
            }
        }
    }
    
    if ($novasRefs.Count -eq 0) {
        Write-Output "    Nenhuma nova referencia encontrada." | Tee-Object -FilePath "recuperacao_completa.log" -Append
        $continuar = $false
    } else {
        Write-Output "    Encontradas $($novasRefs.Count) novas referencias" | Tee-Object -FilePath "recuperacao_completa.log" -Append
        
        foreach ($ref in $novasRefs) {
            [void]$todasRefs.Add($ref)
            
            if (Try-RecoverObject -ObjName $ref) {
                $recuperados++
            }
        }
        
        if ($recuperados -eq $antes) {
            $continuar = $false
        }
    }
}

Write-Output "`n=== CONCLUIDO ===" | Tee-Object -FilePath "recuperacao_completa.log" -Append
Write-Output "Total de objetos recuperados: $recuperados" | Tee-Object -FilePath "recuperacao_completa.log" -Append

# Contar arquivos finais
$totalFinal = (Get-ChildItem -Path "src" -Recurse -File | Measure-Object).Count
Write-Output "Total de arquivos no src: $totalFinal" | Tee-Object -FilePath "recuperacao_completa.log" -Append

