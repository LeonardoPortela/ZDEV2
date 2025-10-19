# Script para recuperar TODOS os arquivos necessarios dos 206 objetos Z

Write-Output "=== RECUPERANDO OBJETOS Z FALTANTES ===" | Tee-Object -FilePath "recuperacao_final.log"

# Ler lista de objetos necessarios
$ObjetosNecessarios = Get-Content "objetos_z_necessarios.txt" | ForEach-Object { $_.Trim().ToLower() }

Write-Output "Total de objetos a verificar: $($ObjetosNecessarios.Count)" | Tee-Object -FilePath "recuperacao_final.log" -Append

# Extensoes possíveis de arquivos ABAP
$extensoes = @(
    ".clas.abap",
    ".clas.xml",
    ".clas.locals_def.abap",
    ".clas.locals_imp.abap",
    ".clas.macros.abap",
    ".clas.testclasses.abap",
    ".intf.abap",
    ".intf.xml",
    ".prog.abap",
    ".prog.xml",
    ".fugr.xml",
    ".fugr.lf01.abap",
    ".fugr.lf01.xml",
    ".fugr.lf02.abap",
    ".fugr.lf02.xml",
    ".fugr.lf03.abap",
    ".fugr.lf03.xml",
    ".fugr.lf04.abap",
    ".fugr.lf04.xml",
    ".fugr.lf05.abap",
    ".fugr.lf05.xml",
    ".fugr.saplzf01.abap",
    ".fugr.saplzf01.xml",
    ".func.abap",
    ".func.xml",
    ".tabl.xml",
    ".dtel.xml",
    ".doma.xml",
    ".ttyp.xml",
    ".shlp.xml",
    ".shma.xml",
    ".msag.xml",
    ".enqu.xml",
    ".auth.xml",
    ".ssfo.xml",
    ".ssst.xml",
    ".xslt.xml"
)

$contador = 0
$recuperados = 0
$jaExistem = 0
$naoEncontrados = 0

foreach ($objeto in $ObjetosNecessarios) {
    $contador++
    
    if ($contador % 20 -eq 0) {
        Write-Output "Progresso: $contador/$($ObjetosNecessarios.Count) objetos verificados..." | Tee-Object -FilePath "recuperacao_final.log" -Append
    }
    
    $algumArquivoRecuperado = $false
    
    foreach ($ext in $extensoes) {
        $nomeArquivo = "$objeto$ext"
        $caminhoLocal = "src\$nomeArquivo"
        
        # Verificar se ja existe localmente
        if (Test-Path $caminhoLocal) {
            continue
        }
        
        # Tentar recuperar da main
        try {
            git checkout main -- "src/$nomeArquivo" 2>&1 | Out-Null
            
            if ($LASTEXITCODE -eq 0) {
                # Verificar se realmente foi recuperado
                if (Test-Path $caminhoLocal) {
                    $recuperados++
                    $algumArquivoRecuperado = $true
                    Write-Output "  [+] Recuperado: $nomeArquivo" | Tee-Object -FilePath "recuperacao_final.log" -Append
                }
            }
        } catch {
            # Silencioso - arquivo nao existe na main
        }
    }
    
    if (-not $algumArquivoRecuperado) {
        # Verificar se pelo menos um arquivo existe
        $temAlgum = $false
        foreach ($ext in $extensoes) {
            if (Test-Path "src\$objeto$ext") {
                $temAlgum = $true
                break
            }
        }
        
        if ($temAlgum) {
            $jaExistem++
        } else {
            $naoEncontrados++
            Write-Output "  [!] NENHUM arquivo encontrado para: $objeto" | Tee-Object -FilePath "recuperacao_final.log" -Append
        }
    }
}

Write-Output "`n========================================" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "RESULTADO FINAL:" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "  Objetos verificados: $contador" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "  Arquivos recuperados: $recuperados" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "  Objetos ja existentes: $jaExistem" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "  Objetos nao encontrados: $naoEncontrados" | Tee-Object -FilePath "recuperacao_final.log" -Append
Write-Output "========================================" | Tee-Object -FilePath "recuperacao_final.log" -Append

# Contar total de arquivos agora
$totalArquivos = (Get-ChildItem -Path src -Recurse -File | Measure-Object).Count
Write-Output "Total de arquivos no src: $totalArquivos" | Tee-Object -FilePath "recuperacao_final.log" -Append

Write-Output "`nConcluido! Detalhes em: recuperacao_final.log"


