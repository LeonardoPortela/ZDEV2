# ===================================================================
# REFATORACAO RIGOROSA - APENAS OBJETOS DA LISTA E DEPENDENCIAS
# ===================================================================

Write-Host ""
Write-Host "========================================================================" -ForegroundColor Cyan
Write-Host "         REFATORACAO RIGOROSA - APENAS ESCOPO DEFINIDO" -ForegroundColor Cyan
Write-Host "========================================================================" -ForegroundColor Cyan
Write-Host ""

# Lista EXATA dos objetos principais
$ObjetosPrincipais = @(
    # Classes e Interfaces
    "zcl_doc_eletronico",
    "zif_doc_eletronico",
    "zcl_webservice",
    "zif_webservice",
    
    # Programas
    "zbrnfe_danfe",
    "zdequeue_all",
    "zgrc_limpa_ref_miro_fiscal",
    
    # Function Groups (precisamos manter o grupo inteiro)
    "zgrc",
    "zbr_gpf_nfe_danfe",
    "zfsd"
)

# Function Modules da lista (dentro do function group ZGRC)
$FunctionModules = @(
    "z_detalhamento_cte",
    "z_detalhamento_cte_in_massa",
    "z_detalhamento_cte_xml",
    "z_detalhamento_nfe",
    "z_grc_ajusta_tp_emissao",
    "z_grc_arquivo_doc",
    "z_grc_arquivo_doc_xml",
    "z_grc_download_xml_pdf",
    "z_grc_envia_legado",
    "z_grc_get_status_doc",
    "z_grc_mdfe_avulsa",
    "z_grc_mdfe_load",
    "z_grc_monta_link",
    "z_grc_new_nfe",
    "z_grc_registra_inf_zib_nfe",
    "z_grc_registra_log_doc",
    "z_grc_send_email_auto",
    "z_j_1bmfe_cancel_event_send",
    "z_j_1b_event_cancel_nf_cte",
    "z_j_1b_mdfe_cancel",
    "z_j_1b_mdfe_close",
    "z_j_1b_mdfe_xml_out",
    "z_j_1b_nf_object_add",
    "z_show_detalhamento_cte",
    "z_show_detalhamento_nfe",
    "zfsd_busca_danfe"
)

Write-Host "[ETAPA 1] Identificando arquivos dos objetos principais..." -ForegroundColor Yellow
$ArquivosManter = @()
$Contador = 0

foreach ($objeto in $ObjetosPrincipais) {
    $arquivos = Get-ChildItem -Path "src" -Recurse -File | Where-Object { 
        $_.Name -like "$objeto.*" 
    }
    
    foreach ($arq in $arquivos) {
        $ArquivosManter += $arq.FullName
        $Contador++
    }
    
    if ($arquivos.Count -gt 0) {
        Write-Host "  OK: $objeto - $($arquivos.Count) arquivos" -ForegroundColor Green
    }
}

Write-Host "  Total de arquivos dos objetos principais: $Contador" -ForegroundColor Cyan
Write-Host ""

Write-Host "[ETAPA 2] Analisando referencias de dicionario de dados..." -ForegroundColor Yellow

# Conjunto para armazenar referencias unicas
$ReferenciasZ = New-Object System.Collections.Generic.HashSet[string]

# Analisar apenas arquivos .abap dos objetos principais
foreach ($arquivo in $ArquivosManter) {
    if ($arquivo -like "*.abap") {
        try {
            $conteudo = Get-Content -Path $arquivo -Raw -ErrorAction SilentlyContinue
            
            if ($conteudo) {
                # Buscar referencias a objetos Z de dicionario
                # Tabelas, estruturas, elementos de dados, dominios
                $patterns = @(
                    '\bz\w+\s+TYPE\s+(\w+)',
                    '\bTYPES?:\s*(\w+)\s+TYPE\s+(\w+)',
                    '\bDATA:\s*(\w+)\s+TYPE\s+(\w+)',
                    '\bPARAMETERS?:\s*(\w+)\s+TYPE\s+(\w+)',
                    '\bSELECT\s+\*?\s+FROM\s+(\w+)',
                    '\bTABLES:\s+(\w+)',
                    '\bUSING\s+(\w+)',
                    '\bIMPORTING\s+(\w+)\s+TYPE\s+(\w+)',
                    '\bEXPORTING\s+(\w+)\s+TYPE\s+(\w+)',
                    '\bCHANGING\s+(\w+)\s+TYPE\s+(\w+)'
                )
                
                foreach ($pattern in $patterns) {
                    $matches = [regex]::Matches($conteudo, $pattern, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
                    foreach ($match in $matches) {
                        for ($i = 1; $i -lt $match.Groups.Count; $i++) {
                            $ref = $match.Groups[$i].Value.ToLower().Trim()
                            if ($ref -match '^z\w+$' -and $ref.Length -gt 1) {
                                [void]$ReferenciasZ.Add($ref)
                            }
                        }
                    }
                }
            }
        }
        catch {
            # Ignorar erros de leitura
        }
    }
}

Write-Host "  Referencias Z encontradas: $($ReferenciasZ.Count)" -ForegroundColor Cyan
Write-Host ""

Write-Host "[ETAPA 3] Identificando arquivos de dicionario para manter..." -ForegroundColor Yellow

# Extensoes de dicionario de dados permitidas
$ExtensoesDicionario = @('.tabl.xml', '.dtel.xml', '.doma.xml', '.ttyp.xml', '.shlp.xml', '.msag.xml')

$ArquivosDicionario = 0
foreach ($ref in $ReferenciasZ) {
    foreach ($ext in $ExtensoesDicionario) {
        $nomeArquivo = "$ref$ext"
        $arquivos = Get-ChildItem -Path "src" -Recurse -File | Where-Object { $_.Name -eq $nomeArquivo }
        
        foreach ($arq in $arquivos) {
            if ($ArquivosManter -notcontains $arq.FullName) {
                $ArquivosManter += $arq.FullName
                $ArquivosDicionario++
            }
        }
    }
}

Write-Host "  Arquivos de dicionario adicionados: $ArquivosDicionario" -ForegroundColor Cyan
Write-Host ""

Write-Host "[ETAPA 4] Identificando arquivos para EXCLUSAO..." -ForegroundColor Yellow

$TodosArquivos = Get-ChildItem -Path "src" -Recurse -File
$ArquivosExcluir = @()

foreach ($arquivo in $TodosArquivos) {
    if ($ArquivosManter -notcontains $arquivo.FullName) {
        $ArquivosExcluir += $arquivo.FullName
    }
}

Write-Host "  Total de arquivos a excluir: $($ArquivosExcluir.Count)" -ForegroundColor Red
Write-Host ""

Write-Host "[ETAPA 5] Executando exclusao RIGOROSA..." -ForegroundColor Yellow

$Excluidos = 0
$Erros = 0

foreach ($arquivo in $ArquivosExcluir) {
    try {
        Remove-Item -Path $arquivo -Force -ErrorAction Stop
        $Excluidos++
        
        if ($Excluidos % 1000 -eq 0) {
            Write-Host "  Progresso: $Excluidos arquivos excluidos..." -ForegroundColor Gray
        }
    }
    catch {
        $Erros++
    }
}

Write-Host ""
Write-Host "========================================================================" -ForegroundColor Green
Write-Host "                    REFATORACAO RIGOROSA CONCLUIDA!" -ForegroundColor Green
Write-Host "========================================================================" -ForegroundColor Green
Write-Host ""
Write-Host "ESTATISTICAS:" -ForegroundColor Cyan
Write-Host "  Arquivos mantidos (objetos principais): $Contador" -ForegroundColor White
Write-Host "  Arquivos mantidos (dicionario): $ArquivosDicionario" -ForegroundColor White
Write-Host "  Total mantido: $($ArquivosManter.Count)" -ForegroundColor Green
Write-Host "  Total excluido: $Excluidos" -ForegroundColor Red
Write-Host "  Erros: $Erros" -ForegroundColor Yellow
Write-Host ""
Write-Host "Referencias Z analisadas: $($ReferenciasZ.Count)" -ForegroundColor Cyan
Write-Host ""

# Salvar relatorio
$relatorio = @"
REFATORACAO RIGOROSA - RELATORIO FINAL
========================================

ARQUIVOS MANTIDOS (OBJETOS PRINCIPAIS): $Contador
ARQUIVOS MANTIDOS (DICIONARIO): $ArquivosDicionario
TOTAL MANTIDO: $($ArquivosManter.Count)
TOTAL EXCLUIDO: $Excluidos
ERROS: $Erros

REFERENCIAS Z ENCONTRADAS: $($ReferenciasZ.Count)

DATA/HORA: $(Get-Date)
"@

$relatorio | Out-File -FilePath "RELATORIO_REFATORACAO_RIGOROSA.txt" -Encoding UTF8

Write-Host "Relatorio salvo em: RELATORIO_REFATORACAO_RIGOROSA.txt" -ForegroundColor Cyan
Write-Host ""

