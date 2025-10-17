# Script PowerShell para Refatoração de Objetos ABAP
# Mantém apenas objetos de dicionário de dados utilizados pelos objetos principais

$ErrorActionPreference = "Continue"

# Objetos principais que devem ser mantidos
$ObjetosPrincipais = @(
    'ZCL_DOC_ELETRONICO',
    'ZIF_DOC_ELETRONICO',
    'ZCL_WEBSERVICE',
    'ZIF_WEBSERVICE',
    'ZBRNFE_DANFE',
    'ZGRC',  # Function Group que contém todas as funções Z_GRC_*, Z_DETALHAMENTO_*, Z_J_1B_*, Z_SHOW_*
    'ZBR_GPF_NFE_DANFE'  # Function Group do ZBRNFE_DANFE
)

# Function Modules específicos dentro do ZGRC que devem ser mantidos
$FunctionModules = @(
    'Z_DETALHAMENTO_CTE',
    'Z_DETALHAMENTO_CTE_IN_MASSA',
    'Z_DETALHAMENTO_CTE_XML',
    'Z_DETALHAMENTO_NFE',
    'Z_GRC_AJUSTA_TP_EMISSAO',
    'Z_GRC_ARQUIVO_DOC',
    'Z_GRC_ARQUIVO_DOC_XML',
    'Z_GRC_DOWNLOAD_XML_PDF',
    'Z_GRC_ENVIA_LEGADO',
    'Z_GRC_GET_STATUS_DOC',
    'Z_GRC_MDFE_AVULSA',
    'Z_GRC_MDFE_LOAD',
    'Z_GRC_MONTA_LINK',
    'Z_GRC_NEW_NFE',
    'Z_GRC_REGISTRA_INF_ZIB_NFE',
    'Z_GRC_REGISTRA_LOG_DOC',
    'Z_GRC_SEND_EMAIL_AUTO',
    'Z_J_1BMFE_CANCEL_EVENT_SEND',
    'Z_J_1B_EVENT_CANCEL_NF_CTE',
    'Z_J_1B_MDFE_CANCEL',
    'Z_J_1B_MDFE_CLOSE',
    'Z_J_1B_MDFE_XML_OUT',
    'Z_J_1B_NF_OBJECT_ADD',
    'Z_SHOW_DETALHAMENTO_CTE',
    'Z_SHOW_DETALHAMENTO_NFE'
)

Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "REFATORAÇÃO DE OBJETOS ABAP" -ForegroundColor Cyan
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host ""

$SrcPath = "src"
$ObjetosManter = @{}
$ObjetosDicionario = @{}
$TotalArquivos = 0
$ArquivosExcluir = @()

# Função para extrair referências de tabelas de um arquivo ABAP
function Get-TabelasReferenciadas {
    param(
        [string]$FilePath
    )
    
    $referencias = @()
    
    try {
        $conteudo = Get-Content $FilePath -Raw -ErrorAction SilentlyContinue
        
        if ($conteudo) {
            # Padrões para identificar tabelas e estruturas Z
            $patterns = @(
                'FROM\s+([Z][A-Z0-9_]+)',
                'INTO\s+TABLE\s+([Z][A-Z0-9_]+)',
                'TABLES?\s*:\s*([Z][A-Z0-9_]+)',
                'TYPE\s+([Z][A-Z0-9_]+)',
                'LIKE\s+([Z][A-Z0-9_]+)',
                'STRUCTURE\s+([Z][A-Z0-9_]+)'
            )
            
            foreach ($pattern in $patterns) {
                $matches = [regex]::Matches($conteudo, $pattern, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
                foreach ($match in $matches) {
                    if ($match.Groups[1].Value.StartsWith('Z')) {
                        $referencias += $match.Groups[1].Value.ToUpper()
                    }
                }
            }
        }
    } catch {
        Write-Host "  AVISO Erro ao ler $FilePath : $_" -ForegroundColor Yellow
    }
    
    return ($referencias | Select-Object -Unique)
}

# 1. MAPEAR ARQUIVOS PRINCIPAIS
Write-Host "ETAPA 1: Mapeando arquivos dos objetos principais..." -ForegroundColor Green
Write-Host ""

foreach ($obj in $ObjetosPrincipais) {
    $objLower = $obj.ToLower()
    $arquivos = Get-ChildItem -Path $SrcPath -Filter "*$objLower*" -Recurse -File
    
    if ($arquivos) {
        $ObjetosManter[$obj] = $arquivos
        Write-Host "OK Objeto: $obj ($($arquivos.Count) arquivos)" -ForegroundColor Cyan
        $TotalArquivos += $arquivos.Count
    }
}

Write-Host ""
Write-Host "Total de arquivos dos objetos principais: $TotalArquivos" -ForegroundColor Yellow
Write-Host ""

# 2. ANALISAR DEPENDÊNCIAS
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "ETAPA 2: Analisando dependências..." -ForegroundColor Green
Write-Host ""

$TodasReferencias = @{}

foreach ($obj in $ObjetosManter.Keys) {
    Write-Host "Analisando: $obj" -ForegroundColor Cyan
    
    $referencias = @()
    foreach ($arquivo in $ObjetosManter[$obj]) {
        if ($arquivo.Extension -eq '.abap') {
            $refs = Get-TabelasReferenciadas -FilePath $arquivo.FullName
            $referencias += $refs
        }
    }
    
    $referencias = $referencias | Select-Object -Unique
    $TodasReferencias[$obj] = $referencias
    
    Write-Host "  → $($referencias.Count) referências encontradas" -ForegroundColor Gray
}

Write-Host ""

# 3. IDENTIFICAR OBJETOS DE DICIONÁRIO
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "ETAPA 3: Identificando objetos de dicionário..." -ForegroundColor Green
Write-Host ""

$extensoes_dicionario = @('.tabl.xml', '.dtel.xml', '.doma.xml', '.ttyp.xml', '.shlp.xml', '.msag.xml')

$TodasRefsUnicas = @()
foreach ($refs in $TodasReferencias.Values) {
    $TodasRefsUnicas += $refs
}
$TodasRefsUnicas = $TodasRefsUnicas | Select-Object -Unique

Write-Host "Total de referências únicas encontradas: $($TodasRefsUnicas.Count)" -ForegroundColor Yellow
Write-Host ""

foreach ($ref in $TodasRefsUnicas) {
    $refLower = $ref.ToLower()
    
    foreach ($ext in $extensoes_dicionario) {
        $arquivos = Get-ChildItem -Path $SrcPath -Filter "*$refLower$ext" -Recurse -File
        
        if ($arquivos) {
            if (-not $ObjetosDicionario.ContainsKey($ref)) {
                $ObjetosDicionario[$ref] = @()
            }
            $ObjetosDicionario[$ref] += $arquivos
            Write-Host "OK Objeto de dicionario: $ref ($ext)" -ForegroundColor Green
        }
    }
}

Write-Host ""
Write-Host "Total de objetos de dicionário mantidos: $($ObjetosDicionario.Count)" -ForegroundColor Yellow
Write-Host ""

# 4. LISTAR OBJETOS PARA EXCLUIR
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "ETAPA 4: Identificando objetos para exclusão..." -ForegroundColor Green
Write-Host ""

$TodosArquivos = Get-ChildItem -Path $SrcPath -Recurse -File

foreach ($arquivo in $TodosArquivos) {
    $nomeArquivo = $arquivo.Name.Split('.')[0].ToUpper()
    
    # Verificar se deve ser mantido
    $deveManter = $false
    
    # Verificar objetos principais
    foreach ($obj in $ObjetosPrincipais) {
        if ($nomeArquivo -eq $obj -or $nomeArquivo.StartsWith($obj)) {
            $deveManter = $true
            break
        }
    }
    
    # Verificar objetos de dicionário
    if (-not $deveManter) {
        foreach ($obj in $ObjetosDicionario.Keys) {
            if ($nomeArquivo -eq $obj -or $nomeArquivo.StartsWith($obj)) {
                $deveManter = $true
                break
            }
        }
    }
    
    # Se não deve manter e é um objeto Z, adicionar à lista de exclusão
    if (-not $deveManter -and $nomeArquivo.StartsWith('Z')) {
        # Excluir apenas objetos de programa/classe/interface que não estão na lista
        $extensoesExcluir = @('.prog.', '.clas.', '.intf.', '.fugr.')
        $ehPrograma = $false
        
        foreach ($ext in $extensoesExcluir) {
            if ($arquivo.FullName -match [regex]::Escape($ext)) {
                $ehPrograma = $true
                break
            }
        }
        
        if ($ehPrograma) {
            $ArquivosExcluir += $arquivo
        }
    }
}

Write-Host "Total de arquivos para excluir: $($ArquivosExcluir.Count)" -ForegroundColor Yellow
Write-Host ""

# 5. RELATÓRIO
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "RELATÓRIO FINAL" -ForegroundColor Cyan
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host ""
Write-Host "Objetos principais mantidos: $($ObjetosPrincipais.Count)" -ForegroundColor Green
Write-Host "Objetos de dicionário mantidos: $($ObjetosDicionario.Count)" -ForegroundColor Green
Write-Host "Arquivos a serem excluídos: $($ArquivosExcluir.Count)" -ForegroundColor Red
Write-Host ""

# Mostrar alguns exemplos de objetos a serem excluídos
if ($ArquivosExcluir.Count -gt 0) {
    Write-Host "Exemplos de arquivos que serão excluídos (primeiros 20):" -ForegroundColor Yellow
    $ArquivosExcluir | Select-Object -First 20 | ForEach-Object {
        Write-Host "  - $($_.Name)" -ForegroundColor Gray
    }
    Write-Host ""
}

# 6. CONFIRMAÇÃO E EXCLUSÃO
Write-Host "=" * 80 -ForegroundColor Red
Write-Host "ATENÇÃO: $($ArquivosExcluir.Count) arquivos serão excluídos!" -ForegroundColor Red
Write-Host "=" * 80 -ForegroundColor Red
Write-Host ""

$resposta = Read-Host "Deseja prosseguir com a exclusão? (sim/não)"

if ($resposta -eq "sim") {
    Write-Host ""
    Write-Host "Iniciando exclusão..." -ForegroundColor Yellow
    Write-Host ""
    
    $excluidos = 0
    foreach ($arquivo in $ArquivosExcluir) {
        try {
            Remove-Item $arquivo.FullName -Force
            $excluidos++
            if ($excluidos % 100 -eq 0) {
                Write-Host "  Excluidos $excluidos de $($ArquivosExcluir.Count)..." -ForegroundColor Gray
            }
        } catch {
            Write-Host "  ERRO ao excluir $($arquivo.Name): $_" -ForegroundColor Red
        }
    }
    
    Write-Host ""
    Write-Host "OK Refatoracao concluida! $excluidos arquivos excluidos." -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "OK Refatoracao cancelada. Nenhum arquivo foi excluido." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Script finalizado." -ForegroundColor Cyan

