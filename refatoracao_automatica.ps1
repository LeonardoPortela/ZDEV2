# SCRIPT DE REFATORACAO AUTOMATICA - ZDEV_REFATORADO
# Executa todo o processo de limpeza automaticamente

$ErrorActionPreference = "Continue"

Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "REFATORACAO AUTOMATICA - BRANCH ZDEV_REFATORADO" -ForegroundColor Cyan
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host ""

# Objetos principais a manter
$ObjetosPrincipais = @(
    'zcl_doc_eletronico',
    'zif_doc_eletronico',
    'zcl_webservice',
    'zif_webservice',
    'zbrnfe_danfe',
    'zgrc',
    'zbr_gpf_nfe_danfe'
)

# ETAPA 1: Extrair referencias de tabelas e tipos
Write-Host "ETAPA 1: Extraindo referencias de tabelas e tipos..." -ForegroundColor Green
$referencias = @{}
$arquivosAnalisados = 0

Get-ChildItem -Path src -Filter "*.abap" -Recurse | Where-Object {
    $name = $_.Name.ToLower()
    $name -match "zcl_doc_eletronico" -or
    $name -match "zif_doc_eletronico" -or
    $name -match "zcl_webservice" -or
    $name -match "zif_webservice" -or
    $name -match "zgrc" -or
    $name -match "zbrnfe_danfe" -or
    $name -match "zbr_gpf"
} | ForEach-Object {
    $arquivosAnalisados++
    $conteudo = Get-Content $_.FullName -Raw -ErrorAction SilentlyContinue

    if ($conteudo) {
        # Extrair referencias FROM
        [regex]::Matches($conteudo, "FROM\s+([Z][A-Z0-9_]+)", "IgnoreCase") | ForEach-Object {
            $ref = $_.Groups[1].Value.ToUpper()
            if (-not $referencias.ContainsKey($ref)) {
                $referencias[$ref] = 0
            }
            $referencias[$ref]++
        }
        # Extrair referencias TYPE
        [regex]::Matches($conteudo, "TYPE\s+([Z][A-Z0-9_]+)", "IgnoreCase") | ForEach-Object {
            $ref = $_.Groups[1].Value.ToUpper()
            if (-not $referencias.ContainsKey($ref)) {
                $referencias[$ref] = 0
            }
            $referencias[$ref]++
        }
        # Extrair referencias LIKE
        [regex]::Matches($conteudo, "LIKE\s+([Z][A-Z0-9_]+)", "IgnoreCase") | ForEach-Object {
            $ref = $_.Groups[1].Value.ToUpper()
            if (-not $referencias.ContainsKey($ref)) {
                $referencias[$ref] = 0
            }
            $referencias[$ref]++
        }
        # Extrair STRUCTURE
        [regex]::Matches($conteudo, "STRUCTURE\s+([Z][A-Z0-9_]+)", "IgnoreCase") | ForEach-Object {
            $ref = $_.Groups[1].Value.ToUpper()
            if (-not $referencias.ContainsKey($ref)) {
                $referencias[$ref] = 0
            }
            $referencias[$ref]++
        }
    }
}

Write-Host "  Arquivos ABAP analisados: $arquivosAnalisados" -ForegroundColor Cyan
Write-Host "  Referencias Z encontradas: $($referencias.Count)" -ForegroundColor Cyan

# ETAPA 2: Identificar arquivos de dicionario a manter
Write-Host ""
Write-Host "ETAPA 2: Identificando arquivos de dicionario..." -ForegroundColor Green
$arquivosDicionarioManter = @{}
$extensoesDicionario = @(".tabl.xml", ".dtel.xml", ".doma.xml", ".ttyp.xml", ".shlp.xml", ".msag.xml", ".enqu.xml", ".auth.xml")

foreach ($ref in $referencias.Keys) {
    $refLower = $ref.ToLower()
    foreach ($ext in $extensoesDicionario) {
        $arquivos = Get-ChildItem -Path src -Filter "*$refLower$ext" -Recurse -ErrorAction SilentlyContinue
        if ($arquivos) {
            foreach ($arq in $arquivos) {
                $arquivosDicionarioManter[$arq.FullName] = $arq
            }
        }
    }
}

Write-Host "  Arquivos de dicionario a manter: $($arquivosDicionarioManter.Count)" -ForegroundColor Cyan

# ETAPA 3: Identificar arquivos para excluir
Write-Host ""
Write-Host "ETAPA 3: Identificando arquivos para exclusao..." -ForegroundColor Green
$todosArquivos = Get-ChildItem -Path src -Recurse -File
$arquivosExcluir = @()
$arquivosManter = @()

foreach ($arquivo in $todosArquivos) {
    $nomeBase = $arquivo.Name.Split(".")[0].ToLower()

    # Verificar se e objeto principal
    $ehPrincipal = $false
    foreach ($obj in $ObjetosPrincipais) {
        if ($nomeBase.StartsWith($obj)) {
            $ehPrincipal = $true
            $arquivosManter += $arquivo
            break
        }
    }

    # Verificar se e dicionario mantido
    if (-not $ehPrincipal) {
        if ($arquivosDicionarioManter.ContainsKey($arquivo.FullName)) {
            $arquivosManter += $arquivo
        }
        elseif ($nomeBase.StartsWith("z")) {
            # Marcar para exclusao
            $arquivosExcluir += $arquivo
        }
    }
}

Write-Host "  Total de arquivos na pasta src: $($todosArquivos.Count)" -ForegroundColor Cyan
Write-Host "  Arquivos a manter: $($arquivosManter.Count)" -ForegroundColor Green
Write-Host "  Arquivos a excluir: $($arquivosExcluir.Count)" -ForegroundColor Red

# ETAPA 4: Executar exclusao
Write-Host ""
Write-Host "ETAPA 4: Executando exclusao automatica..." -ForegroundColor Yellow
Write-Host ""

$excluidos = 0
$erros = 0

foreach ($arquivo in $arquivosExcluir) {
    try {
        Remove-Item $arquivo.FullName -Force -ErrorAction Stop
        $excluidos++

        if ($excluidos % 500 -eq 0) {
            Write-Host "  Excluidos $excluidos de $($arquivosExcluir.Count)..." -ForegroundColor Gray
        }
    }
    catch {
        $erros++
    }
}

# ETAPA 5: Relatorio Final
Write-Host ""
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "RELATORIO FINAL DA REFATORACAO" -ForegroundColor Cyan
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host ""
Write-Host "Arquivos ABAP analisados:" -ForegroundColor White -NoNewline
Write-Host " $arquivosAnalisados" -ForegroundColor Cyan
Write-Host "Referencias Z encontradas:" -ForegroundColor White -NoNewline
Write-Host " $($referencias.Count)" -ForegroundColor Cyan
Write-Host "Arquivos de dicionario mantidos:" -ForegroundColor White -NoNewline
Write-Host " $($arquivosDicionarioManter.Count)" -ForegroundColor Green
Write-Host ""
Write-Host "Total original de arquivos:" -ForegroundColor White -NoNewline
Write-Host " $($todosArquivos.Count)" -ForegroundColor Cyan
Write-Host "Arquivos mantidos:" -ForegroundColor White -NoNewline
Write-Host " $($arquivosManter.Count)" -ForegroundColor Green
Write-Host "Arquivos excluidos:" -ForegroundColor White -NoNewline
Write-Host " $excluidos" -ForegroundColor Red
Write-Host "Erros durante exclusao:" -ForegroundColor White -NoNewline
Write-Host " $erros" -ForegroundColor Yellow
Write-Host ""

# Objetos principais mantidos
Write-Host "OBJETOS PRINCIPAIS MANTIDOS:" -ForegroundColor Green
foreach ($obj in $ObjetosPrincipais) {
    $count = (Get-ChildItem -Path src -Filter "*$obj*" -Recurse -File -ErrorAction SilentlyContinue).Count
    Write-Host "  $obj : $count arquivos" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host "PROCESSO CONCLUIDO COM SUCESSO!" -ForegroundColor Green
Write-Host "=" * 80 -ForegroundColor Cyan
Write-Host ""

# Salvar relatorio em arquivo
$relatorio = @"
RELATORIO DE REFATORACAO AUTOMATICA
Data: $(Get-Date -Format "dd/MM/yyyy HH:mm:ss")
Branch: ZDEV_REFATORADO

ESTATISTICAS:
- Arquivos ABAP analisados: $arquivosAnalisados
- Referencias Z encontradas: $($referencias.Count)
- Arquivos de dicionario mantidos: $($arquivosDicionarioManter.Count)
- Total original de arquivos: $($todosArquivos.Count)
- Arquivos mantidos: $($arquivosManter.Count)
- Arquivos excluidos: $excluidos
- Erros: $erros

OBJETOS PRINCIPAIS MANTIDOS:
$($ObjetosPrincipais | ForEach-Object {
    $count = (Get-ChildItem -Path src -Filter "*$_*" -Recurse -File -ErrorAction SilentlyContinue).Count
    "  $_ : $count arquivos"
} | Out-String)

TOP 50 REFERENCIAS MAIS UTILIZADAS:
$($referencias.GetEnumerator() | Sort-Object Value -Descending | Select-Object -First 50 | ForEach-Object {
    "  $($_.Key) : $($_.Value) referencias"
} | Out-String)
"@

$relatorio | Out-File "RELATORIO_EXECUCAO.txt" -Encoding UTF8

Write-Host "Relatorio salvo em: RELATORIO_EXECUCAO.txt" -ForegroundColor Cyan
Write-Host ""

