# Script de Auditoria - ESTRATÉGIA FINAL CORRETA
Write-Output "=== AUDITORIA - ESTRATÉGIA FINAL ===`n"

$listaInicial = @(
    'ZCL_DOC_ELETRONICO', 'ZIF_DOC_ELETRONICO', 'ZCL_WEBSERVICE', 'ZIF_WEBSERVICE',
    'ZBRNFE_DANFE', 'ZFSD_BUSCA_DANFE', 'ZDEQUEUE_ALL', 'ZGRC_LIMPA_REF_MIRO_FISCAL',
    'Z_DETALHAMENTO_CTE', 'Z_DETALHAMENTO_CTE_IN_MASSA', 'Z_DETALHAMENTO_CTE_XML',
    'Z_DETALHAMENTO_NFE', 'Z_GRC_AJUSTA_TP_EMISSAO', 'Z_GRC_ARQUIVO_DOC',
    'Z_GRC_ARQUIVO_DOC_XML', 'Z_GRC_DOWNLOAD_XML_PDF', 'Z_GRC_ENVIA_LEGADO',
    'Z_GRC_GET_STATUS_DOC', 'Z_GRC_MDFE_AVULSA', 'Z_GRC_MDFE_LOAD',
    'Z_GRC_MONTA_LINK', 'Z_GRC_NEW_NFE', 'Z_GRC_REGISTRA_INF_ZIB_NFE',
    'Z_GRC_REGISTRA_LOG_DOC', 'Z_GRC_SEND_EMAIL_AUTO', 'Z_J_1BMFE_CANCEL_EVENT_SEND',
    'Z_J_1B_EVENT_CANCEL_NF_CTE', 'Z_J_1B_MDFE_CANCEL', 'Z_J_1B_MDFE_CLOSE',
    'Z_J_1B_MDFE_XML_OUT', 'Z_J_1B_NF_OBJECT_ADD', 'Z_SHOW_DETALHAMENTO_CTE',
    'Z_SHOW_DETALHAMENTO_NFE'
)

$tiposDicionario = @('.tabl.', '.dtel.', '.doma.', '.ttyp.', '.shlp.', '.view.', '.ddls.', '.enqu.', '.nrob.')

Write-Output "ESTRATÉGIA:"
Write-Output "1. LISTA INICIAL (33 objetos)"
Write-Output "2. LISTA 'OBJETOS REFERENCIADOS' = NÃO-dicionário direto da lista inicial"
Write-Output "3. Lista 'OBJETOS REFERENCIADOS' → pode referenciar mais NÃO-dicionário (profundidade 1)"
Write-Output "4. DICIONÁRIO: recursivo infinito (desde que venha de lista inicial ou 'OBJETOS REFERENCIADOS')"
Write-Output "5. BLOQUEIO: Dicionário → NÃO-dicionário = NUNCA`n"

# Identificar function groups da lista inicial
$functionGroupsNecessarios = @{}
Write-Output "Identificando function groups da lista inicial..."
foreach ($obj in $listaInicial) {
    $objLower = $obj.ToLower()
    $arquivos = Get-ChildItem -Path "src\" -Recurse -File | Where-Object { 
        $_.Name -match "\.fugr\.$objLower\.abap$" 
    }
    if ($arquivos) {
        foreach ($arq in $arquivos) {
            if ($arq.Name -match '^([^.]+)\.fugr\.') {
                $fg = $matches[1].ToUpper()
                $functionGroupsNecessarios[$fg] = $true
            }
        }
    }
}
Write-Output "Function groups da lista: $($functionGroupsNecessarios.Keys -join ', ')`n"

# Coletar arquivos
Write-Output "Coletando arquivos..."
$todosArquivos = Get-ChildItem -Path "src\" -Recurse -File | Where-Object { 
    $_.Name -match '^z.*\.' -and $_.Extension -in @('.xml', '.abap', '.asddls', '.baseinfo')
}
Write-Output "Total: $($todosArquivos.Count)`n"

# Criar mapa
$mapaObjetos = @{}
foreach ($arquivo in $todosArquivos) {
    $nome = $arquivo.Name
    if ($nome -match '^(z[^.]+)\.') {
        $nomeObj = $matches[1].ToUpper()
        if (-not $mapaObjetos.ContainsKey($nomeObj)) {
            $mapaObjetos[$nomeObj] = @{
                'Arquivos' = @()
                'Tipo' = 'OUTROS'
            }
        }
        $mapaObjetos[$nomeObj]['Arquivos'] += $arquivo.FullName
        
        $ehDicionario = $false
        foreach ($tipoDic in $tiposDicionario) {
            if ($nome -match $tipoDic) {
                $mapaObjetos[$nomeObj]['Tipo'] = 'DICIONARIO'
                $ehDicionario = $true
                break
            }
        }
        if (-not $ehDicionario -and $nome -match '\.(clas|intf|prog|fugr|msag)\.') {
            $mapaObjetos[$nomeObj]['Tipo'] = 'NAO_DICIONARIO'
        }
    }
}

Write-Output "Objetos únicos: $($mapaObjetos.Count)`n"

# Função de extração
function Get-ReferenciasZ {
    param($caminhoArquivo)
    
    $referencias = @()
    try {
        $conteudo = Get-Content $caminhoArquivo -Raw -ErrorAction Stop
        
        $padroes = @(
            '(?:ROLLNAME|REFNAME|TABNAME|DOMNAME|PRECFIELD|CONVEXIT|ROWTYPE|DATATYPE)>\s*(Z[A-Z0-9_]+)\s*<',
            '(?:TYPE|DATA|LIKE)\s+(?:TABLE OF\s+|REF TO\s+)?(Z[A-Z0-9_]+)',
            '(?:FROM|INTO|UPDATE)\s+(Z[A-Z0-9_]+)',
            'CALL\s+(?:FUNCTION|METHOD)\s+[''"]?(Z[A-Z0-9_]+)',
            '<(?:CLSNAME|INTFNAME|TABNAME|TYPENAME)>(Z[A-Z0-9_]+)</',
            '(?:INCLUDE|VIEW|DEFINE\s+VIEW)\s+(Z[A-Z0-9_]+)',
            '<SELMETHOD>(Z[A-Z0-9_]+)<',
            'STRUCTURE\s+(Z[A-Z0-9_]+)',
            '<VIEWNAME>(Z[A-Z0-9_]+)<',
            'CREATE\s+OBJECT\s+.*\s+TYPE\s+(Z[A-Z0-9_]+)',
            'RAISING\s+(Z[A-Z0-9_]+)',
            'CATCH\s+(Z[A-Z0-9_]+)'
        )
        
        foreach ($padrao in $padroes) {
            $matches = [regex]::Matches($conteudo, $padrao, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
            foreach ($match in $matches) {
                if ($match.Groups.Count -ge 2) {
                    $ref = $match.Groups[1].Value.ToUpper()
                    if ($ref -match '^Z' -and $ref.Length -ge 3) {
                        $referencias += $ref
                    }
                }
            }
        }
    }
    catch { }
    
    return $referencias | Select-Object -Unique
}

# Estruturas de controle
$objetosNecessarios = @{}
$listaObjetosReferenciados = @{}  # Segunda lista
$origemObjeto = @{}  # Rastreamento

# PASSO 1: Adicionar lista inicial
Write-Output "PASSO 1: Processando LISTA INICIAL...`n"
foreach ($obj in $listaInicial) {
    $objetosNecessarios[$obj] = $true
    $origemObjeto[$obj] = 'LISTA_INICIAL'
}

# Adicionar function groups necessários
foreach ($fg in $functionGroupsNecessarios.Keys) {
    if (-not $objetosNecessarios.ContainsKey($fg)) {
        $objetosNecessarios[$fg] = $true
        $origemObjeto[$fg] = 'FG_DA_LISTA'
    }
}

Write-Output "Lista inicial: $($objetosNecessarios.Count) objetos`n"

# PASSO 2: Processar lista inicial → criar "OBJETOS REFERENCIADOS"
Write-Output "PASSO 2: Criando lista 'OBJETOS REFERENCIADOS'...`n"
$processados = @{}

foreach ($obj in $objetosNecessarios.Keys) {
    if ($mapaObjetos.ContainsKey($obj)) {
        $arquivos = $mapaObjetos[$obj]['Arquivos']
        
        foreach ($arquivo in $arquivos) {
            $referencias = Get-ReferenciasZ -caminhoArquivo $arquivo
            
            foreach ($ref in $referencias) {
                if ($mapaObjetos.ContainsKey($ref) -and -not $objetosNecessarios.ContainsKey($ref)) {
                    $tipoRef = $mapaObjetos[$ref]['Tipo']
                    
                    if ($tipoRef -eq 'NAO_DICIONARIO') {
                        # Adicionar à lista "OBJETOS REFERENCIADOS"
                        $listaObjetosReferenciados[$ref] = $true
                        $objetosNecessarios[$ref] = $true
                        $origemObjeto[$ref] = "REF_LISTA_INICIAL($obj)"
                    }
                    # Dicionário será processado depois
                }
            }
        }
    }
}

Write-Output "Lista 'OBJETOS REFERENCIADOS': $($listaObjetosReferenciados.Count) objetos`n"

# PASSO 3: Processar "OBJETOS REFERENCIADOS" → mais NÃO-dicionário (profundidade 1)
Write-Output "PASSO 3: Processando 'OBJETOS REFERENCIADOS' (profundidade 1)...`n"
$objetosReferenciados2 = @{}

foreach ($obj in $listaObjetosReferenciados.Keys) {
    if ($mapaObjetos.ContainsKey($obj)) {
        $arquivos = $mapaObjetos[$obj]['Arquivos']
        
        foreach ($arquivo in $arquivos) {
            $referencias = Get-ReferenciasZ -caminhoArquivo $arquivo
            
            foreach ($ref in $referencias) {
                if ($mapaObjetos.ContainsKey($ref) -and -not $objetosNecessarios.ContainsKey($ref)) {
                    $tipoRef = $mapaObjetos[$ref]['Tipo']
                    
                    if ($tipoRef -eq 'NAO_DICIONARIO') {
                        # Profundidade 1 dos "OBJETOS REFERENCIADOS"
                        $objetosReferenciados2[$ref] = $true
                        $objetosNecessarios[$ref] = $true
                        $origemObjeto[$ref] = "REF_NIVEL2($obj)"
                    }
                }
            }
        }
    }
}

Write-Output "Objetos referenciados nível 2: $($objetosReferenciados2.Count)`n"

# PASSO 4: Processar DICIONÁRIO (recursivo infinito)
Write-Output "PASSO 4: Processando DICIONÁRIO (recursivo)...`n"

$filaDicionario = New-Object System.Collections.Queue

# Adicionar à fila: lista inicial + objetos referenciados (todos os níveis)
foreach ($obj in $objetosNecessarios.Keys) {
    if ($mapaObjetos.ContainsKey($obj)) {
        $filaDicionario.Enqueue($obj)
    }
}

$iteracao = 0
$dicionarioAdicionado = 0

while ($filaDicionario.Count -gt 0) {
    $iteracao++
    $objetoAtual = $filaDicionario.Dequeue()
    
    if ($mapaObjetos.ContainsKey($objetoAtual)) {
        $arquivos = $mapaObjetos[$objetoAtual]['Arquivos']
        
        foreach ($arquivo in $arquivos) {
            $referencias = Get-ReferenciasZ -caminhoArquivo $arquivo
            
            foreach ($ref in $referencias) {
                if ($mapaObjetos.ContainsKey($ref) -and -not $objetosNecessarios.ContainsKey($ref)) {
                    $tipoRef = $mapaObjetos[$ref]['Tipo']
                    
                    if ($tipoRef -eq 'DICIONARIO') {
                        # DICIONÁRIO: adicionar e processar recursivamente
                        $objetosNecessarios[$ref] = $true
                        $origemObjeto[$ref] = "DICIONARIO($objetoAtual)"
                        $filaDicionario.Enqueue($ref)
                        $dicionarioAdicionado++
                    }
                    # NÃO-dicionário: ignorar (já foi processado nos passos 2 e 3)
                }
            }
        }
    }
    
    if ($iteracao % 100 -eq 0) {
        Write-Output "Iteração $iteracao | Dicionário adicionado: $dicionarioAdicionado | Fila: $($filaDicionario.Count)"
    }
}

Write-Output "`nDicionário adicionado: $dicionarioAdicionado`n"

# RESULTADO
Write-Output "=== RESULTADO ===`n"
Write-Output "Total necessários: $($objetosNecessarios.Count)"
Write-Output "  - Lista inicial: $($listaInicial.Count + $functionGroupsNecessarios.Count)"
Write-Output "  - Objetos referenciados nível 1: $($listaObjetosReferenciados.Count)"
Write-Output "  - Objetos referenciados nível 2: $($objetosReferenciados2.Count)"
Write-Output "  - Dicionário (recursivo): $dicionarioAdicionado"
Write-Output "`nTotal no repositório: $($mapaObjetos.Count)"
Write-Output "Para remover: $($mapaObjetos.Count - $objetosNecessarios.Count)`n"

# Identificar para remover
$objetosParaRemover = @()
foreach ($obj in $mapaObjetos.Keys) {
    if (-not $objetosNecessarios.ContainsKey($obj)) {
        $objetosParaRemover += $obj
    }
}

if ($objetosParaRemover.Count -gt 0) {
    Write-Output "=== PARA REMOVER: $($objetosParaRemover.Count) ===`n"
    
    $objetosParaRemover | Sort-Object | Out-File "objetos_para_remover_estrategia_final.txt" -Encoding UTF8
    Write-Output "Lista salva em: objetos_para_remover_estrategia_final.txt"
    
    # Amostras
    Write-Output "`nAmostras (primeiros 20):"
    $objetosParaRemover | Sort-Object | Select-Object -First 20 | ForEach-Object {
        $tipo = $mapaObjetos[$_]['Tipo']
        Write-Output "  - $_ ($tipo)"
    }
}
else {
    Write-Output "✅ Repositório correto! Nenhum objeto para remover."
}

Write-Output "`n=== FIM ==="

