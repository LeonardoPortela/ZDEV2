# Auditoria com LOGICA CORRETA
# Regra: NÃO-dicionario → NÃO-dicionario = profundidade infinita
#        NÃO-dicionario → DICIONARIO = profundidade infinita
#        DICIONARIO → DICIONARIO = profundidade infinita
#        DICIONARIO → NÃO-dicionario = NUNCA

Write-Output "=== AUDITORIA - LOGICA CORRETA ===`n"

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

Write-Output "REGRA:"
Write-Output "1. NAO-dicionario -> NAO-dicionario = infinito"
Write-Output "2. NAO-dicionario -> DICIONARIO = infinito"
Write-Output "3. DICIONARIO -> DICIONARIO = infinito"
Write-Output "4. DICIONARIO -> NAO-dicionario = NUNCA`n"

# Identificar function groups
$functionGroupsNecessarios = @{}
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

# Coletar arquivos
$todosArquivos = Get-ChildItem -Path "src\" -Recurse -File | Where-Object { 
    $_.Name -match '^z.*\.' -and $_.Extension -in @('.xml', '.abap', '.asddls', '.baseinfo')
}

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
        if (-not $ehDicionario -and $nome -match '\.(clas|intf|prog|fugr|msag|ssfo|ssst)\.') {
            $mapaObjetos[$nomeObj]['Tipo'] = 'NAO_DICIONARIO'
        }
    }
}

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
            '<(?:CLSNAME|INTFNAME|TABNAME|TYPENAME|SHLPNAME|CHECKTABLE|ENTITYTAB)>(Z[A-Z0-9_]+)</',
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

# Processamento
$objetosNecessarios = @{}
$origemObjeto = @{}
$fila = New-Object System.Collections.Queue

# Adicionar lista inicial
foreach ($obj in $listaInicial) {
    $fila.Enqueue(@{ 'Nome' = $obj; 'Origem' = 'LISTA_INICIAL'; 'TipoOrigem' = 'NAO_DICIONARIO' })
    $objetosNecessarios[$obj] = $true
    $origemObjeto[$obj] = 'LISTA_INICIAL'
}

# Adicionar function groups
foreach ($fg in $functionGroupsNecessarios.Keys) {
    if (-not $objetosNecessarios.ContainsKey($fg)) {
        $fila.Enqueue(@{ 'Nome' = $fg; 'Origem' = 'FG'; 'TipoOrigem' = 'NAO_DICIONARIO' })
        $objetosNecessarios[$fg] = $true
        $origemObjeto[$fg] = 'FG'
    }
}

$iteracao = 0

while ($fila.Count -gt 0) {
    $iteracao++
    $item = $fila.Dequeue()
    $objetoAtual = $item['Nome']
    $tipoOrigem = $item['TipoOrigem']
    
    if ($mapaObjetos.ContainsKey($objetoAtual)) {
        $tipoAtual = $mapaObjetos[$objetoAtual]['Tipo']
        $arquivos = $mapaObjetos[$objetoAtual]['Arquivos']
        
        foreach ($arquivo in $arquivos) {
            $referencias = Get-ReferenciasZ -caminhoArquivo $arquivo
            
            foreach ($ref in $referencias) {
                if ($mapaObjetos.ContainsKey($ref) -and -not $objetosNecessarios.ContainsKey($ref)) {
                    $tipoRef = $mapaObjetos[$ref]['Tipo']
                    
                    # LOGICA CORRETA:
                    if ($tipoAtual -eq 'NAO_DICIONARIO') {
                        # NAO-dicionario -> qualquer coisa: adicionar
                        $objetosNecessarios[$ref] = $true
                        $origemObjeto[$ref] = $objetoAtual
                        $fila.Enqueue(@{ 'Nome' = $ref; 'Origem' = $objetoAtual; 'TipoOrigem' = $tipoRef })
                    }
                    elseif ($tipoAtual -eq 'DICIONARIO' -and $tipoRef -eq 'DICIONARIO') {
                        # DICIONARIO -> DICIONARIO: adicionar
                        $objetosNecessarios[$ref] = $true
                        $origemObjeto[$ref] = $objetoAtual
                        $fila.Enqueue(@{ 'Nome' = $ref; 'Origem' = $objetoAtual; 'TipoOrigem' = $tipoRef })
                    }
                    # DICIONARIO -> NAO-dicionario: NÃO adicionar (bloqueio)
                }
            }
        }
    }
    
    if ($iteracao % 100 -eq 0) {
        Write-Output "Iteracao $iteracao | Necessarios: $($objetosNecessarios.Count) | Fila: $($fila.Count)"
    }
}

Write-Output "`n=== RESULTADO ===`n"
Write-Output "Total necessarios: $($objetosNecessarios.Count)"
Write-Output "Total no repositorio: $($mapaObjetos.Count)"
Write-Output "Para remover: $($mapaObjetos.Count - $objetosNecessarios.Count)`n"

# Identificar para remover
$objetosParaRemover = @()
foreach ($obj in $mapaObjetos.Keys) {
    if (-not $objetosNecessarios.ContainsKey($obj)) {
        $objetosParaRemover += $obj
    }
}

if ($objetosParaRemover.Count -gt 0) {
    $objetosParaRemover | Sort-Object | Out-File "objetos_para_remover_logica_correta.txt" -Encoding UTF8
    Write-Output "Objetos para remover salvos!"
    Write-Output "Primeiros 20:"
    $objetosParaRemover | Sort-Object | Select-Object -First 20 | ForEach-Object {
        Write-Output "  - $_"
    }
}
else {
    Write-Output "Repositorio correto!"
}

Write-Output "`n=== FIM ==="

