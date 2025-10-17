# GUIA PASSO A PASSO - REFATORAÇÃO ZDEV_REFATORADO

## Resumo da Situação Atual

✅ **COMPLETO**: Mapeamento de objetos principais
- 182 arquivos identificados nos objetos da lista
- 7 componentes principais mapeados:
  1. ZCL_DOC_ELETRONICO + ZIF_DOC_ELETRONICO (4 arquivos)
  2. ZCL_WEBSERVICE + ZIF_WEBSERVICE (14 arquivos)
  3. ZGRC (134 arquivos - contém todos os FM da lista)
  4. ZBRNFE_DANFE (22 arquivos)
  5. ZBR_GPF_NFE_DANFE (8 arquivos)

## Passo 1: Extrair Tabelas Referenciadas

Execute o seguinte comando PowerShell para extrair todas as tabelas Z referenciadas:

```powershell
# Criar arquivo com todas as tabelas encontradas
$tabelas = @{}

Get-ChildItem -Path src -Filter "*.abap" -Recurse | Where-Object { 
    $_.Name -match "(zcl_doc_eletronico|zif_doc_eletronico|zcl_webservice|zif_webservice|zgrc|zbrnfe_danfe|zbr_gpf)" 
} | ForEach-Object {
    $conteudo = Get-Content $_.FullName -Raw
    
    # Extrair tabelas após FROM
    $matches = [regex]::Matches($conteudo, "FROM\s+([Z][A-Z0-9_]+)", "IgnoreCase")
    foreach ($match in $matches) {
        $tabela = $match.Groups[1].Value.ToUpper()
        if (-not $tabelas.ContainsKey($tabela)) {
            $tabelas[$tabela] = 0
        }
        $tabelas[$tabela]++
    }
    
    # Extrair TYPE
    $matches = [regex]::Matches($conteudo, "TYPE\s+([Z][A-Z0-9_]+)", "IgnoreCase")
    foreach ($match in $matches) {
        $tipo = $match.Groups[1].Value.ToUpper()
        if (-not $tabelas.ContainsKey($tipo)) {
            $tabelas[$tipo] = 0
        }
        $tabelas[$tipo]++
    }
}

# Salvar resultado
$tabelas.GetEnumerator() | Sort-Object Value -Descending | ForEach-Object {
    "$($_.Key) : $($_.Value) referencias"
} | Out-File "tabelas_referenciadas.txt"

Write-Host "Arquivo tabelas_referenciadas.txt criado com sucesso!"
Write-Host "Total de objetos Z encontrados: $($tabelas.Count)"
```

## Passo 2: Identificar Arquivos de Dicionário a Manter

Execute este comando para listar os arquivos de dicionário das tabelas encontradas:

```powershell
# Ler lista de tabelas
$tabelas = Get-Content "tabelas_referenciadas.txt" | ForEach-Object {
    ($_ -split " : ")[0].Trim()
}

$arquivosManter = @()

# Para cada tabela, procurar arquivos de dicionário
foreach ($tabela in $tabelas) {
    $tabelaLower = $tabela.ToLower()
    
    # Procurar .tabl.xml, .dtel.xml, .doma.xml, .ttyp.xml
    $extensoes = @('.tabl.xml', '.dtel.xml', '.doma.xml', '.ttyp.xml', '.shlp.xml')
    
    foreach ($ext in $extensoes) {
        $arquivos = Get-ChildItem -Path src -Filter "*$tabelaLower$ext" -Recurse
        if ($arquivos) {
            $arquivosManter += $arquivos
            Write-Host "Manter: $($arquivos[0].Name)"
        }
    }
}

Write-Host ""
Write-Host "Total de arquivos de dicionario a manter: $($arquivosManter.Count)"

# Salvar lista
$arquivosManter | Select-Object FullName | Export-Csv "arquivos_dicionario_manter.csv" -NoTypeInformation
```

## Passo 3: Listar Objetos para Excluir

Execute para identificar o que será excluído:

```powershell
# Listar todos os objetos Z que serão excluídos

$objetosManter = @(
    'zcl_doc_eletronico',
    'zif_doc_eletronico',
    'zcl_webservice',
    'zif_webservice',
    'zbrnfe_danfe',
    'zgrc',
    'zbr_gpf_nfe_danfe'
)

# Carregar lista de dicionário a manter
$dicionarioManter = @()
if (Test-Path "arquivos_dicionario_manter.csv") {
    $dicionarioManter = Import-Csv "arquivos_dicionario_manter.csv" | ForEach-Object { $_.FullName }
}

$arquivosExcluir = @()
$totalArquivos = 0

Get-ChildItem -Path src -Filter "z*.abap" -Recurse | ForEach-Object {
    $totalArquivos++
    $nomeBase = $_.Name.Split('.')[0].ToLower()
    
    $deveManter = $false
    foreach ($obj in $objetosManter) {
        if ($nomeBase.StartsWith($obj)) {
            $deveManter = $true
            break
        }
    }
    
    if (-not $deveManter -and $dicionarioManter -notcontains $_.FullName) {
        $arquivosExcluir += $_
    }
}

Write-Host "Total de arquivos Z analisados: $totalArquivos"
Write-Host "Arquivos a excluir: $($arquivosExcluir.Count)"
Write-Host ""
Write-Host "Primeiros 50 arquivos que serão excluídos:"
$arquivosExcluir | Select-Object -First 50 | ForEach-Object {
    Write-Host "  - $($_.Name)"
}

# Salvar lista completa
$arquivosExcluir | Select-Object FullName, Name | Export-Csv "arquivos_para_excluir.csv" -NoTypeInformation
Write-Host ""
Write-Host "Lista completa salva em: arquivos_para_excluir.csv"
```

## Passo 4: BACKUP antes de Excluir

**IMPORTANTE**: Antes de excluir qualquer arquivo, faça um backup:

```powershell
# Fazer backup
git add .
git commit -m "BACKUP antes da refatoracao"
git push origin ZDEV_REFATORADO

Write-Host "Backup realizado com sucesso!"
```

## Passo 5: Executar Exclusão (COM CUIDADO!)

Revise o arquivo `arquivos_para_excluir.csv` antes de executar:

```powershell
# LER O CSV E CONFIRMAR ANTES DE EXCLUIR!

$confirmar = Read-Host "ATENCAO! Voce revisou o arquivo arquivos_para_excluir.csv? (sim/nao)"

if ($confirmar -eq "sim") {
    $arquivos = Import-Csv "arquivos_para_excluir.csv"
    
    $total = $arquivos.Count
    $contador = 0
    
    foreach ($arquivo in $arquivos) {
        if (Test-Path $arquivo.FullName) {
            Remove-Item $arquivo.FullName -Force
            $contador++
            
            if ($contador % 100 -eq 0) {
                Write-Host "Excluidos $contador de $total arquivos..."
            }
        }
    }
    
    Write-Host ""
    Write-Host "Exclusao concluida! $contador arquivos removidos."
    Write-Host ""
    Write-Host "Execute: git status para ver as alteracoes"
} else {
    Write-Host "Operacao cancelada."
}
```

## Passo 6: Validação Final

Após a exclusão, valide:

```powershell
# Contar arquivos restantes
Write-Host "Objetos Principais:"
foreach ($obj in $objetosManter) {
    $count = (Get-ChildItem -Path src -Filter "*$obj*" -Recurse).Count
    Write-Host "  $obj : $count arquivos"
}

Write-Host ""
Write-Host "Status do Git:"
git status --short
```

## Passo 7: Commit e Push

Se tudo estiver correto:

```bash
git add .
git commit -m "Refatoracao: remocao de objetos nao utilizados"
git push origin ZDEV_REFATORADO
```

## Checklist de Verificação

Antes de fazer push, verifique:

- [ ] Todos os objetos principais ainda existem?
- [ ] Arquivos de dicionário referenciados foram mantidos?
- [ ] Nenhum arquivo Standard SAP foi excluído?
- [ ] Backup foi realizado antes da exclusão?
- [ ] CSV de exclusão foi revisado manualmente?

## Suporte

Em caso de dúvidas ou problemas:

1. Verifique o arquivo `RELATORIO_REFATORACAO.md`
2. Revise os CSVs gerados
3. Use `git log` para verificar o histórico
4. Para reverter: `git reset --hard HEAD~1` (use com cuidado!)

## Arquivos Gerados

Este processo gera os seguintes arquivos:

1. `tabelas_referenciadas.txt` - Lista de todas as tabelas/tipos referenciados
2. `arquivos_dicionario_manter.csv` - Arquivos de dicionário a manter
3. `arquivos_para_excluir.csv` - Arquivos que serão excluídos
4. `RELATORIO_REFATORACAO.md` - Relatório completo
5. `GUIA_REFATORACAO.md` - Este guia

---

**Data**: 16/10/2025  
**Branch**: ZDEV_REFATORADO  
**Repositório**: https://github.com/LeonardoPortela/ZDEV2.git


