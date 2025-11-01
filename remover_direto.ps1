# Remoção Direta de Pacotes

Write-Host "=== REMOVENDO PACOTES ===" -ForegroundColor Cyan
$antes = (Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File).Count
Write-Host "Arquivos antes: $antes"

# Remover pacotes
Write-Host "Removendo zapp_aprovacao..." -ForegroundColor Yellow
Remove-Item "EASYSAP_REFATORADO\src\zapp_aprovacao" -Recurse -Force -ErrorAction SilentlyContinue

Write-Host "Removendo zca..." -ForegroundColor Yellow
Remove-Item "EASYSAP_REFATORADO\src\zca" -Recurse -Force -ErrorAction SilentlyContinue

Write-Host "Removendo zequi_patrimonial..." -ForegroundColor Yellow
Remove-Item "EASYSAP_REFATORADO\src\zequi_patrimonial" -Recurse -Force -ErrorAction SilentlyContinue

Write-Host "Removendo zsd_rel_contas_rec..." -ForegroundColor Yellow
Remove-Item "EASYSAP_REFATORADO\src\zsd_rel_contas_rec" -Recurse -Force -ErrorAction SilentlyContinue

$depois = (Get-ChildItem "EASYSAP_REFATORADO\src" -Recurse -File).Count
Write-Host "Arquivos depois: $depois"
Write-Host "Removidos: $($antes - $depois)" -ForegroundColor Green
Write-Host "Concluído!" -ForegroundColor Green

