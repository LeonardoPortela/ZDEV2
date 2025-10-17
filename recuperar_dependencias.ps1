# Script para recuperar dependencias faltantes da branch main

Write-Output "Iniciando recuperacao de dependencias..." | Out-File -FilePath "log_recuperacao.txt"

# Lista de arquivos a recuperar
$arquivos = @(
    "src/zdm_tp_ambiente.doma.xml",
    "src/zde_tp_ambiente.dtel.xml",
    "src/zxnfe_lineid.doma.xml",
    "src/zxnfe_lineid.dtel.xml",
    "src/zxnfe_text_ref.dtel.xml",
    "src/zde_cte_dist_ter.tabl.xml",
    "src/zde_nfe_entrada.tabl.xml",
    "src/zde_nfe_inbound_vlr_ctr.tabl.xml",
    "src/zde_se_workflow.tabl.xml",
    "src/zde_mdfe_prodpred.tabl.xml",
    "src/zsdt0237.tabl.xml",
    "src/zde_inf_cte_compl_obs_cont.tabl.xml",
    "src/zde_inf_cte_norm_inf_nfe.tabl.xml",
    "src/zde_inf_cte_norm_inf_nff.tabl.xml",
    "src/zde_zsdt0238.tabl.xml",
    "src/zde_zsdt0239.tabl.xml",
    "src/zde_zsdt0240.tabl.xml",
    "src/zde_zsdt0242.tabl.xml",
    "src/zdet_nitem.tabl.xml",
    "src/zdetpag.tabl.xml",
    "src/zxnfe_if_cte_infunidcarga_s.tabl.xml",
    "src/zxnfe_if_cte_infunidtransp_s.tabl.xml",
    "src/zxnfe_if_mdfe_autxml_s.tabl.xml",
    "src/zxnfe_if_mdfe_infcte_300_s.tabl.xml",
    "src/zxnfe_if_mdfe_infembcomb_300_s.tabl.xml",
    "src/zxnfe_if_mdfe_infmdfetra_300_s.tabl.xml",
    "src/zxnfe_if_mdfe_infnfe_300_s.tabl.xml",
    "src/zxnfe_if_mdfe_inftermcarreg_s.tabl.xml",
    "src/zxnfe_if_mdfe_inftermdescarr_s.tabl.xml",
    "src/zxnfe_if_mdfe_infunidcargava_s.tabl.xml",
    "src/zxnfe_if_mdfe_infunidtranspv_s.tabl.xml",
    "src/zxnfe_if_mdfe_inf_muncarrega_s.tabl.xml",
    "src/zxnfe_if_mdfe_infmundescarg_s.tabl.xml",
    "src/zxnfe_if_mdfe_inf_percurso_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_condutor_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_disp_300_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_inf_banco_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_inf_ciot_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_inf_comp_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_inf_cont_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_prop_s.tabl.xml",
    "src/zxnfe_if_mdfe_rodo_veic_s.tabl.xml",
    "src/zxnfe_if_mdfe_seg_s.tabl.xml",
    "src/zxnfe_if_mdfe_text_s.tabl.xml",
    # Table Types
    "src/zde_element_array_t.ttyp.xml",
    "src/zde_inf_cte_compl_obs_cont_t.ttyp.xml",
    "src/zde_inf_cte_norm_inf_nfe_t.ttyp.xml",
    "src/zde_inf_cte_norm_inf_nff_t.ttyp.xml",
    "src/zde_zsdt0238_t.ttyp.xml",
    "src/zde_zsdt0239_t.ttyp.xml",
    "src/zde_zsdt0240_t.ttyp.xml",
    "src/zde_zsdt0242_t.ttyp.xml",
    "src/zintg_det.ttyp.xml",
    "src/zintg_pag.ttyp.xml",
    "src/zxnfe_if_cte_infunidcarga_t.ttyp.xml",
    "src/zxnfe_if_cte_infunidtransp_t.ttyp.xml",
    "src/zxnfe_if_mdfe_autxml_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infcte_300_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infembcomb_300_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infmdfetra_300_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infnfe_300_t.ttyp.xml",
    "src/zxnfe_if_mdfe_inftermcarreg_t.ttyp.xml",
    "src/zxnfe_if_mdfe_inftermdescarr_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infunidcargava_t.ttyp.xml",
    "src/zxnfe_if_mdfe_infunidtranspv_t.ttyp.xml",
    "src/zxnfe_if_mdfe_inf_muncarrega_t.ttyp.xml",
    "src/zxnfe_if_mdfe_inf_mundescarg_t.ttyp.xml",
    "src/zxnfe_if_mdfe_inf_percurso_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_condutor_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_disp_300_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_inf_banco_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_inf_ciot_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_inf_comp_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_inf_cont_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_prop_t.ttyp.xml",
    "src/zxnfe_if_mdfe_rodo_veic_t.ttyp.xml",
    "src/zxnfe_if_mdfe_seg_t.ttyp.xml",
    "src/zxnfe_if_mdfe_text_t.ttyp.xml"
)

Write-Output "Total de arquivos a recuperar: $($arquivos.Count)" | Out-File -FilePath "log_recuperacao.txt" -Append

# Recuperar cada arquivo
$recuperados = 0
$erros = 0

foreach ($arquivo in $arquivos) {
    try {
        git checkout main -- $arquivo 2>&1 | Out-Null
        if ($LASTEXITCODE -eq 0) {
            $recuperados++
            Write-Output "OK: $arquivo" | Out-File -FilePath "log_recuperacao.txt" -Append
        } else {
            $erros++
            Write-Output "ERRO: $arquivo" | Out-File -FilePath "log_recuperacao.txt" -Append
        }
    }
    catch {
        $erros++
        Write-Output "ERRO: $arquivo - $_" | Out-File -FilePath "log_recuperacao.txt" -Append
    }
}

Write-Output "" | Out-File -FilePath "log_recuperacao.txt" -Append
Write-Output "========================================" | Out-File -FilePath "log_recuperacao.txt" -Append
Write-Output "RESULTADO:" | Out-File -FilePath "log_recuperacao.txt" -Append
Write-Output "  Recuperados: $recuperados" | Out-File -FilePath "log_recuperacao.txt" -Append
Write-Output "  Erros: $erros" | Out-File -FilePath "log_recuperacao.txt" -Append
Write-Output "========================================" | Out-File -FilePath "log_recuperacao.txt" -Append

# Contar total de arquivos agora
$totalArquivos = (Get-ChildItem -Path src -Recurse -File | Measure-Object).Count
Write-Output "Total de arquivos no src: $totalArquivos" | Out-File -FilePath "log_recuperacao.txt" -Append

Write-Output "Concluido! Ver log_recuperacao.txt para detalhes."

