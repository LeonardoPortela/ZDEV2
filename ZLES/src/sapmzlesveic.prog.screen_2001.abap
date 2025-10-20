
PROCESS BEFORE OUTPUT.
  MODULE status_2001.
  MODULE detail_init.

PROCESS AFTER INPUT.
  MODULE detail_exit_command AT EXIT-COMMAND.
  MODULE detail_set_pfstatus.

  CHAIN.
    FIELD zlest0002-pc_veiculo .
    FIELD zlest0002-proprietario .
    FIELD zlest0002-cd_cidade .
    FIELD zlest0002-cd_uf .
    FIELD zlest0002-chassi .
    FIELD zlest0002-ano .
    FIELD zlest0002-ct_veiculo .
    FIELD zlest0002-tp_carroceria .
    FIELD zlest0002-cd_renavam .
    FIELD zlest0002-observacoes .
    FIELD zlest0002-irregularidade .
    FIELD zlest0002-agregado .
    FIELD zlest0002-st_bloqueio .
    FIELD zlest0002-tp_veiculo .
    FIELD zlest0002-tp_rodado .
    FIELD zlest0002-tp_carroceria2 .
    FIELD zlest0002-tara .
    FIELD zlest0002-cap_kg .
    FIELD zlest0002-cap_m3 .
    FIELD zlest0002-qt_eixo .
    FIELD zlest0002-status .
    FIELD zlest0002-erdat .
    FIELD zlest0002-erzet .
    FIELD zlest0002-ernam .
    FIELD zlest0002-country.
    FIELD zlest0002-taxjurcode.
    FIELD zlest0002-kalsm.
    FIELD zlest0002-spras.
    FIELD zlest0002-marca.
    FIELD zlest0002-modelo.
    FIELD zlest0002-cor.
    FIELD zlest0002-pstlz.
    FIELD zlest0002-dt_venc_cto.
    FIELD zlest0002-cto_comodato.
    FIELD zlest0002-dt_modificacao.
    FIELD zlest0002-usr_modificacao.
    FIELD zlest0002-grupo.
    FIELD zlest0002-frota.
    FIELD zlest0002-werks.
    FIELD zlest0002-equnr.
    FIELD zlest0002-cod_antena.
    FIELD zlest0002-propriet_comodato.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
  FIELD nome_fornec      MODULE busca_nome_fornecedor.
  FIELD bahns            MODULE busca_bahns_fornecedor.

  MODULE trata_comodato.

  FIELD nome_fornec2      MODULE busca_nome_fornecedor2.
  FIELD bahns2            MODULE busca_bahns_fornecedor.
  FIELD zlest0002-equnr  MODULE busca_numero_equip. "Busca cód eqpto PM.
  MODULE user_command_2001.

PROCESS ON VALUE-REQUEST.

  FIELD zlest0002-taxjurcode MODULE f_d0100_taxjurcode.
  FIELD zlest0002-modelo     MODULE f_modelo.
  "  field zlest0002-CTO_COMODATO module teste.
