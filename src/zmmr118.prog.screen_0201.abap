PROCESS BEFORE OUTPUT.

  MODULE status_0201.

  CALL SUBSCREEN sub201 INCLUDING sy-repid sb_tela_0201 .

PROCESS AFTER INPUT.

*  FIELD ZDE_NFE_DIST_ALV-CD_DEPARTAMENTO
*  MODULE ATRIBUIR_CD_DEPARTAMENTO ON REQUEST.

  FIELD zde_nfe_dist_alv-zlspr
  MODULE alterar_bloqueio_pagamento ON REQUEST.

  FIELD zde_nfe_dist_alv-ctr_valor_total
  MODULE alterar_ctr_valor_total ON REQUEST.

*-CS2024000243-05.06.2024-#136397-JT-inicio
  FIELD zde_nfe_dist_alv-ck_fpol
  MODULE verificar_data_vencimento ON REQUEST.

  FIELD zde_nfe_dist_alv-obs_financeira
  MODULE verificar_data_vencimento ON REQUEST.
*-CS2024000243-05.06.2024-#136397-JT-fim

  FIELD zde_nfe_dist_alv-dt_vencimento
  MODULE verificar_data_vencimento ON REQUEST.

  FIELD zde_nfe_dist_alv-zbvtyp
  MODULE atribuir_banco_parceiro ON REQUEST.

  FIELD zde_nfe_dist_alv-pymt_meth
  MODULE atribuir_meio_pagamento ON REQUEST.

  FIELD zde_nfe_dist_alv-housebankid
  MODULE atribuir_banco_empresa ON REQUEST.

*  FIELD ZDE_NFE_DIST_ALV-CK_POSSUI_FRETE
*  MODULE ATRIBUIR_CK_POSSUI_FRETE ON REQUEST.

*  CHAIN.
*    FIELD ZDE_NFE_DIST_ALV-CD_DEPARTAMENTO.
*    MODULE ALTEROU_DEPARTAMEMTO ON CHAIN-REQUEST.
*  ENDCHAIN.

*  CHAIN.
*    FIELD ZDE_NFE_DIST_ALV-F_ARMAZEM.
*    MODULE ALTEROU_ARMAZEM ON CHAIN-REQUEST.
*  ENDCHAIN.

*  CHAIN.
*    FIELD ZDE_NFE_DIST_ALV-F_TRANSPORTE.
*    MODULE ALTEROU_TRANSPORTADOR ON CHAIN-REQUEST.
*  ENDCHAIN.

*  CHAIN.
*    FIELD ZDE_NFE_DIST_ALV-CK_POSSUI_FRETE.
*    MODULE ALTEROU_CK_POSSUI_FRETE ON CHAIN-REQUEST.
*  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-nr_fase.
    MODULE alterou_nr_fase ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-dt_vencimento.
    MODULE alterou_dt_vencimento ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-zbvtyp.
    MODULE alterou_zbvtyp ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-zlspr.
    MODULE alterou_zlspr ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-pymt_meth.
    MODULE alterou_pymt_meth ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-boleto.
    MODULE alterou_boleto ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-housebankid.
    MODULE alterou_housebankid ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-vlr_desconto.
    MODULE alterou_vlr_desconto ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-obs_financeira.
    MODULE alterou_obs_financeira ON CHAIN-REQUEST.
  ENDCHAIN.

*-CS2024000243-05.06.2024-#136397-JT-inicio
  CHAIN.
    FIELD zde_nfe_dist_alv-ck_fpol.
    MODULE alterou_ck_fpol ON CHAIN-REQUEST.
  ENDCHAIN.
*-CS2024000243-05.06.2024-#136397-JT-fim

  CALL SUBSCREEN sub201.

PROCESS ON VALUE-REQUEST.
  FIELD zde_nfe_dist_alv-pymt_meth MODULE search_forma.
