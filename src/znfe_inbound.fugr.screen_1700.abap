PROCESS BEFORE OUTPUT.

  MODULE status_1700.
*
PROCESS AFTER INPUT.

  MODULE user_command_1700_exit AT EXIT-COMMAND.

  FIELD zde_nfe_dist_alv-zlspr
  MODULE alterar_bloqueio_pagamento ON REQUEST.

  FIELD zde_nfe_dist_alv-ctr_valor_total
  MODULE alterar_ctr_valor_total ON REQUEST.

  FIELD zde_nfe_dist_alv-dt_vencimento
  MODULE verificar_data_vencimento ON REQUEST.

  FIELD zde_nfe_dist_alv-zbvtyp
  MODULE atribuir_banco_parceiro ON REQUEST.

  FIELD zde_nfe_dist_alv-pymt_meth
  MODULE atribuir_meio_pagamento ON REQUEST.

  FIELD zde_nfe_dist_alv-housebankid
  MODULE atribuir_banco_empresa ON REQUEST.

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

  CHAIN.
    FIELD zde_nfe_dist_alv-boleto.
    MODULE alterou_boleto ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-aut_embarque.
    MODULE alterou_aut_embarque ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_nfe_dist_alv-placa_cav.
    MODULE alterou_placa_cav ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_1700.
