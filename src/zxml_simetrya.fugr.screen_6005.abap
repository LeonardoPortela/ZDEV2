
PROCESS BEFORE OUTPUT.
  MODULE status_6005.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_ciot-vlr_inss.
    FIELD zcte_ciot-vlr_sest.
    FIELD zcte_ciot-vlr_irpf.
    FIELD zcte_ciot-vlr_iof.
    FIELD zcte_ciot-vlr_iss.
    FIELD zcte_ciot-vlr_impostos.
    FIELD zcte_ciot-perc_tolerancia.
    FIELD zcte_ciot-vlr_unit_merc.
    FIELD zcte_ciot-unid_vlr_merc.
    FIELD zcte_ciot-vlr_unit_frete.
    FIELD zcte_ciot-unid_vlr_frete.
    FIELD zcte_ciot-resp_pagamento.
    FIELD zcte_ciot-peso_chegada.
    FIELD zcte_ciot-nr_bc_banco.
    FIELD zcte_ciot-nr_bc_agencia.
    FIELD zcte_ciot-nr_bc_conta.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
