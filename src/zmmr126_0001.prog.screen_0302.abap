PROCESS BEFORE OUTPUT.

  MODULE status_0302.

  CALL SUBSCREEN: sub0310 INCLUDING sy-cprog vg_tl_0310.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub0310.

  FIELD zde_zsdt0001ov_alv-nr_ordem_venda MODULE set_ordem ON REQUEST.

  FIELD zde_zsdt0001cg_alv-nr_ordem MODULE validar_nr_ordem ON REQUEST.

  CHAIN.
    FIELD zde_zsdt0001ov_alv-nr_ordem_venda.
    MODULE atribui_info_carga ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD zde_zsdt0001cg_alv-nr_ordem.
    FIELD zde_zsdt0001cg_alv-id_local_entrega.
    FIELD zde_zsdt0001cg_alv-id_local_coleta.
    FIELD zde_zsdt0001cg_alv-id_local_destino.
    FIELD zde_zsdt0001cg_alv-id_local_descarga.

    "FRETE
    FIELD zde_zsdt0001cg_alv-id_motorista.
    FIELD zde_zsdt0001cg_alv-id_agent_frete.
    FIELD zde_zsdt0001cg_alv-id_proprietario.
    FIELD zde_zsdt0001cg_alv-ds_placa_trator.
    FIELD zde_zsdt0001cg_alv-ds_placa_reboq_1.
    FIELD zde_zsdt0001cg_alv-ds_placa_reboq_2.
    FIELD zde_zsdt0001cg_alv-ds_placa_reboq_3.
    FIELD zde_zsdt0001cg_alv-ck_gera_aviso.
    MODULE atribui_info_carga ON CHAIN-REQUEST.
  ENDCHAIN.

  FIELD zde_zsdt0001cg_alv-nr_ordem MODULE set_ordem_carreg ON REQUEST.

  MODULE user_command_0302.

PROCESS ON VALUE-REQUEST.
  "Pesquisa de Ordem de Venda
  FIELD zde_zsdt0001ov_alv-nr_ordem_venda MODULE value_request_ov.
