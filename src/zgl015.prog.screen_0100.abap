
PROCESS BEFORE OUTPUT.
  MODULE f_iniciar_tela.

*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TS_100'
  MODULE ts_100_active_tab_set.
  MODULE trata_fields.

  CALL SUBSCREEN ts_100_sca
    INCLUDING g_ts_100-prog g_ts_100-subscreen.

  MODULE status_0100.

  MODULE cria_objetos.

PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TS_100'
  CALL SUBSCREEN ts_100_sca.
  MODULE ts_100_active_tab_get.

  chain.
    field WG_ZGLT035-TP_LCTO.
    module  valida_parametros on chain-request.
 endchain.

  MODULE user_command_0100.

  process on value-request.
  field WG_ZGLT035-DOC_LCTO module search_doc.
