
PROCESS BEFORE OUTPUT.

  MODULE inicializa_detalhes_204.

  LOOP AT ti_204h_acrdecr WITH CONTROL vg_sbs204h_tabcontrol.
    MODULE change_table_control_204h.
  ENDLOOP.

  LOOP AT ti_204d_acrdecr WITH CONTROL vg_sbs204d_tabcontrol.
    MODULE change_table_control_204d.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT ti_204h_acrdecr.
    FIELD ti_204h_acrdecr-acao
          MODULE read_table_control_204h ON REQUEST.
  ENDLOOP.

  LOOP AT ti_204d_acrdecr.
    FIELD ti_204d_acrdecr-lote_aplicar
          MODULE read_table_control_204d ON REQUEST.
  ENDLOOP.

  MODULE check_okcode_204.

PROCESS ON VALUE-REQUEST.
  FIELD ti_204h_acrdecr-acao MODULE preenche_vlrs_acao_204.
  FIELD ti_204d_acrdecr-lote_aplicar MODULE preenche_lotes_204.
