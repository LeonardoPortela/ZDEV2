
PROCESS BEFORE OUTPUT.

* MODULE status_203.

  MODULE visibilidade_subtotais_203.

  LOOP AT ti_203_lotes WITH CONTROL vg_sbs203_tabcontrol.
    MODULE change_203_lotes.
  ENDLOOP.

*
PROCESS AFTER INPUT.

  LOOP AT ti_203_lotes.

    MODULE read_table_control_203.

    FIELD ti_203_lotes-docsapadto
          MODULE chama_fb03_docsap_203 AT CURSOR-SELECTION.

  ENDLOOP.

  MODULE check_okcode_203.

PROCESS ON VALUE-REQUEST.
  FIELD ti_203_lotes-bl MODULE match_code_bl_203.
