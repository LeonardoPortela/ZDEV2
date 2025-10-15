
PROCESS BEFORE OUTPUT.

* MODULE STATUS_300.

  MODULE inicializa_detalhes_300.

  LOOP AT ti_300_lacto WITH CONTROL vg_sbs300_tabcontrol.
    MODULE change_300_lacto.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT ti_300_lacto.
    MODULE read_table_control_300.

    FIELD ti_300_lacto-conhec
          MODULE chama_vt03_tknum_300 AT CURSOR-SELECTION.

  ENDLOOP.

* MODULE check_okcode_300.
