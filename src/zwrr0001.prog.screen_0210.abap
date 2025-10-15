PROCESS BEFORE OUTPUT.
module dados_zfiwrt_0032.
MODULE tc_zfiwrt0032_change_tc_attr.
MODULE trata_fields.

  LOOP AT   tg_zfiwrt0032
       WITH CONTROL tc_zfiwrt0032
       CURSOR tc_zfiwrt0032-current_line.
    MODULE tc_zfiwrt0032_get_lines.
    MODULE mdc_change_screen.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT tg_ZFIWRT0032.
    CHAIN.
      FIELD tg_zfiwrt0032-ncm.
      FIELD tg_zfiwrt0032-lote_aut.
      FIELD tg_zfiwrt0032-material.
      FIELD tg_zfiwrt0032-deposito.
      FIELD tg_zfiwrt0032-lote.
      MODULE tc_zfiwrt0032_modify ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD tg_zfiwrt0032-MARK
      MODULE tc_zfiwrt0032_MARK ON REQUEST.
  ENDLOOP.
  MODULE tc_ZFIWRT0032_user_command.
