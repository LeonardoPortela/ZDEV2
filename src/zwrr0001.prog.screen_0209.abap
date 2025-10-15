PROCESS BEFORE OUTPUT.
  MODULE tc_cfop_change_tc_attr.
  MODULE trata_fields.
  LOOP AT tg_cfop
       WITH CONTROL tc_cfop
       CURSOR tc_cfop-current_line.
    MODULE tc_cfop_get_lines.
    MODULE md_change_screen.
  ENDLOOP.

* MODULE STATUS_0110.
*
PROCESS AFTER INPUT.
  LOOP AT tg_cfop.
    CHAIN.
      FIELD tg_cfop-uf.
      FIELD tg_cfop-cfop.
      MODULE tc_cfop_modify ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD tg_cfop-mark
      MODULE tc_cfop_mark ON REQUEST.
  ENDLOOP.
  MODULE tc_cfop_user_command.
