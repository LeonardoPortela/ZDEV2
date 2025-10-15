
PROCESS BEFORE OUTPUT.
* MODULE STATUS_2006.
*
  MODULE tab_conhec_change_tc_attr.

  LOOP AT it_conhec
       WITH CONTROL tab_conhec
       CURSOR tab_conhec-current_line.
    MODULE bloqueia.
  ENDLOOP.

PROCESS AFTER INPUT.
* MODULE USER_COMMAND_2006.

  LOOP AT it_conhec.
    CHAIN.
      FIELD it_conhec-nr_conhec.
      FIELD it_conhec-dt_conhec.
      MODULE set_update_flag_conhec ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD it_conhec-mark
      MODULE tab_conhec_mark ON REQUEST.
  ENDLOOP.
