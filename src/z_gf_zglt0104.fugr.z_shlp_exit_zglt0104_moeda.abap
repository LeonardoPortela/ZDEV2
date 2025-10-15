FUNCTION z_shlp_exit_zglt0104_moeda.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: BEGIN OF ls_res_tab,
          moeda_funcional TYPE zglt0104-moeda_funcional,
        END OF ls_res_tab.

  DATA: res_tab LIKE TABLE OF ls_res_tab.

  IF callcontrol-step = 'SELECT'.

    SELECT a~domvalue_l AS moeda_funcional
      INTO CORRESPONDING FIELDS OF TABLE res_tab
      FROM dd07l AS a INNER JOIN dd07t AS b
      ON a~domname = b~domname
      AND a~as4local = b~as4local
      AND a~valpos = b~valpos
      AND a~as4vers = b~as4vers
      WHERE a~domname = 'Z_MOEDA_FUNC'
      AND b~ddlanguage = sy-langu.


    "* Prepare for output
    CALL FUNCTION 'F4UT_RESULTS_MAP'
      TABLES
        shlp_tab          = shlp_tab
        record_tab        = record_tab
        source_tab        = res_tab
      CHANGING
        shlp              = shlp
        callcontrol       = callcontrol
      EXCEPTIONS
        illegal_structure = 1
        OTHERS            = 2.

    callcontrol-step = 'DISP'.
    EXIT.
  ENDIF.
ENDFUNCTION.
