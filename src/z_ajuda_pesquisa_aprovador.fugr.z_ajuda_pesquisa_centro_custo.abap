FUNCTION z_ajuda_pesquisa_centro_custo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_cskt,
           kostl TYPE cskt-kostl,
           ltext TYPE cskt-ltext,
         END OF ty_cskt.

  DATA: lt_cskt TYPE STANDARD TABLE OF ty_cskt.

  DATA: kostl TYPE csks-kostl.

  IF callcontrol-step = 'DISP'.

    REFRESH: record_tab.

    SELECT kostl ltext
      FROM cskt
      INTO TABLE lt_cskt
      WHERE kokrs = 'MAGI'
      AND   datbi >= sy-datum.

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_cskt
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol
      EXCEPTIONS
        illegal_structure  = 1
        OTHERS             = 2.
  ENDIF.

ENDFUNCTION.
