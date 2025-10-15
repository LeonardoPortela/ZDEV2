FUNCTION z_ajuda_pesquisa_meio_pagament.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_t042z,
           zlsch TYPE t042z-zlsch,
           text1 TYPE t042z-text1,
         END OF ty_t042z.

  DATA: lt_t042z TYPE STANDARD TABLE OF ty_t042z.

  IF callcontrol-step = 'DISP'.

    REFRESH: record_tab.

    SELECT zlsch text1
      FROM t042z
      INTO TABLE lt_t042z
      WHERE land1  = 'BR'
      AND ( zlsch EQ 'S' OR zlsch EQ 'U' ).

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_t042z
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol
      EXCEPTIONS
        illegal_structure  = 1
        OTHERS             = 2.
  ENDIF.

ENDFUNCTION.
