FUNCTION z_ajuda_pesquisa_tipo_operacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: htext TYPE htext.

  TYPES: BEGIN OF ty_t777w,
           hilfm TYPE t777w-hilfm,
           htext TYPE t777w-htext,
         END OF ty_t777w.

  DATA: lt_t777w TYPE STANDARD TABLE OF ty_t777w.

  IF callcontrol-step = 'DISP'.

    REFRESH: record_tab.

    SELECT hilfm htext
      FROM t777w
      INTO TABLE lt_t777w
      WHERE langu = sy-langu
      AND   subty = '0003'.

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_t777w
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol
      EXCEPTIONS
        illegal_structure  = 1
        OTHERS             = 2.
  ENDIF.

  IF callcontrol-step	EQ 'RETURN'.
    READ TABLE record_tab INTO DATA(ls_record_tab) INDEX 1.
    IF sy-subrc IS INITIAL.
      htext = ls_record_tab+3(25).
      FREE MEMORY ID 'TIPO_OPERACAO'.
      EXPORT htext TO MEMORY ID 'TIPO_OPERACAO'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
