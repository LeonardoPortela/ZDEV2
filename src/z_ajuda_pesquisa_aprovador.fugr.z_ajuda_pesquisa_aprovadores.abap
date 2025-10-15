FUNCTION z_ajuda_pesquisa_aprovadores.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  DATA: pernr TYPE pa0001-pernr.

  DATA: samaccountname TYPE zde_usuario.

  TYPES: BEGIN OF ty_zhcmt0007,
           pernr          TYPE zhcmt0007-pernr,
           cname          TYPE zhcmt0007-cname,
           samaccountname TYPE zhcmt0007-samaccountname,
         END OF ty_zhcmt0007.

  DATA: lt_zhcmt0007 TYPE TABLE OF ty_zhcmt0007.

  IF callcontrol-step = 'DISP'.

    REFRESH: record_tab.

    SELECT pernr cname samaccountname
      FROM zhcmt0007
      INTO TABLE lt_zhcmt0007
      WHERE situacao <> 'SAIU DA EMPRESA'.

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_zhcmt0007
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
      pernr = ls_record_tab-string(8).
      FREE MEMORY ID 'NUMERO_PESSOAL_APROVADOR'.
      EXPORT pernr TO MEMORY ID 'NUMERO_PESSOAL_APROVADOR'.
      samaccountname = ls_record_tab-string+88(40).
      FREE MEMORY ID 'NOME_APROVADOR'.
      EXPORT samaccountname TO MEMORY ID 'NOME_APROVADOR'.
    ENDIF.
  ENDIF.

ENDFUNCTION.
