FUNCTION Z_VALORES_DOMINIO.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(NAME) TYPE  DDOBJNAME
*"     REFERENCE(STATE) TYPE  DDOBJSTATE DEFAULT 'A'
*"     VALUE(LANGU) TYPE  SY-LANGU DEFAULT ' '
*"  TABLES
*"      DD07V_TAB STRUCTURE  DD07V
*"--------------------------------------------------------------------

  IF langu IS INITIAL.
    langu = sy-langu.
  ENDIF.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = name
      state         = state
      langu         = langu
    TABLES
      dd07v_tab     = dd07v_tab
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFUNCTION.
