FUNCTION z_gera_excel.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(TITULO1) TYPE  STRING OPTIONAL
*"     REFERENCE(TITULO2) TYPE  STRING OPTIONAL
*"     REFERENCE(TABNAME) TYPE  DDOBJNAME
*"  TABLES
*"      TABELA
*"----------------------------------------------------------------------

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\'
      mask             = ',Excel,*.xls.'
      mode             = 'A'
      title            = 'Arquivo Excel'(002)
    IMPORTING
      filename         = p_arq
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc EQ 0.
    PERFORM gera_excel TABLES tabela USING titulo1 titulo2 p_arq tabname.
  ENDIF.

ENDFUNCTION.
