PARAMETERS:p_bukrs TYPE anlz-bukrs OBLIGATORY,
           p_file  TYPE string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  DATA: lt_dynpread TYPE TABLE OF dynpread,
        ls_dynpread TYPE dynpread.

  ls_dynpread-fieldname = 'P_BUKRS'.
  APPEND ls_dynpread TO lt_dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpread.

  READ TABLE lt_dynpread INTO ls_dynpread WITH KEY fieldname = 'P_BUKRS'.
  IF sy-subrc = 0 AND ls_dynpread-fieldvalue IS NOT INITIAL.
    lv_bukrs = ls_dynpread-fieldvalue.
  ELSE.
    MESSAGE 'Empresa obrigatória antes de buscar o arquivo.' TYPE 'E'.
  ENDIF.

  CREATE OBJECT lo_import.
  lo_import->get_file( ).

  IF it_saida IS NOT INITIAL.
    CALL SCREEN 0100.
  ENDIF.
