*----------------------------------------------------------------------*
***INCLUDE LZSDT0345F01.
*----------------------------------------------------------------------*

FORM f_gera_id.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZIDOPER'
    IMPORTING
      number                  = zsdt0345-id
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  zsdt0345-usuario = sy-uname.
  zsdt0345-data    = sy-datum.
  zsdt0345-hora    = sy-uzeit.

ENDFORM.

FORM f_habilitar_colunas.
  IF zsdt0345-cancelado IS NOT INITIAL.

    zsdt0345-data_cancel = sy-datum.
    zsdt0345-hora_cancel = sy-uzeit.
    zsdt0345-user_cancel = sy-uname.

  ELSE.

    CLEAR: zsdt0345-data_cancel,
           zsdt0345-hora_cancel,
           zsdt0345-user_cancel.

  ENDIF.
ENDFORM.

FORM f_marcar_cancelado. "FF Bug #136164 - Evento HORA SM30

  "Se clicar em eliminar a linha (Botão Standard), apenas marco o campo CANCELADO e no form f_nao_exibe_cancelados, não deixa ela ser mostrada.

******************************************************************
  DATA: BEGIN OF itab OCCURS 0.
          INCLUDE STRUCTURE zsdt0345.
          INCLUDE STRUCTURE vimflagtab.
  DATA: END OF itab.

  CLEAR total_m[].

  itab[] = extract[].

  LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
    IF <fs_itab>-mark = 'M'.
      vim_marked = '*'.
      <fs_itab>-cancelado = abap_true.
      <fs_itab>-data_cancel = sy-datum.
      <fs_itab>-hora_cancel = sy-uzeit.
      <fs_itab>-user_cancel = sy-uname.

      MODIFY zsdt0345 FROM <fs_itab>.

    ENDIF.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

FORM f_nao_exibe_cancelados. "FF Bug #136164 - Evento HORA SM30

  PERFORM table_get_data. "Perform standard que recupera os dados e preenche a tabela TOTAL

  DATA: BEGIN OF itab OCCURS 0.
          INCLUDE STRUCTURE zsdt0345.
          INCLUDE STRUCTURE vimflagtab.
  DATA: END OF itab.

  itab[] = total[].

  LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
    IF <fs_itab>-cancelado = 'X'.
      DELETE itab INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  total[] = itab[].


ENDFORM.
