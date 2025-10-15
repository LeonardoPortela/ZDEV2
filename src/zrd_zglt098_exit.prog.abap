*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT098_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt098_exit.

FORM f_exit_zglt098_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt098 TYPE zglt098.

  CLEAR: wl_zglt098.

  wl_zglt098-usnam_atual = sy-uname.
  wl_zglt098-zdt_atual = sy-datum.
  wl_zglt098-zhr_atual = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt098 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt098_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt098 TYPE zglt098.

  CLEAR: wl_zglt098.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt098.

  CLEAR: p_error.

  IF wl_zglt098-gjahr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Exercício é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt098-monat IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Mês/Exercício é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt098-usnam IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Nome do usuário é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zglt098-data_lim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Data Final é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
  IF wl_zglt098-hora_lim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Hora Final é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
ENDFORM.

FORM f_exit_zglt098_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt098 TYPE zglt098.

  CLEAR: wl_zglt098.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt098.

  wl_zglt098-usnam_atual = sy-uname.
  wl_zglt098-zdt_atual = sy-datum.
  wl_zglt098-zhr_atual = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt098 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt098_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt098_out TYPE zglt098_out.

  CLEAR: wl_zglt098_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt098_out.

  MOVE-CORRESPONDING wl_zglt098_out TO p_saida.

ENDFORM.
