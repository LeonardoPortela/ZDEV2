*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0279_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0279_exit.

FORM f_exit_zsdt0279_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0279 TYPE zsdt0279.

  CLEAR: wl_zsdt0279.

  DATA: w_zsdt0279     TYPE zsdt0279_out,
        w_zsdt0279_log TYPE zsdt0279_log.

  wl_zsdt0279-dt_atual = sy-datum.
  wl_zsdt0279-hr_atual = sy-uzeit.
  wl_zsdt0279-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0279 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0279_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0279 TYPE zsdt0279.
  DATA: w_zsdt0279_log TYPE zsdt0279_log.

  CLEAR: wl_zsdt0279.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0279.

  CLEAR: p_error.

  IF wl_zsdt0279-model IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Modelo é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zsdt0279-limite IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Limite é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0279 TO w_zsdt0279_log.

  "Gravar registro tabela de log
  SELECT COUNT(*) FROM zsdt0279_log
    INTO  w_zsdt0279_log-seq
    WHERE model EQ w_zsdt0279_log-model.


  SELECT * FROM zsdt0279_log
    INTO TABLE @DATA(t_acao)
    WHERE model EQ @w_zsdt0279_log-model
    ORDER BY seq DESCENDING.

  IF t_acao IS NOT INITIAL.
    IF t_acao[ 1 ]-acao EQ 'D'.
      w_zsdt0279_log-acao       = 'I'.
    ELSE.
      w_zsdt0279_log-acao       = 'U'.
    ENDIF.
  ELSE.
    w_zsdt0279_log-acao       = 'I'.
  ENDIF.

  w_zsdt0279_log-valor   = wl_zsdt0279-limite.
  w_zsdt0279_log-data    = sy-datum.
  w_zsdt0279_log-usuario = sy-uname.
  w_zsdt0279_log-hora    = sy-uzeit.

  MODIFY zsdt0279_log FROM w_zsdt0279_log.
  COMMIT WORK.

ENDFORM.

FORM f_exit_zsdt0279_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0279 TYPE zsdt0279.

  DATA: w_zsdt0279     TYPE zsdt0279_out,
        w_zsdt0279_log TYPE zsdt0279_log.

  CLEAR: wl_zsdt0279.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0279.

  wl_zsdt0279-dt_atual = sy-datum.
  wl_zsdt0279-hr_atual = sy-uzeit.
  wl_zsdt0279-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0279 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0279_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0279_out TYPE zsdt0279_out.

  CLEAR: wl_zsdt0279_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0279_out.

  MOVE-CORRESPONDING wl_zsdt0279_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0279_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: w_zsdt0279     TYPE zsdt0279_out,
        w_zsdt0279_log TYPE zsdt0279_log.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0279.

  MOVE-CORRESPONDING w_zsdt0279 TO w_zsdt0279_log.

  "Gravar registro tabela de log
  SELECT COUNT(*) FROM zsdt0279_log
    INTO  w_zsdt0279_log-seq
    WHERE model EQ w_zsdt0279_log-model.

  w_zsdt0279_log-acao       = 'D'.
  w_zsdt0279_log-valor      = w_zsdt0279-limite.
  w_zsdt0279_log-data       = sy-datum.
  w_zsdt0279_log-usuario    = sy-uname.
  w_zsdt0279_log-hora       = sy-uzeit.

  MODIFY zsdt0279_log FROM w_zsdt0279_log.
  COMMIT WORK.

  MOVE-CORRESPONDING w_zsdt0279 TO p_registro_manter.

ENDFORM.
