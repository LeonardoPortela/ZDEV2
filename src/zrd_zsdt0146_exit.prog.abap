*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0146_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0146_exit.

FORM f_exit_zsdt0146_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0146 TYPE zsdt0146.
  CLEAR: wl_zsdt0146.

  wl_zsdt0146-dt_atual = sy-datum.
  wl_zsdt0146-hr_atual = sy-uzeit.
  wl_zsdt0146-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0146 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0146_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0146 TYPE zsdt0146.
  DATA: w_zsdt0146_log TYPE zsdt0146_log.

  CLEAR: wl_zsdt0146.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0146.

  CLEAR: p_error.

  IF wl_zsdt0146-docnum IS INITIAL AND wl_zsdt0146-obj_key IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Informe pelo ao menos um documento!' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0146 TO w_zsdt0146_log.

  IF wl_zsdt0146-docnum IS NOT INITIAL.

*  "Gravar registro tabela de log
    SELECT COUNT(*) FROM zsdt0146_log
      INTO  w_zsdt0146_log-seq
      WHERE valor EQ wl_zsdt0146-docnum.

    SELECT * FROM zsdt0146_log
      INTO TABLE @DATA(t_acao)
      WHERE valor EQ @wl_zsdt0146-docnum
      ORDER BY seq DESCENDING.

    IF t_acao IS NOT INITIAL.
      IF t_acao[ 1 ]-acao EQ 'D'.
        w_zsdt0146_log-acao       = 'I'.
      ELSE.
        w_zsdt0146_log-acao       = 'U'.
      ENDIF.
    ELSE.
      w_zsdt0146_log-acao       = 'I'.
    ENDIF.

    w_zsdt0146_log-valor   = wl_zsdt0146-docnum.

    w_zsdt0146_log-data    = sy-datum.
    w_zsdt0146_log-usuario = sy-uname.
    w_zsdt0146_log-hora    = sy-uzeit.

    MODIFY zsdt0146_log FROM w_zsdt0146_log.
    COMMIT WORK.

  ENDIF.

  IF wl_zsdt0146-obj_key IS NOT INITIAL.

*  "Gravar registro tabela de log
    SELECT COUNT(*) FROM zsdt0146_log
      INTO  w_zsdt0146_log-seq
      WHERE valor EQ wl_zsdt0146-obj_key.

    SELECT * FROM zsdt0146_log
      INTO TABLE t_acao
      WHERE valor EQ wl_zsdt0146-obj_key
      ORDER BY seq DESCENDING.

    IF t_acao IS NOT INITIAL.
      IF t_acao[ 1 ]-acao EQ 'D'.
        w_zsdt0146_log-acao       = 'I'.
      ELSE.
        w_zsdt0146_log-acao       = 'U'.
      ENDIF.
    ELSE.
      w_zsdt0146_log-acao       = 'I'.
    ENDIF.

    w_zsdt0146_log-valor   = wl_zsdt0146-obj_key.

    w_zsdt0146_log-data    = sy-datum.
    w_zsdt0146_log-usuario = sy-uname.
    w_zsdt0146_log-hora    = sy-uzeit.

    MODIFY zsdt0146_log FROM w_zsdt0146_log.
    COMMIT WORK.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0146_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0146 TYPE zsdt0146.

  CLEAR: wl_zsdt0146.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0146.

  wl_zsdt0146-dt_atual = sy-datum.
  wl_zsdt0146-hr_atual = sy-uzeit.
  wl_zsdt0146-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0146 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0146_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0146 TYPE zsdt0146.

  CLEAR: wl_zsdt0146.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0146.

  MOVE-CORRESPONDING wl_zsdt0146 TO p_saida.

ENDFORM.

FORM f_exit_zsdt0146_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: w_zsdt0146     TYPE zsdt0146.
  DATA: w_zsdt0146_log TYPE zsdt0146_log.

  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0146.

  IF w_zsdt0146-docnum IS NOT INITIAL.

*  "Gravar registro tabela de log
    SELECT COUNT(*) FROM zsdt0146_log
      INTO  w_zsdt0146_log-seq
      WHERE valor EQ w_zsdt0146-docnum.

    w_zsdt0146_log-acao       = 'D'.

    w_zsdt0146_log-valor   = w_zsdt0146-docnum.

    w_zsdt0146_log-data    = sy-datum.
    w_zsdt0146_log-usuario = sy-uname.
    w_zsdt0146_log-hora    = sy-uzeit.

    MODIFY zsdt0146_log FROM w_zsdt0146_log.
    COMMIT WORK.

  ENDIF.

  IF w_zsdt0146-obj_key IS NOT INITIAL.

*  "Gravar registro tabela de log
    SELECT COUNT(*) FROM zsdt0146_log
      INTO  w_zsdt0146_log-seq
      WHERE valor EQ w_zsdt0146-obj_key.

    w_zsdt0146_log-acao       = 'D'.

    w_zsdt0146_log-valor   = w_zsdt0146-obj_key.

    w_zsdt0146_log-data    = sy-datum.
    w_zsdt0146_log-usuario = sy-uname.
    w_zsdt0146_log-hora    = sy-uzeit.

    MODIFY zsdt0146_log FROM w_zsdt0146_log.
    COMMIT WORK.

  ENDIF.

  MOVE-CORRESPONDING w_zsdt0146 TO p_registro_manter.

ENDFORM.
