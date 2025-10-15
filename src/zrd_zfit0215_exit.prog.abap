*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0215_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0215_exit.

FORM f_exit_zfit0215_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0215 TYPE zfit0215.

  CLEAR: wl_zfit0215.

  DATA: w_zfit0215     TYPE zfit0215. "_out,
*        w_zfit0215_log TYPE zfit0215_log.

*  wl_zfit0215-dt_atual = sy-datum.
*  wl_zfit0215-hr_atual = sy-uzeit.
*  wl_zfit0215-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zfit0215 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0215_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0215 TYPE zfit0215.
*  DATA: w_zfit0215_log TYPE zfit0215_log.

  CLEAR: wl_zfit0215.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0215.

  CLEAR: p_error.

*  IF wl_zfit0215-model IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Modelo é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.

*  IF wl_zfit0215-limite IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Limite é um campo obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.

*  MOVE-CORRESPONDING wl_zfit0215 TO w_zfit0215. "_log.

*  "Gravar registro tabela de log
*  SELECT COUNT(*) FROM zfit0215_log
*    INTO  w_zfit0215_log-seq
*    WHERE model EQ w_zfit0215_log-model.


*  SELECT * FROM zfit0215_log
*    INTO TABLE @DATA(t_acao)
*    WHERE model EQ @w_zfit0215_log-model
*    ORDER BY seq DESCENDING.

*  IF t_acao IS NOT INITIAL.
*    IF t_acao[ 1 ]-acao EQ 'D'.
*      w_zfit0215_log-acao       = 'I'.
*    ELSE.
*      w_zfit0215_log-acao       = 'U'.
*    ENDIF.
*  ELSE.
*    w_zfit0215_log-acao       = 'I'.
*  ENDIF.

*  w_zfit0215_log-valor   = wl_zfit0215-limite.
*  w_zfit0215_log-data    = sy-datum.
*  w_zfit0215_log-usuario = sy-uname.
*  w_zfit0215_log-hora    = sy-uzeit.
*
*  MODIFY zfit0215_log FROM w_zfit0215_log.
*  COMMIT WORK.

ENDFORM.

FORM f_exit_zfit0215_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0215 TYPE zfit0215.

  DATA: w_zfit0215     TYPE zfit0215. "_out,
*        w_zfit0215_log TYPE zfit0215_log.

  CLEAR: wl_zfit0215.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0215.

*  wl_zfit0215-dt_atual = sy-datum.
*  wl_zfit0215-hr_atual = sy-uzeit.
*  wl_zfit0215-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zfit0215 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0215_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0215_out TYPE zfit0215."_out.

  CLEAR: wl_zfit0215_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0215_out.

  MOVE-CORRESPONDING wl_zfit0215_out TO p_saida.

ENDFORM.

FORM f_exit_zfit0215_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: w_zfit0215     TYPE zfit0215. "_out,
*        w_zfit0215_log TYPE zfit0215_log.

  MOVE-CORRESPONDING p_registro_manter TO w_zfit0215.

*  MOVE-CORRESPONDING w_zfit0215 TO w_zfit0215_log.

*  "Gravar registro tabela de log
*  SELECT COUNT(*) FROM zfit0215_log
*    INTO  w_zfit0215_log-seq
*    WHERE model EQ w_zfit0215_log-model.

*  w_zfit0215_log-acao       = 'D'.
*  w_zfit0215_log-valor      = w_zfit0215-limite.
*  w_zfit0215_log-data       = sy-datum.
*  w_zfit0215_log-usuario    = sy-uname.
*  w_zfit0215_log-hora       = sy-uzeit.
*
*  MODIFY zfit0215_log FROM w_zfit0215_log.
*  COMMIT WORK.

  MOVE-CORRESPONDING w_zfit0215 TO p_registro_manter.

ENDFORM.
