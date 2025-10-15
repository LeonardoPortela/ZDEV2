*&---------------------------------------------------------------------*
*& Report  ZRD_ZPPT0027_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zppt0027_log_exit.

FORM f_exit_zppt0027_log_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zppt0027_log_out  TYPE zppt0027_log_out.

  MOVE p_registro_manter  TO w_zppt0027_log_out.

  IF     w_zppt0027_log_out-acao = 'E'.
    w_zppt0027_log_out-acao = 'Excluído'.
  ELSEIF w_zppt0027_log_out-acao = 'I'.
    w_zppt0027_log_out-acao = 'Incluído'.
  ENDIF.

  MOVE w_zppt0027_log_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0027_log_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'USUARIO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'ACAO'.
    p_scrtext_l = 'Acão'.
    p_outputlen = 15.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'WERKS_FROM'.
    p_scrtext_l = 'Centro De'.
    p_outputlen = 10.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'WERKS_TO'.
    p_scrtext_l = 'Centro Até'.
    p_outputlen = 10.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'USNAM_REG'.
    p_scrtext_l = 'Usuário Registro'.
    p_outputlen = 15.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'DATUM_REG'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_LOG_OUT' AND
     p_field       = 'UZEIT_REG'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0027_log_0010  TABLES it_tabela STRUCTURE zppt0027_log_out.

  SORT it_tabela BY datum_reg DESCENDING
                    uzeit_reg DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
