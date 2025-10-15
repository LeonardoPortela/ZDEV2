*&---------------------------------------------------------------------*
*& Report  ZRD_zppt0036_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0300_exit.

FORM f_exit_zsdt0300_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0300' AND
     p_field       = 'ID_MATNR_AGRIQ'.
    p_scrtext_l = 'Mat.AgriQ'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0300' AND
     p_field       = 'MENSAGEM'.
    p_scrtext_l = 'Mensagem'.
    p_outputlen = 100.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
