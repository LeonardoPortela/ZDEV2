*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0307_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0307_exit.

FORM f_exit_zsdt0307_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0307 TYPE zsdt0307.

  CLEAR: wl_zsdt0307.

  wl_zsdt0307-dt_registro = sy-datum.
  wl_zsdt0307-hr_registro = sy-uzeit.
  wl_zsdt0307-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0307 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0307_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0307 TYPE zsdt0307_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0307.

  SELECT SINGLE *
           FROM t001
           INTO @DATA(w_t001_a)
          WHERE bukrs = @w_zsdt0307-emp_pedido.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Empresa do Pedido está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001
           INTO @DATA(w_t001_b)
          WHERE bukrs = @w_zsdt0307-emp_fat_serv.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Empresa Fat.Serviço está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zsdt0307-centro_fat_serv.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Centro Fat.Serviço está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0307_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0307    TYPE zsdt0307.

  CLEAR: wl_zsdt0307.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zsdt0307.

  wl_zsdt0307-dt_registro = sy-datum.
  wl_zsdt0307-hr_registro = sy-uzeit.
  wl_zsdt0307-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0307 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0307_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0307_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0307_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0307_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'EMP_PEDIDO'.
    p_scrtext_l = 'Empresa Pedido'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'EMP_FAT_SERV'.
    p_scrtext_l = 'Empresa Fat.Serviço'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'CENTRO_FAT_SERV'.
    p_scrtext_l = 'Centro Fat.Serviço'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'US_REGISTRO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'DT_REGISTRO'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0307_OUT' AND
     p_field       = 'HR_REGISTRO'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0307_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
