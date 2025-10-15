*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0280_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0280_exit.

FORM f_exit_zsdt0280_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0280 TYPE zsdt0280.

  CLEAR: wl_zsdt0280.

  wl_zsdt0280-dt_registro = sy-datum.
  wl_zsdt0280-hr_registro = sy-uzeit.
  wl_zsdt0280-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0280 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0280_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0280 TYPE zsdt0280.

  CLEAR: wl_zsdt0280,
         p_error.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0280.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zsdt0280-lifnr
    IMPORTING
      output = wl_zsdt0280-lifnr.

  IF wl_zsdt0280-lifnr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Parceiro LR Code é um campo obrigatório!' TYPE 'S'.
    p_error = abap_true.
    EXIT.
  ENDIF.

  SELECT SINGLE lifnr
    FROM kna1
    INTO @DATA(l_lifnr_lfa1)
   WHERE kunnr = @wl_zsdt0280-lifnr.

  IF sy-subrc <> 0.
    MESSAGE 'Parceiro LR Incorreto!' TYPE 'S'.
    p_error = abap_true.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0280_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0280 TYPE zsdt0280.

  CLEAR: wl_zsdt0280.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0280.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zsdt0280-lifnr
    IMPORTING
      output = wl_zsdt0280-lifnr.

  wl_zsdt0280-dt_registro = sy-datum.
  wl_zsdt0280-hr_registro = sy-uzeit.
  wl_zsdt0280-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0280 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0280_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0280_out TYPE zsdt0280_out.

  CLEAR: wl_zsdt0280_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0280_out.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zsdt0280_out-lifnr
    IMPORTING
      output = wl_zsdt0280_out-lifnr.

  MOVE-CORRESPONDING wl_zsdt0280_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0280_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0280_OUT' AND
     p_field       = 'LIFNR'.
    p_scrtext_l    = 'Parceiro LR'.
  ENDIF.

ENDFORM.
