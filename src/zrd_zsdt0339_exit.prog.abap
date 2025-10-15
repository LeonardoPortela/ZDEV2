*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0339_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0339_exit.


FORM f_exit_ZSDT0339_0001 USING p_registro_manter TYPE any.


  DATA: wl_ZSDT0339 TYPE ZSDT0339.

  CLEAR: wl_ZSDT0339.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZSDT0339.

  IF wl_ZSDT0339-us_criacao IS INITIAL.
    wl_ZSDT0339-dt_criacao      = sy-datum.
    wl_ZSDT0339-hr_criacao      = sy-uzeit.
    wl_ZSDT0339-us_criacao      = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_ZSDT0339 TO p_registro_manter.

ENDFORM.

FORM f_exit_ZSDT0339_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZSDT0339 TYPE ZSDT0339.

  CLEAR: wl_ZSDT0339.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZSDT0339.

  IF wl_ZSDT0339-VBELN IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha a numero da OV!'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM vbak INTO @DATA(ls_vbak) WHERE VBELN EQ @wl_ZSDT0339-VBELN.
    IF sy-subrc NE 0.
      MESSAGE i024(sd) WITH 'Ordem de venda não existe !'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_ZSDT0339-KSCHL IS INITIAL.
    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Preencha o tipo de condição!'.
    EXIT.
  endif.

  MOVE-CORRESPONDING wl_ZSDT0339 TO p_registro_manter.

ENDFORM.


FORM f_exit_ZSDT0339_0005 CHANGING p_saida TYPE any.

  DATA: wl_ZSDT0339 TYPE ZSDT0339.

  CLEAR: wl_ZSDT0339.

  MOVE-CORRESPONDING p_saida TO wl_ZSDT0339.

  IF wl_ZSDT0339-us_criacao IS INITIAL.
    wl_ZSDT0339-dt_criacao      = sy-datum.
    wl_ZSDT0339-hr_criacao      = sy-uzeit.
    wl_ZSDT0339-us_criacao      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_ZSDT0339 TO p_saida.


ENDFORM.

FORM f_exit_ZSDT0339_0004 USING p_registro_manter TYPE any.

*
*  DATA: wl_zpmt0072 TYPE zpmt0073.
*
*  CLEAR: wl_zpmt0072.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0072.
*
*  IF wl_zpmt0072-us_criacao IS INITIAL.
*    wl_zpmt0072-dt_criacao      = sy-datum.
*    wl_zpmt0072-hr_criacao      = sy-uzeit.
*    wl_zpmt0072-us_criacao      = sy-uname.
*  ELSE.
*    wl_zpmt0072-dt_modif      = sy-datum.
*    wl_zpmt0072-hr_modif      = sy-uzeit.
*    wl_zpmt0072-us_modif      = sy-uname.
*  ENDIF.
*
*
*
*  MOVE-CORRESPONDING wl_zpmt0072 TO p_registro_manter.

ENDFORM.

FORM  f_exit_ZSDT0339_0016 USING p_ucomm  TYPE sy-ucomm CHANGING p_registro_manter TYPE any p_saida TYPE any.


  DATA: wl_ZSDT0339 TYPE ZSDT0339.
*
  CLEAR: wl_ZSDT0339.
*
  MOVE-CORRESPONDING p_registro_manter TO wl_ZSDT0339.



  MOVE-CORRESPONDING wl_ZSDT0339 TO p_registro_manter.



ENDFORM.
