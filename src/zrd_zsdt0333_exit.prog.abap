*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0333_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0333_exit.

FORM f_exit_zsdt0333_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0333 TYPE zsdt0333.

  CLEAR: wl_zsdt0333.
  wl_zsdt0333-uname = sy-uname.
  wl_zsdt0333-datum    = sy-datum.
  wl_zsdt0333-uzeit    = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0333 TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0333_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

*  DATA: wl_zsdt0333 TYPE zsdt0333.
*
*  CLEAR: wl_zsdt0333.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0333.
*
*  IF wl_zsdt0333-werks IS NOT INITIAL.
*    SELECT SINGLE * FROM t001w INTO @DATA(wl_t001w)
*      WHERE werks EQ @wl_zsdt0333-werks.
*
*    IF sy-subrc NE 0.
*      p_error = abap_true.
*      MESSAGE 'Centro Fornecedor informado não existe!' TYPE 'E'.
*      EXIT.
*    ENDIF.
*  ELSE.
*    p_error = abap_true.
*    MESSAGE 'Favor informar Centro Fornecedor' TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF wl_zsdt0333-spart IS NOT INITIAL.
*    SELECT SINGLE * FROM tspat INTO @DATA(wl_tspat)
*      WHERE spart EQ @wl_zsdt0333-spart.
*
*    IF sy-subrc NE 0.
*      p_error = abap_true.
*      MESSAGE 'Setor de Atividade informado não existe' TYPE 'E'.
*      EXIT.
*    ENDIF.
*  ELSE.
*    p_error = abap_true.
*    MESSAGE 'Favor informar Setor de Atividade' TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*
*  IF wl_zsdt0333-mtart IS NOT INITIAL.
*    SELECT SINGLE * FROM t134t INTO @DATA(wl_t134t)
*      WHERE mtart EQ @wl_zsdt0333-mtart.
*
*    IF sy-subrc NE 0.
*      p_error =  abap_true.
*      MESSAGE 'Tipo de Material informado não existe' TYPE 'E'.
*      EXIT.
*    ENDIF.
*  ELSE.
*    p_error =  abap_true.
*    MESSAGE 'Favor informar Tipo de Material' TYPE 'E'.
*    EXIT.
*  ENDIF.
*  CLEAR: p_error.

ENDFORM.


FORM f_exit_zsdt0333_0003 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0333_out TYPE zsdt0333.

  CLEAR: wl_zsdt0333_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0333_out.

  MOVE-CORRESPONDING wl_zsdt0333_out TO p_saida.


ENDFORM.


FORM f_exit_zsdt0333_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0333_out TYPE zsdt0333.

  CLEAR: wl_zsdt0333_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0333_out.

*  IF sy-ucomm = 'NOVO'.
*
*    IF wl_zsdt0333_out-bukrs IS INITIAL.
*      MESSAGE 'Empresa é obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0333_out TO p_saida.

ENDFORM.


FORM f_exit_zsdt0333_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0333_out TYPE zsdt0333.

  CLEAR: wl_zsdt0333_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0333_out.

*  IF wl_zsdt0333_out-werks IS NOT INITIAL.
*
*    SELECT SINGLE * FROM t001w INTO @DATA(wl_t001w)
*      WHERE werks EQ @wl_zsdt0333_out-werks.
*
*    IF sy-subrc EQ 0.
*      wl_zsdt0333_out-desc_centro = wl_t001w-name1.
*    ENDIF.
*  ENDIF.
*
*  IF wl_zsdt0333_out-spart IS NOT INITIAL.
*    SELECT SINGLE * FROM tspat INTO @DATA(wa_tspat)
*      WHERE spart EQ @wl_zsdt0333_out-spart
*        AND spras EQ @sy-langu.
*
*    IF sy-subrc EQ 0.
*      wl_zsdt0333_out-desc_sa = wa_tspat-vtext.
*    ENDIF.
*  ENDIF.
*
*  IF wl_zsdt0333_out-mtart IS NOT INITIAL.
*
*    SELECT SINGLE * FROM t134t INTO @DATA(wa_t134t)
*      WHERE mtart EQ @wl_zsdt0333_out-mtart
*       AND  spras EQ @sy-langu.
*
*    IF sy-subrc EQ 0.
*      wl_zsdt0333_out-desc_tip_mat = wa_t134t-mtbez.
*    ENDIF.
*  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0333_out TO p_registro_manter.

ENDFORM.


FORM  f_exit_zsdt0333_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZSDT0333'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
