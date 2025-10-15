*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0282_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0282_exit.

FORM f_exit_zsdt0282_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0282 TYPE zsdt0282.

  CLEAR: wl_zsdt0282.
  wl_zsdt0282-usuario = sy-uname.
  wl_zsdt0282-data    = sy-datum.
  wl_zsdt0282-hora    = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0282 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0282_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0282 TYPE zsdt0282.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zsdt0282, gv_domvalue_l.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0282.

  IF wl_zsdt0282-vkorg IS NOT INITIAL.
    SELECT SINGLE * FROM t001 INTO @DATA(wl_t001)
      WHERE bukrs EQ @wl_zsdt0282-vkorg.

    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Cód.Empresa informado não existe!' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    p_error = abap_true.
    MESSAGE 'Favor informar Cód.Empresa!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0282-transp IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Favor informar Cód.Transportadora!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0282-nome IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Favor informar Descrição Transportadora!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0282-tp_visao IS NOT INITIAL.

    SELECT SINGLE * FROM zsdt0282 INTO @DATA(wa_zsdt0282)
      WHERE tp_visao EQ @wl_zsdt0282-tp_visao.

    IF sy-subrc EQ 0 AND sy-ucomm EQ 'NOVO'.
      p_error = abap_true.
      MESSAGE 'Tipo de Visão informado já cadastrado!' TYPE 'E'.
      EXIT.
    ENDIF.

  ELSE.
    p_error = abap_true.
    MESSAGE 'Favor informar Tipo de Visão!' TYPE 'E'.
    EXIT.
  ENDIF.


  IF wl_zsdt0282-tp_tela IS NOT INITIAL.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZTP_TELA'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zsdt0282-tp_tela.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc NE 0.
      p_error =  abap_true.
      MESSAGE 'Tipo de Tela informado não existe!' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    p_error =  abap_true.
    MESSAGE 'Favor informar Tipo de Tela !' TYPE 'E'.
    EXIT.
  ENDIF.

  CLEAR p_error.

ENDFORM.

FORM f_exit_zsdt0282_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0282_out TYPE zsdt0282_out.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.


  CLEAR: wl_zsdt0282_out, gv_domvalue_l.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0282_out.

  IF wl_zsdt0282_out-vkorg IS NOT INITIAL.

    SELECT SINGLE * FROM t001 INTO @DATA(wl_t001)
      WHERE bukrs EQ @wl_zsdt0282_out-vkorg.

    IF sy-subrc EQ 0.
      wl_zsdt0282_out-name1 = wl_t001-butxt.
    ENDIF.
  ENDIF.

  IF wl_zsdt0282_out-tp_tela IS NOT INITIAL.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZTP_TELA'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zsdt0282_out-tp_tela.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      wl_zsdt0282_out-desc_tela = s_dd07v-ddtext.
    ENDIF.
  ENDIF.


  MOVE-CORRESPONDING wl_zsdt0282_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0282_0005 CHANGING p_registro_manter TYPE any.

  IF sy-ucomm EQ 'CHANGE'.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-TP_VISAO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-VTEXT'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM f_exit_zsdt0282_0006 USING p_saida TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0282_out TYPE zsdt0282_out,
        wl_zsdt0137     TYPE zsdt0137.

  CLEAR: wl_zsdt0282_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0282_out.

  SELECT SINGLE * FROM zsdt0137 INTO wl_zsdt0137
    WHERE filial_resp EQ wl_zsdt0282_out-transp.

  IF sy-subrc EQ 0.
    p_error = abap_true.
    MESSAGE | Ação não permitida! Cód transporte está sendo utilizado. | TYPE 'S'  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR p_error.

ENDFORM.
