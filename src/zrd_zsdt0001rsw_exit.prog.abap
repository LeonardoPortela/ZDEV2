*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0001rsw_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0001rsw_exit.

FORM f_exit_zsdt0001rsw_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0001rsw TYPE zsdt0001rsw.

  CLEAR: wl_zsdt0001rsw.


  MOVE-CORRESPONDING wl_zsdt0001rsw TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0001rsw_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0001rsw TYPE zsdt0001rsw.
  CLEAR: wl_zsdt0001rsw.
  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0001rsw.

  IF wl_zsdt0001rsw-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é um campo obrigatório!' TYPE 'S'.
    EXIT.

  ELSE.
    SELECT SINGLE * FROM t001w INTO @DATA(_wl_t001w) WHERE werks EQ @wl_zsdt0001rsw-werks.
    IF sy-subrc NE 0.

      p_error = abap_true.
      MESSAGE 'Centro não encontrado' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  IF wl_zsdt0001rsw-tp_caracteristica IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tipo de Característica para Classificação é um campo obrigatório!' TYPE 'S'.
    EXIT.

  ENDIF.


  MOVE-CORRESPONDING wl_zsdt0001rsw TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zsdt0001rsw_0003 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0001rsw TYPE zsdt0001rsw.

  CLEAR: wl_zsdt0001rsw.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0001rsw.

  MOVE-CORRESPONDING wl_zsdt0001rsw TO p_saida.


ENDFORM.

FORM f_exit_zsdt0001rsw_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0001rsw_out TYPE zsdt0001rsw_out.

  CLEAR: wl_zsdt0001rsw_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0001rsw_out.
  CLEAR p_saida.


  IF sy-subrc IS INITIAL.
    CASE wl_zsdt0001rsw_out-tp_caracteristica.
      WHEN '01'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Umidade'.
      WHEN '02'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Impureza'.
      WHEN '03'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Avariado'.
      WHEN '04'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Ardido'.
      WHEN '05'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Quebrado'.
      WHEN '06'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Esverdeado'.
      WHEN '07'.
        wl_zsdt0001rsw_out-desc_tp_carac = 'Carunchado'.
      WHEN OTHERS.
        WRITE ' '.
    ENDCASE.
    MOVE-CORRESPONDING wl_zsdt0001rsw_out TO p_saida.
  ENDIF.



ENDFORM.

FORM f_exit_zsdt0001rsw_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0001rsw_out TYPE zsdt0001rsw_out.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zsdt0001rsw_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0001rsw_out.




  MOVE-CORRESPONDING wl_zsdt0001rsw_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdt0001rsw_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zsdt0001rsw'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
