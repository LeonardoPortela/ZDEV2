*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0001RS_03W_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0001rs_03w_exit.

FORM f_exit_zsdt0001rs_03w_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0001rs_03w TYPE zsdt0001rs_03w.

  CLEAR: wl_zsdt0001rs_03w.


  MOVE-CORRESPONDING wl_zsdt0001rs_03w TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0001rs_03w_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0001rs_03w TYPE zsdt0001rs_03w.
  CLEAR: wl_zsdt0001rs_03w.
  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0001rs_03w.


  IF wl_zsdt0001rs_03w-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é um campo obrigatório!' TYPE 'S'.
    EXIT.

  ELSE.
    SELECT SINGLE * FROM t001w INTO @DATA(_wl_t001w) WHERE werks EQ @wl_zsdt0001rs_03w-werks.
    IF sy-subrc NE 0.

      p_error = abap_true.
      MESSAGE 'Centro não encontrado' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  IF wl_zsdt0001rs_03w-tp_sub_carac_avariado IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Subcaracterístca Avariado é um campo obrigatório!' TYPE 'S'.
    EXIT.

  ENDIF.


  MOVE-CORRESPONDING wl_zsdt0001rs_03w TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zsdt0001rs_03w_0003 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0001rs_03w TYPE zsdt0001rs_03w.

  CLEAR: wl_zsdt0001rs_03w.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0001rs_03w.

  MOVE-CORRESPONDING wl_zsdt0001rs_03w TO p_saida.


ENDFORM.

FORM f_exit_zsdt0001rs_03w_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0001rs_03w_out TYPE zsdt0001rs_03w_out.

  CLEAR: wl_zsdt0001rs_03w_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0001rs_03w_out.
  CLEAR p_saida.


  IF sy-subrc IS INITIAL.
    CASE wl_zsdt0001rs_03w_out-tp_sub_carac_avariado.
      WHEN '1'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Ardido/Queimado'.
      WHEN '2'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Queimados'.
      WHEN '3'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Mofados'.
      WHEN '4'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Picados'.
      WHEN '5'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Fermentados'.
      WHEN '6'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Germinados/Imaturos/Chochos'.
      WHEN '7'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Ardidos'.
      WHEN '8'.
        wl_zsdt0001rs_03w_out-desc_tp_sub_carac = 'Gessados'.
      WHEN OTHERS.
        WRITE ' '.
    ENDCASE.
    MOVE-CORRESPONDING wl_zsdt0001rs_03w_out TO p_saida.
  ENDIF.



ENDFORM.

FORM f_exit_zsdt0001rs_03w_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0001rs_03w_out TYPE zsdt0001rs_03w_out.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zsdt0001rs_03w_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0001rs_03w_out.




  MOVE-CORRESPONDING wl_zsdt0001rs_03w_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdt0001rs_03w_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'ZSDT0001RS_03W'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
