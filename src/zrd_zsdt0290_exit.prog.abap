*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0290_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0290_exit.

FORM f_exit_zsdt0290_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0290 TYPE zsdt0290.

  CLEAR: wl_zsdt0290.

  wl_zsdt0290-data = sy-datum.
  wl_zsdt0290-hora = sy-uzeit.
  wl_zsdt0290-usname = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0290 TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0290_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0290 TYPE zsdt0290.
  CLEAR: wl_zsdt0290.
  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0290.


  IF wl_zsdt0290-vkorg IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Organização de vendas é um campo obrigatório!' TYPE 'S'.
    EXIT.

  ENDIF.



  AUTHORITY-CHECK OBJECT 'ZSDT0193VK'
 ID 'VKORG' FIELD wl_zsdt0290-vkorg.

  IF sy-subrc IS NOT INITIAL.
    p_error = abap_true.
    MESSAGE 'Sem acesso a organização de vendas selecionada!' TYPE 'S'.

    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0290 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zsdt0290_0003 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0290 TYPE zsdt0290.

  CLEAR: wl_zsdt0290.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0290.

  wl_zsdt0290-data = sy-datum.
  wl_zsdt0290-hora = sy-uzeit.
  wl_zsdt0290-usname = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0290 TO p_saida.


ENDFORM.

FORM f_exit_zsdt0290_0004 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0290_out TYPE zsdt0290_out.

  CLEAR: wl_zsdt0290_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0290_out.
  CLEAR p_saida.
  AUTHORITY-CHECK OBJECT 'ZSDT0193VK'
    ID 'VKORG' FIELD wl_zsdt0290_out-vkorg.

  IF sy-subrc IS INITIAL.
    CASE wl_zsdt0290_out-area.
      WHEN 'MI'.
        wl_zsdt0290_out-desc_area = 'MERCADO INTERNO'.
      WHEN 'IN'.
        wl_zsdt0290_out-desc_area = 'INSUMOS'.
      WHEN OTHERS.
        WRITE ' '.
    ENDCASE.
    MOVE-CORRESPONDING wl_zsdt0290_out TO p_saida.
  ENDIF.



ENDFORM.

FORM f_exit_zsdt0290_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zsdt0290_out TYPE zsdt0290_out.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zsdt0290_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0290_out.




  MOVE-CORRESPONDING wl_zsdt0290_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdt0290_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zsdt0290'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
