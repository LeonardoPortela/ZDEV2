*&---------------------------------------------------------------------*
*& Report ZRD_ZFIT0220_EXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0220_exit.

FORM f_exit_zfit0220_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0220 TYPE zfit0220.

  CLEAR: wl_zfit0220.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0220.

  SELECT MAX( cod_operacao )
  FROM zfit0220
  INTO @DATA(lv_linhas).

  ADD 1 TO lv_linhas.

  wl_zfit0220-cod_operacao = lv_linhas.

  IF wl_zfit0220-usuario IS INITIAL.
    wl_zfit0220-data      = sy-datum.
    wl_zfit0220-hora      = sy-uzeit.
    wl_zfit0220-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0220 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0220_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.


FORM f_exit_zfit0220_0003 CHANGING p_saida TYPE any.

  DATA: wl_zfit0220_out TYPE zfit0220.

  CLEAR: wl_zfit0220_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0220_out.

  MOVE-CORRESPONDING wl_zfit0220_out TO p_saida.


ENDFORM.


FORM f_exit_zfit0220_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0220_out TYPE zfit0220.

  CLEAR: wl_zfit0220_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0220_out.

  MOVE-CORRESPONDING wl_zfit0220_out TO p_saida.

ENDFORM.


FORM f_exit_zfit0220_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0220_out TYPE zfit0220.

  CLEAR: wl_zfit0220_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0220_out.

  MOVE-CORRESPONDING wl_zfit0220_out TO p_registro_manter.

ENDFORM.


FORM  f_exit_zfit0220_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.


FORM  f_exit_zfit0220_0012 CHANGING p_registro_manter TYPE any
                                    p_saida TYPE any.

  DATA: wl_zfit0220 TYPE zfit0220.
  DATA: wl_zfit0220_out TYPE zfit0220_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0220.

  MOVE-CORRESPONDING wl_zfit0220 TO wl_zfit0220_out.

  MOVE-CORRESPONDING wl_zfit0220_out TO p_saida.

ENDFORM.
