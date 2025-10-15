*&---------------------------------------------------------------------*
*& Report ZRD_ZFIT0234_EXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0234_exit.

FORM f_exit_zfit0234_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0234 TYPE zfit0234.

  CLEAR: wl_zfit0234.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0234.

  SELECT MAX( cod_subop )
  FROM zfit0234
  INTO @DATA(lv_linhas).

  ADD 1 TO lv_linhas.

  wl_zfit0234-cod_subop = lv_linhas.

  IF wl_zfit0234-usuario IS INITIAL.
    wl_zfit0234-data      = sy-datum.
    wl_zfit0234-hora      = sy-uzeit.
    wl_zfit0234-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0234 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0234_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.


FORM f_exit_zfit0234_0003 CHANGING p_saida TYPE any.

  DATA: wl_zfit0234_out TYPE zfit0234.

  CLEAR: wl_zfit0234_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0234_out.

  MOVE-CORRESPONDING wl_zfit0234_out TO p_saida.


ENDFORM.


FORM f_exit_zfit0234_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0234_out TYPE zfit0234.

  CLEAR: wl_zfit0234_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0234_out.

  MOVE-CORRESPONDING wl_zfit0234_out TO p_saida.

ENDFORM.


FORM f_exit_zfit0234_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0234_out TYPE zfit0234.

  CLEAR: wl_zfit0234_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0234_out.

  MOVE-CORRESPONDING wl_zfit0234_out TO p_registro_manter.

ENDFORM.


FORM  f_exit_zfit0234_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.


FORM  f_exit_zfit0234_0012 CHANGING p_registro_manter TYPE any
                                    p_saida TYPE any.

  DATA: wl_zfit0234 TYPE zfit0234.
  DATA: wl_zfit0234_out TYPE zfit0234_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0234.

  MOVE-CORRESPONDING wl_zfit0234 TO wl_zfit0234_out.

  MOVE-CORRESPONDING wl_zfit0234_out TO p_saida.

ENDFORM.
