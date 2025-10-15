*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0339_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0008_exit.


FORM f_exit_zmmt0008_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0008 TYPE zmmt0008.

  CLEAR: wl_zmmt0008.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0008.

  IF wl_zmmt0008-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha o centro!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-safra IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a safra!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-charg IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a numero do fardo!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-lgort IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a numero do bloco!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-menge IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha o peso do fardo!' TYPE 'I'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0008 TO p_registro_manter.

ENDFORM.


FORM f_exit_zmmt0008_0019 USING p_registro_search TYPE any
                       CHANGING p_error
                                p_cond TYPE rsds_where.

  DATA: lwa_cond_line  TYPE rsdswhere.

  DATA: wl_zmmt0008 TYPE zmmt0008.

  CLEAR: wl_zmmt0008, p_cond.

  MOVE-CORRESPONDING p_registro_search TO wl_zmmt0008.

  IF wl_zmmt0008-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha o centro!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-safra IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a safra!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wl_zmmt0008-lgort IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Preencha a numero do bloco!' TYPE 'I'.
    EXIT.
  ENDIF.

  APPEND VALUE #( line = |     ( WERKS EQ '{ wl_zmmt0008-werks  }' ) | ) TO  p_cond-where_tab.
  APPEND VALUE #( line = | AND ( LGORT EQ '{ wl_zmmt0008-lgort }' ) | ) TO  p_cond-where_tab.

  IF wl_zmmt0008-charg IS NOT INITIAL.
    APPEND VALUE #( line = | AND ( CHARG EQ '{ wl_zmmt0008-charg }' ) | ) TO  p_cond-where_tab.
  ENDIF.

  APPEND VALUE #( line = | AND ( SAFRA EQ '{ wl_zmmt0008-safra }' ) | ) TO  p_cond-where_tab.


ENDFORM.
