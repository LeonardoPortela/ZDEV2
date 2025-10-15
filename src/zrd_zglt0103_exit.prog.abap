*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0103_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0103_exit.

FORM f_exit_zglt0103_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0103 TYPE zglt0103.

  CLEAR: wl_zglt0103.


  MOVE-CORRESPONDING wl_zglt0103 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0103_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0103 TYPE zglt0103.
  CLEAR: wl_zglt0103.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0103.


  MOVE-CORRESPONDING wl_zglt0103 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0103_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zglt0103 TYPE zglt0103.
*
*  CLEAR: wl_zglt0103.
*
*  MOVE-CORRESPONDING p_saida TO wl_zglt0103.
*
*  MOVE-CORRESPONDING wl_zglt0103 TO p_saida.


ENDFORM.

FORM f_exit_zglt0103_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0103_out TYPE zglt0103_out.

  CLEAR: wl_zglt0103_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0103_out.
  CLEAR p_saida.


    MOVE-CORRESPONDING wl_zglt0103_out TO p_saida.
ENDFORM.

FORM f_exit_zglt0103_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_zglt0103_out TYPE zglt0103_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_zglt0103_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0103_out.
*
*
*  MOVE-CORRESPONDING wl_zglt0103_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zglt0103_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0103'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
