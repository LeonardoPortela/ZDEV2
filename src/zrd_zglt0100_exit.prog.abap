*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0100_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0100_exit.

FORM f_exit_zglt0100_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0100 TYPE zglt0100.

  CLEAR: wl_zglt0100.


  MOVE-CORRESPONDING wl_zglt0100 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0100_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0100 TYPE zglt0100.
  CLEAR: wl_zglt0100.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0100.

if  wl_zglt0100-tp_imposto is INITIAL.
  MESSAGE 'Campo Tipo de imposto é um campo obrigatório' TYPE 'E'.
  exit.
  ENDIF.
  if wl_zglt0100-tp_taxa is INITIAL.
    MESSAGE 'Campo Taxa é um campo obrigatório' TYPE 'E'.
    ENDIF.


  MOVE-CORRESPONDING wl_zglt0100 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0100_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zglt0100 TYPE zglt0100.
*
*  CLEAR: wl_zglt0100.
*
*  MOVE-CORRESPONDING p_saida TO wl_zglt0100.
*
*  MOVE-CORRESPONDING wl_zglt0100 TO p_saida.


ENDFORM.

FORM f_exit_zglt0100_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0100_out TYPE zglt0100_out.

  CLEAR: wl_zglt0100_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0100_out.
  CLEAR p_saida.



    CASE wl_zglt0100_out-tp_taxa.
      WHEN '01'.
        wl_zglt0100_out-DESC_TAXA = 'Histórica'.
      WHEN '02'.
        wl_zglt0100_out-DESC_TAXA = 'Fechamento'.
      WHEN OTHERS.
        WRITE ' '.
    ENDCASE.
    MOVE-CORRESPONDING wl_zglt0100_out TO p_saida.



ENDFORM.

FORM f_exit_zglt0100_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_zglt0100_out TYPE zglt0100_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_zglt0100_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0100_out.
*
*
*  MOVE-CORRESPONDING wl_zglt0100_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zglt0100_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0100'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
