*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0102_exit.

FORM f_exit_ZGLT0102_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_ZGLT0102 TYPE ZGLT0102.

  CLEAR: wl_ZGLT0102.


  MOVE-CORRESPONDING wl_ZGLT0102 TO p_registro_manter.

ENDFORM.


FORM f_exit_ZGLT0102_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZGLT0102 TYPE ZGLT0102.
  CLEAR: wl_ZGLT0102.
  MOVE-CORRESPONDING p_registro_manter TO wl_ZGLT0102.

if wl_ZGLT0102-tp_imposto is not INITIAL.
select single DESC_IMPOST into wl_ZGLT0102-desc_tp_imposto from ZGLT0100 where tp_imposto = wl_ZGLT0102-tp_imposto.

ENDIF.
  MOVE-CORRESPONDING wl_ZGLT0102 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_ZGLT0102_0003 CHANGING p_saida TYPE any.

*  DATA: wl_ZGLT0102 TYPE ZGLT0102.
*
*  CLEAR: wl_ZGLT0102.
*
*  MOVE-CORRESPONDING p_saida TO wl_ZGLT0102.
*
*  MOVE-CORRESPONDING wl_ZGLT0102 TO p_saida.


ENDFORM.

FORM f_exit_ZGLT0102_0004 CHANGING p_saida TYPE any.

  DATA: wl_ZGLT0102_out TYPE ZGLT0102_out.

  CLEAR: wl_ZGLT0102_out.

  MOVE-CORRESPONDING p_saida TO wl_ZGLT0102_out.
  CLEAR p_saida.
if wl_ZGLT0102_out-tp_imposto is NOT INITIAL.
  select single DESC_IMPOST into wl_ZGLT0102_out-desc_tp_imposto from ZGLT0100 where tp_imposto = wl_ZGLT0102_out-tp_imposto.
ENDIF.

 CASE wl_ZGLT0102_out-saldo.
      WHEN '1'.
        wl_ZGLT0102_out-dsaldo = 'Ativo'.
      WHEN '2'.
        wl_zglt0102_out-dsaldo = 'Passivo'.
      WHEN OTHERS.
        WRITE ' '.
    ENDCASE.

    MOVE-CORRESPONDING wl_ZGLT0102_out TO p_saida.
ENDFORM.

FORM f_exit_ZGLT0102_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_ZGLT0102_out TYPE ZGLT0102_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_ZGLT0102_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_ZGLT0102_out.
*
*
*  MOVE-CORRESPONDING wl_ZGLT0102_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_ZGLT0102_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'ZGLT0102'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
