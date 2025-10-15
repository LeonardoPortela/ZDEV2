*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0101_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0101_exit.

FORM f_exit_zglt0101_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0101 TYPE zglt0101.

  CLEAR: wl_zglt0101.


  MOVE-CORRESPONDING wl_zglt0101 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0101_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0101 TYPE zglt0101.
  CLEAR: wl_zglt0101.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0101.

  IF wl_zglt0101-tp_imposto IS NOT INITIAL.
    SELECT SINGLE desc_impost INTO wl_zglt0101-desc_tp_imposto FROM zglt0100 WHERE tp_imposto = wl_zglt0101-tp_imposto.

  ELSE.
    MESSAGE 'Campo Tipo de imposto é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.
  IF wl_zglt0101-conta IS NOT INITIAL.
    SELECT SINGLE txt50 INTO wl_zglt0101-desc_conta FROM gl_acct_ca_text WHERE saknr = wl_zglt0101-conta AND spras = 'PT' and ktopl eq '0050'.
  ELSE.
    MESSAGE 'Campo Conta é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0101 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0101_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zglt0101 TYPE zglt0101.
*
*  CLEAR: wl_zglt0101.
*
*  MOVE-CORRESPONDING p_saida TO wl_zglt0101.
*
*  MOVE-CORRESPONDING wl_zglt0101 TO p_saida.


ENDFORM.

FORM f_exit_zglt0101_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0101_out TYPE zglt0101_out.
  DATA: wl_zglt0101 TYPE zglt0101.
  CLEAR: wl_zglt0101_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0101_out.
  CLEAR p_saida.

  IF wl_zglt0101_out-tp_imposto IS NOT INITIAL.
    SELECT SINGLE desc_impost INTO wl_zglt0101_out-desc_tp_imposto FROM zglt0100 WHERE tp_imposto = wl_zglt0101_out-tp_imposto.

  ENDIF.

  IF wl_zglt0101_out-conta IS NOT INITIAL.
    SELECT SINGLE txt50 INTO wl_zglt0101_out-desc_conta FROM gl_acct_ca_text WHERE sakan = wl_zglt0101_out-conta AND spras = 'PT' and ktopl eq '0050'.
  ENDIF.


  MOVE-CORRESPONDING wl_zglt0101_out TO p_saida.
ENDFORM.

FORM f_exit_zglt0101_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zglt0101_out TYPE zglt0101_out.
  DATA wl_zglt0101  TYPE zglt0101.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zglt0101_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0101_out.

IF wl_zglt0101_out-conta IS NOT INITIAL and wl_zglt0101_out-conta(1) <> 'M'.
      SELECT SINGLE txt50 INTO wl_zglt0101_out-desc_conta FROM gl_acct_ca_text WHERE sakan = wl_zglt0101_out-conta AND spras = 'PT' and ktopl eq '0050'.
        if sy-subrc is not INITIAL.
          wl_zglt0101_out-desc_conta = ' '.
          endif.
  ENDIF.



  MOVE-CORRESPONDING wl_zglt0101_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zglt0101_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0101'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.


FORM  f_exit_zglt0101_0008 CHANGING p_col_pos
                                     p_ref_tabname
                                     p_ref_fieldname
                                     p_tabname
                                     p_field
                                     p_scrtext_l
                                     p_outputlen
                                     p_edit
                                     p_sum
                                     p_emphasize
                                     p_just
                                     p_hotspot
                                     p_f4
                                     p_check.

*  IF p_ref_tabname = 'ZGLT0101_OUT'.
*    CASE p_field.
*      WHEN 'DESC_CONTA'.
*        p_edit = 'X'.
*
*
*    ENDCASE.
*  ENDIF.

ENDFORM.
