*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0110_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0110_exit.

FORM f_exit_zglt0110_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0110 TYPE zglt0110.

  CLEAR: wl_zglt0110.


  MOVE-CORRESPONDING wl_zglt0110 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0110_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0110 TYPE zglt0110.
  CLEAR: wl_zglt0110.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0110.

  IF wl_zglt0110-tp_imposto IS NOT INITIAL.
    "SELECT SINGLE desc_impost INTO wl_zglt0110-desc_tp_imposto FROM zglt0100 WHERE tp_imposto = wl_zglt0110-tp_imposto.

  ELSE.
    MESSAGE 'Campo Tipo de imposto é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.
  IF wl_zglt0110-conta IS NOT INITIAL.
    "SELECT SINGLE txt50 INTO wl_zglt0110-desc_ FROM gl_acct_ca_text WHERE saknr = wl_zglt0110-conta AND spras = 'PT' and ktopl eq '0050'.
  ELSE.
    MESSAGE 'Campo Conta é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0110 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0110_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zglt0110 TYPE zglt0110.
*
*  CLEAR: wl_zglt0110.
*
*  MOVE-CORRESPONDING p_saida TO wl_zglt0110.
*
*  MOVE-CORRESPONDING wl_zglt0110 TO p_saida.


ENDFORM.

FORM f_exit_zglt0110_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0110_out TYPE zglt0110_out.
  DATA: wl_zglt0110 TYPE zglt0110.
  CLEAR: wl_zglt0110_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0110_out.
  CLEAR p_saida.

  IF wl_zglt0110_out-tp_imposto IS NOT INITIAL.
    SELECT SINGLE desc_impost INTO wl_zglt0110_out-desc_imposto FROM zglt0100 WHERE tp_imposto = wl_zglt0110_out-tp_imposto.

  ENDIF.

  IF wl_zglt0110_out-conta IS NOT INITIAL.
    SELECT SINGLE desc_conta INTO wl_zglt0110_out-desc_conta FROM zglt0101 WHERE conta = wl_zglt0110_out-conta .
  ENDIF.


  MOVE-CORRESPONDING wl_zglt0110_out TO p_saida.
ENDFORM.

FORM f_exit_zglt0110_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zglt0110_out TYPE zglt0110_out.
  DATA wl_zglt0110  TYPE zglt0110.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zglt0110_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0110_out.

  IF wl_zglt0110_out-tp_imposto IS NOT INITIAL .
    SELECT SINGLE desc_tp_imposto INTO wl_zglt0110_out-desc_imposto FROM zglt0101 WHERE conta = wl_zglt0110_out-tp_imposto .

  ENDIF.

  IF wl_zglt0110_out-conta IS NOT INITIAL .
    SELECT SINGLE desc_conta INTO wl_zglt0110_out-desc_conta FROM zglt0101 WHERE conta = wl_zglt0110_out-conta .

  ENDIF.



  MOVE-CORRESPONDING wl_zglt0110_out TO p_registro_manter.

ENDFORM.
