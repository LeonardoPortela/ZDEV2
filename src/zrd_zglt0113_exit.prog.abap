*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0113_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0113_exit.

FORM f_exit_zglt0113_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0113 TYPE zglt0113.

  CLEAR: wl_zglt0113.


  MOVE-CORRESPONDING wl_zglt0113 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0113_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0113 TYPE zglt0113.
  CLEAR: wl_zglt0113.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0113.

  IF wl_zglt0113-tp_imposto IS NOT INITIAL.

  ELSE.
    MESSAGE 'Campo Tipo de imposto é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.
  IF wl_zglt0113-saknr IS NOT INITIAL.

  ELSE.
    MESSAGE 'Campo Conta é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0113 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0113_0003 CHANGING p_saida TYPE any.

ENDFORM.

FORM f_exit_zglt0113_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0113_out TYPE zglt0113_out.
  DATA: wl_zglt0113 TYPE zglt0113.
  DATA: vl_saknr    TYPE saknr.
  CLEAR: wl_zglt0113_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0113_out.
  CLEAR p_saida.

  IF wl_zglt0113_out-tp_imposto IS NOT INITIAL.
    SELECT SINGLE desc_impost INTO wl_zglt0113_out-desc_impost FROM zglt0100 WHERE tp_imposto = wl_zglt0113_out-tp_imposto.
  ENDIF.

  CLEAR vl_saknr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zglt0113_out-saknr
    IMPORTING
      output = vl_saknr.

  IF wl_zglt0113_out-saknr IS NOT INITIAL.
    SELECT SINGLE txt20 INTO wl_zglt0113_out-desc_conta FROM skat WHERE saknr = vl_saknr AND spras EQ sy-langu.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0113_out TO p_saida.
ENDFORM.

FORM f_exit_zglt0113_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zglt0113_out TYPE zglt0113_out.
  DATA wl_zglt0110  TYPE zglt0110.
  DATA: vl_saknr    TYPE saknr.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zglt0113_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0113_out.

  IF wl_zglt0113_out-tp_imposto IS NOT INITIAL .
    SELECT SINGLE desc_impost INTO wl_zglt0113_out-desc_impost FROM zglt0100 WHERE tp_imposto = wl_zglt0113_out-tp_imposto .

  ENDIF.

  CLEAR vl_saknr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zglt0113_out-saknr
    IMPORTING
      output = vl_saknr.

  IF wl_zglt0113_out-saknr IS NOT INITIAL .
    SELECT SINGLE txt20 INTO wl_zglt0113_out-desc_conta FROM skat WHERE saknr = vl_saknr AND spras EQ sy-langu.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0113_out TO p_registro_manter.

ENDFORM.
