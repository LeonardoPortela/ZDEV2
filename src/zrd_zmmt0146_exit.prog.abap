*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0146_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0146_exit.

FORM f_exit_zmmt0146_0001 USING p_registro_manter TYPE any.
  DATA: wa_zmmt0146_out TYPE zmmt0146_out.
  CLEAR wa_zmmt0146_out.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0146_out.

  MOVE-CORRESPONDING wa_zmmt0146_out TO p_registro_manter.

ENDFORM.


FORM f_exit_zmmt0146_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0146 TYPE zmmt0146.

  CLEAR: wl_zmmt0146.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0146.

  CLEAR p_error.

  SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w) WHERE werks EQ @wl_zmmt0146-werks.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'Filial informada não existe' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zmmt0146-matnr
    IMPORTING
      output = wl_zmmt0146-matnr.

  SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
     WHERE matnr EQ @wl_zmmt0146-matnr
       AND spras EQ @sy-langu.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'Material informado não existe' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0146 TO p_registro_manter.

ENDFORM.


FORM f_exit_zmmt0146_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0146 TYPE zmmt0146.

  CLEAR: wl_zmmt0146.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0146.

  MOVE-CORRESPONDING wl_zmmt0146 TO p_registro_manter.

ENDFORM.


FORM f_exit_zmmt0146_0004 CHANGING p_saida TYPE any.

  DATA: wa_zmmt0146_out TYPE zmmt0146_out.

  CLEAR wa_zmmt0146_out .

  MOVE-CORRESPONDING p_saida TO wa_zmmt0146_out.

  SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w) WHERE werks EQ @wa_zmmt0146_out-werks.
  IF sy-subrc EQ 0.
    wa_zmmt0146_out-desc_centro = wa_t001w-name1.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zmmt0146_out-matnr
    IMPORTING
      output = wa_zmmt0146_out-matnr.

  SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
    WHERE matnr EQ @wa_zmmt0146_out-matnr
      AND spras EQ @sy-langu.
  IF sy-subrc EQ 0.
    wa_zmmt0146_out-desc_material = wa_makt-maktx.
  ENDIF.

  MOVE-CORRESPONDING wa_zmmt0146_out TO p_saida.

ENDFORM.

FORM f_exit_zmmt0146_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zmmt0146_out TYPE zmmt0146_out.

  CLEAR wa_zmmt0146_out .

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0146_out.

  SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w) WHERE werks EQ @wa_zmmt0146_out-werks.
  IF sy-subrc EQ 0.
    wa_zmmt0146_out-desc_centro = wa_t001w-name1.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zmmt0146_out-matnr
    IMPORTING
      output = wa_zmmt0146_out-matnr.

  SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
    WHERE matnr EQ @wa_zmmt0146_out-matnr
      AND spras EQ @sy-langu.
  IF sy-subrc EQ 0.
    wa_zmmt0146_out-desc_material = wa_makt-maktx.
  ENDIF.

  MOVE-CORRESPONDING wa_zmmt0146_out TO p_registro_manter.

ENDFORM.
