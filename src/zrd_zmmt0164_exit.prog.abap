*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0164_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0164_exit.

FORM f_exit_zmmt0164_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0164 TYPE zmmt0164.
  CLEAR: wl_zmmt0164.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0164.

  wl_zmmt0164-dt_atual = sy-datum.
  wl_zmmt0164-hr_atual = sy-uzeit.
  wl_zmmt0164-usnam = sy-uname.



  MOVE-CORRESPONDING wl_zmmt0164 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0164_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zmmt0164 TYPE zmmt0164.
  CLEAR:  wa_zmmt0164.

  MOVE-CORRESPONDING p_registro_manter TO wa_zmmt0164.

*  "Verificar se o pedido existe.
*  SELECT SINGLE * FROM ekko INTO @DATA(ws_ekko) WHERE ebeln EQ @wa_zmmt0164-ebeln.
*  IF sy-subrc NE 0.
*    CLEAR: p_error.
*    p_error = abap_true.
*    MESSAGE 'Pedido não existe!' TYPE 'S' DISPLAY LIKE 'E'.
*    CLEAR: ws_ekko.
*    EXIT.
*  ENDIF.
*  CLEAR: ws_ekko.

*  IF wa_zmmt0164-lifnr IS INITIAL AND wa_zmmt0164-mwskz IS INITIAL.
*   CLEAR: p_error.
*    p_error = abap_true.
*    MESSAGE 'Informe o fornecedor ou IVA !' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

ENDFORM.

FORM  f_exit_zmmt0164_0016 USING p_ucomm  TYPE sy-ucomm CHANGING p_registro_manter TYPE any p_saida TYPE any.


  DATA: wl_zmmt0164 TYPE zmmt0164.
*
*  CLEAR: wl_zmmt0164.
*
  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0164.

  IF wl_zmmt0164-lifnr IS NOT INITIAL.
    SELECT SINGLE name1 FROM lfa1 INTO wl_zmmt0164-name1 WHERE lifnr EQ wl_zmmt0164-lifnr.
  ENDIF.


  MOVE-CORRESPONDING wl_zmmt0164 TO p_registro_manter.
ENDFORM.
