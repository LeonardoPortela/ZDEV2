*----------------------------------------------------------------------*
***INCLUDE ZLESR0165_STATUS_0110O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

  FREE: t_ucomm_0110, ok_code_0110.

  IF tl_0110-novo = abap_false.
    APPEND '&CONFIRMAR'   TO t_ucomm_0110.
  ELSE.
    APPEND '&VOLTAR'      TO t_ucomm_0110.
  ENDIF.

  SET PF-STATUS 'ZLSR0165_0110' EXCLUDING t_ucomm_0110.
* SET TITLEBAR 'xxx'.

  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      IF tl_0110-novo = abap_false.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
