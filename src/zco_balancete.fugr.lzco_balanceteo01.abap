*----------------------------------------------------------------------*
***INCLUDE LZCO_BALANCETEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  DATA: l_variable TYPE string VALUE '(ZCOR0010N)P_F01'.

  ASSIGN (l_variable) TO FIELD-SYMBOL(<value>).

  IF <value> EQ 'X'.
    SET TITLEBAR '1000'.
  ELSE.
    SET TITLEBAR '1010'.
  ENDIF.

  SET PF-STATUS 'BALANCENTE_EXCEL'.

ENDMODULE.
