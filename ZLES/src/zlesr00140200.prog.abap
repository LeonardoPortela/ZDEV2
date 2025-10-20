*----------------------------------------------------------------------*
***INCLUDE ZLESR00140200 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  IF vg_tela_0200 IS INITIAL.
    vg_tela_0200 = c_0201.
  ENDIF.

  CASE vg_tela_0200.
    WHEN c_0201.
      SET PF-STATUS 'PFSIMULACAO'.
      SET TITLEBAR 'TLSIMULACAO'.
    WHEN c_0202.
      SET PF-STATUS 'PFITENS'.
      SET TITLEBAR 'TLITENS'.
    WHEN c_0203.
      SET PF-STATUS 'PFCONTAS'.
      SET TITLEBAR 'TLCONTAS'.
  ENDCASE.

ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  IF vg_tela_0200 EQ c_0201.
    CASE ok_code.
      WHEN c_exit OR c_cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
