*&---------------------------------------------------------------------*
*&  Include           ZPPR001PAI
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_VIEW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_VIEW INPUT.
  MOVE: SY-REPID TO PROGRAM,
        SY-DYNNR TO DYNPRO.

  TRY.
      "// Get Controller Instance for actual program and dynpro
      CONTROLLER = ZSAPMVC_CONTROLLER=>GET_CONTROLLER( REPID = PROGRAM DYNNR = DYNPRO ).
      "// Get View Instance for actual program and dynpro
      VIEW = ZSAPMVC_VIEW=>GET_VIEW( REPID = PROGRAM DYNNR = DYNPRO ).

    CATCH ZCX_SAPMVC INTO EXCEPTION.
      "// Display Long text message
      DATA(MSG) = EXCEPTION->GET_TEXT( ).
      MESSAGE MSG TYPE 'E'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE USER_COMMAND INPUT.
  VIEW->TRIGGER_USER_COMMAND( UCOMM = SY-UCOMM ).
ENDMODULE.
