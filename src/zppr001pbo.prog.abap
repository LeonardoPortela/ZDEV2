*&---------------------------------------------------------------------*
*&  Include           ZPPR001PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_VIEW  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_VIEW OUTPUT.
  MOVE: SY-REPID TO PROGRAM,
        SY-DYNNR TO DYNPRO.

  DATA CONTROLLER_REF          TYPE STRING.
  DATA VIEW_REF                TYPE STRING.

  CASE DYNPRO.
    WHEN 0001.
      MOVE 'MAIN_CONTROLLER'      TO CONTROLLER_REF.
    WHEN 0002.
      MOVE 'DISPLAY_CONTROLLER'   TO CONTROLLER_REF.
      MOVE 'DISPLAY_VIEW'         TO VIEW_REF.
    WHEN 0005.
      MOVE 'SHIPMENTS_CONTROLLER' TO CONTROLLER_REF.
  ENDCASE.

  TRY.
      ZSAPMVC_CONTROLLER=>CREATE_CONTROLLER( EXPORTING
                                             REPID = PROGRAM
                                             DYNNR = DYNPRO
                                             CONTROLLER_REFERENCE = CONTROLLER_REF
                                             IMPORTING
                                             CONTROLLER_INSTANCE = CONTROLLER ).

      VIEW = ZSAPMVC_VIEW=>CREATE_VIEW( REPID          = PROGRAM
                                        DYNNR          = DYNPRO
                                        CONTROLLER     = CONTROLLER
                                        VIEW_REFERENCE = VIEW_REF
                                        ).
    CATCH ZCX_SAPMVC INTO EXCEPTION.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  VIEW->TRIGGER_STATUS( ).
  VIEW->TRIGGER_TITLEBAR( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PROCESS_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PROCESS_PBO OUTPUT.
  VIEW->TRIGGER_PBO( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  AT_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AT_EXIT_COMMAND INPUT.

  MOVE: SY-REPID TO PROGRAM,
        SY-DYNNR TO DYNPRO.

  "// Get Controller Instance for actual program and dynpro
  CONTROLLER = ZSAPMVC_CONTROLLER=>GET_CONTROLLER( REPID = PROGRAM DYNNR = DYNPRO ).
  "// Get View Instance for actual program and dynpro
  VIEW = ZSAPMVC_VIEW=>GET_VIEW( REPID = PROGRAM DYNNR = DYNPRO ).

  VIEW->TRIGGER_AT_EXIT_COMMAND( UCOMM = SY-UCOMM ).

ENDMODULE.
