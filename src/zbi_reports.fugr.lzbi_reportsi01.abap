*----------------------------------------------------------------------*
***INCLUDE LZBI_REPORTSI01.
*----------------------------------------------------------------------*

DATA: SPLITTER        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER TYPE REF TO CL_GUI_CONTAINER,
      HTML_CONTROL    TYPE REF TO CL_GUI_HTML_VIEWER.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  DATA: LC_ANSWER TYPE C LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL          = TEXT-001
      TEXTLINE1      = TEXT-002
      CANCEL_DISPLAY = ABAP_FALSE
    IMPORTING
      ANSWER         = LC_ANSWER.

  IF LC_ANSWER EQ 'J'.
    PERFORM LIMPAR_TELA_0100.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: LC_TEXTO TYPE C LENGTH 200.

  MOVE WA_ZBIT0003-DS_REPORT TO LC_TEXTO.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100' WITH LC_TEXTO+000(50) LC_TEXTO+050(50) LC_TEXTO+100(50) LC_TEXTO+150(50).

  IF SPLITTER IS INITIAL.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER.

    CREATE OBJECT HTML_CONTROL
      EXPORTING
        PARENT = CTL_CCCONTAINER.

    CALL METHOD HTML_CONTROL->SHOW_URL
      EXPORTING
        URL = LC_URI.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_0100 .

  IF HTML_CONTROL IS NOT INITIAL.
    HTML_CONTROL->FREE( ).
  ENDIF.
  CLEAR: HTML_CONTROL.

  IF CTL_CCCONTAINER IS NOT INITIAL.
    CTL_CCCONTAINER->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER.

  IF SPLITTER IS NOT INITIAL.
    SPLITTER->FREE( ).
  ENDIF.
  CLEAR: SPLITTER.

ENDFORM.
