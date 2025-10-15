*----------------------------------------------------------------------*
***INCLUDE ZMMR128_STATUS_0100.
*----------------------------------------------------------------------*

DATA: SPLITTER_0100    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_PICTURE_0100 TYPE REF TO CL_GUI_CONTAINER,
      PICTURE_0100     TYPE REF TO CL_GUI_PICTURE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.

  SELECT SINGLE * INTO @DATA(WA_LOCAL)
    FROM J_1BBRANCH
   WHERE BUKRS  EQ @PEMPRE
     AND BRANCH EQ @PFILIA.

  CONCATENATE WA_LOCAL-BRANCH '-' WA_LOCAL-NAME INTO DATA(TX_LOCAL).

  SET TITLEBAR 'TL0100' WITH PSAFRA TX_LOCAL.

  IF SPLITTER_0100 IS INITIAL.
    CREATE OBJECT SPLITTER_0100
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_0100->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_PICTURE_0100.
  ENDIF.

  IF PICTURE_0100 IS INITIAL.

    CREATE OBJECT PICTURE_0100
      EXPORTING
        PARENT = CTL_PICTURE_0100
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE_0100->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE_0100->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB USING PICTURE_0100.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

##PERF_NO_TYPE
FORM LOAD_PIC_FROM_DB  USING  GUI_PICTURE TYPE REF TO CL_GUI_PICTURE.

  DATA URL(255).
  TYPES PIC_LINE(1022) TYPE X.
  DATA  PIC_TAB TYPE TABLE OF PIC_LINE.

  CLEAR URL.
  URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  C_SERVICE=>GET_PIC_TAB(
        EXPORTING MIME_URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
        IMPORTING PIC_TAB  = PIC_TAB ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE    = 'image'
      SUBTYPE = 'GIF'
    TABLES
      DATA    = PIC_TAB
    CHANGING
      URL     = URL
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD GUI_PICTURE->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL.

ENDFORM.                               " LOAD_PIC_FROM_DB

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN 'SOLI_PESQ'.
      CLEAR: OK_CODE.
      CALL SCREEN 9002 STARTING AT 20 02.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA .

  IF PICTURE_0100 IS NOT INITIAL.
    PICTURE_0100->FREE( ).
  ENDIF.
  CLEAR: PICTURE_0100.

  IF CTL_PICTURE_0100 IS NOT INITIAL.
    CTL_PICTURE_0100->FREE( ).
  ENDIF.
  CLEAR: CTL_PICTURE_0100.

  IF SPLITTER_0100 IS NOT INITIAL.
    SPLITTER_0100->FREE( ).
  ENDIF.
  CLEAR: SPLITTER_0100.

ENDFORM.
