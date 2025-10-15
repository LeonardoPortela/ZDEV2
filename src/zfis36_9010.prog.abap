*----------------------------------------------------------------------*
***INCLUDE ZFIS36_9010.
*----------------------------------------------------------------------*
CLASS C_SERVICE DEFINITION DEFERRED.

DATA: SPLITTER_9010    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_PICTURE_9010 TYPE REF TO CL_GUI_CONTAINER,
      PICTURE_9010     TYPE REF TO CL_GUI_PICTURE.


*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS C_SERVICE DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS GET_PIC_TAB             IMPORTING MIME_URL TYPE CSEQUENCE EXPORTING PIC_TAB  TYPE STANDARD TABLE.
ENDCLASS.                    "c_service DEFINITION

*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9010 OUTPUT.

  SUPPRESS DIALOG.

  SET TITLEBAR 'TL9010'.

  IF SPLITTER_9010 IS INITIAL.

    CREATE OBJECT SPLITTER_9010
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_9010->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_PICTURE_9010.

    CREATE OBJECT PICTURE_9010
      EXPORTING
        PARENT = CTL_PICTURE_9010
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE_9010->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE_9010->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB USING PICTURE_9010.

  ENDIF.

  LEAVE TO LIST-PROCESSING.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9010 INPUT.

  CALL SCREEN 9001 STARTING AT 50 08.

  PICTURE_9010->FREE( ).
  CLEAR: PICTURE_9010.
  CTL_PICTURE_9010->FREE( ).
  CLEAR: CTL_PICTURE_9010.
  SPLITTER_9010->FREE( ).
  CLEAR: SPLITTER_9010.

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


CLASS C_SERVICE IMPLEMENTATION.
  METHOD GET_PIC_TAB.
    DATA PIC_WA TYPE XSTRING.
    DATA LENGTH TYPE I.
    DATA MIME_API TYPE REF TO IF_MR_API.
    MIME_API = CL_MIME_REPOSITORY_API=>GET_API( ).
    MIME_API->GET( EXPORTING
                     I_URL             = MIME_URL
                     I_CHECK_AUTHORITY = ABAP_FALSE
                   IMPORTING
                     E_CONTENT = PIC_WA
                   EXCEPTIONS OTHERS = 4 ).
    IF SY-SUBRC = 4.
      RETURN.
    ENDIF.
    CLEAR PIC_TAB.
    LENGTH = XSTRLEN( PIC_WA ).
    WHILE LENGTH >= 1022.
      APPEND PIC_WA(1022) TO PIC_TAB.
      SHIFT PIC_WA BY 1022 PLACES LEFT IN BYTE MODE.
      LENGTH = XSTRLEN( PIC_WA ).
    ENDWHILE.
    IF LENGTH > 0.
      APPEND PIC_WA TO PIC_TAB.
    ENDIF.
  ENDMETHOD.                    "get_pic_tab

ENDCLASS.                    "c_service IMPLEMENTATION
