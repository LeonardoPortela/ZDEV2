*----------------------------------------------------------------------*
***INCLUDE LZLES_CARGUEROO01.
*----------------------------------------------------------------------*
DATA: SPLITTER                TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER_PICTURE TYPE REF TO CL_GUI_CONTAINER,
      PICTURE                 TYPE REF TO CL_GUI_PICTURE.



*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS C_SERVICE DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS GET_PIC_TAB IMPORTING MIME_URL TYPE CSEQUENCE EXPORTING PIC_TAB  TYPE STANDARD TABLE.
ENDCLASS.                    "c_service DEFINITION

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

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  DATA: IT_CODE TYPE TABLE OF SY-UCOMM.

  CLEAR: IT_CODE.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0182'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'TENANT' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0172'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'DEPARAM' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0173'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'DEPARAU' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0184'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'DEPARU2' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0178'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'LOTEFRETE' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZLES0179'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'VIAGENS' TO IT_CODE.
  ENDIF.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      TCODE  = 'ZWS0004'
    EXCEPTIONS
      OK     = 1
      NOT_OK = 2
      OTHERS = 3.

  IF SY-SUBRC NE 1.
    APPEND 'MONITORINT' TO IT_CODE.
  ENDIF.

  SET PF-STATUS 'PF0001' EXCLUDING IT_CODE.
  SET TITLEBAR 'TL0001'.

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
        CONTAINER = CTL_CCCONTAINER_PICTURE.

  ENDIF.

  IF PICTURE IS INITIAL.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = CTL_CCCONTAINER_PICTURE
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB USING PICTURE.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  DATA: STPREF TYPE RANGE OF ZDE_TP_REFERENCIA.

  CASE OK_CODE.
    WHEN 'TENANT'.

      TRY .
          CALL TRANSACTION 'ZLES0182' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO DATA(LC_ROOT).
          DATA(LC_TEXTO) = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'DEPARAM'.

      TRY .
          CALL TRANSACTION 'ZLES0172' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'DEPARAU'.

      TRY .
          CALL TRANSACTION 'ZLES0173' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'DEPARU2'.

      TRY .
          CALL TRANSACTION 'ZLES0184' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'LOTEFRETE'.

      TRY .
          CALL TRANSACTION 'ZLES0178' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'VIAGENS'.

      TRY .
          CALL TRANSACTION 'ZLES0179' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

    WHEN 'MONITORINT'.

      TRY .
          CALL TRANSACTION 'ZWS0004' WITH AUTHORITY-CHECK.
        CATCH CX_ROOT INTO LC_ROOT.
          LC_TEXTO = LC_ROOT->GET_LONGTEXT( ).
          MESSAGE LC_TEXTO TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

      "STPREF = VALUE #( SIGN = 'I' OPTION = 'CP' ( LOW = 'CARGUERO*' ) ).
      "SUBMIT ZINTEGRACAO VIA SELECTION-SCREEN WITH STPREF EQ STPREF AND RETURN.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


FORM LOAD_PIC_FROM_DB  USING  GUI_PICTURE TYPE REF TO CL_GUI_PICTURE.

  DATA URL(255).
  TYPES PIC_LINE(1022) TYPE X.
  DATA  PIC_TAB TYPE TABLE OF PIC_LINE.

  CLEAR URL.
  URL = '/SAP/PUBLIC/AMAGGI/Monitora.jpg'.

  C_SERVICE=>GET_PIC_TAB(
        EXPORTING MIME_URL = '/SAP/PUBLIC/AMAGGI/Monitora.jpg'
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

  GUI_PICTURE->LOAD_PICTURE_FROM_URL( URL = URL ).

ENDFORM.                               " LOAD_PIC_FROM_DB
