*----------------------------------------------------------------------*
***INCLUDE LZSTRINGF01.
*----------------------------------------------------------------------*

DATA: EDITOR_STRING    TYPE REF TO CL_GUI_TEXTEDIT,
      CONTAINER_STRING TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      LC_STRING        TYPE STRING,
      LC_TITULO        TYPE STRING,
      LC_HTML          TYPE CHAR01,
      LONGTEXT_TAB     TYPE CATSXT_LONGTEXT_ITAB.
*&---------------------------------------------------------------------*
*&      Form  MOSTRA_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_STRING  text
*----------------------------------------------------------------------*
FORM MOSTRA_STRING  USING P_STRING TYPE STRING P_TITULO TYPE STRING P_HTML TYPE CHAR01.
  LC_STRING = P_STRING.
  LC_TITULO = P_TITULO.
  LC_HTML   = P_HTML.
  CALL SCREEN 0100 STARTING AT 05 10.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100' WITH LC_TITULO.

  IF CONTAINER_STRING IS INITIAL.

    CREATE OBJECT CONTAINER_STRING
      EXPORTING
        CONTAINER_NAME              = 'LONGTEXT'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CHECK SY-SUBRC IS INITIAL.

    CASE LC_HTML.
      WHEN ABAP_FALSE.

        CREATE OBJECT EDITOR_STRING
          EXPORTING
            PARENT                 = CONTAINER_STRING
            WORDWRAP_MODE          = '2'
            WORDWRAP_POSITION      = '72'
          EXCEPTIONS
            ERROR_CNTL_CREATE      = 1
            ERROR_CNTL_INIT        = 2
            ERROR_CNTL_LINK        = 3
            ERROR_DP_CREATE        = 4
            GUI_TYPE_NOT_SUPPORTED = 5
            OTHERS                 = 6.

        CHECK SY-SUBRC IS INITIAL.

        CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
          EXPORTING
            I_STRING         = LC_STRING
            I_TABLINE_LENGTH = 72
          TABLES
            ET_TABLE         = LONGTEXT_TAB.

        CALL METHOD EDITOR_STRING->SET_TEXT_AS_R3TABLE
          EXPORTING
            TABLE           = LONGTEXT_TAB
          EXCEPTIONS
            ERROR_DP        = 1
            ERROR_DP_CREATE = 2
            OTHERS          = 3.

        EDITOR_STRING->SET_READONLY_MODE( ).

      WHEN ABAP_TRUE.

        CL_ABAP_BROWSER=>SHOW_HTML(
         EXPORTING
           HTML_STRING = LC_STRING
           MODAL       = ABAP_FALSE
           FORMAT      = CL_ABAP_BROWSER=>LANDSCAPE
           SIZE        = CL_ABAP_BROWSER=>SMALL
           CONTAINER   = CONTAINER_STRING ).

    ENDCASE.


  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  CLEAR: LONGTEXT_TAB[], LONGTEXT_TAB.

  IF EDITOR_STRING IS NOT INITIAL.
    EDITOR_STRING->FREE( ).
  ENDIF.
  CLEAR: EDITOR_STRING.

  IF LC_HTML EQ ABAP_TRUE.
    CL_ABAP_BROWSER=>CLOSE_BROWSER( ).
  ENDIF.

  IF CONTAINER_STRING IS NOT INITIAL.
    CONTAINER_STRING->FREE( ).
  ENDIF.
  CLEAR: CONTAINER_STRING.

  LEAVE TO SCREEN 0.

ENDMODULE.
