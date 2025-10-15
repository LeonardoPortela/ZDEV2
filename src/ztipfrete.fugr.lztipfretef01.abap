*----------------------------------------------------------------------*
***INCLUDE LZTIPFRETEF01.
*----------------------------------------------------------------------*

DATA: IT_TLINES    TYPE TABLE OF TLINE,
      EDITOR       TYPE REF TO CL_GUI_TEXTEDIT,
      LONGTEXT_TAB TYPE ZDE_TDLINE_T,
      LONGTEXT     TYPE ZDE_TDLINE,
      CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_TLINES  text
*      -->P_WA_AUTORIZACAO  text
*----------------------------------------------------------------------*
FORM VISUALIZAR_TEXTO  TABLES P_TL_TLINES STRUCTURE TLINE.

  MOVE P_TL_TLINES[] TO IT_TLINES[].

  CALL SCREEN 9001 STARTING AT 5 5.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.

  IF EDITOR IS NOT INITIAL.
    EDITOR->FREE( ).
  ENDIF.
  CLEAR: EDITOR.

  IF CONTAINER IS NOT INITIAL.
    CONTAINER->FREE( ).
  ENDIF.
  CLEAR: CONTAINER.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.

  IF ( EDITOR IS INITIAL ).

    CREATE OBJECT CONTAINER
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

    CREATE OBJECT EDITOR
      EXPORTING
        PARENT                 = CONTAINER
        WORDWRAP_MODE          = '2'
        WORDWRAP_POSITION      = '132'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    CALL METHOD EDITOR->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = EDITOR->TRUE.

    CLEAR: LONGTEXT_TAB.

    LOOP AT IT_TLINES INTO DATA(WA_LINE).
      LONGTEXT = WA_LINE-TDLINE.
      APPEND LONGTEXT TO LONGTEXT_TAB.
    ENDLOOP.

    CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

  ENDIF.

ENDMODULE.
