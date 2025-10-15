*&---------------------------------------------------------------------*
*&  Include           ZGF004_LCI
*&---------------------------------------------------------------------*
* Begin YEKAL0K026011
CLASS LCL_SIMPLE_TEXT_EDITOR IMPLEMENTATION.
  METHOD CONSTRUCTOR.

    TITLE        = IM_TITLE.
    LONGTEXT_TAB = IM_LONGTEXT_TAB.
    DISPLAY_MODE = IM_DISPLAY_MODE.
    PROTEGER     = IM_PROTEGER.

  ENDMETHOD.                    "constructor

  METHOD FREE.
    CALL METHOD: EDITOR->FREE,
                 CONTAINER->FREE.
  ENDMETHOD.                    "free

  METHOD GET_TEXT.
    DATA: LF_COUNT TYPE SYTABIX.

    CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

    CALL METHOD CL_GUI_CFW=>FLUSH.

    LF_COUNT = LINES( LONGTEXT_TAB ).

    DO.
      DELETE LONGTEXT_TAB FROM  LF_COUNT
                     WHERE TABLE_LINE IS INITIAL.
      IF SY-SUBRC IS INITIAL AND LF_COUNT > 1.
        SUBTRACT 1 FROM LF_COUNT.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    RE_LONGTEXT = LONGTEXT_TAB.

  ENDMETHOD.                    "get_text

  METHOD START.

    DATA: WL_TABIX TYPE I.

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
        WORDWRAP_MODE          = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION "'2'
        WORDWRAP_POSITION      = '72'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

    IF NOT DISPLAY_MODE IS INITIAL.
*     Set control to display only
      CALL METHOD EDITOR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR->TRUE.
    ENDIF.

    CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

    IF PROTEGER IS NOT INITIAL.
      DESCRIBE TABLE LONGTEXT_TAB LINES WL_TABIX.

      APPEND INITIAL LINE TO LONGTEXT_TAB.
      CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE           = LONGTEXT_TAB
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.

      EDITOR->PROTECT_LINES( FROM_LINE = 1
                             TO_LINE = WL_TABIX ).

    ELSE.

      CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE           = LONGTEXT_TAB
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.
    ENDIF.

  ENDMETHOD.                    "start

ENDCLASS.                    "lcl_simple_text_editor IMPLEMENTATION
