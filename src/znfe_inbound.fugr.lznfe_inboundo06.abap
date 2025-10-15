*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO06.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_HANDLER_9002 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK_9002  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER_9002 TYPE REF TO LCL_EVENT_HANDLER_9002.

DATA: CTL_ALV_9002       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_9002       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_9002        TYPE LVC_S_LAYO,
      GS_VAR_9002        TYPE DISVARIANT,
      GS_SCROLL_COL_9002 TYPE LVC_S_COL,
      GS_SCROLL_ROW_9002 TYPE LVC_S_ROID,
      IT_CATALOG_9002    TYPE LVC_T_FCAT,
      EDITOR_9003        TYPE REF TO CL_GUI_TEXTEDIT,
      CONTAINER_9003     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      LONGTEXT_TAB_9003  TYPE CATSXT_LONGTEXT_ITAB,
      CK_BT_TEXT         TYPE CHAR100,
      CK_BT_TELA         TYPE CHAR04,
      ICON_NAME(20)      TYPE C,
      ICON_TEXT(10)      TYPE C,
      ICON_INFO(50)      TYPE C,
      BTN_APROVACAO      TYPE CHAR50.

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER_9002 IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK_9002.
    PERFORM HANDLE_DOUBLE_CLICK_9002 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'PF9002'.
  SET TITLEBAR 'TL9002'.

  IF CTL_CON_9002 IS INITIAL.

    CREATE OBJECT CTL_CON_9002
      EXPORTING
        CONTAINER_NAME = 'ALV_9002'.

    CREATE OBJECT CTL_ALV_9002
      EXPORTING
        I_PARENT = CTL_CON_9002.

    PERFORM FILL_IT_FIELDCATALOG_9002.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_9002.
*   Set layout parameters for ALV grid

    GS_LAY_9002-SEL_MODE   = 'A'.
    GS_LAY_9002-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_9002->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_9002
        IS_VARIANT      = GS_VAR_9002
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_9002
        IT_OUTTAB       = IT_DD07V.

    CALL METHOD CTL_ALV_9002->REFRESH_TABLE_DISPLAY.

    CREATE OBJECT EVENT_HANDLER_9002.
    SET HANDLER EVENT_HANDLER_9002->HANDLE_DOUBLE_CLICK_9002 FOR CTL_ALV_9002.
  ELSE.
    CALL METHOD CTL_ALV_9002->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_9002->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_9002
      ES_ROW_NO   = GS_SCROLL_ROW_9002.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_9002  USING P_ROW TYPE LVC_S_ROW.
  DATA: LC_ROW TYPE LVC_T_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.
    APPEND P_ROW TO LC_ROW.

    CALL METHOD CTL_ALV_9002->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_DD07V INDEX P_ROW-INDEX INTO WA_DD07V.
    CK_SELECIONOU_APROVACAO = ABAP_TRUE.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_9002.

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_9002> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'DD07V'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_9002.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_9002 ASSIGNING <FS_CAT_9002>.
    CASE <FS_CAT_9002>-FIELDNAME.
      WHEN 'DDTEXT'.
        <FS_CAT_9002>-OUTPUTLEN = 40.
        <FS_CAT_9002>-SCRTEXT_L = 'Descrição'.
        <FS_CAT_9002>-SCRTEXT_M = 'Descrição'.
        <FS_CAT_9002>-SCRTEXT_S = 'Descrição'.
      WHEN OTHERS.
        <FS_CAT_9002>-NO_OUT = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_N55

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_9002.

  GS_VAR_9002-REPORT      = SY-REPID.
  GS_VAR_9002-HANDLE      = '9002'.
  GS_VAR_9002-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_9002-USERNAME    = ABAP_FALSE.
  GS_VAR_9002-VARIANT     = ABAP_FALSE.
  GS_VAR_9002-TEXT        = ABAP_FALSE.
  GS_VAR_9002-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_N55

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.

  CLEAR: IT_UCOMM[].

  IF CK_VIEW_APROVACAO EQ ABAP_TRUE.
    APPEND: OK_APROVAR   TO IT_UCOMM.
    APPEND: OK_RECUSAR   TO IT_UCOMM.
    APPEND: OK_CONFIRMAR TO IT_UCOMM.
    APPEND: OK_DESFAZER  TO IT_UCOMM.
  ELSEIF WA_ULTIMA_APROVACAO IS NOT INITIAL.
    "01	Autorizado
    "02	Negado
    IF WA_ULTIMA_APROVACAO-TP_AUTORIZADO EQ '01'.
      APPEND: OK_APROVAR TO IT_UCOMM.
    ELSE.
      APPEND: OK_RECUSAR TO IT_UCOMM.
    ENDIF.
  ELSEIF WA_ULTIMA_APROVACAO IS INITIAL.
    APPEND: OK_RECUSAR TO IT_UCOMM.
  ENDIF.

  IF SEL_TP_AUTORIZACAO IS INITIAL.
    CK_BT_TELA = ICON_FAILURE.
    APPEND: OK_CONFIRMAR TO IT_UCOMM.
    APPEND: OK_DESFAZER  TO IT_UCOMM.
  ELSE.
    APPEND: OK_APROVAR   TO IT_UCOMM.
    APPEND: OK_RECUSAR   TO IT_UCOMM.
  ENDIF.

  SET PF-STATUS 'PF9003' EXCLUDING IT_UCOMM.
  SET TITLEBAR 'TL9003' WITH WA_DD07V-DDTEXT.

  IF ( EDITOR_9003 IS INITIAL ).

    CREATE OBJECT CONTAINER_9003
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

    CREATE OBJECT EDITOR_9003
      EXPORTING
        PARENT                 = CONTAINER_9003
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

    IF CK_VIEW_APROVACAO EQ ABAP_TRUE.
      CALL METHOD EDITOR_9003->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR_9003->TRUE.

      CLEAR: LONGTEXT_TAB_9003.

      LOOP AT TL_TLINES.
        APPEND TL_TLINES-TDLINE TO LONGTEXT_TAB_9003.
      ENDLOOP.

      CALL METHOD EDITOR_9003->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE           = LONGTEXT_TAB_9003
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.
    ELSE.
      CALL METHOD EDITOR_9003->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR_9003->FALSE.
    ENDIF.
  ENDIF.

  CASE CK_BT_TELA.
    WHEN ICON_ALLOW.
      ICON_NAME = 'ICON_ALLOW'.
      CONCATENATE TEXT-015 WA_DD07V-DDTEXT INTO CK_BT_TEXT SEPARATED BY SPACE.
      ICON_TEXT = TEXT-015.
      ICON_INFO = CK_BT_TEXT.
    WHEN ICON_REJECT.
      ICON_NAME = 'ICON_REJECT'.
      CONCATENATE TEXT-016 WA_DD07V-DDTEXT INTO CK_BT_TEXT SEPARATED BY SPACE.
      ICON_TEXT = 'Rejeitar'.
      ICON_INFO = TEXT-016.
    WHEN ICON_FAILURE.
      ICON_NAME = 'ICON_FAILURE'.
      CK_BT_TEXT = TEXT-017.
      ICON_TEXT = 'Selecionar'.
      ICON_INFO = CK_BT_TEXT.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME                  = ICON_NAME
      TEXT                  = ICON_TEXT
    IMPORTING
      RESULT                = BTN_APROVACAO
    EXCEPTIONS
      ICON_NOT_FOUND        = 1
      OUTPUTFIELD_TOO_SHORT = 2
      OTHERS                = 3.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003_EXIT INPUT.

  CLEAR: TL_TLINES[].

  IF EDITOR_9003 IS NOT INITIAL.
    CALL METHOD EDITOR_9003->FREE.
    CLEAR: EDITOR_9003.
  ENDIF.

  IF CONTAINER_9003 IS NOT INITIAL.
    CALL METHOD CONTAINER_9003->FREE.
    CLEAR: CONTAINER_9003.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003 INPUT.

  CHECK CK_VIEW_APROVACAO EQ ABAP_FALSE.

  CALL METHOD EDITOR_9003->GET_TEXT_AS_R3TABLE
    IMPORTING
      TABLE           = LONGTEXT_TAB_9003
    EXCEPTIONS
      ERROR_DP        = 1
      ERROR_DP_CREATE = 2
      OTHERS          = 3.

  DESCRIBE TABLE LONGTEXT_TAB_9003 LINES DATA(QTD_LINHAS).

  IF QTD_LINHAS IS INITIAL.
    MESSAGE S090.
    EXIT.
  ENDIF.

  CASE OK_CODE.
    WHEN OK_APROVAR.

      SEL_TP_AUTORIZACAO = '01'.
      CK_BT_TELA         = ICON_ALLOW.
      CALL METHOD EDITOR_9003->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR_9003->TRUE.

    WHEN OK_RECUSAR.

      SEL_TP_AUTORIZACAO = '02'.
      CK_BT_TELA         = ICON_REJECT.
      CALL METHOD EDITOR_9003->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR_9003->TRUE.

    WHEN OK_DESFAZER.

      CLEAR: SEL_TP_AUTORIZACAO.
      CK_BT_TELA = ICON_FAILURE.

      CALL METHOD EDITOR_9003->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = EDITOR_9003->FALSE.

    WHEN OK_CONFIRMAR.
      CK_SELECIONOU_APROVACAO = ABAP_TRUE.

      CLEAR: TL_TLINES[].

      LOOP AT LONGTEXT_TAB_9003 INTO DATA(WA_LINE).
        TL_TLINES-TDLINE = WA_LINE.
        APPEND TL_TLINES.
      ENDLOOP.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
