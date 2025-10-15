*----------------------------------------------------------------------*
***INCLUDE ZSDY0003_0100.
*----------------------------------------------------------------------*

DATA: CTL_ALV_0100       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0100       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0100        TYPE LVC_S_LAYO,
      GS_VAR_0100        TYPE DISVARIANT,
      GS_SCROLL_COL_0100 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0100 TYPE LVC_S_ROID,
      IT_CATALOG_0100    TYPE LVC_T_FCAT,
      CONTAINER          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR             TYPE REF TO CL_GUI_TEXTEDIT.

DATA: IT_TEXTO     TYPE TABLE OF TLINE WITH HEADER LINE,
      LONGTEXT_TAB TYPE CATSXT_LONGTEXT_ITAB,
      WL_NAME      LIKE THEAD-TDNAME.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  DATA: WL_HEADERD TYPE THEAD.

  CASE OK_CODE.
    WHEN OK_NEW.
      CLEAR: ZDE_ZSDYT0051.
      LC_OPERACO = 'I'.
      CK_PRIMEIRO = ABAP_TRUE.
      CALL SCREEN 0101 STARTING AT 04 05.
      PERFORM PESQUISAR.
    WHEN OK_EDIT.

      IF IT_ZSDYT0051_SEL[] IS INITIAL.
        MESSAGE 'Seleccione una fila' TYPE 'S'.
        RETURN.
      ENDIF.

      LC_OPERACO = 'E'.
      CK_PRIMEIRO = ABAP_TRUE.

      MOVE-CORRESPONDING IT_ZSDYT0051_SEL TO ZDE_ZSDYT0051.

      CALL SCREEN 0101 STARTING AT 04 05.
      PERFORM PESQUISAR.

    WHEN OK_DELETE.

      IF IT_ZSDYT0051_SEL[] IS INITIAL.
        MESSAGE 'Seleccione una fila' TYPE 'S'.
        RETURN.
      ENDIF.

      LOOP AT IT_ZSDYT0051_SEL.

        LC_OPERACO = 'D'.

        WL_HEADERD-TDOBJECT = 'ZCLIENTE'.
        WL_HEADERD-TDID     = 'ZCAR'.
        WL_HEADERD-TDSPRAS  = SY-LANGU.
        CONCATENATE SY-MANDT IT_ZSDYT0051_SEL-KUNNR IT_ZSDYT0051_SEL-VKORG IT_ZSDYT0051_SEL-VTWEG IT_ZSDYT0051_SEL-SPART INTO WL_HEADERD-TDNAME.

        CALL FUNCTION 'DELETE_TEXT'
          EXPORTING
            ID        = WL_HEADERD-TDID
            LANGUAGE  = WL_HEADERD-TDSPRAS
            NAME      = WL_HEADERD-TDNAME
            OBJECT    = WL_HEADERD-TDOBJECT
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.

        DELETE FROM ZSDYT0051
         WHERE KUNNR EQ IT_ZSDYT0051_SEL-KUNNR
           AND VKORG EQ IT_ZSDYT0051_SEL-VKORG
           AND VTWEG EQ IT_ZSDYT0051_SEL-VTWEG
           AND SPART EQ IT_ZSDYT0051_SEL-SPART.

        COMMIT WORK.

      ENDLOOP.


      PERFORM PESQUISAR.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF CTL_CON_0100 IS INITIAL.

    CREATE OBJECT CTL_CON_0100
      EXPORTING
        CONTAINER_NAME = 'ALV_0100'.

    CREATE OBJECT CTL_ALV_0100
      EXPORTING
        I_PARENT = CTL_CON_0100.

    PERFORM FILL_IT_FIELDCATALOG_0100.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0100.

    GS_LAY_0100-SEL_MODE   = 'A'.
    GS_LAY_0100-ZEBRA      = ABAP_TRUE.
    GS_LAY_0100-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0100->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0100
        IS_VARIANT      = GS_VAR_0100
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0100
        IT_OUTTAB       = IT_ZSDYT0051_ALV[].

  ELSE.
    CALL METHOD CTL_ALV_0100->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0100->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0100
      ES_ROW_NO   = GS_SCROLL_ROW_0100.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0100 .

  FIELD-SYMBOLS: <FS_CAT_0100> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZSDYT0051'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0100.

  LOOP AT IT_CATALOG_0100 ASSIGNING <FS_CAT_0100>.
    CASE <FS_CAT_0100>-FIELDNAME.
      WHEN 'MANDT'.
        <FS_CAT_0100>-NO_OUT = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0100

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0100.
  GS_VAR_0100-REPORT      = SY-REPID.
  GS_VAR_0100-HANDLE      = '0100'.
  GS_VAR_0100-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0100-USERNAME    = ABAP_FALSE.
  GS_VAR_0100-VARIANT     = ABAP_FALSE.
  GS_VAR_0100-TEXT        = ABAP_FALSE.
  GS_VAR_0100-DEPENDVARS  = ABAP_FALSE.
ENDFORM.                    " FILL_GS_VARIANT_0100

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TL0101'.

  IF ZDE_ZSDYT0051-KUNNR IS NOT INITIAL.
    SELECT SINGLE NAME1 INTO ZDE_ZSDYT0051-NAME1
      FROM KNA1
     WHERE KUNNR EQ ZDE_ZSDYT0051-KUNNR.
  ENDIF.

  IF ZDE_ZSDYT0051-VKORG IS NOT INITIAL.
    SELECT SINGLE VTEXT INTO ZDE_ZSDYT0051-VTEXT_O
       FROM TVKOT
      WHERE SPRAS EQ SY-LANGU
        AND VKORG EQ ZDE_ZSDYT0051-VKORG.
  ENDIF.

  IF ZDE_ZSDYT0051-VTWEG IS NOT INITIAL.
    SELECT SINGLE VTEXT INTO ZDE_ZSDYT0051-VTEXT_C
      FROM TVTWT
     WHERE SPRAS EQ SY-LANGU
       AND VTWEG EQ ZDE_ZSDYT0051-VTWEG.
  ENDIF.

  IF ZDE_ZSDYT0051-SPART IS NOT INITIAL.
    SELECT SINGLE VTEXT INTO ZDE_ZSDYT0051-VTEXT_S
      FROM TSPAT
     WHERE SPRAS EQ SY-LANGU
       AND SPART EQ ZDE_ZSDYT0051-SPART.
  ENDIF.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'A1'.
      CASE LC_OPERACO.
        WHEN 'I'.
          SCREEN-INPUT = 1.
        WHEN 'E'.
          SCREEN-INPUT = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

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
        WORDWRAP_POSITION      = '50'
      EXCEPTIONS
        ERROR_CNTL_CREATE      = 1
        ERROR_CNTL_INIT        = 2
        ERROR_CNTL_LINK        = 3
        ERROR_DP_CREATE        = 4
        GUI_TYPE_NOT_SUPPORTED = 5
        OTHERS                 = 6.

    CHECK SY-SUBRC IS INITIAL.

  ENDIF.

  IF ( EDITOR IS NOT INITIAL ) AND ( CK_PRIMEIRO EQ ABAP_TRUE ).

    CK_PRIMEIRO = ABAP_FALSE.

    IF LC_OPERACO EQ 'E'.

      CONCATENATE SY-MANDT ZDE_ZSDYT0051-KUNNR ZDE_ZSDYT0051-VKORG ZDE_ZSDYT0051-VTWEG ZDE_ZSDYT0051-SPART INTO WL_NAME.

      CLEAR: IT_TEXTO[], LONGTEXT_TAB.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'ZCAR'
          LANGUAGE                = SY-LANGU
          NAME                    = WL_NAME
          OBJECT                  = 'ZCLIENTE'
        TABLES
          LINES                   = IT_TEXTO
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.

      IF ( SY-SUBRC IS INITIAL ).
        LOOP AT IT_TEXTO.
          APPEND IT_TEXTO-TDLINE TO LONGTEXT_TAB.
        ENDLOOP.

        CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
          EXPORTING
            TABLE           = LONGTEXT_TAB
          EXCEPTIONS
            ERROR_DP        = 1
            ERROR_DP_CREATE = 2
            OTHERS          = 3.
      ENDIF.
    ELSE.
      CALL METHOD EDITOR->DELETE_TEXT.
    ENDIF.

    CALL METHOD EDITOR->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = EDITOR->FALSE.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO INPUT.

  CALL METHOD CTL_ALV_0100->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0100
      ES_ROW_NO   = GS_SCROLL_ROW_0100.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS INPUT.

  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_ZSDYT0051_SEL[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZSDYT0051_ALV INDEX WA_SELECTED_ROWS-INDEX.
    MOVE-CORRESPONDING IT_ZSDYT0051_ALV TO IT_ZSDYT0051_SEL.
    APPEND IT_ZSDYT0051_SEL.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  DATA: TL_TLINES LIKE TLINE OCCURS 0 WITH HEADER LINE,
        WA_LINE   TYPE TXLINE,
        WL_HEADER TYPE THEAD.

  IF OK_CODE EQ OK_SALVAR.

    WL_HEADER-TDOBJECT = 'ZCLIENTE'.
    WL_HEADER-TDID     = 'ZCAR'.
    WL_HEADER-TDSPRAS  = SY-LANGU.
    CONCATENATE SY-MANDT ZDE_ZSDYT0051-KUNNR ZDE_ZSDYT0051-VKORG ZDE_ZSDYT0051-VTWEG ZDE_ZSDYT0051-SPART INTO WL_HEADER-TDNAME.

    "//Obtem novo texto para salvar;
    CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE           = LONGTEXT_TAB
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
        OTHERS          = 3.

    CLEAR: TL_TLINES[].

    LOOP AT LONGTEXT_TAB INTO WA_LINE.
      TL_TLINES-TDLINE = WA_LINE.
      APPEND TL_TLINES.
    ENDLOOP.

    IF ( TL_TLINES[] IS NOT INITIAL ).
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER          = WL_HEADER
          INSERT          = ABAP_TRUE
          SAVEMODE_DIRECT = ABAP_TRUE
        TABLES
          LINES           = TL_TLINES
        EXCEPTIONS
          ID              = 1
          LANGUAGE        = 2
          NAME            = 3
          OBJECT          = 4
          OTHERS          = 5.
    ENDIF.

    MOVE-CORRESPONDING ZDE_ZSDYT0051 TO ZSDYT0051.
    MODIFY ZSDYT0051.
    COMMIT WORK.
    LEAVE TO SCREEN 0.


  ENDIF.
ENDMODULE.
