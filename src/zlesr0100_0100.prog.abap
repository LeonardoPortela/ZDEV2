*&---------------------------------------------------------------------*
*&  Include           ZLESR0100_0100
*&---------------------------------------------------------------------*

CONSTANTS: OK_EDITAR TYPE SY-UCOMM VALUE 'EDITAR',
           OK_SALVAR TYPE SY-UCOMM VALUE 'SALVAR'.
DATA:  IT_SELECT           TYPE STANDARD TABLE OF ZLEST0032,
       G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       DG_SPLITTER_1       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       DG_PARENT_1         TYPE REF TO CL_GUI_CONTAINER,
       DG_SPLITTER_2       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       DG_PARENT_2         TYPE REF TO CL_GUI_CONTAINER,
       DG_PARENT_2A        TYPE REF TO CL_GUI_CONTAINER,
       DG_PARENT_ALV       TYPE REF TO CL_GUI_CONTAINER,
       PICTURE             TYPE REF TO CL_GUI_PICTURE,
       CTL_ALV             TYPE REF TO CL_GUI_ALV_GRID,
       DG_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
       TABLE_ELEMENT       TYPE REF TO CL_DD_TABLE_ELEMENT,
       COLUMN              TYPE REF TO CL_DD_AREA,
       TABLE_ELEMENT2      TYPE REF TO CL_DD_TABLE_ELEMENT,
       COLUMN_1            TYPE REF TO CL_DD_AREA,
       COLUMN_2            TYPE REF TO CL_DD_AREA,
       DG_HTML_CNTRL       TYPE REF TO CL_GUI_HTML_VIEWER,
       IT_EXCLUDE_FCODE    TYPE UI_FUNCTIONS,
       WA_EXCLUDE_FCODE    LIKE LINE OF IT_EXCLUDE_FCODE,
       CK_GRAVOU           TYPE C LENGTH 1,
       CK_PRIMEIRA_ENTRADA TYPE C LENGTH 1,
       GS_LAYOUT           TYPE LVC_S_LAYO,
       GS_VARIANT          TYPE DISVARIANT,
       IT_FIELDCATALOG     TYPE LVC_T_FCAT,
       WA_FIELDCATALOG     TYPE LVC_S_FCAT,
       GS_SCROLL_COL       TYPE LVC_S_COL,
       GS_SCROLL_ROW       TYPE LVC_S_ROID,
       GS_STABLE           TYPE LVC_S_STBL,
       IT_SELECTED_ROWS    TYPE LVC_T_ROW,
       WA_SELECTED_ROWS    TYPE LVC_S_ROW.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN OK_EDITAR.
      PERFORM EDITAR_BASE.

      EDIT = ABAP_TRUE.
      CLEAR: OK_CODE.

    WHEN 'SAVE'.
      PERFORM SAVE.

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  CLEAR OK_CODE.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: URL(255)                TYPE C,
        P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
        SDYDO_TEXT_ELEMENT(255),
        P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF G_CUSTOM_CONTAINER IS INITIAL.

* create a container for the tree control
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 16.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 40.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = DG_PARENT_2A.

    PERFORM F_PEGA_IMAGEM USING 'LOGO_NOVO' CHANGING URL.

    CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
      EXPORTING
        URL = URL.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_FIT_CENTER.

    PERFORM FILL_IT_FIELDCATALOG.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-ZEBRA      = 'X'.
    GS_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.

    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    PERFORM EXCLUDE.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'X'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_0032.

    CALL METHOD CTL_ALV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD CTL_ALV->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.


    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

    CALL METHOD DG_DYNDOC_ID->ADD_TABLE
      EXPORTING
        NO_OF_COLUMNS = 1
        BORDER        = '0'
        WIDTH         = '100%'
      IMPORTING
        TABLE         = TABLE_ELEMENT.

    CALL METHOD TABLE_ELEMENT->ADD_COLUMN
      IMPORTING
        COLUMN = COLUMN.

    CALL METHOD TABLE_ELEMENT->SET_COLUMN_STYLE
      EXPORTING
        COL_NO    = 1
        SAP_ALIGN = 'CENTER'
        SAP_STYLE = CL_DD_DOCUMENT=>HEADING.

    P_TEXT = TEXT-004.

    CALL METHOD COLUMN->ADD_TEXT
      EXPORTING
        TEXT      = P_TEXT
        SAP_STYLE = 'HEADING'.

    CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT.

    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_2.

    DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.

    CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = DG_PARENT_2
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.

  ELSE.

    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STABLE.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                    CHANGING URL.

  DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
          LINE(255) TYPE X,
        END OF GRAPHIC_TABLE.

  DATA: L_GRAPHIC_XSTR TYPE XSTRING.
  DATA: GRAPHIC_SIZE   TYPE I.
  DATA: L_GRAPHIC_CONV TYPE I.
  DATA: L_GRAPHIC_OFFS TYPE I.

  REFRESH GRAPHIC_TABLE.

  CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
    EXPORTING
      P_OBJECT = 'GRAPHICS'
      P_NAME   = NOME_LOGO
      P_ID     = 'BMAP'
      P_BTYPE  = 'BCOL'
    RECEIVING
      P_BMP    = L_GRAPHIC_XSTR.

  GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
  L_GRAPHIC_CONV = GRAPHIC_SIZE.
  L_GRAPHIC_OFFS = 0.

  WHILE L_GRAPHIC_CONV > 255.

    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
    APPEND GRAPHIC_TABLE.
    L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
    L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.

  ENDWHILE.

  GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
  APPEND GRAPHIC_TABLE.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE     = 'IMAGE'
      SUBTYPE  = 'X-UNKNOWN'
      SIZE     = GRAPHIC_SIZE
      LIFETIME = 'T'
    TABLES
      DATA     = GRAPHIC_TABLE
    CHANGING
      URL      = URL.

ENDFORM.                    " F_PEGA_IMAGEM


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZLEST0032'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO INPUT.

  CALL METHOD CTL_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS INPUT.

  CLEAR IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_SELECT.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZLEST0032 INTO WA_ZLEST0032 INDEX WA_SELECTED_ROWS-INDEX.
    IF SY-SUBRC IS INITIAL.
      APPEND WA_ZLEST0032 TO IT_SELECT.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_BASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EDITAR_BASE .

  FREE: IT_FIELDCATALOG, IT_SELECTED_ROWS.

  CALL METHOD CTL_ALV->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = IT_FIELDCATALOG.

  CHECK NOT IT_FIELDCATALOG IS INITIAL.

  CALL METHOD CTL_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CHECK NOT IT_SELECTED_ROWS IS INITIAL.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_0032 ASSIGNING FIELD-SYMBOL(<WA_0032>) INDEX WA_SELECTED_ROWS-INDEX.

    IF SY-SUBRC IS INITIAL.

      FREE: <WA_0032>-FIELD_STYLE, LS_EDIT, LT_EDIT.

      LOOP AT IT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FIELD>).

        CASE <FIELD>-FIELDNAME.
          WHEN 'MANDT' OR 'TKNUM' OR 'FKNUM' OR 'EBELN' OR 'EBELP' OR 'LBLNI'.
            ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            <FIELD>-EDIT = ABAP_FALSE.
          WHEN OTHERS.
            ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            <FIELD>-EDIT = ABAP_TRUE.
        ENDCASE.

        FREE:  LS_EDIT, LT_EDIT.

        LS_EDIT-FIELDNAME = <FIELD>-FIELDNAME.
        LS_EDIT-STYLE = ROW_EDIT-STYLE.
        LS_EDIT-STYLE2 = SPACE.
        LS_EDIT-STYLE3 = SPACE.
        LS_EDIT-STYLE4 = SPACE.
        LS_EDIT-MAXLEN = 8.

        INSERT LS_EDIT INTO TABLE LT_EDIT.

        INSERT LINES OF LT_EDIT INTO TABLE <WA_0032>-FIELD_STYLE.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE .

*   Excluir Buttons Toolbar
  FREE: IT_EXCLUDE_FCODE.

  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
  WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE.

  CHECK NOT EDIT IS INITIAL.

  FREE IT_ZLEST0032.

  LOOP AT IT_0032 INTO WA_0032 WHERE NOT FIELD_STYLE IS INITIAL.
    MOVE-CORRESPONDING WA_0032 TO WA_ZLEST0032.
    APPEND WA_ZLEST0032 TO IT_ZLEST0032.
  ENDLOOP.

  MODIFY ZLEST0032 FROM TABLE IT_ZLEST0032.

  LOOP AT IT_FIELDCATALOG ASSIGNING FIELD-SYMBOL(<FIELD>).
    <FIELD>-EDIT = ABAP_FALSE.
  ENDLOOP.

  PERFORM: LIMPAR_TABELAS,
           SELECIONAR_REGISTROS.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.

  CLEAR EDIT.

ENDFORM.
