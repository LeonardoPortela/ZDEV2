*&---------------------------------------------------------------------*
*& Report  ZLESR0134
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0134 MESSAGE-ID ZLES.

CLASS LCL_ALV_TOOLBAR DEFINITION DEFERRED.
CLASS LCL_EVENT_HANDLER DEFINITION DEFERRED.

DATA: DG_SPLITTER        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER    TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG    TYPE LVC_T_FCAT,
      GS_VARIANT         TYPE DISVARIANT,
      GS_LAYOUT          TYPE LVC_S_LAYO,
      OBG_TOOLBAR        TYPE REF TO LCL_ALV_TOOLBAR,
      OBJ_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      EVENT_HANDLER      TYPE REF TO LCL_EVENT_HANDLER.

*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    "METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  "METHOD HANDLE_DOUBLE_CLICK.
  "  PERFORM HANDLE_DOUBLE_CLICK USING E_ROW.
  "ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler

CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR  IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR          FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT OBJ_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    DATA: TY_TOOLBAR   TYPE STB_BUTTON.

*    "Separador
    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_EXECUTE_OBJECT.
    TY_TOOLBAR-FUNCTION  = 'CONSULTAR'.
    TY_TOOLBAR-QUICKINFO = TEXT-004.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CALL METHOD OBJ_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'CONSULTAR'.
        PERFORM SELECIONA_LOTES_FRETE.
    ENDCASE.

    CTL_ALV->REFRESH_TABLE_DISPLAY( I_SOFT_REFRESH = ABAP_TRUE ).

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

TABLES: ZLEST0181.

DATA: IT_ZLEST0181 TYPE TABLE OF ZLEST0181,
      OK_CODE      TYPE SY-UCOMM.

SELECTION-SCREEN BEGIN OF BLOCK LOTE01 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P0001 FOR ZLEST0181-ID_LOTE_FRETE,
                P0002 FOR ZLEST0181-BUKRS OBLIGATORY NO-EXTENSION NO INTERVALS MEMORY ID BUK,
                P0003 FOR ZLEST0181-BRANCH,
                P0004 FOR ZLEST0181-DT_REGISTRO,
                P0005 FOR ZLEST0181-HR_REGISTRO,
                P0006 FOR ZLEST0181-US_REGISTRO,
                P0007 FOR ZLEST0181-DT_MODIFICA,
                P0008 FOR ZLEST0181-HR_MODIFICA,
                P0009 FOR ZLEST0181-US_MODIFICA.
SELECTION-SCREEN END OF BLOCK LOTE01.

SELECTION-SCREEN BEGIN OF BLOCK LOTE02 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: P0010 FOR ZLEST0181-VBELN,
                P0011 FOR ZLEST0181-POSNR.
SELECTION-SCREEN END OF BLOCK LOTE02.

SELECTION-SCREEN BEGIN OF BLOCK LOTE03 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: P0012 FOR ZLEST0181-ID_PARCEIRO_REME,
                P0013 FOR ZLEST0181-ID_PARCEIRO_DEST,
                P0014 FOR ZLEST0181-ID_PARCEIRO_COLE,
                P0015 FOR ZLEST0181-ID_PARCEIRO_ENTR,
                P0016 FOR ZLEST0181-ID_PARCEIRO_FRETE.
SELECTION-SCREEN END OF BLOCK LOTE03.

AT SELECTION-SCREEN.                                        "1375894

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'                       "1375894
    ID 'ACTVT' FIELD '03'       "display                    "1375894
    ID 'BUKRS' FIELD P0002-LOW.                             "1375894

  IF SY-SUBRC IS NOT INITIAL.                                   "1375894
    SET CURSOR FIELD 'P0002-LOW'.                           "1375894
    MESSAGE E091(8B) WITH P0002-LOW.                        "1375894
  ENDIF.                                                    "1375894

START-OF-SELECTION.

  PERFORM SELECIONA_LOTES_FRETE.


END-OF-SELECTION.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_LOTES_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_LOTES_FRETE .

  CLEAR: IT_ZLEST0181[], IT_ZLEST0181.

  SELECT * INTO TABLE @IT_ZLEST0181
    FROM ZLEST0181
   WHERE ID_LOTE_FRETE     IN @P0001
     AND BUKRS             IN @P0002
     AND BRANCH            IN @P0003
     AND DT_REGISTRO       IN @P0004
     AND HR_REGISTRO       IN @P0005
     AND US_REGISTRO       IN @P0006
     AND DT_MODIFICA       IN @P0007
     AND HR_MODIFICA       IN @P0008
     AND US_MODIFICA       IN @P0009
     AND VBELN             IN @P0010
     AND POSNR             IN @P0011
     AND ID_PARCEIRO_REME  IN @P0012
     AND ID_PARCEIRO_DEST  IN @P0013
     AND ID_PARCEIRO_COLE  IN @P0014
     AND ID_PARCEIRO_ENTR  IN @P0015
     AND ID_PARCEIRO_FRETE IN @P0016.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.



ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF DG_SPLITTER IS INITIAL.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM FILL_IT_FIELDCATALOG.

    PERFORM FILL_GS_VARIANT.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = CTL_ALV.
*
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR CTL_ALV.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR CTL_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        IS_VARIANT      = GS_VARIANT
        I_SAVE          = 'A'
        "IT_EXCEPT_QINFO = ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_QINFO_ALV( )
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_ZLEST0181[].

    CREATE OBJECT EVENT_HANDLER.
    SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CTL_ALV.
*    SET HANDLER EVENT_HANDLER->HANDLE_DOUBLE_CLICK  FOR CTL_ALV.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZLEST0181'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.
*    IF <FS_CAT>-FIELDNAME(4) = 'ICO_'.
*      <FS_CAT>-ICON    = ABAP_TRUE.
*      <FS_CAT>-JUST    = 'C'.
*    ENDIF.
*
    IF <FS_CAT>-FIELDNAME = 'ID_INTEGRACAO'.
      <FS_CAT>-HOTSPOT = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0001'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

  GS_LAYOUT-SEL_MODE     = 'A'.
  GS_LAYOUT-INFO_FNAME   = 'LINE_COLOR'.
  GS_LAYOUT-STYLEFNAME   = 'STYLE'.
  GS_LAYOUT-CTAB_FNAME   = 'COLOR_CELL'.
  GS_LAYOUT-ZEBRA        = ABAP_FALSE.
  GS_LAYOUT-CWIDTH_OPT   = ABAP_TRUE.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_ZLEST0181 INDEX ROW_ID INTO DATA(WA_0181).

  CASE FIELDNAME.
    WHEN 'ID_INTEGRACAO'.
      CHECK WA_0181-ID_INTEGRACAO IS NOT INITIAL.
      PERFORM CHAMAR_INTEGRACAO USING WA_0181-ID_INTEGRACAO.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_INTEGRACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0185_ID_INTEGRACAO_APROVAR  text
*----------------------------------------------------------------------*
FORM CHAMAR_INTEGRACAO  USING P_ID_INTEGRACAO TYPE ZDE_ID_INTEGRACAO.

  CALL FUNCTION 'ZLES_CARGUERO_0004'
    EXPORTING
      ID_INTEGRACAO = P_ID_INTEGRACAO.

ENDFORM.
