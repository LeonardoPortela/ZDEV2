*----------------------------------------------------------------------*
***INCLUDE LZMMG_PEDIDO_COMPRAI01.
*----------------------------------------------------------------------*

*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
*    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: EVENT_HANDLER      TYPE REF TO LCL_EVENT_HANDLER.

DATA: CTL_CCCONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV         TYPE REF TO CL_GUI_ALV_GRID,
      GS_VARIANT      TYPE DISVARIANT,
      IT_FIELDCATALOG TYPE LVC_T_FCAT,
      GS_LAYOUT       TYPE LVC_S_LAYO.

DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

DATA: GS_SCROLL_COL TYPE LVC_S_COL,
      GS_SCROLL_ROW TYPE LVC_S_ROID.

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HANDLE_HOTSPOT_CLICK USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
*  ENDMETHOD.                    "handle_hotspot_click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK USING E_ROW E_COLUMN ES_ROW_NO.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE USER_COMMAND_9001 INPUT.
*  CASE OK_CODE.
*    WHEN OK_SELECIONAR.
*      CLEAR: OK_CODE.
*  ENDCASE.
*ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  DATA: IT_FILTER TYPE LVC_T_FILT,
        WA_FILTER TYPE LVC_S_FILT.

  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.

  IF CTL_CCCONTAINER IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER
      EXPORTING
        CONTAINER_NAME = 'ALV_PEDIDOS'.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM FILL_IT_FIELDCATALOG.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

    GS_LAYOUT-GRID_TITLE = TEXT-100.
    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-INFO_FNAME = 'ROWCOLOR'.
    GS_LAYOUT-ZEBRA      = ABAP_TRUE.

    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    WA_FILTER-SIGN      = 'I'. "c_i.
    WA_FILTER-OPTION    = 'GT'. "c_ne.
    WA_FILTER-FIELDNAME = 'MENGE_SALDO'."c_dmbtr.
    WA_FILTER-LOW       = '0'.
    APPEND WA_FILTER TO IT_FILTER.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FILTER            = IT_FILTER
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_PEDIDOS_ITENS[].

    CREATE OBJECT EVENT_HANDLER.
    "SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_NFE.
    SET HANDLER EVENT_HANDLER->HANDLE_DOUBLE_CLICK  FOR CTL_ALV.
  ENDIF.

  CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_EKPO_HELP_SALDO'
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
  GS_VARIANT-HANDLE      = '9001'.
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

*  CLEAR IT_SELECTED.
*
*  CALL METHOD CTL_ALV->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = IT_SELECTED.
*
*  CLEAR: IT_SELECTED[], IT_SEL[].
*
*  LOOP AT IT_SELECTED INTO WA_SELECTED.
*    READ TABLE IT_ INTO WA_ INDEX WA_SELECTED-INDEX.
*    APPEND WA_ TO IT_SEL.
* ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING E_ROW     TYPE  LVC_S_ROW
                                E_COLUMN  TYPE  LVC_S_COL
                                ES_ROW_NO TYPE  LVC_S_ROID.

  DATA: LC_ROW TYPE LVC_T_ROW.

  IF E_ROW-ROWTYPE IS INITIAL.

    APPEND E_ROW TO LC_ROW.

    CALL METHOD CTL_ALV->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_PEDIDOS_ITENS INDEX E_ROW-INDEX INTO DATA(WA_PEDIDOS_ITENS).

    PERFORM SELECIONA_PEDIDO USING WA_PEDIDOS_ITENS.
    IF SY-SUBRC IS INITIAL.
      CK_SELECIONOU_PEDIDO = ABAP_TRUE.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PEDIDOS_ITENS  text
*----------------------------------------------------------------------*
FORM SELECIONA_PEDIDO  USING I_PEDIDO TYPE ZDE_EKPO_HELP_SALDO.

  IF I_PEDIDO-MENGE_SALDO GE GB_MENGE_SALDO.
    SELECT SINGLE * INTO GB_EKPO
      FROM EKPO
     WHERE EBELN EQ I_PEDIDO-EBELN
       AND EBELP EQ I_PEDIDO-EBELP.

    GB_SALDO_ITEM = I_PEDIDO.
    EXIT.
  ELSE.
    MESSAGE S002 WITH I_PEDIDO-EBELN I_PEDIDO-EBELP.
    SY-SUBRC = 1.
  ENDIF.

ENDFORM.
