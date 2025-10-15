*----------------------------------------------------------------------*
***INCLUDE MZSEGTRANSPORTE1120.
*----------------------------------------------------------------------*

*---------- Definition -----------------------------------------------*
CLASS LCL_EVENT_HANDLER_1120 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS LCL_EVENT_HANDLER_1120 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_1120 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK_1120 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.

DATA: EVENT_HANDLER_1120 TYPE REF TO LCL_EVENT_HANDLER_1120.

DATA: CTL_CON_1120 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_1120 TYPE REF TO CL_GUI_ALV_GRID.

DATA: GS_LAYOUT_1120       TYPE LVC_S_LAYO,
      GS_VARIANT_1120      TYPE DISVARIANT,
      IT_FIELDCATALOG_1120 TYPE LVC_T_FCAT,
      WA_FIELDCATALOG_1120 TYPE LVC_S_FCAT.

DATA: IT_EXCLUDE_FCODE_1120 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE_1120 LIKE LINE OF IT_EXCLUDE_FCODE_1120.

DATA: GS_SCROLL_COL_1120 TYPE LVC_S_COL,
      GS_SCROLL_ROW_1120 TYPE LVC_S_ROID.

DATA: IT_SELECTED_ROWS_1120 TYPE LVC_T_ROW,
      WA_SELECTED_ROWS_1120 TYPE LVC_S_ROW.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1120 OUTPUT.

  IF CTL_CON_1120 IS INITIAL.

    CREATE OBJECT CTL_CON_1120
      EXPORTING
        CONTAINER_NAME = 'ALV_1120'.

    CREATE OBJECT CTL_ALV_1120
      EXPORTING
        I_PARENT = CTL_CON_1120.

    PERFORM FILL_IT_FIELDCATALOG_1120.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_1120.

    "GS_LAYOUT_1120-GRID_TITLE = TEXT-003.
    GS_LAYOUT_1120-SEL_MODE   = 'A'.

    CALL METHOD CTL_ALV_1120->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_1120
        IS_VARIANT           = GS_VARIANT_1120
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_1120
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_1120
        IT_OUTTAB            = IT_ZLEST0115_ALV[].

    CREATE OBJECT EVENT_HANDLER_1120.
    SET HANDLER EVENT_HANDLER_1120->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_1120.
    SET HANDLER EVENT_HANDLER_1120->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_1120.

  ENDIF.

  CALL METHOD CTL_ALV_1120->REFRESH_TABLE_DISPLAY.

  CALL METHOD CTL_ALV_1120->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_1120
      ES_ROW_NO   = GS_SCROLL_ROW_1120.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1120
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_1120 .

  FIELD-SYMBOLS: <FS_CAT_1120> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZLEST0115_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_1120.

  LOOP AT IT_FIELDCATALOG_1120 ASSIGNING <FS_CAT_1120>.
    IF <FS_CAT_1120>-FIELDNAME EQ 'CK_EXCLUIDO'.
      <FS_CAT_1120>-NO_OUT = ABAP_TRUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT_1120 .

  GS_VARIANT_1120-REPORT      = SY-REPID.
  GS_VARIANT_1120-HANDLE      = '1120'.
  GS_VARIANT_1120-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_1120-USERNAME    = ABAP_FALSE.
  GS_VARIANT_1120-VARIANT     = ABAP_FALSE.
  GS_VARIANT_1120-TEXT        = ABAP_FALSE.
  GS_VARIANT_1120-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_1120
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_ZLEST0115_ALV INDEX ROW_ID INTO WA_ZLEST0115_ALV.

  CASE FIELDNAME.
    WHEN 'NR_APOLICE'.
      "Abrir Empresas e Grupos de Mercadoria
      CLEAR: IT_ZLEST0115_SEL[].
      APPEND WA_ZLEST0115_ALV TO IT_ZLEST0115_SEL.
      PERFORM ABRIR_APOLICE.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_1120  USING P_ROW TYPE LVC_S_ROW.

  DATA: LC_ROW TYPE LVC_T_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.

    CALL METHOD CTL_ALV_1120->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_ZLEST0115_ALV INDEX P_ROW-INDEX INTO WA_ZLEST0115_ALV.

    CLEAR: IT_ZLEST0115_SEL[].
    APPEND WA_ZLEST0115_ALV TO IT_ZLEST0115_SEL.

    "Abrir Empresas e Grupos de Mercadoria
    PERFORM ABRIR_APOLICE.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_1120 INPUT.

  CALL METHOD CTL_ALV_1120->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_1120
      ES_ROW_NO   = GS_SCROLL_ROW_1120.

ENDMODULE.                 " GET_SCROLL_INFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_1120 INPUT.
  PERFORM POPULA_SELECAO_APOLICE.
ENDMODULE.                 " GET_SELECTED_ROWS  INPUT

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_APOLICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULA_SELECAO_APOLICE .

  CLEAR IT_SELECTED_ROWS_1120.

  CALL METHOD CTL_ALV_1120->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS_1120.

  CLEAR IT_ZLEST0115_SEL[].

  LOOP AT IT_SELECTED_ROWS_1120 INTO WA_SELECTED_ROWS_1120.
    READ TABLE IT_ZLEST0115_ALV INTO WA_ZLEST0115_ALV INDEX WA_SELECTED_ROWS_1120-INDEX.
    MOVE-CORRESPONDING WA_ZLEST0115_ALV TO IT_ZLEST0115_SEL.
    APPEND IT_ZLEST0115_SEL.
  ENDLOOP.

ENDFORM.
