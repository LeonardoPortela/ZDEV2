*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0106 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_C57       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_C57       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_C57        TYPE LVC_S_LAYO,
      GS_VAR_C57        TYPE DISVARIANT,
      GS_SCROLL_COL_C57 TYPE LVC_S_COL,
      GS_SCROLL_ROW_C57 TYPE LVC_S_ROID,
      IT_CATALOG_C57    TYPE LVC_T_FCAT.

DATA: IT_EXCLUDE_C57 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_C57 LIKE LINE OF IT_EXCLUDE_FCODE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0106 OUTPUT.

  IF CTL_CON_C57 IS INITIAL.

    CREATE OBJECT CTL_CON_C57
      EXPORTING
        CONTAINER_NAME = 'ALV_C57'.

    CREATE OBJECT CTL_ALV_C57
      EXPORTING
        I_PARENT = CTL_CON_C57.

    PERFORM FILL_IT_FIELDCATALOG_C57.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_C57.
    GS_LAY_C57-SEL_MODE   = SPACE.
    GS_LAY_C57-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_C57->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_C57
        IS_VARIANT           = GS_VAR_C57
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_C57
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_C57
        IT_OUTTAB            = IT_CTE_C57.

    "CREATE OBJECT EVENT_HANDLER_N01.
    "SET HANDLER EVENT_HANDLER_N01->HANDLE_HOTSPOT_CLICK
    "        FOR CTL_ALV_N01.
  ELSE.
    CALL METHOD CTL_ALV_C57->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_C57->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_C57
      ES_ROW_NO   = GS_SCROLL_ROW_C57.

ENDMODULE.                 " STATUS_0106  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_C57
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_C57 .

  FIELD-SYMBOLS: <FS_CAT_C57> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_C57'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_C57.

  LOOP AT IT_CATALOG_C57 ASSIGNING <FS_CAT_C57>.
    CASE <FS_CAT_C57>-FIELDNAME.
        "WHEN 'IC_EDITAR'.
        "  <FS_CAT_N01>-KEY     = ABAP_TRUE.
        "  <FS_CAT_N01>-HOTSPOT = ABAP_TRUE.
        "  <FS_CAT_N01>-JUST    = 'C'.
      WHEN 'DOCNUM_CTE'.
        <FS_CAT_C57>-HOTSPOT = ABAP_TRUE.
      WHEN 'C57_VL_SERVICO' OR 'C57_VL_BICMS' OR 'C57_VL_ICMS' OR 'C57_VALR_CREDITO'.
        <FS_CAT_C57>-DO_SUM = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_C57

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_C57
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_C57 .

  GS_VAR_C57-REPORT      = SY-REPID.
  GS_VAR_C57-HANDLE      = '0106'.
  GS_VAR_C57-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_C57-USERNAME    = ABAP_FALSE.
  GS_VAR_C57-VARIANT     = ABAP_FALSE.
  GS_VAR_C57-TEXT        = ABAP_FALSE.
  GS_VAR_C57-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_C57
