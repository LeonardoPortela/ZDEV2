*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0120.
*----------------------------------------------------------------------*

DATA: CTL_ALV_0120       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0120       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0120        TYPE LVC_S_LAYO,
      GS_VAR_0120        TYPE DISVARIANT,
      GS_SCROLL_COL_0120 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0120 TYPE LVC_S_ROID,
      IT_CATALOG_0120    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0120 OUTPUT.

  IF CTL_CON_0120 IS INITIAL.

    CREATE OBJECT CTL_CON_0120
      EXPORTING
        CONTAINER_NAME = 'ALV_0120'.

    CREATE OBJECT CTL_ALV_0120
      EXPORTING
        I_PARENT = CTL_CON_0120.

*    CREATE OBJECT OBG_TOOLBAR_0120
*      EXPORTING
*        IO_ALV_GRID = CTL_ALV_0120.
*
*    SET HANDLER OBG_TOOLBAR_0120->ON_TOOLBAR FOR CTL_ALV_0120.
*    SET HANDLER OBG_TOOLBAR_0120->HANDLE_USER_COMMAND FOR CTL_ALV_0120.

    PERFORM FILL_IT_FIELDCATALOG_0120.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0120.
*   Set layout parameters for ALV grid

    GS_LAY_0120-SEL_MODE   = 'A'.
    GS_LAY_0120-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0120->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0120
        IS_VARIANT      = GS_VAR_0120
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0120
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0120
        IT_OUTTAB       = IT_CTE_MOT.

    CALL METHOD CTL_ALV_0120->REFRESH_TABLE_DISPLAY.

*    CREATE OBJECT EVENT_HANDLER_0120.
*    SET HANDLER EVENT_HANDLER_0120->HANDLE_HOTSPOT_CLICK
*            FOR CTL_ALV_0120.

  ELSE.
    CALL METHOD CTL_ALV_0120->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0120->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0120
      ES_ROW_NO   = GS_SCROLL_ROW_0120.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0120
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0120 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0120> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_MOT'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0120.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0120 ASSIGNING <FS_CAT_0120>.
    <FS_CAT_0120>-COL_POS = LC_COL_POS.
    <FS_CAT_0120>-TABNAME = 'IT_CTE_MOT'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_0120>-FIELDNAME.
      WHEN 'CD_CHAVE_CTE'.
        <FS_CAT_0120>-NO_OUT  = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0120

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0120
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0120 .

  GS_VAR_0120-REPORT      = SY-REPID.
  GS_VAR_0120-HANDLE      = '0120'.
  GS_VAR_0120-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0120-USERNAME    = ABAP_FALSE.
  GS_VAR_0120-VARIANT     = ABAP_FALSE.
  GS_VAR_0120-TEXT        = ABAP_FALSE.
  GS_VAR_0120-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0120
