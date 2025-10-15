*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0119.
*----------------------------------------------------------------------*

DATA: CTL_ALV_0119       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0119       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0119        TYPE LVC_S_LAYO,
      GS_VAR_0119        TYPE DISVARIANT,
      GS_SCROLL_COL_0119 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0119 TYPE LVC_S_ROID,
      IT_CATALOG_0119    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0119  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0119 OUTPUT.

  IF CTL_CON_0119 IS INITIAL.

    CREATE OBJECT CTL_CON_0119
      EXPORTING
        CONTAINER_NAME = 'ALV_0119'.

    CREATE OBJECT CTL_ALV_0119
      EXPORTING
        I_PARENT = CTL_CON_0119.

    PERFORM FILL_IT_FIELDCATALOG_0119.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0119.
*   Set layout parameters for ALV grid

    GS_LAY_0119-SEL_MODE   = 'A'.
    GS_LAY_0119-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0119->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0119
        IS_VARIANT      = GS_VAR_0119
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0119
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0119
        IT_OUTTAB       = IT_CTE_VEI.

    CALL METHOD CTL_ALV_0119->REFRESH_TABLE_DISPLAY.

  ELSE.
    CALL METHOD CTL_ALV_0119->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0119->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0119
      ES_ROW_NO   = GS_SCROLL_ROW_0119.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0119
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0119 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0119> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_VEI'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0119.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0119 ASSIGNING <FS_CAT_0119>.
    <FS_CAT_0119>-COL_POS = LC_COL_POS.
    <FS_CAT_0119>-TABNAME = 'IT_CTE_VEI'.
    ADD 1 TO LC_COL_POS.
    CASE <FS_CAT_0119>-FIELDNAME.
      WHEN 'CD_CHAVE_CTE'.
        <FS_CAT_0119>-NO_OUT  = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0119

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0119
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0119 .

  GS_VAR_0119-REPORT      = SY-REPID.
  GS_VAR_0119-HANDLE      = '0119'.
  GS_VAR_0119-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0119-USERNAME    = ABAP_FALSE.
  GS_VAR_0119-VARIANT     = ABAP_FALSE.
  GS_VAR_0119-TEXT        = ABAP_FALSE.
  GS_VAR_0119-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0119
