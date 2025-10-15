*----------------------------------------------------------------------*
***INCLUDE ZLESR0095_0102.
*----------------------------------------------------------------------*


DATA: CTL_CON_0102       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0102        TYPE LVC_S_LAYO,
      GS_VAR_0102        TYPE DISVARIANT,
      GS_SCROLL_COL_0102 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0102 TYPE LVC_S_ROID,
      IT_CATALOG_0102    TYPE LVC_T_FCAT,
      CTL_ALV_0102       TYPE REF TO CL_GUI_ALV_GRID.

DATA: IT_EXCLUDE_0102 TYPE UI_FUNCTIONS,
      WA_EXCLUDE_0102 LIKE LINE OF IT_EXCLUDE_0102.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.

  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'TL0102' WITH IT_FERRO_SEL-IDVAGAO IT_FERRO_SEL-DCL.

  IF CTL_CON_0102 IS INITIAL.

    CREATE OBJECT CTL_CON_0102
      EXPORTING
        CONTAINER_NAME = 'ALV_FERRO_0102'.

    CREATE OBJECT CTL_ALV_0102
      EXPORTING
        I_PARENT = CTL_CON_0102.

    PERFORM FILL_IT_FIELDCATALOG_0102.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0102.
*   Set layout parameters for ALV grid

    GS_LAY_0102-SEL_MODE   = 'A'.
    GS_LAY_0102-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_0102->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAY_0102
        IS_VARIANT           = GS_VAR_0102
        I_DEFAULT            = SPACE
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_0102
      CHANGING
        IT_FIELDCATALOG      = IT_CATALOG_0102
        IT_OUTTAB            = IT_FERRO_NOTA[].

    CALL METHOD CTL_ALV_0102->REFRESH_TABLE_DISPLAY.

  ELSE.
    CALL METHOD CTL_ALV_0102->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0102->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0102
      ES_ROW_NO   = GS_SCROLL_ROW_0102.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0102 .

  DATA: LC_COL_POS TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT_0102> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_FERROVIARIO_NOTAS_ALV'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0102.

  LC_COL_POS = 1.

  LOOP AT IT_CATALOG_0102 ASSIGNING <FS_CAT_0102>.
    <FS_CAT_0102>-COL_POS = LC_COL_POS.
    <FS_CAT_0102>-TABNAME = 'IT_FERRO_NOTA'.
    ADD 1 TO LC_COL_POS.

    CASE <FS_CAT_0102>-DATATYPE.
      WHEN 'QUAN'.
        <FS_CAT_0102>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0102>-OUTPUTLEN = 15.
      WHEN 'CURR'.
        <FS_CAT_0102>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0102>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0102

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0102 .

  GS_VAR_0102-REPORT      = SY-REPID.
  GS_VAR_0102-HANDLE      = '0102'.
  GS_VAR_0102-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0102-USERNAME    = ABAP_FALSE.
  GS_VAR_0102-VARIANT     = ABAP_FALSE.
  GS_VAR_0102-TEXT        = ABAP_FALSE.
  GS_VAR_0102-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0102
