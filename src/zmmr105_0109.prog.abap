*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0109 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0109       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0109       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0109        TYPE LVC_S_LAYO,
      GS_VAR_0109        TYPE DISVARIANT,
      GS_SCROLL_COL_0109 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0109 TYPE LVC_S_ROID,
      IT_CATALOG_0109    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0109  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0109 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0109'.

  IF CTL_CON_0109 IS INITIAL.

    CREATE OBJECT CTL_CON_0109
      EXPORTING
        CONTAINER_NAME = 'ALV_0109'.

    CREATE OBJECT CTL_ALV_0109
      EXPORTING
        I_PARENT = CTL_CON_0109.

    PERFORM FILL_IT_FIELDCATALOG_0109.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0109.

    GS_LAY_0109-SEL_MODE   = SPACE.
    GS_LAY_0109-ZEBRA      = ABAP_TRUE.
    GS_LAY_0109-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0109->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0109
        IS_VARIANT      = GS_VAR_0109
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0109
        IT_OUTTAB       = IT_CTE_D55[].

  ELSE.
    CALL METHOD CTL_ALV_0109->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0109->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0109
      ES_ROW_NO   = GS_SCROLL_ROW_0109.

ENDMODULE.                 " STATUS_0109  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0109 .

  FIELD-SYMBOLS: <FS_CAT_0109> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_D55'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0109.

  LOOP AT IT_CATALOG_0109 ASSIGNING <FS_CAT_0109>.
    CASE <FS_CAT_0109>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0109>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0109>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0109>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0109>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0109

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0109 .

  GS_VAR_0109-REPORT      = SY-REPID.
  GS_VAR_0109-HANDLE      = '0109'.
  GS_VAR_0109-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0109-USERNAME    = ABAP_FALSE.
  GS_VAR_0109-VARIANT     = ABAP_FALSE.
  GS_VAR_0109-TEXT        = ABAP_FALSE.
  GS_VAR_0109-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0109

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0109  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0109 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0109  INPUT
