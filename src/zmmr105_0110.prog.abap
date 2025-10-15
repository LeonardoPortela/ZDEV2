*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0110 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0110       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0110       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0110        TYPE LVC_S_LAYO,
      GS_VAR_0110        TYPE DISVARIANT,
      GS_SCROLL_COL_0110 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0110 TYPE LVC_S_ROID,
      IT_CATALOG_0110    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0110'.

  IF CTL_CON_0110 IS INITIAL.

    CREATE OBJECT CTL_CON_0110
      EXPORTING
        CONTAINER_NAME = 'ALV_0110'.

    CREATE OBJECT CTL_ALV_0110
      EXPORTING
        I_PARENT = CTL_CON_0110.

    PERFORM FILL_IT_FIELDCATALOG_0110.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0110.

    GS_LAY_0110-SEL_MODE   = SPACE.
    GS_LAY_0110-ZEBRA      = ABAP_TRUE.
    GS_LAY_0110-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0110
        IS_VARIANT      = GS_VAR_0110
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0110
        IT_OUTTAB       = IT_CTE_D01[].

  ELSE.
    CALL METHOD CTL_ALV_0110->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0110->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0110
      ES_ROW_NO   = GS_SCROLL_ROW_0110.

ENDMODULE.                 " STATUS_0110  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0110 .

  FIELD-SYMBOLS: <FS_CAT_0110> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_D01'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0110.

  LOOP AT IT_CATALOG_0110 ASSIGNING <FS_CAT_0110>.
    CASE <FS_CAT_0110>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0110>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0110>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0110>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0110>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0110

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0110 .

  GS_VAR_0110-REPORT      = SY-REPID.
  GS_VAR_0110-HANDLE      = '0110'.
  GS_VAR_0110-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0110-USERNAME    = ABAP_FALSE.
  GS_VAR_0110-VARIANT     = ABAP_FALSE.
  GS_VAR_0110-TEXT        = ABAP_FALSE.
  GS_VAR_0110-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0110
