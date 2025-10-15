*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0107 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0107       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0107       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0107        TYPE LVC_S_LAYO,
      GS_VAR_0107        TYPE DISVARIANT,
      GS_SCROLL_COL_0107 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0107 TYPE LVC_S_ROID,
      IT_CATALOG_0107    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0107 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0107'.

  IF CTL_CON_0107 IS INITIAL.

    CREATE OBJECT CTL_CON_0107
      EXPORTING
        CONTAINER_NAME = 'ALV_0107'.

    CREATE OBJECT CTL_ALV_0107
      EXPORTING
        I_PARENT = CTL_CON_0107.

    PERFORM FILL_IT_FIELDCATALOG_0107.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0107.

    GS_LAY_0107-SEL_MODE   = SPACE.
    GS_LAY_0107-ZEBRA      = ABAP_TRUE.
    GS_LAY_0107-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0107->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0107
        IS_VARIANT      = GS_VAR_0107
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0107
        IT_OUTTAB       = IT_CTE_CVL[].

  ELSE.
    CALL METHOD CTL_ALV_0107->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0107->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0107
      ES_ROW_NO   = GS_SCROLL_ROW_0107.

ENDMODULE.                 " STATUS_0107  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0107 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0107  INPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0107 .

  FIELD-SYMBOLS: <FS_CAT_0107> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_CVL'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0107.

  LOOP AT IT_CATALOG_0107 ASSIGNING <FS_CAT_0107>.
    CASE <FS_CAT_0107>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0107>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0107>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0107>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0107>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0107

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0107 .

  GS_VAR_0107-REPORT      = SY-REPID.
  GS_VAR_0107-HANDLE      = '0107'.
  GS_VAR_0107-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0107-USERNAME    = ABAP_FALSE.
  GS_VAR_0107-VARIANT     = ABAP_FALSE.
  GS_VAR_0107-TEXT        = ABAP_FALSE.
  GS_VAR_0107-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0107
