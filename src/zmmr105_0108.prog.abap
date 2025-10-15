*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0108 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0108       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0108       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0108        TYPE LVC_S_LAYO,
      GS_VAR_0108        TYPE DISVARIANT,
      GS_SCROLL_COL_0108 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0108 TYPE LVC_S_ROID,
      IT_CATALOG_0108    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0108 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0108'.

  IF CTL_CON_0108 IS INITIAL.

    CREATE OBJECT CTL_CON_0108
      EXPORTING
        CONTAINER_NAME = 'ALV_0108'.

    CREATE OBJECT CTL_ALV_0108
      EXPORTING
        I_PARENT = CTL_CON_0108.

    PERFORM FILL_IT_FIELDCATALOG_0108.

    PERFORM FILL_GS_VARIANT_0108.

    GS_LAY_0108-SEL_MODE   = SPACE.
    GS_LAY_0108-ZEBRA      = ABAP_TRUE.
    GS_LAY_0108-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0108->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0108
        IS_VARIANT      = GS_VAR_0108
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0108
        IT_OUTTAB       = IT_CTE_VGA[].

  ELSE.
    CALL METHOD CTL_ALV_0108->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0108->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0108
      ES_ROW_NO   = GS_SCROLL_ROW_0108.

ENDMODULE.                 " STATUS_0108  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0108
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0108 .

  GS_VAR_0108-REPORT      = SY-REPID.
  GS_VAR_0108-HANDLE      = '0108'.
  GS_VAR_0108-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0108-USERNAME    = ABAP_FALSE.
  GS_VAR_0108-VARIANT     = ABAP_FALSE.
  GS_VAR_0108-TEXT        = ABAP_FALSE.
  GS_VAR_0108-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0108

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0108
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0108 .

  FIELD-SYMBOLS: <FS_CAT_0108> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_VGA'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0108.

  LOOP AT IT_CATALOG_0108 ASSIGNING <FS_CAT_0108>.
    CASE <FS_CAT_0108>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0108>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0108>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0108>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0108>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0108

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0108  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0108 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0108  INPUT
