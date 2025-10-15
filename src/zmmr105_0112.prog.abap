*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0112 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0112       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0112       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0112        TYPE LVC_S_LAYO,
      GS_VAR_0112        TYPE DISVARIANT,
      GS_SCROLL_COL_0112 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0112 TYPE LVC_S_ROID,
      IT_CATALOG_0112    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0112 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0112  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0112  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0112 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0112'.

  IF CTL_CON_0112 IS INITIAL.

    CREATE OBJECT CTL_CON_0112
      EXPORTING
        CONTAINER_NAME = 'ALV_0112'.

    CREATE OBJECT CTL_ALV_0112
      EXPORTING
        I_PARENT = CTL_CON_0112.

    PERFORM FILL_IT_FIELDCATALOG_0112.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0112.

    GS_LAY_0112-SEL_MODE   = SPACE.
    GS_LAY_0112-ZEBRA      = ABAP_TRUE.
    GS_LAY_0112-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0112->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0112
        IS_VARIANT      = GS_VAR_0112
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0112
        IT_OUTTAB       = IT_CTE_CPL[].

  ELSE.
    CALL METHOD CTL_ALV_0112->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0112->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0112
      ES_ROW_NO   = GS_SCROLL_ROW_0112.

ENDMODULE.                 " STATUS_0112  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0112
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0112 .

  FIELD-SYMBOLS: <FS_CAT_0112> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_CPL'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0112.

  LOOP AT IT_CATALOG_0112 ASSIGNING <FS_CAT_0112>.
    CASE <FS_CAT_0112>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0112>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0112>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0112>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0112>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0112

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0112
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0112 .

  GS_VAR_0112-REPORT      = SY-REPID.
  GS_VAR_0112-HANDLE      = '0112'.
  GS_VAR_0112-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0112-USERNAME    = ABAP_FALSE.
  GS_VAR_0112-VARIANT     = ABAP_FALSE.
  GS_VAR_0112-TEXT        = ABAP_FALSE.
  GS_VAR_0112-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0112
