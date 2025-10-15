*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0111 .
*----------------------------------------------------------------------*

DATA: CTL_ALV_0111       TYPE REF TO CL_GUI_ALV_GRID,
      CTL_CON_0111       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAY_0111        TYPE LVC_S_LAYO,
      GS_VAR_0111        TYPE DISVARIANT,
      GS_SCROLL_COL_0111 TYPE LVC_S_COL,
      GS_SCROLL_ROW_0111 TYPE LVC_S_ROID,
      IT_CATALOG_0111    TYPE LVC_T_FCAT.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0111 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0111  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0111  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0111 OUTPUT.

  SET PF-STATUS 'PFMODAL2'.
  SET TITLEBAR 'TL0111'.

  IF CTL_CON_0111 IS INITIAL.

    CREATE OBJECT CTL_CON_0111
      EXPORTING
        CONTAINER_NAME = 'ALV_0111'.

    CREATE OBJECT CTL_ALV_0111
      EXPORTING
        I_PARENT = CTL_CON_0111.

    PERFORM FILL_IT_FIELDCATALOG_0111.
*   Fill info for layout variant

    PERFORM FILL_GS_VARIANT_0111.

    GS_LAY_0111-SEL_MODE   = SPACE.
    GS_LAY_0111-ZEBRA      = ABAP_TRUE.
    GS_LAY_0111-NO_TOOLBAR = ABAP_TRUE.

    CALL METHOD CTL_ALV_0111->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAY_0111
        IS_VARIANT      = GS_VAR_0111
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_CATALOG_0111
        IT_OUTTAB       = IT_CTE_DUP[].

  ELSE.
    CALL METHOD CTL_ALV_0111->REFRESH_TABLE_DISPLAY.
  ENDIF.

  CALL METHOD CTL_ALV_0111->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL_0111
      ES_ROW_NO   = GS_SCROLL_ROW_0111.

ENDMODULE.                 " STATUS_0111  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0111 .

  FIELD-SYMBOLS: <FS_CAT_0111> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZIB_CTE_DIST_DUP'
    CHANGING
      CT_FIELDCAT      = IT_CATALOG_0111.

  LOOP AT IT_CATALOG_0111 ASSIGNING <FS_CAT_0111>.
    CASE <FS_CAT_0111>-FIELDNAME.
      WHEN 'MANDT' OR 'CD_CHAVE_CTE'.
        <FS_CAT_0111>-NO_OUT = ABAP_TRUE.
    ENDCASE.
    CASE <FS_CAT_0111>-DATATYPE.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <FS_CAT_0111>-DO_SUM    = ABAP_TRUE.
        <FS_CAT_0111>-OUTPUTLEN = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0111

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0111 .

  GS_VAR_0111-REPORT      = SY-REPID.
  GS_VAR_0111-HANDLE      = '0111'.
  GS_VAR_0111-LOG_GROUP   = ABAP_FALSE.
  GS_VAR_0111-USERNAME    = ABAP_FALSE.
  GS_VAR_0111-VARIANT     = ABAP_FALSE.
  GS_VAR_0111-TEXT        = ABAP_FALSE.
  GS_VAR_0111-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT_0111
