*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9003.
*----------------------------------------------------------------------*

DATA: CTL_CCCONTAINER_9003 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_9003         TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_9003 TYPE LVC_T_FCAT,
      GS_VARIANT_9003      TYPE DISVARIANT,
      GS_LAYOUT_9003       TYPE LVC_S_LAYO.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9003'.

  IF CTL_ALV_9003 IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER_9003
      EXPORTING
        CONTAINER_NAME = 'ALV_9003'.

    CREATE OBJECT CTL_ALV_9003
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_9003.

    PERFORM FILL_IT_FIELDCATALOG_9003.

    PERFORM FILL_GS_VARIANT_9003.

    GS_LAYOUT_9003-SEL_MODE   = 'A'.
    GS_LAYOUT_9003-ZEBRA      = ABAP_FALSE.
    GS_LAYOUT_9003-CWIDTH_OPT = ABAP_TRUE.

    CALL METHOD CTL_ALV_9003->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_9003
        IS_VARIANT      = GS_VARIANT_9003
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_9003
        IT_OUTTAB       = IT_MENSAGENS[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003_EXIT INPUT.
  PERFORM LIMPA_TELA_9003.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_9003 .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG_9003[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_OB_MENSAGEM_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_9003.

  LOOP AT IT_FIELDCATALOG_9003 ASSIGNING <FS_CAT>.
    <FS_CAT>-TABNAME = 'ZDE_OB_MENSAGEM_ALV'.

    CASE <FS_CAT>-FIELDNAME.
      WHEN 'ST_MENSAGEM'.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 1.
    ENDCASE.
*
    IF <FS_CAT>-FIELDNAME <> 'ST_MENSAGEM'.
      <FS_CAT>-COL_POS = LC_COL_POS.
      ADD 1 TO LC_COL_POS.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_9003 .

  GS_VARIANT_9003-REPORT      = SY-REPID.
  GS_VARIANT_9003-HANDLE      = '9003'.
  GS_VARIANT_9003-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_9003-USERNAME    = ABAP_FALSE.
  GS_VARIANT_9003-VARIANT     = ABAP_FALSE.
  GS_VARIANT_9003-TEXT        = ABAP_FALSE.
  GS_VARIANT_9003-DEPENDVARS  = ABAP_FALSE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_TELA_9003 .

  IF CTL_ALV_9003 IS NOT INITIAL.
    CTL_ALV_9003->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_9003.

  IF CTL_CCCONTAINER_9003 IS NOT INITIAL.
    CTL_CCCONTAINER_9003->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_9003.

ENDFORM.
