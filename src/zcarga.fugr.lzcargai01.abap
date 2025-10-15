*----------------------------------------------------------------------*
***INCLUDE LZCARGAI01.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER_9001 DEFINITION DEFERRED.

DATA: CTL_CCCONTAINER_9001  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_9001          TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_9001  TYPE LVC_T_FCAT,
      GS_VARIANT_9001       TYPE DISVARIANT,
      GS_LAYOUT_9001        TYPE LVC_S_LAYO,
      GS_SCROLL_COL_9001    TYPE LVC_S_COL,
      GS_SCROLL_ROW_9001    TYPE LVC_S_ROID,
      WA_STABLE_9001        TYPE LVC_S_STBL,
      IT_SELECTED_ROWS_9001 TYPE LVC_T_ROW.

DATA: EVENT_HANDLER_9001 TYPE REF TO LCL_EVENT_RECEIVER_9001.

CLASS LCL_EVENT_RECEIVER_9001 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_receiver DEFINITION


CLASS LCL_EVENT_RECEIVER_9001 IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.
  PERFORM SAIR_9001.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  IF OK_CODE EQ 'SELECIONAR'.
    CLEAR: OK_CODE.
    IF WA_TIPO_ENTRADA IS INITIAL.
      MESSAGE S147(ZCARGA).
    ELSE.
      CK_SELECIONOU = ABAP_TRUE.
      PERFORM SAIR_9001.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  SET PF-STATUS 'PF9001'.
  "Tipo de Entrada para: Empresa &1 Filial &2 e Safra &3
  SET TITLEBAR 'TL9001' WITH PID_BUKRS PID_BRANCH PNR_SAFRA.

  IF CTL_ALV_9001 IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER_9001
      EXPORTING
        CONTAINER_NAME = 'ALV_9001'.

    CREATE OBJECT CTL_ALV_9001
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_9001.

    PERFORM FILL_IT_FIELDCATALOG_9001.

    PERFORM FILL_GS_VARIANT_9001.

    GS_LAYOUT_9001-SEL_MODE   = 'A'.
    GS_LAYOUT_9001-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_9001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_9001
        IS_VARIANT      = GS_VARIANT_9001
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_9001
        IT_OUTTAB       = IT_TIPO_ENTRADA[].

    CREATE OBJECT EVENT_HANDLER_9001.
    SET HANDLER EVENT_HANDLER_9001->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_9001.

  ENDIF.

  WA_STABLE_9001-ROW = ABAP_TRUE.
  WA_STABLE_9001-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_9001->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = WA_STABLE_9001
      I_SOFT_REFRESH = ABAP_TRUE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING P_ROW TYPE LVC_S_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.

    READ TABLE IT_TIPO_ENTRADA INDEX P_ROW-INDEX INTO WA_TIPO_ENTRADA.
    IF SY-SUBRC IS INITIAL.
      CK_SELECIONOU = ABAP_TRUE.
      PERFORM SAIR_9001.
    ENDIF.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_9001 .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG_9001[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZSDT0001TETX'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_9001.


  LOOP AT IT_FIELDCATALOG_9001 ASSIGNING FIELD-SYMBOL(<FS_9001>).
    CASE <FS_9001>-FIELDNAME.
      WHEN 'ID_ENTRADA'.
        <FS_9001>-OUTPUTLEN = 10.
      WHEN 'DS_ENTRADA'.
        <FS_9001>-OUTPUTLEN = 60.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_9001 .
  GS_VARIANT_9001-REPORT      = SY-REPID.
  GS_VARIANT_9001-HANDLE      = '9001'.
  GS_VARIANT_9001-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_9001-USERNAME    = ABAP_FALSE.
  GS_VARIANT_9001-VARIANT     = ABAP_FALSE.
  GS_VARIANT_9001-TEXT        = ABAP_FALSE.
  GS_VARIANT_9001-DEPENDVARS  = ABAP_FALSE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAIR_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIR_9001 .
  CLEAR: IT_TIPO_ENTRADA[], IT_FIELDCATALOG_9001[].

  IF CTL_ALV_9001 IS NOT INITIAL.
    CTL_ALV_9001->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_9001.

  IF CTL_CCCONTAINER_9001 IS NOT INITIAL.
    CTL_CCCONTAINER_9001->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_9001.

  CLEAR: EVENT_HANDLER_9001.

  LEAVE TO SCREEN 0.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_9001 INPUT.

  IF CTL_ALV_9001 IS NOT INITIAL.
    CALL METHOD CTL_ALV_9001->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_COL_INFO = GS_SCROLL_COL_9001
        ES_ROW_NO   = GS_SCROLL_ROW_9001.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_9001 INPUT.

  IF CTL_ALV_9001 IS NOT INITIAL.

    CLEAR: IT_SELECTED_ROWS_9001, WA_TIPO_ENTRADA.

    CALL METHOD CTL_ALV_9001->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS_9001.

    LOOP AT IT_SELECTED_ROWS_9001 INTO DATA(WA_SELECTED_ROWS).
      READ TABLE IT_TIPO_ENTRADA INTO WA_TIPO_ENTRADA INDEX WA_SELECTED_ROWS-INDEX.
    ENDLOOP.
  ENDIF.

ENDMODULE.
