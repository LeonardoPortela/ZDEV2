*----------------------------------------------------------------------*
***INCLUDE LZCARGAI02.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER_9002 DEFINITION DEFERRED.

DATA: CTL_CCCONTAINER_9002  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_9002          TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_9002  TYPE LVC_T_FCAT,
      GS_VARIANT_9002       TYPE DISVARIANT,
      GS_LAYOUT_9002        TYPE LVC_S_LAYO,
      GS_SCROLL_COL_9002    TYPE LVC_S_COL,
      GS_SCROLL_ROW_9002    TYPE LVC_S_ROID,
      WA_STABLE_9002        TYPE LVC_S_STBL,
      IT_SELECTED_ROWS_9002 TYPE LVC_T_ROW.

DATA: EVENT_HANDLER_9002 TYPE REF TO LCL_EVENT_RECEIVER_9002.

CLASS LCL_EVENT_RECEIVER_9002 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcl_event_receiver DEFINITION


CLASS LCL_EVENT_RECEIVER_9002 IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK_9002 USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_9002 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click


ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002_EXIT INPUT.
  PERFORM SAIR_9002.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SAIR_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIR_9002 .

  CLEAR: IT_ORDENS[], IT_FIELDCATALOG_9002[].

  IF CTL_ALV_9002 IS NOT INITIAL.
    CTL_ALV_9002->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_9002.

  IF CTL_CCCONTAINER_9002 IS NOT INITIAL.
    CTL_CCCONTAINER_9002->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_9002.

  CLEAR: EVENT_HANDLER_9002.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_9002 INPUT.

  IF CTL_ALV_9002 IS NOT INITIAL.
    CALL METHOD CTL_ALV_9002->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_COL_INFO = GS_SCROLL_COL_9002
        ES_ROW_NO   = GS_SCROLL_ROW_9002.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_9002 INPUT.

  IF CTL_ALV_9002 IS NOT INITIAL.

    CLEAR: IT_SELECTED_ROWS_9002, WA_ORDENS.

    CALL METHOD CTL_ALV_9002->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS_9002.

    LOOP AT IT_SELECTED_ROWS_9002 INTO DATA(WA_SELECTED_ROWS_9002).
      READ TABLE IT_ORDENS INTO WA_ORDENS INDEX WA_SELECTED_ROWS_9002-INDEX.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  IF OK_CODE EQ 'SELECIONAR'.
    CLEAR: OK_CODE.
    IF WA_ORDENS IS INITIAL.
      MESSAGE S167(ZCARGA).
    ELSE.
      CK_SELECIONOU = ABAP_TRUE.
      PERFORM SAIR_9002.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.

  SET PF-STATUS 'PF9001'.
  "Tipo de Entrada para: Empresa &1 Filial &2 e Safra &3
  SET TITLEBAR 'TL9002' WITH PID_BUKRS PID_BRANCH PNR_SAFRA.

  IF CTL_ALV_9002 IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER_9002
      EXPORTING
        CONTAINER_NAME = 'ALV_9002'.

    CREATE OBJECT CTL_ALV_9002
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_9002.

    PERFORM FILL_IT_FIELDCATALOG_9002.

    PERFORM FILL_GS_VARIANT_9002.

    GS_LAYOUT_9002-SEL_MODE   = 'A'.
    GS_LAYOUT_9002-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_9002->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_9002
        IS_VARIANT      = GS_VARIANT_9002
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_9002
        IT_OUTTAB       = IT_ORDENS[].

    CREATE OBJECT EVENT_HANDLER_9002.
    SET HANDLER EVENT_HANDLER_9002->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_9002.
    SET HANDLER EVENT_HANDLER_9002->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_9002.

  ENDIF.

  WA_STABLE_9002-ROW = ABAP_TRUE.
  WA_STABLE_9002-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_9002->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = WA_STABLE_9002
      I_SOFT_REFRESH = ABAP_TRUE.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_9002 .

  DATA: WA_FIELDCATALOG_9002 LIKE LINE OF IT_FIELDCATALOG_9002.

  CLEAR: IT_FIELDCATALOG_9002[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ORDEM_VENDA_PSQ'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_9002.

  WA_FIELDCATALOG_9002-FIELDNAME = 'ICO_ORDEM'.
  WA_FIELDCATALOG_9002-DATATYPE  = 'CHAR'.
  WA_FIELDCATALOG_9002-INTTYPE   = 'C'.
  WA_FIELDCATALOG_9002-INTLEN    = '000004'.
  WA_FIELDCATALOG_9002-LOWERCASE = 'X'.
  WA_FIELDCATALOG_9002-DOMNAME   = 'CHAR04'.
  WA_FIELDCATALOG_9002-SCRTEXT_L = TEXT-005.
  WA_FIELDCATALOG_9002-SCRTEXT_M = TEXT-005.
  WA_FIELDCATALOG_9002-SCRTEXT_S = TEXT-005.
  WA_FIELDCATALOG_9002-HOTSPOT   = ABAP_TRUE.
  WA_FIELDCATALOG_9002-ICON      = ABAP_TRUE.
  WA_FIELDCATALOG_9002-JUST      = 'C'.
  APPEND WA_FIELDCATALOG_9002 TO IT_FIELDCATALOG_9002.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_9002 .
  GS_VARIANT_9002-REPORT      = SY-REPID.
  GS_VARIANT_9002-HANDLE      = '9002'.
  GS_VARIANT_9002-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT_9002-USERNAME    = ABAP_FALSE.
  GS_VARIANT_9002-VARIANT     = ABAP_FALSE.
  GS_VARIANT_9002-TEXT        = ABAP_FALSE.
  GS_VARIANT_9002-DEPENDVARS  = ABAP_FALSE.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK_9002  USING P_ROW TYPE LVC_S_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.

    READ TABLE IT_ORDENS INDEX P_ROW-INDEX INTO WA_ORDENS.
    IF SY-SUBRC IS INITIAL.
      CK_SELECIONOU = ABAP_TRUE.
      PERFORM SAIR_9002.
    ENDIF.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK_9002

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_9002

         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_ORDENS INDEX ROW_ID INTO WA_ORDENS.

  CASE FIELDNAME.
    WHEN 'ICO_ORDEM'.
      PERFORM MOSTRAR_REMESSAS USING WA_ORDENS-VBELN.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_9002

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REMESSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDENS_VBELN  text
*----------------------------------------------------------------------*
FORM MOSTRAR_REMESSAS  USING P_VBELN TYPE VBELN_VA.

  CALL FUNCTION 'Z_PSQ_REMESSA_ERRO_ELIMINAR'
    EXPORTING
      I_VBELN = P_VBELN
    EXCEPTIONS
      ERRO    = 1
      OTHERS  = 2.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
