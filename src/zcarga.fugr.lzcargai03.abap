*----------------------------------------------------------------------*
***INCLUDE LZCARGAI03.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER_9003 DEFINITION DEFERRED.

DATA: CTL_CCCONTAINER_9003  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV_9003          TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCATALOG_9003  TYPE LVC_T_FCAT,
      GS_VARIANT_9003       TYPE DISVARIANT,
      GS_LAYOUT_9003        TYPE LVC_S_LAYO,
      GS_SCROLL_COL_9003    TYPE LVC_S_COL,
      GS_SCROLL_ROW_9003    TYPE LVC_S_ROID,
      WA_STABLE_9003        TYPE LVC_S_STBL,
      IT_SELECTED_ROWS_9003 TYPE LVC_T_ROW.

DATA: EVENT_HANDLER_9003 TYPE REF TO LCL_EVENT_RECEIVER_9003.

CLASS LCL_EVENT_RECEIVER_9003 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcl_event_receiver DEFINITION


CLASS LCL_EVENT_RECEIVER_9003 IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK_9003 USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003_EXIT INPUT.
  PERFORM SAIR_9003.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SAIR_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIR_9003 .

  CLEAR: IT_REMESSA[], IT_FIELDCATALOG_9003[].

  IF CTL_ALV_9003 IS NOT INITIAL.
    CTL_ALV_9003->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_9003.

  IF CTL_CCCONTAINER_9003 IS NOT INITIAL.
    CTL_CCCONTAINER_9003->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_9003.

  CLEAR: EVENT_HANDLER_9003.

  LEAVE TO SCREEN 0.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_9003

         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_REMESSA INDEX ROW_ID INTO WA_REMESSA.

  CASE FIELDNAME.
    WHEN 'ICO_REME'.
      PERFORM ESTORNAR_MEMESSA USING WA_REMESSA-VBELN.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_9003

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_MEMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_REMESSA_VBELN  text
*----------------------------------------------------------------------*
FORM ESTORNAR_MEMESSA  USING  P_VBELN TYPE VBELN_VL.

  DATA: SL_HDATA    TYPE BAPIOBDLVHDRCHG,
        SL_HCONT    TYPE BAPIOBDLVHDRCTRLCHG,
        TL_BAPIRET2 TYPE BAPIRET2_T.

  "Deleta Delivery Criado
  SL_HDATA-DELIV_NUMB = P_VBELN.
  SL_HCONT-DELIV_NUMB = P_VBELN.
  SL_HCONT-DLV_DEL    = 'X'.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
    EXPORTING
      HEADER_DATA    = SL_HDATA
      HEADER_CONTROL = SL_HCONT
      DELIVERY       = SL_HDATA-DELIV_NUMB
    TABLES
      RETURN         = TL_BAPIRET2.

  READ TABLE TL_BAPIRET2 WITH KEY TYPE = 'E' INTO DATA(WA_ERRO).

  IF SY-SUBRC IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE ID WA_ERRO-ID TYPE 'S' NUMBER WA_ERRO-NUMBER
       WITH WA_ERRO-MESSAGE_V1
            WA_ERRO-MESSAGE_V2
            WA_ERRO-MESSAGE_V3
            WA_ERRO-MESSAGE_V4
        DISPLAY LIKE 'E'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    PERFORM ATUALIZAR_REMESSA.
    LEAVE TO SCREEN 9003.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO_9003 INPUT.

  IF CTL_ALV_9003 IS NOT INITIAL.
    CALL METHOD CTL_ALV_9003->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_COL_INFO = GS_SCROLL_COL_9003
        ES_ROW_NO   = GS_SCROLL_ROW_9003.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS_9003 INPUT.

  IF CTL_ALV_9003 IS NOT INITIAL.

    CLEAR: IT_SELECTED_ROWS_9003, WA_REMESSA.

    CALL METHOD CTL_ALV_9003->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SELECTED_ROWS_9003.

    LOOP AT IT_SELECTED_ROWS_9003 INTO DATA(WA_SELECTED_ROWS_9003).
      READ TABLE IT_REMESSA INTO WA_REMESSA INDEX WA_SELECTED_ROWS_9003-INDEX.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.

  SET PF-STATUS 'PF9003'.
  "Tipo de Entrada para: Empresa &1 Filial &2 e Safra &3
  SET TITLEBAR 'TL9003' WITH PI_VBELN.

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
    GS_LAYOUT_9003-ZEBRA      = ABAP_TRUE.

    CALL METHOD CTL_ALV_9003->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_9003
        IS_VARIANT      = GS_VARIANT_9003
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_9003
        IT_OUTTAB       = IT_REMESSA[].

    CREATE OBJECT EVENT_HANDLER_9003.
    SET HANDLER EVENT_HANDLER_9003->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_9003.

  ENDIF.

  WA_STABLE_9003-ROW = ABAP_TRUE.
  WA_STABLE_9003-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_9003->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = WA_STABLE_9003
      I_SOFT_REFRESH = ABAP_TRUE.

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

  DATA: WA_FIELDCATALOG_9003 LIKE LINE OF IT_FIELDCATALOG_9003.

  CLEAR: IT_FIELDCATALOG_9003[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_REMESSA_ELIMINAR_PSQ'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG_9003.

  WA_FIELDCATALOG_9003-FIELDNAME = 'ICO_REME'.
  WA_FIELDCATALOG_9003-DATATYPE  = 'CHAR'.
  WA_FIELDCATALOG_9003-INTTYPE   = 'C'.
  WA_FIELDCATALOG_9003-INTLEN    = '000004'.
  WA_FIELDCATALOG_9003-LOWERCASE = 'X'.
  WA_FIELDCATALOG_9003-DOMNAME   = 'CHAR04'.
  WA_FIELDCATALOG_9003-SCRTEXT_L = TEXT-001.
  WA_FIELDCATALOG_9003-SCRTEXT_M = TEXT-001.
  WA_FIELDCATALOG_9003-SCRTEXT_S = TEXT-001.
  WA_FIELDCATALOG_9003-HOTSPOT   = ABAP_TRUE.
  WA_FIELDCATALOG_9003-ICON      = ABAP_TRUE.
  WA_FIELDCATALOG_9003-JUST      = 'C'.
  APPEND WA_FIELDCATALOG_9003 TO IT_FIELDCATALOG_9003.

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
*&      Form  ATUALIZAR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZAR_REMESSA .

  DATA: EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        ERROR_TEXT TYPE STRING.

  CLEAR: IT_REMESSA[], IT_REMESSA, WA_REMESSA.

  TRY.
      EXEC SQL.
        OPEN REMESSAS FOR
          SELECT KP.VBELN, KP.ERNAM, KP.INCO1, KP.ROUTE, LP.MEINS, LP.LFIMG, LP.NTGEW, LP.BRGEW, LP.ARKTX
            FROM SAPHANADB.LIPS LP,
                 SAPHANADB.LIKP KP
           WHERE LP.MANDT = :SY-MANDT
             AND LP.VGBEL = :PI_VBELN
             AND LP.MANDT = KP.MANDT
             AND LP.VBELN = KP.VBELN
             AND EXISTS ( SELECT *
                            FROM SAPHANADB.VBFA VF
                           WHERE VF.MANDT = LP.MANDT
                             AND VF.VBELV = LP.VBELN
                             AND VF.POSNV = LP.POSNR
                             AND VF.VBTYP_V = 'J'
                             AND VF.VBTYP_N = 'h' )
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REMESSAS INTO
      :WA_REMESSA-VBELN,
      :WA_REMESSA-ERNAM,
      :WA_REMESSA-INCO1,
      :WA_REMESSA-ROUTE,
      :WA_REMESSA-MEINS,
      :WA_REMESSA-LFIMG,
      :WA_REMESSA-NTGEW,
      :WA_REMESSA-BRGEW,
      :WA_REMESSA-ARKTX
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WA_REMESSA-ICO_REME = ICON_DELETE.
      APPEND WA_REMESSA TO IT_REMESSA.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE REMESSAS
  ENDEXEC.

ENDFORM.
