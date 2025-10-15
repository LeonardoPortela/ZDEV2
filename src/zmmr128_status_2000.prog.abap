*----------------------------------------------------------------------*
***INCLUDE ZMMR128_STATUS_2000.
*----------------------------------------------------------------------*

CLASS LCL_EVENT_HANDLER DEFINITION DEFERRED.

TYPES: BEGIN OF TY_ITENS_ALV.
         INCLUDE STRUCTURE ZDE_ZSDT0001ACG_ALV.
         TYPES:   LINE_COLOR(4) TYPE C, "Used to store row color attributes
         COLOR_CELL    TYPE LVC_T_SCOL,  " Cell color
         STYLE         TYPE LVC_T_STYL,
         ICO_CARGA     TYPE CHAR04,
         ICO_FILIAL    TYPE CHAR04,
         ICO_FISCAL    TYPE CHAR04,
         ICO_COMERCIAL TYPE CHAR04,
       END OF TY_ITENS_ALV.

DATA: DG_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER  TYPE REF TO CL_GUI_CONTAINER,
      CTL_ALV          TYPE REF TO CL_GUI_ALV_GRID,
      GS_VARIANT       TYPE DISVARIANT,
      GS_LAYOUT        TYPE LVC_S_LAYO,
      IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      IT_FIELDCATALOG  TYPE LVC_T_FCAT,
      IT_EXCEPT_QINFO  TYPE LVC_T_QINF,
      IT_RETORNO_ALV   TYPE TABLE OF TY_ITENS_ALV WITH HEADER LINE,
      EVENT_HANDLER    TYPE REF TO LCL_EVENT_HANDLER.

CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_COLUMN_ID ES_ROW_NO.
    METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING ES_ROW_NO-ROW_ID E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK USING E_ROW.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.

  SET PF-STATUS 'PF2000'.
  SET TITLEBAR 'TL2000'.

  IF IT_RETORNO_ALV[] IS INITIAL.
    PERFORM CARREGA_SAIDA.
  ENDIF.

  IF DG_SPLITTER IS INITIAL.

    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 1.

    CTL_CCCONTAINER = DG_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    PERFORM FILL_IT_FIELDCATALOG.

    "Hints
    PERFORM FILL_IT_HINTS.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
    GS_LAYOUT-STYLEFNAME = 'STYLE'.
    GS_LAYOUT-CTAB_FNAME = 'COLOR_CELL'.
    GS_LAYOUT-ZEBRA      = ABAP_FALSE.

    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
        IT_EXCEPT_QINFO      = IT_EXCEPT_QINFO
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_RETORNO_ALV[].

    CREATE OBJECT EVENT_HANDLER.
    SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CTL_ALV.
    SET HANDLER EVENT_HANDLER->HANDLE_DOUBLE_CLICK  FOR CTL_ALV.

  ENDIF.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS      TYPE LVC_COLPOS,
        WA_FIELDCATALOG LIKE LINE OF IT_FIELDCATALOG.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_ZSDT0001ACG_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  WA_FIELDCATALOG-FIELDNAME = 'ICO_CARGA'.
  WA_FIELDCATALOG-DATATYPE  = 'CHAR'.
  WA_FIELDCATALOG-INTTYPE   = 'C'.
  WA_FIELDCATALOG-INTLEN    = '000004'.
  WA_FIELDCATALOG-LOWERCASE = 'X'.
  WA_FIELDCATALOG-DOMNAME   = 'CHAR04'.
  WA_FIELDCATALOG-SCRTEXT_L = TEXT-001.
  WA_FIELDCATALOG-SCRTEXT_M = TEXT-001.
  WA_FIELDCATALOG-SCRTEXT_S = TEXT-001.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  WA_FIELDCATALOG-FIELDNAME = 'ICO_FILIAL'.
  WA_FIELDCATALOG-SCRTEXT_L = TEXT-002.
  WA_FIELDCATALOG-SCRTEXT_M = TEXT-002.
  WA_FIELDCATALOG-SCRTEXT_S = TEXT-002.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  WA_FIELDCATALOG-FIELDNAME = 'ICO_FISCAL'.
  WA_FIELDCATALOG-SCRTEXT_L = TEXT-003.
  WA_FIELDCATALOG-SCRTEXT_M = TEXT-003.
  WA_FIELDCATALOG-SCRTEXT_S = TEXT-003.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  WA_FIELDCATALOG-FIELDNAME = 'ICO_COMERCIAL'.
  WA_FIELDCATALOG-SCRTEXT_L = TEXT-004.
  WA_FIELDCATALOG-SCRTEXT_M = TEXT-004.
  WA_FIELDCATALOG-SCRTEXT_S = TEXT-004.
  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.
    <FS_CAT>-TABNAME = 'ZDE_ZSDT0001ACG_ALV'.

    CASE <FS_CAT>-FIELDNAME.
      WHEN 'ICO_CARGA'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 1.
      WHEN 'ICO_FILIAL'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 2.
      WHEN 'ICO_FISCAL'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 3.
      WHEN 'ICO_COMERCIAL'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
        <FS_CAT>-COL_POS = 4.
    ENDCASE.

    IF <FS_CAT>-FIELDNAME <> 'ICO_CARGA'.
      <FS_CAT>-COL_POS = LC_COL_POS.
      ADD 1 TO LC_COL_POS.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_HINTS .

  DATA: IT_DD07V        TYPE TABLE OF DD07V WITH HEADER LINE,
        WA_EXCEPT_QINFO LIKE LINE OF IT_EXCEPT_QINFO,
        LC_TP_STATUS    TYPE ZDE_ST_SOL_AJUSTE,
        LC_ICO_CARGA    TYPE CHAR04.
*
  CLEAR: IT_EXCEPT_QINFO[].
*
*  "Informações de Status da Solicitação de Manutenção
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_ST_SOL_AJUSTE'
    TABLES
      VALUES_TAB = IT_DD07V.
*
  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    LC_TP_STATUS = CONV #( IT_DD07V-DOMVALUE_L ).
    PERFORM SETA_ICONE_STATUS USING LC_TP_STATUS CHANGING LC_ICO_CARGA.
    WA_EXCEPT_QINFO-VALUE = LC_ICO_CARGA.
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_ZSDT0001ACG_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_CARGA'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

  "A  Solicitação Aprovada
  "R  Solicitação Recusada
  "W  Solicitação Em Espera de Aprovação
  "S  Solicitação não gera Aprovação

*  "Informações de Status das Respostas de Aprovação
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_TP_ROM_RESPOSTA'
    TABLES
      VALUES_TAB = IT_DD07V.

  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    LC_TP_STATUS = CONV #( IT_DD07V-DOMVALUE_L ).
    PERFORM SETA_ICONE_STATUS_APROVACAO USING LC_TP_STATUS CHANGING LC_ICO_CARGA.
    WA_EXCEPT_QINFO-VALUE = LC_ICO_CARGA.
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_ZSDT0001ACG_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_FILIAL'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_FISCAL'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_COMERCIAL'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '2200'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  CASE FIELDNAME.
    WHEN 'ICO_CARGA'.
      READ TABLE IT_RETORNO_ALV INDEX ROW_ID ASSIGNING FIELD-SYMBOL(<FS_RETORNO>).
      PERFORM MOSTRAR_CARGA USING <FS_RETORNO>-ID_CARGA <FS_RETORNO>-ID_SOLICITACAO CHANGING <FS_RETORNO>.
      PERFORM ATUALIZA_TELA USING ABAP_TRUE.
      LEAVE TO SCREEN 2000.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING P_ROW TYPE LVC_S_ROW.

  "DATA: LC_ROW TYPE LVC_T_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.
    "APPEND P_ROW TO LC_ROW.
    "CALL METHOD CTL_ALV->SET_SELECTED_ROWS EXPORTING IT_INDEX_ROWS = LC_ROW.
    READ TABLE IT_RETORNO_ALV INDEX P_ROW-INDEX ASSIGNING FIELD-SYMBOL(<FS_RETORNO>).
    PERFORM MOSTRAR_CARGA USING <FS_RETORNO>-ID_CARGA <FS_RETORNO>-ID_SOLICITACAO CHANGING <FS_RETORNO>.
    PERFORM ATUALIZA_TELA USING ABAP_TRUE.
    LEAVE TO SCREEN 2000.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ATUALIZA_TELA USING CK_AJUSTA_TITULO TYPE CHAR01.

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  IF CTL_ALV IS NOT INITIAL.
    GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
    GS_ALV_REFRES_COND-COL = ABAP_TRUE.

    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = GS_ALV_REFRES_COND
        I_SOFT_REFRESH = ABAP_TRUE.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_TELA_2000.

  CLEAR: EVENT_HANDLER.

  IF CTL_ALV IS NOT INITIAL.
    CTL_ALV->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV.

  IF CTL_CCCONTAINER IS NOT INITIAL.
    CTL_CCCONTAINER->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER.

  IF DG_SPLITTER IS NOT INITIAL.
    DG_SPLITTER->FREE( ).
  ENDIF.
  CLEAR: DG_SPLITTER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV_ID_CARGA  text
*----------------------------------------------------------------------*
FORM MOSTRAR_CARGA  USING P_ID_CARGA TYPE ZDE_ID_CARGA P_ID_SOLICITACAO TYPE ZDE_ID_SOL_AJUSTE CHANGING P_RET_ALV TYPE TY_ITENS_ALV.

  DATA: OBJ_CARGA TYPE REF TO ZCL_CARGA_RECEBIMENTO.

  SUBMIT ZMMR126_0001 WITH PCK_CAD  EQ ABAP_TRUE
                 WITH PMANUT   EQ ABAP_TRUE
                 WITH PSAFRA   EQ PSAFRA
                 WITH PEMPRE   EQ PEMPRE
                 WITH PFILIA   EQ PFILIA
                 WITH PIDCARGA EQ P_ID_CARGA
                 WITH PIDSOLIC EQ P_ID_SOLICITACAO AND RETURN.

  CREATE OBJECT OBJ_CARGA.
  TRY .
      OBJ_CARGA->ZIF_CARGA~SET_REGISTRO_MANUTENCAO( I_ID_SOLICITACAO = P_ID_SOLICITACAO )->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO) ).
      MOVE-CORRESPONDING E_APRESENTACAO-CARGA TO P_RET_ALV.
      P_RET_ALV-TP_SOLICITACAO_STATUS = E_APRESENTACAO-MANUTENCAO-TP_SOLICITACAO_STATUS.
      P_RET_ALV-DT_SOLICITACAO        = E_APRESENTACAO-MANUTENCAO-DT_SOLICITACAO.
      P_RET_ALV-HR_SOLICITACAO        = E_APRESENTACAO-MANUTENCAO-HR_SOLICITACAO.
      P_RET_ALV-US_SOLICITACAO        = E_APRESENTACAO-MANUTENCAO-US_SOLICITACAO.
      PERFORM SETA_ICONE_STATUS USING P_RET_ALV-TP_SOLICITACAO_STATUS CHANGING P_RET_ALV-ICO_CARGA.
    CATCH ZCX_CARGA.
    CATCH ZCX_ORDEM_CARREGAMENTO.
  ENDTRY.
  TRY .
      OBJ_CARGA->ZIF_CARGA~FREE( ).
    CATCH ZCX_CARGA.
  ENDTRY.
  CLEAR: OBJ_CARGA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETA_ICONE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RETORNO_ALV_TP_STATUS  text
*      <--P_IT_RETORNO_ALV_ICO_CARGA  text
*----------------------------------------------------------------------*
FORM SETA_ICONE_STATUS  USING    P_TP_STATUS TYPE ZDE_ST_SOL_AJUSTE
                        CHANGING P_ICO_CARGA TYPE CHAR04.

  CASE P_TP_STATUS.
    WHEN ZIF_CARGA=>ST_STATUS_MANUT_ABERTO.
      P_ICO_CARGA = ICON_INITIAL.
    WHEN ZIF_CARGA=>ST_STATUS_MANUT_ENVIADO.
      P_ICO_CARGA = ICON_WORKFLOW_WAIT_FOR_EVENTS.
    WHEN ZIF_CARGA=>ST_STATUS_MANUT_APROVADO.
      P_ICO_CARGA = ICON_RELEASE.
    WHEN ZIF_CARGA=>ST_STATUS_MANUT_RECUSADA.
      P_ICO_CARGA = ICON_DEFECT.
    WHEN ZIF_CARGA=>ST_STATUS_MANUT_CANCELADA.
      P_ICO_CARGA = ICON_TERMINATED_POSITION.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETA_ICONE_STATUS_APROVACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RETORNO_ALV_TP_STATUS  text
*      <--P_IT_RETORNO_ALV_ICO_CARGA  text
*----------------------------------------------------------------------*
FORM SETA_ICONE_STATUS_APROVACAO  USING    P_TP_RESPOSTA TYPE ZDE_TP_ROM_RESPOSTA
                                  CHANGING P_ICO_APROVACAO TYPE CHAR04.

  CASE P_TP_RESPOSTA.
    WHEN ZIF_CARGA=>ST_RS_ACEITE_MANUT_APROVADA.
      P_ICO_APROVACAO = ICON_LED_GREEN.
    WHEN ZIF_CARGA=>ST_RS_ACEITE_MANUT_RECUSADA.
      P_ICO_APROVACAO = ICON_LED_RED.
    WHEN ZIF_CARGA=>ST_RS_ACEITE_MANUT_ESPERA.
      P_ICO_APROVACAO = ICON_TIME.
    WHEN ZIF_CARGA=>ST_RS_ACEITE_MANUT_NAO_GERA.
      P_ICO_APROVACAO = ICON_SPACE.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000_EXIT INPUT.

  PERFORM LIMPA_TELA_2000.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CARREGA_SAIDA .

  LOOP AT IT_RETORNO INTO DATA(WA_RETORNO).
    MOVE-CORRESPONDING WA_RETORNO TO IT_RETORNO_ALV.
    PERFORM SETA_ICONE_STATUS           USING IT_RETORNO_ALV-TP_SOLICITACAO_STATUS CHANGING IT_RETORNO_ALV-ICO_CARGA.
    PERFORM SETA_ICONE_STATUS_APROVACAO USING IT_RETORNO_ALV-RS_ACEITE_FILIAL      CHANGING IT_RETORNO_ALV-ICO_FILIAL.
    PERFORM SETA_ICONE_STATUS_APROVACAO USING IT_RETORNO_ALV-RS_ACEITE_FISCAL      CHANGING IT_RETORNO_ALV-ICO_FISCAL.
    PERFORM SETA_ICONE_STATUS_APROVACAO USING IT_RETORNO_ALV-RS_ACEITE_COMERCIAL   CHANGING IT_RETORNO_ALV-ICO_COMERCIAL.
    APPEND IT_RETORNO_ALV.
  ENDLOOP.

ENDFORM.
