*----------------------------------------------------------------------*
***INCLUDE ZMMR115_0100.
*----------------------------------------------------------------------*

TYPE-POOLS: ICON.

CONSTANTS:
  BEGIN OF C_VS_FORNECEDOR,
    COLUMN1 TYPE TV_ITMNAME VALUE 'Fornecedor',
    COLUMN2 TYPE TV_ITMNAME VALUE 'CNPJ/CPF',
  END OF C_VS_FORNECEDOR.

*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS C_SERVICE DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS  GET_PIC_TAB IMPORTING MIME_URL TYPE CSEQUENCE
                               EXPORTING PIC_TAB  TYPE STANDARD TABLE.
ENDCLASS.                    "c_service DEFINITION

CLASS LCL_EVENTS_D0100 DEFINITION.

  PUBLIC SECTION.
    DATA: ERROR_IN_DATA TYPE C.
    METHODS:
      DATA_CHANGED  FOR EVENT DATA_CHANGED
        OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED
                  E_ONF4
                  E_ONF4_BEFORE
                  E_ONF4_AFTER,

      DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED
        OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED
                  ET_GOOD_CELLS.

  PRIVATE SECTION.

    METHODS: PERFORM_SEMANTIC_CHECKS
      IMPORTING
        PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDCLASS.                    "lcl_events_d0100 DEFINITION

*----------------------------------------------------------------------*
*   INCLUDE TLIST_TREE_CONTROL_DEMOCL1                                 *
*----------------------------------------------------------------------*

CLASS LCL_APPLICATION DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_NODE_DOUBLE_CLICK  FOR EVENT NODE_DOUBLE_CLICK  OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY,
      HANDLE_EXPAND_NO_CHILDREN FOR EVENT EXPAND_NO_CHILDREN OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY,
      HANDLE_ITEM_DOUBLE_CLICK  FOR EVENT ITEM_DOUBLE_CLICK  OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_BUTTON_CLICK       FOR EVENT BUTTON_CLICK       OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_LINK_CLICK         FOR EVENT LINK_CLICK         OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_CHECKBOX_CHANGE    FOR EVENT CHECKBOX_CHANGE    OF CL_GUI_COLUMN_TREE IMPORTING NODE_KEY ITEM_NAME CHECKED.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM WITH DEFAULT KEY.

DATA: PICTURE       TYPE REF TO CL_GUI_PICTURE,
      G_TREE        TYPE REF TO CL_GUI_COLUMN_TREE,
      EVENTO        TYPE CNTL_SIMPLE_EVENT,
      EVENTS        TYPE CNTL_SIMPLE_EVENTS,
      G_APPLICATION TYPE REF TO LCL_APPLICATION.

DATA: GR_EVENTS_D0100 TYPE REF TO LCL_EVENTS_D0100.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.

  METHOD  HANDLE_NODE_DOUBLE_CLICK.

  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD  HANDLE_ITEM_DOUBLE_CLICK.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  HANDLE_LINK_CLICK.

    PERFORM MOSTRAR_NODE USING NODE_KEY ITEM_NAME.

  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  HANDLE_BUTTON_CLICK.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD  HANDLE_CHECKBOX_CHANGE.
  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE

  METHOD HANDLE_EXPAND_NO_CHILDREN.
  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION

CLASS LCL_EVENTS_D0100 IMPLEMENTATION.
  METHOD DATA_CHANGED.
  ENDMETHOD.                    "data_changed

  METHOD PERFORM_SEMANTIC_CHECKS.
  ENDMETHOD.

  METHOD DATA_CHANGED_FINISHED.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_


ENDCLASS.

CONTROLS: TABPRINC TYPE TABSTRIP,
          TABRODO  TYPE TABSTRIP,
          TABPARC  TYPE TABLEVIEW USING SCREEN 0102,
          TABNF55  TYPE TABLEVIEW USING SCREEN 0103,
          TABNF01  TYPE TABLEVIEW USING SCREEN 0104,
          TABDOCS  TYPE TABLEVIEW USING SCREEN 0105,
          TABCT57  TYPE TABLEVIEW USING SCREEN 0106.

DATA: VG_TELA_0101 TYPE SY-DYNNR,
      VG_TELA_0113 TYPE SY-DYNNR,
      VG_TELA_0114 TYPE SY-DYNNR.

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_CTE
*&---------------------------------------------------------------------*
FORM POPULA_SELECAO_NFE .

  CLEAR IT_SELECTED_ROWS.

  CHECK CTL_ALV_NFE IS NOT INITIAL.

  CALL METHOD CTL_ALV_NFE->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  CLEAR IT_NFE_SELECT[].

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_NFE_ALV INTO WA_NFE_ALV INDEX WA_SELECTED_ROWS-INDEX.
    MOVE-CORRESPONDING WA_NFE_ALV TO IT_NFE_SELECT.
    APPEND IT_NFE_SELECT.
  ENDLOOP.

ENDFORM.                    " POPULA_SELECAO_CTE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN OK_REFRESH.
      CLEAR: OK_CODE.
      PERFORM REFRESH_ALL.
    WHEN OK_REINICIAR.
      CLEAR: OK_CODE.
      PERFORM REINICIAR_PAGAMENTOS.
    WHEN OK_ACEITE.
      CLEAR: OK_CODE.
**********************************************************************"150179 CS2024000780 Inclusão de pedidos de Calcário no COUPA (ZFTE, ZEFI) PSA

      DATA: LT_BAPIPARAM TYPE STANDARD TABLE OF BAPIPARAM INITIAL SIZE 0,
            LT_RETURN    TYPE STANDARD TABLE OF BAPIRET2 INITIAL SIZE 0.

      DATA: _LINES   TYPE I,
            _QTD_FOR TYPE I,
            IT_FOR   LIKE TABLE OF WA_NFE_ALV WITH KEY FORNE_CNPJ.
      CLEAR: _LINES,_QTD_FOR.
      DESCRIBE TABLE IT_SELECTED_ROWS LINES _LINES.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME  = SY-UNAME
        TABLES
          RETURN    = LT_RETURN
          PARAMETER = LT_BAPIPARAM.

      READ TABLE LT_BAPIPARAM INTO DATA(LS_BAPIPARAM) WITH KEY PARID = 'ZMM0110_LM_AUTHORIZE' PARVA = 'X'.

      IF SY-SUBRC = 0 .

        APPEND LINES OF IT_NFE_ALV TO IT_FOR.
        SORT IT_FOR BY FORNE_CNPJ ASCENDING.

        DELETE ADJACENT DUPLICATES FROM IT_FOR COMPARING FORNE_CNPJ.
        DESCRIBE TABLE IT_FOR LINES _QTD_FOR.

        IF _QTD_FOR > 1.
          MESSAGE 'Existem mais de um fornecedor selecionado!' TYPE 'I'.
          EXIT.
        ENDIF.

        IF _LINES > 1.
          DATA: LS_ANSWER TYPE ANSWER.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TITLEBAR              = 'Lançamento em Massa'
              TEXT_QUESTION         = 'Deseja continua com Lançamento em Massa ?'
              TEXT_BUTTON_1         = 'Sim'
              ICON_BUTTON_1         = '@01@'
              TEXT_BUTTON_2         = 'Não'
              ICON_BUTTON_2         = '@02@'
              DISPLAY_CANCEL_BUTTON = SPACE
            IMPORTING
              ANSWER                = LS_ANSWER.

          IF LS_ANSWER = '1'.

            READ TABLE IT_NFE_ALV INTO DATA(_NFE) INDEX 1.

            IF _NFE-FORNE_CNPJ IS NOT INITIAL.

              DATA: P_CNPJ   TYPE STCD1,
                    P_ANO    TYPE GJAHR,
                    IT_CHAVE TYPE STANDARD TABLE OF ZIB_NFE_DIST_TER-CHAVE_NFE WITH HEADER LINE.
              FREE: IT_CHAVE.
              CLEAR: P_CNPJ,P_ANO.
              P_CNPJ = _NFE-FORNE_CNPJ.
              P_ANO = _NFE-DT_EMISSAO+0(4) - 1.

              LOOP AT IT_NFE_ALV ASSIGNING FIELD-SYMBOL(<_NFE>).
                IT_CHAVE = <_NFE>-CHAVE_NFE.
                APPEND IT_CHAVE TO IT_CHAVE[].
                CLEAR IT_CHAVE.
              ENDLOOP.

              SORT  IT_CHAVE[].

              DELETE ADJACENT DUPLICATES FROM IT_CHAVE[].

              CALL FUNCTION 'Z_LANC_MASS_ZMMR115'
                EXPORTING
                  I_CNPJ  = P_CNPJ
                  I_ANO   = P_ANO
                TABLES
                  I_CHAVE = IT_CHAVE[].
            ENDIF.

            PERFORM REFRESH_ALL.

          ELSE.

          ENDIF.
        ELSE.
          PERFORM ACEITE_NFE_INBOUND.
        ENDIF.
      ELSE.

        IF _LINES = 1.
          PERFORM ACEITE_NFE_INBOUND.
        ELSE.
          MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.
      "150179 CS2024000780 Inclusão de pedidos de Calcário no COUPA (ZFTE, ZEFI) PSA
      "PERFORM aceite_nfe_inbound.
**********************************************************************

**-CS2025000249-08.04.2025-#173180-JT-inicio
    WHEN OK_ACEITE_MASSA.
      CLEAR: OK_CODE.
      PERFORM ACEITE_NFE_INBOUND_MASSA.
**-CS2025000249-08.04.2025-#173180-JT-fim

    WHEN OK_FISICO.
      CLEAR: OK_CODE.
      PERFORM ACEITE_FISICO_NFE_INBOUND.

    WHEN OK_PROGRAMAR.
      CLEAR: OK_CODE.
      PERFORM GERAR_PAGAMENTOS USING ABAP_FALSE. "*-CS2025000249-17.04.2025-#173311-JT-inicio
      "WHEN OK_ESTORNAR.
      "  CLEAR: OK_CODE.
      "  PERFORM GERAR_PAGAMENTOS USING ABAP_TRUE.
    WHEN OK_VISNFE.
      CLEAR: OK_CODE.
      PERFORM VISUALIZAR_NFE.
    WHEN OK_ANEXAR.
      CLEAR: OK_CODE.
      PERFORM HABILITAR_WORKFLOW_DOCUMENTOS.
    WHEN OK_VS_NFEIN.
      CLEAR: OK_CODE.
      OK_VISAO = OK_VS_NFEIN.
      PERFORM APRESENTAR_INFORMACAO.
      PERFORM LIMPAR_CONTROLES_TELA USING ABAP_FALSE.
    WHEN OK_VS_FORNECE.
      CLEAR: IT_NFE_ALV[].
      CLEAR: OK_CODE.
      OK_VISAO = OK_VS_FORNECE.
      PERFORM LIMPAR_CONTROLES_TELA USING ABAP_FALSE.

    WHEN OK_LOG_APROV. "PBI - 64541 - CSB
      PERFORM LOG_APROVACAO.
    WHEN OK_ESTRAT.   "PBI - 64541 - CSB
      PERFORM ESTRATEGIA.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: WA_OBJ  TYPE BORIDENT,
        IP_MODE TYPE SGS_RWMOD.

  SET PF-STATUS 'PFNFEINBOUND'.

  CASE OK_VISAO.
    WHEN OK_VS_NFEIN.
      PERFORM CRIAR_VISAO_NFE_INBOUND.
    WHEN OK_VS_FORNECE.
      PERFORM CRIAR_VISAO_FORNECEDOR.
  ENDCASE.

  IF CTL_ALV_NFE IS NOT INITIAL.
    CALL METHOD CTL_ALV_NFE->SET_SCROLL_INFO_VIA_ID
      EXPORTING
        IS_COL_INFO = GS_SCROLL_COL
        IS_ROW_NO   = GS_SCROLL_ROW.
  ENDIF.

  IF CTL_ALV_NFE_HIST IS NOT INITIAL.
    CALL METHOD CTL_ALV_NFE_HIST->SET_SCROLL_INFO_VIA_ID
      EXPORTING
        IS_COL_INFO = GS_SCROLL_COL2
        IS_ROW_NO   = GS_SCROLL_ROW2.
  ENDIF.

  IF MANAGER IS NOT INITIAL.
    CALL METHOD MANAGER->UNPUBLISH.
    CLEAR: MANAGER.
  ENDIF.

  IF IT_NFE_SELECTW[] IS NOT INITIAL.
    READ TABLE IT_NFE_SELECTW INDEX 1.
    WA_OBJ-OBJTYPE = 'ZMM0110'.
    CONCATENATE IT_NFE_SELECTW-MANDT IT_NFE_SELECTW-CHAVE_NFE INTO WA_OBJ-OBJKEY.

    IF GF_AUTHORIZATION_FT_09 EQ ABAP_TRUE.
      IP_MODE = 'E'.
    ELSE.
      IP_MODE = 'D'.
    ENDIF.

    CREATE OBJECT MANAGER
      EXPORTING
        IS_OBJECT        = WA_OBJ
        IP_NO_COMMIT     = 'R'
        IP_MODE          = IP_MODE
      EXCEPTIONS
        OBJECT_INVALID   = 1
        CALLBACK_INVALID = 2
        OTHERS           = 3.

    "Workflow CT-e: Nr.: &1 Sr.: &2 Dt.: &3 Forn.: &4
    SELECT SINGLE * INTO WA_NFE_DIST
      FROM ZIB_NFE_DIST_TER
     WHERE CHAVE_NFE EQ IT_NFE_SELECTW-CHAVE_NFE.

* PBI - 64541 - CBRAND
    SELECT SINGLE * INTO WA_ZMMT0149
      FROM ZMMT0149
      WHERE CHAVE_NFE EQ IT_NFE_SELECTW-CHAVE_NFE
      AND STATUS = 'L'.


    SET TITLEBAR 'TLWORK' WITH WA_NFE_DIST-NUMERO WA_NFE_DIST-SERIE WA_NFE_DIST-DT_EMISSAO WA_NFE_DIST-FORNE_RAZAO.
    CLEAR: WA_NFE_DIST.
  ELSE.
    SET TITLEBAR 'TLNFEINBOUND'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_NFE_DIST_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.

    <FS_CAT>-TABNAME = 'ZDE_NFE_DIST_ALV'.

    CASE <FS_CAT>-FIELDNAME.
      WHEN 'ICO_ST_DOCUMENTO'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'ICO_ST_FISCAL'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'ICO_ST_FISICO'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'ICO_ST_ARMAZEM'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-ICON    = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'DOCNUM_NFE' OR 'DOCNUM_ARM' OR 'DOCNUM_DEV'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
      WHEN 'EBELN' OR 'BELNR' OR 'VBELN' OR 'TKNUM' OR 'FKNUM' OR 'MBLNR' OR 'MJAHR' OR 'MBLNR_ARM' OR 'DOCNUM_DEV' OR 'BELNR_DEV'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
      WHEN 'GJAHR'.
        <FS_CAT>-OUTPUTLEN = 05.
      WHEN 'MATNR'.                 "*-CS2025000249-08.04.2025-#173180-JT
        <FS_CAT>-OUTPUTLEN = 14.    "*-CS2025000249-08.04.2025-#173180-JT
      WHEN 'PROD_QTD_COMERCI'.      "*-CS2025000249-08.04.2025-#173180-JT
        <FS_CAT>-OUTPUTLEN = 10.    "*-CS2025000249-08.04.2025-#173180-JT
      WHEN 'DS_ST_NOTA'.
        <FS_CAT>-COL_POS   = 5.
        <FS_CAT>-OUTPUTLEN = 10.
        <FS_CAT>-REPTEXT   = 'St.Nota'.
        <FS_CAT>-SCRTEXT_L = 'St.Nota'.
        <FS_CAT>-SCRTEXT_M = 'St.Nota'.
        <FS_CAT>-SCRTEXT_S = 'St.Nota'.
    ENDCASE.

    IF <FS_CAT>-FIELDNAME <> 'CK_CHEGADA_DOC'.
      <FS_CAT>-COL_POS = LC_COL_POS.
      ADD 1 TO LC_COL_POS.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0001'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG2
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG2 .

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CLEAR: IT_FIELDCATALOG2[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_NFE_DIST_LOG_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG2.

  LOOP AT IT_FIELDCATALOG2 ASSIGNING <FS_CAT>.
    CASE <FS_CAT>-FIELDNAME.
      WHEN 'MESSAGE'.
        <FS_CAT>-OUTPUTLEN = 50.
      WHEN 'MESSAGE_V1' OR 'MESSAGE_V2' OR 'MESSAGE_V3' OR 'MESSAGE_V4'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'IC_MESSAGE'.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'IC_TEXTO'.
        <FS_CAT>-HOTSPOT = ABAP_TRUE.
        <FS_CAT>-JUST    = 'C'.
      WHEN 'CK_ESTRATEGIA'.
        <FS_CAT>-NO_OUT  = ABAP_TRUE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG2

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  READ TABLE IT_NFE_ALV INDEX ROW_ID INTO WA_NFE_ALV.

  CASE FIELDNAME.
    WHEN 'DOCNUM_NFE'.
      PERFORM MOSTRAR_DOC_FISCAL USING WA_NFE_ALV-DOCNUM_NFE.
    WHEN 'DOCNUM_ARM'.
      PERFORM MOSTRAR_DOC_FISCAL USING WA_NFE_ALV-DOCNUM_ARM.
    WHEN 'DOCNUM_DEV'.
      PERFORM MOSTRAR_DOC_FISCAL USING WA_NFE_ALV-DOCNUM_DEV.
    WHEN 'EBELN'.
      PERFORM MOSTRAR_PEDIDO USING WA_NFE_ALV-EBELN.
    WHEN 'BELNR'.
      PERFORM MOSTRAR_FATURA USING WA_NFE_ALV-BELNR WA_NFE_ALV-GJAHR.
    WHEN 'BELNR_DEV'.
      PERFORM MOSTRAR_FATURA USING WA_NFE_ALV-BELNR_DEV WA_NFE_ALV-GJAHR_DEV.
    WHEN 'MBLNR' OR 'MJAHR'.
      PERFORM MOSTRAR_DOC_MATERIAL USING WA_NFE_ALV-MBLNR WA_NFE_ALV-MJAHR.
    WHEN 'MBLNR_ARM'.
      PERFORM MOSTRAR_DOC_MATERIAL USING WA_NFE_ALV-MBLNR_ARM WA_NFE_ALV-MJAHR_ARM.
    WHEN 'MBLNR_DEV'.
      PERFORM MOSTRAR_DOC_MATERIAL USING WA_NFE_ALV-MBLNR_DEV WA_NFE_ALV-MJAHR_DEV.
    WHEN 'VBELN'.
      PERFORM MOSTRAR_REMESSA USING WA_NFE_ALV-VBELN.
    WHEN 'ICO_ST_DOCUMENTO'.
      PERFORM BUSCA_LOGS_NFE USING WA_NFE_ALV-CHAVE_NFE.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK_LOG
         USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
               VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.

  DATA: I_CD_APROVACAO  TYPE ZDE_EST_APROVACAO.

  READ TABLE IT_LOG_ALV INDEX ROW_ID INTO WA_LOG_ALV.

  CASE FIELDNAME.
    WHEN 'IC_TEXTO'.
      IF WA_LOG_ALV-CK_ESTRATEGIA EQ ABAP_TRUE.
        CALL FUNCTION 'ZNFE_SHOW_INFO_MOTIVO_ESTRATEG'
          EXPORTING
            I_CD_APROVACAO = WA_LOG_ALV-CD_APROVACAO.
      ENDIF.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT100 INPUT.

  CASE OK_CODE.
    WHEN 'BACK'.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_EXIT100  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOGS_CTE
*&---------------------------------------------------------------------*
FORM BUSCA_LOGS_NFE  USING P_CHAVE TYPE ZDE_CHAVE_DOC_E.

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  DATA: IT_LOGS TYPE ZDE_NFE_DIST_LOG_ALV_T,
        WA_LOGS TYPE ZDE_NFE_DIST_LOG_ALV.

  CLEAR: IT_LOG_ALV.

  IT_LOGS = OBJ_NFE->AT_NFE_INBOUND->GET_LOG_PROC_NFE( P_CHAVE = P_CHAVE ).

  LOOP AT IT_LOGS INTO WA_LOGS.
    MOVE-CORRESPONDING WA_LOGS TO WA_LOG_ALV.
    APPEND WA_LOG_ALV TO IT_LOG_ALV.
  ENDLOOP.

  SORT IT_LOG_ALV BY DT_ATUALIZACAO DESCENDING   "*-CS2025000249-17.04.2025-#173311-JT
                     HR_ATUALIZACAO DESCENDING.  "*-CS2025000249-17.04.2025-#173311-JT

  IF CK_LOG_ATIVO EQ ABAP_TRUE.

    CHECK CTL_ALV_NFE_HIST IS NOT INITIAL.

    GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
    GS_ALV_REFRES_COND-COL = ABAP_TRUE.

    CALL METHOD CTL_ALV_NFE_HIST->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = GS_ALV_REFRES_COND
        I_SOFT_REFRESH = ABAP_TRUE.

  ELSE.
    CK_LOG_ATIVO = ABAP_TRUE.
    PERFORM LIMPAR_CONTROLES_TELA USING ABAP_TRUE.
    LEAVE TO SCREEN 0100.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " BUSCA_LOGS_CTE

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_NOTAS_FISCAIS .

  DATA: OBJ_PESQ    TYPE REF TO ZCL_NFE_INBOUND,
        LC_FILTRO   TYPE ZIB_NFE_DIST_TER_FILTRO,
        E_REGISTROS TYPE ZDE_IB_NFE_DIST_TER_T.

  CLEAR: IT_NFE_ALV[],
         IT_NFE_ALV,
         IT_NFE_DIST[],
         IT_NFE_DIST.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Consulta de NF-e p/ Pagamento """""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  LC_FILTRO-DOCNUM_NFE[]      = DOCNUM[].
  LC_FILTRO-NUMERO[]          = NUMRCT[].
  LC_FILTRO-DT_EMISSAO[]      = DTEMIT[].
  LC_FILTRO-CHAVE_NFE[]       = CHAVEN[].
  LC_FILTRO-E_TOMADORA[]      = ETOMAD[].
  LC_FILTRO-F_TOMADORA[]      = FTOMAD[].
  LC_FILTRO-P_EMISSOR[]       = PTOMAD[].
  LC_FILTRO-FORNE_CNPJ[]      = EMCNPJ[].
  LC_FILTRO-FORNE_IE[]        = EMINSC[].
  LC_FILTRO-ST_DOCUMENTO[]    = STDOC[].
  LC_FILTRO-ST_FISCAL[]       = STFIS[].
  LC_FILTRO-ST_FISICO[]       = STFIC[].
  LC_FILTRO-ST_ARMAZEM[]      = STARM[].
*  lc_filtro-cd_departamento[] = depart[].
  LC_FILTRO-PROD_DESCRICAO[]  = IPRODD[].
  LC_FILTRO-PROD_CFOP[]       = IPCFOP[].
  LC_FILTRO-PROD_NCM[]        = IPRNCM[].
  LC_FILTRO-ICMS_CST[]        = IPICM[].
  LC_FILTRO-IPI_CST[]         = IPIPI[].
  LC_FILTRO-PIS_CST[]         = IPPIS[].
  LC_FILTRO-COF_CST[]         = IPCOF[].

*-CS2025000249-02.05.2025-#174115-JT-inicio
  LC_FILTRO-SEM_DETPED        = COND #( WHEN PDDETOFF = ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_OFF ).
  LC_FILTRO-COM_DETPED        = COND #( WHEN PDDETON  = ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_OFF ).
  LC_FILTRO-ALL_DETPED        = COND #( WHEN PDDETALL = ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_OFF ).
*-CS2025000249-02.05.2025-#174115-JT-fim

  TRY .
      CREATE OBJECT OBJ_PESQ.
      DATA(E_PESQUISOU) = OBJ_PESQ->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS = LC_FILTRO IMPORTING E_REGISTROS = E_REGISTROS ).
      IT_NFE_DIST[] = E_REGISTROS.
    CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
      NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
      NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

*** PBI - 64541 - CSB - Inicio
  PERFORM MONITORA_TAXA_CAMBIO.
*** PBI - 64541 - CSB - Fim
ENDFORM.                    " PESQUISAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Form  REINICIAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REINICIAR_PAGAMENTOS .

  DATA VXBLNR TYPE RBKP-XBLNR.
  DATA W_RBKP TYPE RBKP.
  DATA LV_NUM TYPE C LENGTH 40.
  DATA LV_NUM2 TYPE C LENGTH 40.
  DATA TABIX   TYPE SY-TABIX.


  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

****************************Verifica status************************************
*  SELECT * INTO TABLE @DATA(it_nfe_atu)
*    FROM zib_nfe_dist_ter
*     FOR ALL ENTRIES IN @it_nfe_select
*   WHERE chave_nfe EQ @it_nfe_select-chave_nfe.
*
*  SELECT * INTO TABLE @DATA(it_nfe_itm)
*    FROM zib_nfe_dist_itm
*     FOR ALL ENTRIES IN @it_nfe_select
*   WHERE chave_nfe EQ @it_nfe_select-chave_nfe.
*
*  SORT it_nfe_itm BY chave_nfe.
*  LOOP AT it_nfe_atu INTO DATA(wa_nfe_atu).
*    "
*    tabix = sy-tabix.
*    READ TABLE it_nfe_itm INTO DATA(wa_nfe_itm) WITH KEY chave_nfe  = wa_nfe_atu-chave_nfe BINARY SEARCH.
*
*    CLEAR w_rbkp.
*    " MIRO FOI ESTORNADA MANUALMENTE ?
*    IF wa_nfe_atu-belnr IS NOT INITIAL.
*
*      SELECT SINGLE stblg FROM rbkp
*        INTO @DATA(lv_stblg)
*        WHERE belnr = @wa_nfe_atu-belnr
*          AND gjahr = @wa_nfe_atu-gjahr.
*
*      IF sy-subrc EQ 0 AND lv_stblg IS NOT INITIAL.
*        wa_nfe_atu-belnr = space.
*        wa_nfe_atu-gjahr = space.
*      ENDIF.
*    ENDIF.
*
*    IF wa_nfe_atu-mblnr IS NOT INITIAL.
*      SELECT SINGLE smbln FROM mseg
*        INTO @DATA(lv_smbln)
*          WHERE smbln = @wa_nfe_atu-mblnr
*            AND sjahr = @wa_nfe_atu-mjahr.
*
*      IF sy-subrc EQ 0 AND lv_smbln IS NOT INITIAL.
*        wa_nfe_atu-mblnr = space.
*        wa_nfe_atu-mjahr = space.
*      ENDIF.
*    ENDIF.
*
*    IF ( wa_nfe_atu-belnr IS INITIAL OR  wa_nfe_atu-mblnr IS INITIAL ) AND wa_nfe_atu-ebeln IS INITIAL.
*
*      IF wa_nfe_atu-belnr IS INITIAL.
*        lv_num = wa_nfe_atu-numero.
*
*        SHIFT lv_num LEFT DELETING LEADING '0'.
*
*        CONDENSE lv_num NO-GAPS.
*
*        CHECK lv_num IS NOT INITIAL.
*
*        DATA(_tam) = strlen( lv_num ).
*        IF _tam GT 9.
*          _tam = _tam - 9.
*          lv_num = lv_num+_tam(9) .
*          SHIFT lv_num LEFT DELETING LEADING '0'.
*        ELSE.
*          lv_num = lv_num .
*        ENDIF.
*
*        lv_num2 = lv_num.
*        CONCATENATE lv_num '-'  wa_nfe_atu-serie INTO lv_num.
*
*        vxblnr = lv_num.
*        SELECT SINGLE * FROM rbkp
*          INTO w_rbkp
*          WHERE bukrs = wa_nfe_atu-bukrs
*            AND bldat = wa_nfe_atu-dt_emissao
*            AND lifnr = wa_nfe_atu-p_emissor
*            AND xblnr = vxblnr
*            AND rmwwr = wa_nfe_atu-vl_total_fatura
*            AND stblg = ' '.
*      ELSE.
*        SELECT SINGLE * FROM rbkp
*       INTO w_rbkp
*       WHERE belnr = wa_nfe_atu-belnr
*       AND   gjahr = wa_nfe_atu-gjahr
*       AND   stblg = ' '.
*      ENDIF.
*      IF sy-subrc = 0.
*        wa_nfe_atu-belnr = w_rbkp-belnr.
*        wa_nfe_atu-gjahr = w_rbkp-gjahr.
*
*        SELECT SINGLE *
*          INTO @DATA(w_rseg)
*          FROM rseg
*         WHERE belnr = @w_rbkp-belnr
*         AND   gjahr = @w_rbkp-gjahr.
*        IF sy-subrc = 0.
*          wa_nfe_atu-ebeln = w_rseg-ebeln.
*          SELECT SINGLE *
*            FROM ekbe
*            INTO @DATA(w_ekbe)
*            WHERE belnr = @w_rseg-lfbnr
*            AND   ebeln = @w_rseg-ebeln
*            AND   ebelp = @w_rseg-ebelp
*            AND   vgabe = '1'.
*          IF sy-subrc = 0.
*            wa_nfe_atu-mblnr = w_ekbe-belnr.
*            wa_nfe_atu-mjahr = w_ekbe-gjahr.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF wa_nfe_atu-mblnr IS INITIAL AND wa_nfe_itm-ebeln IS NOT INITIAL.
*      SELECT SINGLE *
*        FROM mseg AS m1
*        INTO @DATA(w_mseg)
*        WHERE ebeln = @wa_nfe_itm-ebeln
*        AND   ebelp = @wa_nfe_itm-ebelp
*        AND   NOT EXISTS ( SELECT * FROM mseg AS m2 WHERE smbln = m1~mblnr ).
*
*      IF sy-subrc = 0.
*        wa_nfe_atu-mblnr = w_mseg-mblnr.
*        wa_nfe_atu-mjahr = w_mseg-mjahr.
*      ENDIF.
*
*    ENDIF.
*    "
*    IF ( wa_nfe_atu-belnr IS INITIAL AND wa_nfe_atu-mblnr IS NOT INITIAL AND wa_nfe_atu-st_fisico NE '03' ). "Gerou a MIGO mas não gerou a MIRO
*      wa_nfe_atu-st_fiscal    = '99'.
*      wa_nfe_atu-st_fisico    = '03'.
*      wa_nfe_atu-st_documento = '01'.
*    ENDIF.
*
*    IF ( wa_nfe_atu-ebeln IS INITIAL AND wa_nfe_atu-belnr IS INITIAL AND wa_nfe_atu-mblnr IS INITIAL AND wa_nfe_atu-st_fiscal IS NOT INITIAL ). "Sem documentos
*      wa_nfe_atu-st_fiscal    = ''.
*      wa_nfe_atu-st_fisico    = ''.
*      wa_nfe_atu-st_documento = ''.
*    ENDIF.
*
*    IF ( wa_nfe_atu-ebeln IS not INITIAL AND wa_nfe_atu-belnr IS not INITIAL AND wa_nfe_atu-mblnr IS not INITIAL ). "todos gerados
*      wa_nfe_atu-st_fiscal    = '99'.
*      wa_nfe_atu-st_fisico    = '99'.
*      wa_nfe_atu-st_documento = '99'.
*    ENDIF.
*
*
*    MODIFY it_nfe_atu FROM wa_nfe_atu INDEX tabix.
*
*  ENDLOOP.
*
*  MODIFY zib_nfe_dist_ter FROM TABLE it_nfe_atu.
*  COMMIT WORK.

***************************Verifica status************************************

  LOOP AT IT_NFE_SELECT.

    TRY .
        OBJ_NFE->AT_NFE_INBOUND->DESBLOQUEAR_OBJETO( ).
        OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO   = IT_NFE_SELECT-CHAVE_NFE ).
        OBJ_NFE->AT_NFE_INBOUND->SET_INFO_SAP( ).
      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
      CATCH ZCX_PEDIDO_COMPRA_EXCEPTION INTO NFE_PEDIDO_COMPRA.
        NFE_PEDIDO_COMPRA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
    OBJ_NFE->AT_NFE_INBOUND->FREE( ).

  ENDLOOP.

  PERFORM ATUALIZAR_SELECAO.

ENDFORM.                    " REINICIAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SCROLL_INFO INPUT.

  IF CTL_ALV_NFE IS NOT INITIAL.
    CALL METHOD CTL_ALV_NFE->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_COL_INFO = GS_SCROLL_COL
        ES_ROW_NO   = GS_SCROLL_ROW.
  ENDIF.

  IF CTL_ALV_NFE_HIST IS NOT INITIAL.
    CALL METHOD CTL_ALV_NFE_HIST->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_COL_INFO = GS_SCROLL_COL2
        ES_ROW_NO   = GS_SCROLL_ROW2.
  ENDIF.

ENDMODULE.                 " GET_SCROLL_INFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SELECTED_ROWS INPUT.
  PERFORM POPULA_SELECAO_NFE.
ENDMODULE.                 " GET_SELECTED_ROWS  INPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZAR_SELECAO.

  DATA: IT_NFE TYPE ZIB_NFE_DIST_TER_T,
        IT_ALV TYPE ZDE_NFE_DIST_ALV_T,
        WA_ALV TYPE ZDE_NFE_DIST_ALV.

  CHECK IT_NFE_SELECT[] IS NOT INITIAL.

  SELECT * INTO TABLE IT_NFE
    FROM ZIB_NFE_DIST_TER
     FOR ALL ENTRIES IN IT_NFE_SELECT
   WHERE CHAVE_NFE EQ IT_NFE_SELECT-CHAVE_NFE.

  LOOP AT IT_NFE INTO DATA(WA_NFE).
    READ TABLE IT_NFE_DIST ASSIGNING FIELD-SYMBOL(<FS_NFE_PSQ>) WITH KEY CHAVE_NFE = WA_NFE-CHAVE_NFE.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING WA_NFE TO <FS_NFE_PSQ>.
    ENDIF.
  ENDLOOP.

  IT_ALV = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).

  LOOP AT IT_ALV INTO WA_ALV.
    READ TABLE IT_NFE_ALV ASSIGNING FIELD-SYMBOL(<FS_NFE_ALV>) WITH TABLE KEY CHAVE_NFE = WA_ALV-CHAVE_NFE.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING WA_ALV TO <FS_NFE_ALV>.
    ENDIF.
  ENDLOOP.

  PERFORM ATUALIZA_TELA.

ENDFORM.                    " ATUALIZAR_SELECAO

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ATUALIZA_TELA .

  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  CHECK CTL_ALV_NFE IS NOT INITIAL.

  GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
  GS_ALV_REFRES_COND-COL = ABAP_TRUE.

  CALL METHOD CTL_ALV_NFE->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_ALV_REFRES_COND
      I_SOFT_REFRESH = ABAP_TRUE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_FISCAL  USING P_FISCAL TYPE J_1BDOCNUM.

  DATA: GF_NFOBJN LIKE J_1BINTERF-NFOBJN.

  CHECK P_FISCAL IS NOT INITIAL.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      DOC_NUMBER         = P_FISCAL
    IMPORTING
      OBJ_NUMBER         = GF_NFOBJN
    EXCEPTIONS
      DOCUMENT_NOT_FOUND = 1
      DOCUM_LOCK         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      OBJ_NUMBER         = GF_NFOBJN
    EXCEPTIONS
      OBJECT_NOT_FOUND   = 1
      SCR_CTRL_NOT_FOUND = 2
      OTHERS             = 3.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDIDO
*&---------------------------------------------------------------------*
FORM MOSTRAR_PEDIDO  USING  P_EBELN TYPE EBELN.

  IF P_EBELN IS NOT INITIAL.
    SET PARAMETER ID 'BES' FIELD P_EBELN.
    CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA
*&---------------------------------------------------------------------*
FORM MOSTRAR_FATURA  USING    P_BELNR TYPE RE_BELNR
                              P_GJAHR TYPE GJAHR.
  IF P_BELNR IS NOT INITIAL AND P_GJAHR IS NOT INITIAL.
    SET PARAMETER ID 'RBN' FIELD P_BELNR.
    SET PARAMETER ID 'GJR' FIELD P_GJAHR.
    CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " MOSTRAR_FATURA

*&---------------------------------------------------------------------*
*&      Form  GERAR_PAGAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERAR_PAGAMENTOS USING P_MIRO_AUTOMATICA.

  CLEAR: IT_NFE_SELECTW[].

**-CS2025000249-17.04.2025-#173311-JT-inicio
  IF P_MIRO_AUTOMATICA = ABAP_TRUE.
    IF OBJ_NFE->AT_NFE_INBOUND->GET_CK_MIRO_AUTOMATICA( ) = ABAP_FALSE.
      RETURN.
    ENDIF.
  ELSE.
    OBJ_NFE->AT_NFE_INBOUND->SET_CK_MIRO_AUTOMATICA( ABAP_TRUE ).
  ENDIF.
**-CS2025000249-17.04.2025-#173311-JT-fim

  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

  READ TABLE IT_NFE_SELECT INDEX 1.

  IF SY-SUBRC IS INITIAL.
    TRY .
        OBJ_NFE->AT_NFE_INBOUND->DESBLOQUEAR_OBJETO( ).
        OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = IT_NFE_SELECT-CHAVE_NFE ).
        OBJ_NFE->AT_NFE_INBOUND->NFE_INBOUND_ACEITE_FATURA( ).
      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
    OBJ_NFE->AT_NFE_INBOUND->FREE( ).
  ENDIF.

  PERFORM ATUALIZAR_SELECAO.

ENDFORM.                    " GERAR_PAGAMENTOS

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VISUALIZAR_NFE .

  CLEAR: IT_NFE_SELECTW[].

*  IF GF_AUTHORIZATION_FT_03 IS INITIAL.
*    MESSAGE S112.
*    EXIT.
*  ENDIF.
*
  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

  "READ TABLE IT_NFE_SELECT INDEX 1.

  "IF SY-SUBRC IS INITIAL.

  LOOP AT IT_NFE_SELECT INTO DATA(WA_NFE_DANFE).
    TRY.
        ZCL_NFE_INBOUND=>DANFE( I_CHAVE_NFE = WA_NFE_DANFE-CHAVE_NFE ).
      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
  ENDLOOP.

*    TRY .
*        OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO   = IT_NFE_SELECT-CHAVE_NFE ).
*        OBJ_NFE->AT_NFE_INBOUND->NFE_INBOUND_VISUALIZAR( ).
*      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
*        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
*      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
*        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
*    ENDTRY.
*    OBJ_NFE->AT_NFE_INBOUND->FREE( ).

  " ENDIF.

ENDFORM.                    " VISUALIZAR_CTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA_VF
*&---------------------------------------------------------------------*
FORM MOSTRAR_FATURA_VF  USING P_VBELN_VF TYPE VBELN_VF.

  IF P_VBELN_VF IS NOT INITIAL.
    SET PARAMETER ID 'VF' FIELD P_VBELN_VF.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_FATURA_VF

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REMESSA_VL
*&---------------------------------------------------------------------*
FORM MOSTRAR_REMESSA_VL  USING P_VBELN_VL TYPE VBELN_VL.

  IF P_VBELN_VL IS NOT INITIAL.
    SET PARAMETER ID 'VL' FIELD P_VBELN_VL.
    CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_REMESSA_VL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_TRANSPORTE
*&---------------------------------------------------------------------*
FORM MOSTRAR_DOC_TRANSPORTE  USING  P_TKNUM TYPE TKNUM.

  IF P_TKNUM IS NOT INITIAL.
    SET PARAMETER ID 'TNR' FIELD P_TKNUM.
    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
  ENDIF.


ENDFORM.                    " MOSTRAR_DOC_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_CUSTO_TRANS
*&---------------------------------------------------------------------*
FORM MOSTRAR_DOC_CUSTO_TRANS  USING P_FKNUM TYPE FKNUM.

  IF P_FKNUM IS NOT INITIAL.
    SET PARAMETER ID 'FKK' FIELD P_FKNUM.
    CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_CUSTO_TRANS

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_MATERIAL  USING P_MBLNR TYPE MBLNR
                                 P_MJAHR TYPE MJAHR.

  IF P_MBLNR IS NOT INITIAL.
    SET PARAMETER ID 'MBN' FIELD P_MBLNR.
    SET PARAMETER ID 'MJA' FIELD P_MJAHR.
    "CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
    CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        I_ACTION            = 'A04'
        I_REFDOC            = 'R02'
        I_NOTREE            = 'X'
        I_NO_AUTH_CHECK     = ''
        I_SKIP_FIRST_SCREEN = 'X'
        I_DEADEND           = 'X'
        I_OKCODE            = 'OK_GO'
        I_MBLNR             = P_MBLNR
        I_MJAHR             = P_MJAHR
      EXCEPTIONS
        ILLEGAL_COMBINATION = 1
        OTHERS              = 2.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_REMESSA  USING P_VBELN TYPE VBELN_VL.

  IF P_VBELN IS NOT INITIAL.
    SET PARAMETER ID 'VL'  FIELD P_VBELN.
    SET PARAMETER ID 'VLM' FIELD P_VBELN.
    CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT2 .

  GS_VARIANT2-REPORT      = SY-REPID.
  GS_VARIANT2-HANDLE      = '0002'.
  GS_VARIANT2-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT2-USERNAME    = ABAP_FALSE.
  GS_VARIANT2-VARIANT     = ABAP_FALSE.
  GS_VARIANT2-TEXT        = ABAP_FALSE.
  GS_VARIANT2-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT2


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING P_ROW TYPE LVC_S_ROW.
  DATA: LC_ROW TYPE LVC_T_ROW.

  IF P_ROW-ROWTYPE IS INITIAL.

    APPEND P_ROW TO LC_ROW.

    CALL METHOD CTL_ALV_NFE->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = LC_ROW.

    READ TABLE IT_NFE_ALV INDEX P_ROW-INDEX INTO WA_NFE_ALV.

    PERFORM ABRIR_NFE USING WA_NFE_ALV-CHAVE_NFE.

  ENDIF.
ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HABILITAR_WORKFLOW_DOCUMENTOS .

  CLEAR: IT_NFE_SELECTW, IT_NFE_SELECTW[].

*  IF GF_AUTHORIZATION_FT_09 IS INITIAL.
*    MESSAGE S128.
*    EXIT.
*  ENDIF.

  "Somente validar acesso para modificar
  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

  READ TABLE IT_NFE_SELECT INDEX 1 INTO IT_NFE_SELECTW.
  APPEND IT_NFE_SELECTW.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NFE_ALV_CHAVE_NFE  text
*----------------------------------------------------------------------*
FORM ABRIR_NFE  USING I_CHAVE_NFE TYPE ZDE_CHAVE_DOC_E.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ACEITE_NFE_INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACEITE_NFE_INBOUND .

  CLEAR: IT_NFE_SELECTW[].

*  IF GF_AUTHORIZATION_FT_03 IS INITIAL.
*    MESSAGE S112.
*    EXIT.
*  ENDIF.
*
  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

  READ TABLE IT_NFE_SELECT INDEX 1.

  IF SY-SUBRC IS INITIAL.

    TRY .
        OBJ_NFE->AT_NFE_INBOUND->DESBLOQUEAR_OBJETO( ).
        OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO   = IT_NFE_SELECT-CHAVE_NFE ).
        OBJ_NFE->AT_NFE_INBOUND->NFE_INBOUND_ACEITE( ).
      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
    OBJ_NFE->AT_NFE_INBOUND->FREE( ).

  ENDIF.

  PERFORM ATUALIZAR_SELECAO.

ENDFORM.

*-CS2025000249-08.04.2025-#173180-JT-inicio
***************************************************************************
* aceite pedidos em massa
***************************************************************************
FORM ACEITE_NFE_INBOUND_MASSA .

  DATA: LV_LINES          TYPE I,
        LV_QUANT          TYPE I,
        IT_NFE_SELECT_AUX LIKE TABLE OF WA_NFE_ALV WITH HEADER LINE.

  DESCRIBE TABLE IT_SELECTED_ROWS LINES LV_LINES.

  IF LV_LINES <= 1.
    MESSAGE S024(SD) WITH 'Selecionar mais de UMA linha!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT IT_NFE_SELECT.

    SELECT SINGLE ST_FISICO, ST_ARMAZEM, ST_DOCUMENTO
      INTO @DATA(_STATUS)
      FROM ZIB_NFE_DIST_TER
     WHERE CHAVE_NFE = @IT_NFE_SELECT-CHAVE_NFE.

    IF ( SY-SUBRC <> 0 ) OR ( SY-SUBRC = 0 AND _STATUS-ST_DOCUMENTO <> ABAP_OFF AND _STATUS-ST_DOCUMENTO <> '00' ) OR
*                           ( sy-subrc = 0 AND _status-st_armazem   <> abap_off AND _status-st_armazem   <> '00' ) OR
                            ( SY-SUBRC = 0 AND _STATUS-ST_FISICO    <> ABAP_OFF AND _STATUS-ST_FISICO    <> '00' ).
      MESSAGE S024(SD) WITH 'Status da NFe não permite ' 'este Procedimento!' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT COUNT( * )
      INTO LV_QUANT
      FROM ZIB_NFE_DIST_ITM
     WHERE CHAVE_NFE = IT_NFE_SELECT-CHAVE_NFE.

    IF LV_QUANT > 1.
      MESSAGE S024(SD) WITH 'Notas Selecionadas contem mais de UM Item!' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

*------------------------------
* Informar pedido/item
*------------------------------
  CALL SCREEN 0300  STARTING AT 70 05
                      ENDING AT 95 07.

  CHECK LV_CONF_PED_MASSA = ABAP_TRUE.

  IT_NFE_SELECT_AUX[] = IT_NFE_SELECT[].

  READ TABLE IT_NFE_SELECT INDEX 1.

  DELETE IT_NFE_SELECT_AUX WHERE CHAVE_NFE = IT_NFE_SELECT-CHAVE_NFE.

  OBJ_NFE->AT_NFE_INBOUND->FREE( ).

  LOOP AT IT_NFE_SELECT_AUX.
    TRY .
        OBJ_NFE->AT_NFE_INBOUND->DESBLOQUEAR_OBJETO( ).
        OBJ_NFE->AT_NFE_INBOUND->BLOQUEAR_OBJETO( I_ID_REGISTRO = IT_NFE_SELECT_AUX-CHAVE_NFE ).

      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
  ENDLOOP.

  READ TABLE IT_NFE_SELECT INDEX 1.

  TRY .
      OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = IT_NFE_SELECT-CHAVE_NFE I_NAO_LIMPA = ABAP_TRUE ).
      OBJ_NFE->AT_NFE_INBOUND->SET_PEDIDO_MASSA( I_EBELN = LV_EBELN I_EBELP = LV_EBELP ).
      OBJ_NFE->AT_NFE_INBOUND->NFE_INBOUND_ACEITE( ).
    CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
      NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
    CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
      NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

  OBJ_NFE->AT_NFE_INBOUND->FREE( ).

  PERFORM ATUALIZAR_SELECAO.

ENDFORM.
*-CS2025000249-08.04.2025-#173180-JT-fim

*&---------------------------------------------------------------------*
*&      Form  ACEITE_FISICO_NFE_INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACEITE_FISICO_NFE_INBOUND .

  CLEAR: IT_NFE_SELECTW[].

*  IF GF_AUTHORIZATION_FT_03 IS INITIAL.
*    MESSAGE S112.
*    EXIT.
*  ENDIF.
*
  IF IT_NFE_SELECT[] IS INITIAL.
    MESSAGE S022.
    RETURN.
  ENDIF.

  READ TABLE IT_NFE_SELECT INDEX 1.

  IF SY-SUBRC IS INITIAL.
    TRY .
        OBJ_NFE->AT_NFE_INBOUND->DESBLOQUEAR_OBJETO( ).
        OBJ_NFE->AT_NFE_INBOUND->SET_CK_MIRO_AUTOMATICA( ABAP_TRUE ).  "*-CS2025000249-17.04.2025-#173311-JT
        OBJ_NFE->AT_NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = IT_NFE_SELECT-CHAVE_NFE ).
        OBJ_NFE->AT_NFE_INBOUND->NFE_INBOUND_ACEITE_FISICO( ).
        PERFORM GERAR_PAGAMENTOS USING ABAP_TRUE.  "*-CS2025000249-17.04.2025-#173311-JT-inicio

      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO NFE_INBOUND_EXCEPTION.
        NFE_INBOUND_EXCEPTION->PUBLISHED_ERRO( ).
      CATCH ZCX_CADASTRO INTO NFE_CADASTRO_EXCEPTION.
        NFE_CADASTRO_EXCEPTION->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
    OBJ_NFE->AT_NFE_INBOUND->FREE( ).
  ENDIF.

  PERFORM ATUALIZAR_SELECAO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_CONTROLES_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_CONTROLES_TELA USING CK_PARCIAL TYPE CHAR01.

  IF MANAGER IS NOT INITIAL.
    MANAGER->UNPUBLISH( ).
  ENDIF.

  CLEAR: MANAGER,
         EVENT_HANDLER_LOG,
         EVENT_HANDLER,
         TOOLBARMANAGER_LOG,
         OBG_TOOLBAR_LOG.

  IF PICTURE IS NOT INITIAL.
    PICTURE->FREE( ).
  ENDIF.
  CLEAR: PICTURE.

  IF CTL_ALV_NFE_HIST IS NOT INITIAL.
    CTL_ALV_NFE_HIST->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_NFE_HIST.

  IF CTL_ALV_NFE IS NOT INITIAL.
    CTL_ALV_NFE->FREE( ).
  ENDIF.
  CLEAR: CTL_ALV_NFE.

  IF CTL_CCCONTAINER3 IS NOT INITIAL.
    CTL_CCCONTAINER3->FREE( ).
  ENDIF.
  IF CTL_CCCONTAINER2 IS NOT INITIAL.
    CTL_CCCONTAINER2->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER3, CTL_CCCONTAINER2.

  IF DG_SPLITTER IS NOT INITIAL.
    DG_SPLITTER->FREE( ).
  ENDIF.
  CLEAR: DG_SPLITTER.

  IF CK_PARCIAL EQ ABAP_TRUE AND OK_VISAO EQ OK_VS_NFEIN.
    "IF CTL_CCCONTAINER IS NOT INITIAL.
    "  CTL_CCCONTAINER->FREE( ).
    "ENDIF.
    "CLEAR: CTL_CCCONTAINER.
  ENDIF.

  CHECK CK_PARCIAL EQ ABAP_FALSE.

  CLEAR: G_APPLICATION.

  IF G_TREE IS NOT INITIAL.
    G_TREE->FREE( ).
  ENDIF.
  CLEAR: G_TREE.

  IF CTL_CCCONTAINER_TREE IS NOT INITIAL.
    CTL_CCCONTAINER_TREE->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_TREE.

  IF CTL_CCCONTAINER_NFE_INBOUND IS NOT INITIAL.
    CTL_CCCONTAINER_NFE_INBOUND->FREE( ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_NFE_INBOUND.

  IF DG_SPLITTER_P IS NOT INITIAL.
    DG_SPLITTER_P->FREE( ).
  ENDIF.
  CLEAR: DG_SPLITTER_P.

  "IF CTL_CCCONTAINER IS NOT INITIAL.
  "  CTL_CCCONTAINER->FREE( ).
  "ENDIF.
  "CLEAR: CTL_CCCONTAINER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  APRESENTAR_INFORMACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APRESENTAR_INFORMACAO .

  CLEAR: IT_NFE_ALV[].

  MOVE IT_NFE_DIST[] TO IT_NFE.

  DATA(IT_ALV) = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).

  LOOP AT IT_ALV INTO DATA(WA_ALV).
    MOVE-CORRESPONDING WA_ALV TO WA_NFE_ALV.
    APPEND WA_NFE_ALV TO IT_NFE_ALV.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_VISAO_NFE_INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIAR_VISAO_NFE_INBOUND .


  "IF CTL_CCCONTAINER IS INITIAL.
  "
  "  CREATE OBJECT CTL_CCCONTAINER
  "    EXPORTING
  "      CONTAINER_NAME = 'CT_NFE'.
  "ENDIF.

  PERFORM CRIA_SPLITTER_ALVS_PRINCIPAL USING CTL_CCCONTAINER_NFE_INBOUND.

  PERFORM CRIA_ALVS_PRINCIPAL USING CTL_CCCONTAINER2 CTL_CCCONTAINER3.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_VISAO_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIAR_VISAO_FORNECEDOR .


  IF DG_SPLITTER_P IS INITIAL.

    CREATE OBJECT DG_SPLITTER_P
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = 1
        COLUMNS = 2.

    CALL METHOD DG_SPLITTER_P->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 18.

    CALL METHOD DG_SPLITTER_P->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_TREE.

    CALL METHOD DG_SPLITTER_P->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 2
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_NFE_INBOUND.

    PERFORM CRIAR_VISAO_FORNECEDOR_TREE USING CTL_CCCONTAINER_TREE.

  ENDIF.

  IF IT_NFE_ALV IS NOT INITIAL AND DG_SPLITTER IS INITIAL.

    IF PICTURE IS NOT INITIAL.
      PICTURE->FREE( ).
    ENDIF.
    CLEAR PICTURE.

    PERFORM CRIA_SPLITTER_ALVS_PRINCIPAL USING CTL_CCCONTAINER_NFE_INBOUND.

    PERFORM CRIA_ALVS_PRINCIPAL USING CTL_CCCONTAINER2 CTL_CCCONTAINER3.

  ELSEIF PICTURE IS INITIAL.

    CREATE OBJECT PICTURE
      EXPORTING
        PARENT = CTL_CCCONTAINER_NFE_INBOUND
      EXCEPTIONS
        ERROR  = 1.

    CALL METHOD PICTURE->SET_DISPLAY_MODE
      EXPORTING
        DISPLAY_MODE = PICTURE->DISPLAY_MODE_STRETCH
      EXCEPTIONS
        ERROR        = 1.

    PERFORM LOAD_PIC_FROM_DB.

  ENDIF.

ENDFORM.

##PERF_NO_TYPE
FORM LOAD_PIC_FROM_DB.

  DATA URL(255).
  TYPES PIC_LINE(1022) TYPE X.
  DATA  PIC_TAB TYPE TABLE OF PIC_LINE.

  CLEAR URL.
  URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  C_SERVICE=>GET_PIC_TAB(
    EXPORTING
      MIME_URL = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
    IMPORTING
      PIC_TAB  = PIC_TAB ).

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      TYPE    = 'image'
      SUBTYPE = 'GIF'
    TABLES
      DATA    = PIC_TAB
    CHANGING
      URL     = URL
    EXCEPTIONS
      OTHERS  = 1.

  CALL METHOD PICTURE->LOAD_PICTURE_FROM_URL
    EXPORTING
      URL = URL.

ENDFORM.                               " LOAD_PIC_FROM_DB

CLASS C_SERVICE IMPLEMENTATION.
  METHOD GET_PIC_TAB.
    DATA PIC_WA TYPE XSTRING.
    DATA LENGTH TYPE I.
    DATA MIME_API TYPE REF TO IF_MR_API.
    MIME_API = CL_MIME_REPOSITORY_API=>GET_API( ).
    MIME_API->GET( EXPORTING  I_URL             = MIME_URL
                              I_CHECK_AUTHORITY = ABAP_FALSE
                   IMPORTING  E_CONTENT         = PIC_WA
                   EXCEPTIONS OTHERS            = 4 ).
    IF SY-SUBRC = 4.
      RETURN.
    ENDIF.
    CLEAR PIC_TAB.
    LENGTH = XSTRLEN( PIC_WA ).
    WHILE LENGTH >= 1022.
      APPEND PIC_WA(1022) TO PIC_TAB.
      SHIFT PIC_WA BY 1022 PLACES LEFT IN BYTE MODE.
      LENGTH = XSTRLEN( PIC_WA ).
    ENDWHILE.
    IF LENGTH > 0.
      APPEND PIC_WA TO PIC_TAB.
    ENDIF.
  ENDMETHOD.                    "get_pic_tab
ENDCLASS.                    "c_service IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CRIAR_VISAO_FORNECEDOR_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIAR_VISAO_FORNECEDOR_TREE USING CONTAINER TYPE REF TO CL_GUI_CONTAINER.

  DATA: NODE_TABLE       TYPE TREEV_NTAB,
        ITEM_TABLE       TYPE ITEM_TABLE_TYPE,
        HIERARCHY_HEADER TYPE TREEV_HHDR.

  CHECK CONTAINER IS NOT INITIAL.

  CHECK IT_NFE_DIST[] IS NOT INITIAL.

  CHECK G_TREE IS INITIAL.

  CLEAR: NODE_TABLE,
         ITEM_TABLE.

  HIERARCHY_HEADER-HEADING = 'Fornecedor'.
  HIERARCHY_HEADER-WIDTH   = 50.

  CREATE OBJECT G_TREE
    EXPORTING
      PARENT                      = CONTAINER
      NODE_SELECTION_MODE         = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION              = 'X'
      HIERARCHY_COLUMN_NAME       = C_VS_FORNECEDOR-COLUMN1
      "WITH_HEADERS                = ' '
      HIERARCHY_HEADER            = HIERARCHY_HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6.

  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENTO-APPL_EVENT = 'X'.                                   "
  APPEND EVENTO TO EVENTS.

  " item double click
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENTO-APPL_EVENT = 'X'.
  APPEND EVENTO TO EVENTS.

  " expand no children
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENTO-APPL_EVENT = 'X'.
  APPEND EVENTO TO EVENTS.

  " link click
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  EVENTO-APPL_EVENT = 'X'.
  APPEND EVENTO TO EVENTS.

  " button click
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK.
  EVENTO-APPL_EVENT = 'X'.
  APPEND EVENTO TO EVENTS.

  " checkbox change
  EVENTO-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENTO-APPL_EVENT = 'X'.
  APPEND EVENTO TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

  CREATE OBJECT G_APPLICATION.
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK  FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK  FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK         FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK       FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE    FOR G_TREE.

* Column2
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME                         = C_VS_FORNECEDOR-COLUMN2
      WIDTH                        = 21
      HEADER_TEXT                  = 'CNPJ/CPF'
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

  PERFORM BUILD_NODE_AND_ITEM_TABLE USING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                     = NODE_TABLE
      ITEM_TABLE                     = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
    EXCEPTIONS
      FAILED                         = 1
      CNTL_SYSTEM_ERROR              = 3
      ERROR_IN_TABLES                = 4
      DP_ERROR                       = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.

  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

ENDFORM.

FORM BUILD_NODE_AND_ITEM_TABLE
  USING
    NODE_TABLE TYPE TREEV_NTAB
    ITEM_TABLE TYPE ITEM_TABLE_TYPE.

  DATA: IT_NFE_TEMP   TYPE TABLE OF ZIB_NFE_DIST_TER WITH HEADER LINE,
        IT_NFE_TEMP_F TYPE TABLE OF ZIB_NFE_DIST_TER WITH HEADER LINE,
        IT_NFE_TEMP_I TYPE TABLE OF ZIB_NFE_DIST_ITM WITH HEADER LINE,
        IT_FORNECEDOR TYPE TABLE OF LFA1 WITH HEADER LINE,
        RFORN         TYPE RANGE OF LIFNR,
        WFORN         LIKE LINE OF RFORN,
        QTD_ITENS_T   TYPE I,
        QTD_ITENS_C   TYPE I,
        QTD_ITENS     TYPE I.

  DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM.

  DATA: NODE_1 TYPE TREEV_NODE,
        ITEM_1 TYPE MTREEITM.

  MOVE IT_NFE_DIST[] TO IT_NFE_TEMP[].
  DELETE IT_NFE_TEMP WHERE P_EMISSOR EQ SPACE.
  SORT IT_NFE_TEMP BY P_EMISSOR.

  DESCRIBE TABLE IT_NFE_DIST LINES QTD_ITENS_T.
  DESCRIBE TABLE IT_NFE_TEMP LINES QTD_ITENS_C.

  CLEAR: IT_NFE_ITENS[].

  IF IT_NFE_TEMP[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_NFE_ITENS
      FROM ZIB_NFE_DIST_ITM
       FOR ALL ENTRIES IN IT_NFE_TEMP
     WHERE CHAVE_NFE EQ IT_NFE_TEMP-CHAVE_NFE.
  ENDIF.

  LOOP AT IT_NFE_TEMP.
    IF IT_NFE_TEMP-P_EMISSOR IS NOT INITIAL.
      WFORN-SIGN   = 'I'.
      WFORN-OPTION = 'EQ'.
      WFORN-LOW    = IT_NFE_TEMP-P_EMISSOR.
      WFORN-HIGH   = IT_NFE_TEMP-P_EMISSOR.
      APPEND WFORN TO RFORN.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE RFORN LINES QTD_ITENS.

  IF QTD_ITENS > 100.
    IF RFORN IS NOT INITIAL.
      SELECT * INTO TABLE IT_FORNECEDOR
        FROM LFA1
       WHERE LIFNR IN RFORN
       ORDER BY NAME1.
    ENDIF.
  ELSE.
    SELECT * INTO TABLE IT_FORNECEDOR
      FROM LFA1
     ORDER BY NAME1.
  ENDIF.

  QTD_ITENS = 1.

  "Fonecedores """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  LOOP AT IT_FORNECEDOR.

    READ TABLE IT_NFE_TEMP WITH KEY P_EMISSOR = IT_FORNECEDOR-LIFNR BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: NODE-RELATKEY,  " Special case: A root node has no parent
           NODE-RELATSHIP, " node.
           NODE-N_IMAGE,   " Folder-/ Leaf-Symbol in state "closed": " use default
           NODE-EXP_IMAGE, " Folder-/ Leaf-Symbol in state "open": " use default
           NODE-EXPANDER.  " see below.

    CLEAR: IT_TREE.
    IT_TREE-NUMERO = QTD_ITENS.
    IT_TREE-LIFNR  = IT_FORNECEDOR-LIFNR.
    APPEND IT_TREE.

    "Node with key 'Root'
    MOVE QTD_ITENS TO NODE-NODE_KEY.
    CONDENSE NODE-NODE_KEY NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING INPUT = NODE-NODE_KEY IMPORTING OUTPUT = NODE-NODE_KEY.

    NODE-NODE_KEY = QTD_ITENS.
    NODE-HIDDEN   = ' '.                 " The node is visible,
    NODE-DISABLED = ' '.                 " selectable,
    NODE-ISFOLDER = 'X'.                 " a folder.
    NODE-EXPANDER = 'X'.
    NODE-N_IMAGE   = ICON_CUSTOMER.
    NODE-EXP_IMAGE = ICON_CUSTOMER.
    APPEND NODE TO NODE_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY  = NODE-NODE_KEY.
    ITEM-ITEM_NAME = C_VS_FORNECEDOR-COLUMN1.
    ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_CLASS_LINK. " Text Item
    " the with of the item is adjusted to its content (text)
    ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
    " use proportional font for the item
    ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
    ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
    ITEM-TEXT      = IT_FORNECEDOR-NAME1.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY  = NODE-NODE_KEY.
    ITEM-ITEM_NAME = C_VS_FORNECEDOR-COLUMN2.
    ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
    " the with of the item is adjusted to its content (text)
    ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
    " use proportional font for the item
    ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_DEFAULT.
    ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
    CASE IT_FORNECEDOR-STKZN.
      WHEN ABAP_FALSE.
        ITEM-TEXT      = IT_FORNECEDOR-STCD1.
      WHEN ABAP_TRUE.
        ITEM-TEXT      = IT_FORNECEDOR-STCD2.
    ENDCASE.
    APPEND ITEM TO ITEM_TABLE.

    ADD 1 TO QTD_ITENS.

    MOVE IT_NFE_TEMP[] TO IT_NFE_TEMP_F[].
    DELETE IT_NFE_TEMP_F WHERE P_EMISSOR NE IT_FORNECEDOR-LIFNR.

    "Pegar Pedios de Compra
    CLEAR: IT_NFE_TEMP_I[].
    "Notas do Emissor
    IF IT_NFE_TEMP_F[] IS NOT INITIAL.
      LOOP AT IT_NFE_TEMP_F.
        "Itens das notas
        LOOP AT IT_NFE_ITENS WHERE CHAVE_NFE EQ IT_NFE_TEMP_F-CHAVE_NFE.
          APPEND IT_NFE_ITENS TO IT_NFE_TEMP_I.
        ENDLOOP.
      ENDLOOP.
      SORT IT_NFE_TEMP_I BY EBELN.
      DELETE ADJACENT DUPLICATES FROM IT_NFE_TEMP_I COMPARING EBELN.
    ENDIF.

    IF IT_NFE_TEMP_I[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_EKKO)
        FROM EKKO
         FOR ALL ENTRIES IN @IT_NFE_TEMP_I
       WHERE EBELN EQ @IT_NFE_TEMP_I-EBELN.

      SORT IT_EKKO BY EBELN ASCENDING.
    ENDIF.

    "Pedidos """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT IT_EKKO INTO DATA(WA_EKKO).
      CLEAR: NODE_1-RELATKEY,  " Special case: A root node has no parent
             NODE_1-RELATSHIP, " node.
             NODE_1-N_IMAGE,   " Folder-/ Leaf-Symbol in state "closed": " use default
             NODE_1-EXP_IMAGE, " Folder-/ Leaf-Symbol in state "open": " use default
             NODE_1-EXPANDER.  " see below.

      CLEAR: IT_TREE.
      IT_TREE-NUMERO = QTD_ITENS.
      IT_TREE-LIFNR  = IT_FORNECEDOR-LIFNR.
      IT_TREE-EBELN  = WA_EKKO-EBELN.
      APPEND IT_TREE.

      "Node with key 'Root'
      MOVE QTD_ITENS TO NODE_1-NODE_KEY.
      CONDENSE NODE_1-NODE_KEY NO-GAPS.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING INPUT = NODE_1-NODE_KEY IMPORTING OUTPUT = NODE_1-NODE_KEY.

      NODE_1-RELATKEY  = NODE-NODE_KEY.
      NODE_1-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      NODE_1-HIDDEN    = ' '.                 " The node is visible,
      NODE_1-DISABLED  = ' '.                 " selectable,
      NODE_1-ISFOLDER  = 'X'.                 " a folder.
      NODE_1-N_IMAGE   = ICON_DELIVERY.
      NODE_1-EXP_IMAGE = ICON_DELIVERY.
      APPEND NODE_1 TO NODE_TABLE.

      CLEAR ITEM_1.
      ITEM_1-NODE_KEY  = NODE_1-NODE_KEY.
      ITEM_1-ITEM_NAME = C_VS_FORNECEDOR-COLUMN1.
      ITEM_1-CLASS     = CL_GUI_LIST_TREE=>ITEM_CLASS_LINK. " Text Item
      " the with of the item is adjusted to its content (text)
      ITEM_1-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      " use proportional font for the item
      ITEM-STYLE       = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
      ITEM_1-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      CONCATENATE WA_EKKO-EBELN WA_EKKO-ERNAM INTO ITEM_1-TEXT SEPARATED BY SPACE.
      APPEND ITEM_1 TO ITEM_TABLE.
      ADD 1 TO QTD_ITENS.
    ENDLOOP.

    "Sem Pedido """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT IT_NFE_TEMP_I WHERE EBELN IS INITIAL.
      CLEAR: NODE_1-RELATKEY,  " Special case: A root node has no parent
             NODE_1-RELATSHIP, " node.
             NODE_1-N_IMAGE,   " Folder-/ Leaf-Symbol in state "closed": " use default
             NODE_1-EXP_IMAGE, " Folder-/ Leaf-Symbol in state "open": " use default
             NODE_1-EXPANDER.  " see below.

      CLEAR: IT_TREE.
      IT_TREE-NUMERO = QTD_ITENS.
      IT_TREE-LIFNR  = IT_FORNECEDOR-LIFNR.
      IT_TREE-EBELN  = '9999999999'.
      APPEND IT_TREE.

      "Node with key 'Root'
      MOVE QTD_ITENS TO NODE_1-NODE_KEY.
      CONDENSE NODE_1-NODE_KEY NO-GAPS.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING INPUT = NODE_1-NODE_KEY IMPORTING OUTPUT = NODE_1-NODE_KEY.

      NODE_1-RELATKEY  = NODE-NODE_KEY.
      NODE_1-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      NODE_1-HIDDEN    = ' '.                 " The node is visible,
      NODE_1-DISABLED  = ' '.                 " selectable,
      NODE_1-ISFOLDER  = 'X'.                 " a folder.
      NODE_1-N_IMAGE   = ICON_WF_WORKITEM_COMPLETED.
      NODE_1-EXP_IMAGE = ICON_WF_WORKITEM_COMPLETED.
      APPEND NODE_1 TO NODE_TABLE.

      CLEAR ITEM_1.
      ITEM_1-NODE_KEY  = NODE_1-NODE_KEY.
      ITEM_1-ITEM_NAME = C_VS_FORNECEDOR-COLUMN1.
      ITEM_1-CLASS     = CL_GUI_LIST_TREE=>ITEM_CLASS_LINK. " Text Item
      " the with of the item is adjusted to its content (text)
      ITEM_1-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      " use proportional font for the item
      ITEM-STYLE       = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
      ITEM_1-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      ITEM_1-TEXT      = 'Sem Pedido Determinado'.
      APPEND ITEM_1 TO ITEM_TABLE.
      ADD 1 TO QTD_ITENS.
    ENDLOOP.

  ENDLOOP.

  "Sem Fornecedor e Pedido """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF QTD_ITENS_T NE QTD_ITENS_C.
    CLEAR: NODE-RELATKEY,  " Special case: A root node has no parent
           NODE-RELATSHIP, " node.
           NODE-N_IMAGE,   " Folder-/ Leaf-Symbol in state "closed": " use default
           NODE-EXP_IMAGE, " Folder-/ Leaf-Symbol in state "open": " use default
           NODE-EXPANDER.  " see below.

    CLEAR: IT_TREE.
    IT_TREE-NUMERO = QTD_ITENS.
    IT_TREE-LIFNR  = '9999999999'.
    IT_TREE-EBELN  = '9999999999'.
    APPEND IT_TREE.

    "Node with key 'Root'
    MOVE QTD_ITENS TO NODE-NODE_KEY.
    CONDENSE NODE-NODE_KEY NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING INPUT = NODE-NODE_KEY IMPORTING OUTPUT = NODE-NODE_KEY.

    NODE-HIDDEN    = ' '.                 " The node is visible,
    NODE-DISABLED  = ' '.                 " selectable,
    NODE-ISFOLDER  = ' '.                 " a folder.
    NODE-EXPANDER  = ' '.
    NODE-N_IMAGE   = ICON_WF_WORKITEM_ERROR.
    NODE-EXP_IMAGE = ICON_WF_WORKITEM_ERROR.
    APPEND NODE TO NODE_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY  = NODE-NODE_KEY.
    ITEM-ITEM_NAME = C_VS_FORNECEDOR-COLUMN1.
    ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_CLASS_LINK. " Text Item
*  " the with of the item is adjusted to its content (text)
    ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
*  " use proportional font for the item
    ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
    ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
    ITEM-TEXT      = 'Sem Fornecedor Determinado'.
    APPEND ITEM TO ITEM_TABLE.
    ADD 1 TO QTD_ITENS.
  ENDIF.

  SORT IT_TREE BY NUMERO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
FORM MOSTRAR_NODE  USING  P_NODE_KEY  TYPE  TV_NODEKEY
                          P_ITEM_NAME TYPE  TV_ITMNAME.

  DATA: IT_ALV          TYPE ZDE_NFE_DIST_ALV_T,
        WA_ALV          TYPE ZDE_NFE_DIST_ALV,
        CK_ACHOU_PEDIDO TYPE C LENGTH 1.

  CASE OK_VISAO.
    WHEN OK_VS_FORNECE.

      IT_TREE-NUMERO = P_NODE_KEY.
      READ TABLE IT_TREE WITH KEY NUMERO = IT_TREE-NUMERO INTO DATA(WA_TREE) BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        CASE WA_TREE-LIFNR.
          WHEN '9999999999'.
            CLEAR: IT_NFE_ALV[].
            MOVE IT_NFE_DIST[] TO IT_NFE.
            DELETE IT_NFE WHERE P_EMISSOR NE SPACE.
            IT_ALV = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).
            LOOP AT IT_ALV INTO WA_ALV.
              MOVE-CORRESPONDING WA_ALV TO WA_NFE_ALV.
              APPEND WA_NFE_ALV TO IT_NFE_ALV.
            ENDLOOP.
          WHEN OTHERS.
            CASE WA_TREE-EBELN.
              WHEN SPACE.
                "Geral
                CLEAR: IT_NFE_ALV[].
                MOVE IT_NFE_DIST[] TO IT_NFE.
                DELETE IT_NFE WHERE P_EMISSOR NE WA_TREE-LIFNR.
                IT_ALV = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).
                LOOP AT IT_ALV INTO WA_ALV.
                  MOVE-CORRESPONDING WA_ALV TO WA_NFE_ALV.
                  APPEND WA_NFE_ALV TO IT_NFE_ALV.
                ENDLOOP.
              WHEN '9999999999'.
                "Sem Pedido
                CLEAR: IT_NFE_ALV[].
                MOVE IT_NFE_DIST[] TO IT_NFE.
                DELETE IT_NFE WHERE P_EMISSOR NE WA_TREE-LIFNR.
                IT_ALV = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).
                LOOP AT IT_ALV INTO WA_ALV.
                  CK_ACHOU_PEDIDO = ABAP_FALSE.
                  LOOP AT IT_NFE_ITENS WHERE CHAVE_NFE = WA_ALV-CHAVE_NFE AND EBELN EQ SPACE.
                    CK_ACHOU_PEDIDO = ABAP_TRUE.
                  ENDLOOP.
                  IF CK_ACHOU_PEDIDO EQ ABAP_TRUE.
                    MOVE-CORRESPONDING WA_ALV TO WA_NFE_ALV.
                    APPEND WA_NFE_ALV TO IT_NFE_ALV.
                  ENDIF.
                ENDLOOP.
              WHEN OTHERS.
                "Com Pedido
                CLEAR: IT_NFE_ALV[].
                MOVE IT_NFE_DIST[] TO IT_NFE.
                DELETE IT_NFE WHERE P_EMISSOR NE WA_TREE-LIFNR.
                "Filtrar Pedido
                IT_ALV = OBJ_NFE->AT_NFE_INBOUND->GET_NFE_INBOUND_ALV_SAIDA( EXPORTING I_NFE_DIST = IT_NFE ).
                LOOP AT IT_ALV INTO WA_ALV.
                  CK_ACHOU_PEDIDO = ABAP_FALSE.
                  LOOP AT IT_NFE_ITENS WHERE CHAVE_NFE = WA_ALV-CHAVE_NFE AND EBELN EQ WA_TREE-EBELN.
                    IF CK_ACHOU_PEDIDO EQ ABAP_FALSE.
                      MOVE-CORRESPONDING WA_ALV TO WA_NFE_ALV.
                      APPEND WA_NFE_ALV TO IT_NFE_ALV.
                      CK_ACHOU_PEDIDO = ABAP_TRUE.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.
            ENDCASE.
        ENDCASE.
      ENDIF.
  ENDCASE.

  IF IT_NFE_ALV[] IS NOT INITIAL.
    PERFORM ATUALIZA_TELA.

    CHECK CTL_ALV_NFE IS NOT INITIAL.
    LEAVE TO SCREEN 0100.
  ENDIF.

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

  DATA: IT_DD07V TYPE TABLE OF DD07V WITH HEADER LINE.

  CLEAR: IT_EXCEPT_QINFO.

  "Informações Documento
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_ST_NFE_DOCUMENTO'
    TABLES
      VALUES_TAB = IT_DD07V.

  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    WA_EXCEPT_QINFO-VALUE = ZCL_NFE_INBOUND=>GET_ICON_STATUS_DOCUMENTO( I_STATUS = CONV #( IT_DD07V-DOMVALUE_L ) ).
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_NFE_DIST_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_ST_DOCUMENTO'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

  "Informações Fiscais
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_ST_NFE_FISCAL'
    TABLES
      VALUES_TAB = IT_DD07V.

  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    WA_EXCEPT_QINFO-VALUE = ZCL_NFE_INBOUND=>GET_ICON_STATUS_FISCAL( I_STATUS = CONV #( IT_DD07V-DOMVALUE_L ) ).
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_NFE_DIST_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_ST_FISCAL'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

  "Informações Físicas
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_ST_NFE_FISICO'
    TABLES
      VALUES_TAB = IT_DD07V.

  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    WA_EXCEPT_QINFO-VALUE = ZCL_NFE_INBOUND=>GET_ICON_STATUS_FISCAL( I_STATUS = CONV #( IT_DD07V-DOMVALUE_L ) ).
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_NFE_DIST_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_ST_FISICO'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

  "Informações Armazenagem
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME    = 'ZDM_ST_NFE_ARMAZEM'
    TABLES
      VALUES_TAB = IT_DD07V.

  LOOP AT IT_DD07V WHERE DOMVALUE_L IS NOT INITIAL.
    WA_EXCEPT_QINFO-TYPE  = CL_SALV_TOOLTIP=>C_TYPE_SYMBOL.
    WA_EXCEPT_QINFO-VALUE = ZCL_NFE_INBOUND=>GET_ICON_STATUS_ARMAZEM( I_STATUS = CONV #( IT_DD07V-DOMVALUE_L ) ).
    WA_EXCEPT_QINFO-TEXT  = IT_DD07V-DDTEXT.
    WA_EXCEPT_QINFO-TABNAME   = 'ZDE_NFE_DIST_ALV'.
    WA_EXCEPT_QINFO-FIELDNAME = 'ICO_ST_ARMAZEM'.
    APPEND WA_EXCEPT_QINFO TO IT_EXCEPT_QINFO.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALVS_PRINCIPAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CTL_CCCONTAINER2  text
*      -->P_CTL_CCCONTAINER3  text
*----------------------------------------------------------------------*
FORM CRIA_ALVS_PRINCIPAL  USING    P_CCCONTAINER1 TYPE REF TO CL_GUI_CONTAINER
                                   P_CCCONTAINER2 TYPE REF TO CL_GUI_CONTAINER.
  IF CTL_ALV_NFE IS INITIAL.

    CREATE OBJECT CTL_ALV_NFE
      EXPORTING
        I_PARENT = P_CCCONTAINER1.

    PERFORM FILL_IT_FIELDCATALOG.

    "Hints
    PERFORM FILL_IT_HINTS.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

*   Set layout parameters for ALV grid
    GS_LAYOUT-GRID_TITLE = TEXT-100.
    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-INFO_FNAME = 'ROWCOLOR'.
    GS_LAYOUT-ZEBRA      = ABAP_TRUE.

    CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CALL METHOD CTL_ALV_NFE->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
        IT_EXCEPT_QINFO      = IT_EXCEPT_QINFO
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_NFE_ALV.

    CREATE OBJECT EVENT_HANDLER.
    SET HANDLER EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_NFE.
    SET HANDLER EVENT_HANDLER->HANDLE_DOUBLE_CLICK  FOR CTL_ALV_NFE.
  ENDIF.

  IF CK_LOG_ATIVO EQ ABAP_TRUE AND CTL_CCCONTAINER2 IS NOT INITIAL AND CTL_ALV_NFE_HIST IS INITIAL.

    CREATE OBJECT CTL_ALV_NFE_HIST
      EXPORTING
        I_PARENT = P_CCCONTAINER2.

    PERFORM FILL_IT_FIELDCATALOG2.

*   Fill info for layout variant2
    PERFORM FILL_GS_VARIANT2.

    CREATE OBJECT OBG_TOOLBAR_LOG
      EXPORTING
        IO_ALV_GRID = CTL_ALV_NFE_HIST.

    SET HANDLER OBG_TOOLBAR_LOG->ON_TOOLBAR FOR CTL_ALV_NFE_HIST.
    SET HANDLER OBG_TOOLBAR_LOG->HANDLE_USER_COMMAND FOR CTL_ALV_NFE_HIST.

    CALL METHOD CTL_ALV_NFE_HIST->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT2
        IS_VARIANT           = GS_VARIANT2
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG2
        IT_OUTTAB            = IT_LOG_ALV.

    CREATE OBJECT EVENT_HANDLER_LOG.
    SET HANDLER EVENT_HANDLER_LOG->HANDLE_HOTSPOT_CLICK FOR CTL_ALV_NFE_HIST.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_SPLITTER_ALVS_PRINCIPAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CTL_CCCONTAINER_NFE_INBOUND  text
*----------------------------------------------------------------------*
FORM CRIA_SPLITTER_ALVS_PRINCIPAL  USING P_CCCONTAINER TYPE REF TO CL_GUI_CONTAINER.

  DATA: I_ROWS TYPE	I.

  "CTL_CCCONTAINER

  IF CK_LOG_ATIVO EQ ABAP_FALSE.
    I_ROWS = 1.
  ELSE.
    I_ROWS = 2.
  ENDIF.

  IF P_CCCONTAINER IS NOT INITIAL AND DG_SPLITTER IS INITIAL.
    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = P_CCCONTAINER
        ROWS    = I_ROWS
        COLUMNS = 1.
  ELSEIF DG_SPLITTER IS INITIAL.
    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0
        ROWS    = I_ROWS
        COLUMNS = 1.
  ENDIF.

  IF CTL_CCCONTAINER2 IS INITIAL.
    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER2.
  ENDIF.

  IF CK_LOG_ATIVO EQ ABAP_TRUE AND CTL_CCCONTAINER3 IS INITIAL AND DG_SPLITTER IS NOT INITIAL.

    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER3.

    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 2
        HEIGHT = 20.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  OCULTAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OCULTAR_LOG .
  CK_LOG_ATIVO = ABAP_FALSE.
  PERFORM LIMPAR_CONTROLES_TELA USING ABAP_TRUE.
  LEAVE TO SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONITORA_TAXA_CAMBIO
*&---------------------------------------------------------------------*
FORM MONITORA_TAXA_CAMBIO .

  DATA: IT_NFE_TAXA   TYPE TABLE OF ZIB_NFE_DIST_TER WITH HEADER LINE.

  CHECK IT_NFE_DIST[] IS NOT INITIAL.

  MOVE IT_NFE_DIST[] TO  IT_NFE_TAXA[].

  IF IT_NFE_TAXA[] IS NOT INITIAL.
    SELECT * INTO TABLE IT_ZMMT0149
      FROM ZMMT0149
       FOR ALL ENTRIES IN IT_NFE_TAXA
     WHERE CHAVE_NFE EQ IT_NFE_TAXA-CHAVE_NFE.

    IF IT_ZMMT0149[] IS NOT INITIAL.
      LOOP AT IT_NFE_TAXA INTO DATA(WA_NFE_TAXA).
        IF WA_NFE_TAXA-BELNR IS INITIAL.
          READ TABLE IT_ZMMT0149 INTO DATA(WA_ZMMT0149) WITH KEY CHAVE_NFE = WA_NFE_TAXA-CHAVE_NFE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_APROVACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOG_APROVACAO .

  TYPES:

    BEGIN OF TY_LOG.
      INCLUDE TYPE ZMMT0151.
  TYPES: VDATA(10),
      VHORA(10),
      V_DE(15),
      V_ATE(15),
      MSG_LOTE  TYPE CHAR80,
    END OF TY_LOG.


  DATA: LINHA_SELECIONADA TYPE SLIS_SELFIELD.
  DATA: _EXIT             TYPE C.

  DATA: IT_ZMMT0151    TYPE TABLE OF TY_LOG,
        WA_ZMMT0151    TYPE ZMMT0151,
        Z_ENDPOS_COL   TYPE INT4,
        Z_ENDPOS_ROW   TYPE INT4,
        Z_STARTPOS_COL TYPE INT4,
        Z_STARTPOS_ROW TYPE INT4.

  DATA: BEGIN OF ITAB OCCURS 0,
          NAME(255) TYPE C,
*          name TYPE string,
        END OF ITAB.
  DATA: VDATA(10),VHORA(10),V_DE(15),V_ATE(15), MSG_LOTE TYPE CHAR80.

  LOOP AT IT_NFE_SELECT INTO DATA(WA_NFE_DANFE).

    SELECT *
       FROM ZMMT0151
       INTO CORRESPONDING FIELDS OF TABLE IT_ZMMT0151
    WHERE  CHAVE_NFE  =  WA_NFE_DANFE-CHAVE_NFE.

    LOOP AT IT_ZMMT0151 ASSIGNING FIELD-SYMBOL(<WA_ZMMT0151>).

      CONCATENATE <WA_ZMMT0151>-HORA_ATUAL+0(2) <WA_ZMMT0151>-HORA_ATUAL+2(2) <WA_ZMMT0151>-HORA_ATUAL+4(2) INTO <WA_ZMMT0151>-VHORA SEPARATED BY ':'.
      CONCATENATE <WA_ZMMT0151>-DATA_ATUAL+6(2) <WA_ZMMT0151>-DATA_ATUAL+4(2) <WA_ZMMT0151>-DATA_ATUAL+0(4) INTO <WA_ZMMT0151>-VDATA SEPARATED BY '.'.

    ENDLOOP.

    CONCATENATE 'Chave NFe ' WA_NFE_DANFE-CHAVE_NFE INTO MSG_LOTE SEPARATED BY SPACE.

    IF ( IT_ZMMT0151 IS NOT INITIAL ).

      DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

      ( FIELDNAME = 'NIVEL     '        SELTEXT_M = TEXT-P02  OUTPUTLEN = '07' )
      ( FIELDNAME = 'APROVADOR '        SELTEXT_M = TEXT-P03  OUTPUTLEN = '12' )
      ( FIELDNAME = 'VDATA     '        SELTEXT_M = TEXT-P04  OUTPUTLEN = '12' )
      ( FIELDNAME = 'VHORA     '        SELTEXT_M = TEXT-P05  OUTPUTLEN = '12' )
      ( FIELDNAME = 'VALOR_DE  '        SELTEXT_M = TEXT-A01  OUTPUTLEN = '15' )
      ( FIELDNAME = 'VALOR_ATE '        SELTEXT_M = TEXT-A02  OUTPUTLEN = '15' )
      ( FIELDNAME = 'INFO_WKURS'        SELTEXT_M = TEXT-015  OUTPUTLEN = '16' )
      ( FIELDNAME = 'CALC_WKURS'        SELTEXT_M = TEXT-016  OUTPUTLEN = '16' )
      ( FIELDNAME = 'DESVIO    '        SELTEXT_M = TEXT-017  OUTPUTLEN = '10' )
      ( FIELDNAME = 'MOTIVO    '        SELTEXT_M = TEXT-018  OUTPUTLEN = '10' ) ).

      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          I_TITLE               = MSG_LOTE
          I_SELECTION           = 'X'
          I_TABNAME             = 'it_zmmt0151'
          I_SCREEN_START_COLUMN = 12
          I_ZEBRA               = 'X'
          I_SCROLL_TO_SEL_LINE  = 'X'
          IT_FIELDCAT           = TL_FIELDCAT
        IMPORTING
          ES_SELFIELD           = LINHA_SELECIONADA
          E_EXIT                = _EXIT
        TABLES
          T_OUTTAB              = IT_ZMMT0151.

    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET TITLEBAR '0101'.
  SET PF-STATUS '0101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS_0101 OUTPUT.

  DATA:
    TL_FILTER   TYPE LVC_T_FILT,
    WL_FILTER   TYPE LVC_S_FILT,
    TL_FUNCTION TYPE UI_FUNCTIONS,
    WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE,
    WA_LAYOUT   TYPE LVC_S_LAYO,
    WA_STABLE   TYPE LVC_S_STBL.


  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_TOOLBAR = 'X'.
  WA_LAYOUT-NO_ROWMARK = 'X'.
  WA_STABLE-ROW        = 'X'.
  WA_LAYOUT-GRID_TITLE = ' '.

  "GRID2
  IF OBG_CONTEINER_ESTRA IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_ESTRA
      EXPORTING
        CONTAINER_NAME = G_CC_ESTRA.


    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = OBG_CONTEINER_ESTRA.


    PERFORM MONTAR_LAYOUT_ESTRA.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    WA_LAYOUT-NO_TOOLBAR = SPACE.
    WA_LAYOUT-STYLEFNAME = 'STYLE2'.
    WA_LAYOUT-GRID_TITLE = TEXT-T01.
    WA_LAYOUT-NO_TOOLBAR = 'X'.
    PERFORM MONTAR_LAYOUT_ESTRA.

    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ESTRA[].

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAIR' OR 'EXIT'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_ESTRA .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZGLT037'    'NIVEL'       'TG_ESTRA'    'NIVEL'              TEXT-P02               '05' ' ' ' ' ' ',
        1 'ZGLT037'    'VALOR_DE'    'TG_ESTRA'    'VALOR_DE'           TEXT-A01               '15' ' ' ' ' ' ',
        1 'ZGLT037'    'VALOR_ATE'   'TG_ESTRA'    'VALOR_ATE'          TEXT-A02               '15' ' ' ' ' ' ',
        1 'ZGLT037'    'APROVADOR'   'TG_ESTRA'    'APROVADOR'          TEXT-A03               '20' ' ' ' ' ' '.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            P_SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

*  IF p_field EQ 'OPCOES'. " OR P_FIELD EQ 'DOC_LCTO'.
*    w_fieldcatalog-hotspot = c_x.
*  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  ESTRATEGIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ESTRATEGIA .
  DATA:  TABIX     TYPE SY-TABIX.

  DATA: V_MSG   TYPE CHAR50,
        T_LOTES TYPE TABLE OF ZMMT0149,
        W_LOTES TYPE          ZMMT0149,
        T_ESTRA TYPE TABLE OF ZMM_ESTRATEGIA_TAXA,
        W_ESTRA TYPE          ZMM_ESTRATEGIA_TAXA.


  LOOP AT IT_NFE_SELECT INTO DATA(WA_NFE_DANFE).


    REFRESH: T_LOTES, T_ESTRA, T_DOCS.
    CALL FUNCTION 'Z_TX_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO   = SY-UNAME
        V_CHAVE_NFE = WA_NFE_DANFE-CHAVE_NFE
      IMPORTING
        MSG         = V_MSG
      TABLES
        T_LOTES     = T_LOTES
        T_ESTRA     = T_ESTRA
        T_DOCS      = T_DOCS.

    REFRESH TG_ESTRA.
    LOOP AT T_ESTRA INTO W_ESTRA.
      MOVE-CORRESPONDING W_ESTRA TO WG_ESTRA.
      APPEND WG_ESTRA TO TG_ESTRA.
    ENDLOOP.
  ENDLOOP.

  CALL SCREEN 0101 STARTING AT 050 3
                   ENDING   AT 165 12.
ENDFORM.
