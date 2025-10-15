*&---------------------------------------------------------------------*
*&  Include           ZLES0068SEL
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
    METHODS: HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_INTERACTIVE.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click


  METHOD HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar


ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING    I_ROW_ID     TYPE LVC_S_ROW
                                    I_COLUMN_ID  TYPE LVC_S_COL
                                    IS_ROW_NO    TYPE LVC_S_ROID.
  CLEAR: WA_SAIDA_PAG.

  READ TABLE IT_SAIDA_PAG INDEX I_ROW_ID INTO WA_SAIDA_PAG.

  CASE I_COLUMN_ID.

    WHEN: 'SELECT'.

      IF ( WA_SAIDA_PAG-SELECT EQ 'X' ) AND ( WA_SAIDA_PAG-LOTE_PGTO IS INITIAL ).

        WA_SAIDA_PAG-SELECT = SPACE.
        WA_SAIDA_PAG-COR_LINHA = ''.
        T_LOTE   = T_LOTE - WA_SAIDA_PAG-VLR_FRETE.
        T_COMI_P = T_COMI_P - WA_SAIDA_PAG-VLR_COMISSAO.
        MODIFY IT_SAIDA_PAG INDEX I_ROW_ID FROM WA_SAIDA_PAG TRANSPORTING SELECT COR_LINHA.
        CALL METHOD CL_GRID_PAG->REFRESH_TABLE_DISPLAY.

        LEAVE TO SCREEN 0100.
      ELSEIF ( WA_SAIDA_PAG-SELECT IS INITIAL ) AND ( WA_SAIDA_PAG-LOTE_PGTO IS INITIAL ) .

        WA_SAIDA_PAG-SELECT = 'X'.
        WA_SAIDA_PAG-COR_LINHA = 'C610'.
        T_LOTE = T_LOTE + WA_SAIDA_PAG-VLR_FRETE.
        T_COMI_P = T_COMI_P + WA_SAIDA_PAG-VLR_COMISSAO.
        MODIFY IT_SAIDA_PAG INDEX I_ROW_ID FROM WA_SAIDA_PAG TRANSPORTING SELECT COR_LINHA.
        CALL METHOD CL_GRID_PAG->REFRESH_TABLE_DISPLAY.

        LEAVE TO SCREEN 0100.

      ELSE.
        MESSAGE S888(SABAPDOCU) WITH 'Registro já selecionado e gerado!' DISPLAY LIKE 'W'.

      ENDIF..
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR  USING    P_OBJECT      TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                P_INTERACTIVE TYPE CHAR1 .

  DATA: SL_TOOLBAR TYPE STB_BUTTON.

  CONSTANTS: C_SEPARADOR TYPE I VALUE 3,
             C_CHECK     TYPE I VALUE 5.

  MOVE C_SEPARADOR TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR: SL_TOOLBAR.
  MOVE: C_CHECK              TO SL_TOOLBAR-BUTN_TYPE,
        'TODOS'              TO SL_TOOLBAR-FUNCTION ,
         ICON_WD_CHECK_BOX   TO SL_TOOLBAR-ICON     ,
         'Selec. Todos'      TO SL_TOOLBAR-QUICKINFO,
         'Selec. Todos'      TO SL_TOOLBAR-TEXT,
         SPACE               TO SL_TOOLBAR-DISABLED .

  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.                    "Z_HANDLE_TOOLBAR

*----------------------------------------------------------------------*
*       CLASS lcl_sel_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS LCL_SEL_VIEW DEFINITION.

  PUBLIC SECTION.
    METHODS: SELECIONA_DADOS.

  PRIVATE SECTION.

    METHODS: MODIFICA_WERKS.

    METHODS: INPUT_ICON.

    METHODS: CAMPOS_TELA.

    METHODS: CALL_TELA01.

    METHODS: CONTAINER_PAG.

    METHODS: SAIDA_PAG.

    METHODS: CATALOG_PAG.

ENDCLASS.                    "lcl_sel_view DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_sel_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_SEL_VIEW IMPLEMENTATION.

  "------------------------
  " Método responsavel selecionar todas as informações de saída.
  "-------------------------

  METHOD SELECIONA_DADOS.

    "------------------------
    " Select responsavel por selecionar Filiais/Centro
    "-------------------------

    SELECT VKORG WERKS
      FROM T001W
      INTO TABLE IT_T001W
    WHERE VKORG IN P_VKORG.

    CHECK NOT IT_T001W[] IS INITIAL.

    "------------------------
    " Método responsavel por modificar o tipo de dados WERKS.
    "-------------------------

    MODIFICA_WERKS( ).

    "------------------------
    " Select responsavel por selecionar o código identificar de operação de transporte (CIOT).
    "-------------------------

    SELECT MANDT CD_CIOT ID_OP_VIAGEM_ADM NR_CIOT ST_CIOT RNTRC NUCONTRATO UF_ORIGEM
           MUNICIPIO_ORIGEM DT_ORIGEM UF_TERMIN MUNICIPIO_TERMIN DT_TERMIN DOCNUM
           TKNUM EMISSOR CANCELADO CT_CODIGO CT_NOME VLR_FRETE LOTE
          FROM ZCTE_CIOT
          INTO TABLE IT_ZCTE_CIOT
          FOR ALL ENTRIES IN IT_T001W
     WHERE EMISSOR  EQ IT_T001W-WERKS_AUX
           AND DT_TERMIN IN P_DATA.

    CHECK NOT IT_ZCTE_CIOT[] IS INITIAL. " Verifica se a seleção contem dados do CIOT.

    "------------------------
    " Select responsavel por selecionar itens do transporte.
    "-------------------------

    SELECT TKNUM VBELN
      FROM VTTP
      INTO TABLE IT_VTTP
      FOR ALL ENTRIES IN IT_ZCTE_CIOT
    WHERE TKNUM EQ IT_ZCTE_CIOT-TKNUM.

    "------------------------
    " Select responsavel por selecionar Documento SD: fornecimento: dados de item
    "-------------------------

    SELECT VBELN MATNR WERKS  LFIMG
      FROM LIPS
      INTO TABLE IT_LIPS
      FOR ALL ENTRIES IN IT_VTTP
    WHERE VBELN EQ IT_VTTP-VBELN.

    "------------------------
    " Select responsavel por selecionar Textos breves de material
    "-------------------------
    SELECT MAKTX MATNR
      FROM MAKT
      INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_LIPS
   WHERE MATNR EQ IT_LIPS-MATNR
     AND SPRAS EQ SY-LANGU.

    "------------------------
    " Select responsavel por selecionar Cabeçalho transporte.
    "-------------------------

    SELECT TKNUM SHTYP
      FROM VTTK
      INTO TABLE IT_VTTK
      FOR ALL ENTRIES IN IT_ZCTE_CIOT
    WHERE TKNUM EQ IT_ZCTE_CIOT-TKNUM.

    CHECK NOT IT_VTTK[] IS INITIAL.

    SELECT SPRAS SHTYP BEZEI
       FROM TVTKT
       INTO TABLE IT_TVTKT
       FOR ALL ENTRIES IN IT_VTTK
     WHERE SHTYP EQ IT_VTTK-SHTYP
       AND SPRAS EQ 'PT'.

    "------------------------
    " Select responsavel por selecionar Controle de tipo de operação.
    " DESPESAS DE VENDA.
    "-------------------------

    SELECT TP_OPER SAKNR HKONT SHTYP
      FROM ZCOT0001
      INTO TABLE IT_ZCOT0001_VENDA
      FOR ALL ENTRIES IN IT_VTTK
    WHERE SHTYP EQ IT_VTTK-SHTYP
      AND TP_OPER EQ '01'.

    IF NOT ( IT_ZCOT0001_VENDA[] IS INITIAL ).

      "------------------------
      " Select responsavel por selecionar controle de tipo de operação.
      " DESPESAS DE VENDA.
      "-------------------------

      SELECT MATNR WERKS KOSTL
        FROM ZCOT0005
        INTO TABLE IT_ZCOT0005
        FOR ALL ENTRIES IN IT_LIPS
      WHERE MATNR EQ IT_LIPS-MATNR
        AND WERKS EQ IT_LIPS-WERKS.

      "------------------------
      " Select responsavel por selecionar Fluxo de documentos de vendas e distribuição.
      " DESPESAS DE VENDA.
      "-------------------------

      SELECT VBELN VBELV VBTYP_N VBTYP_V
        FROM VBFA
        INTO TABLE IT_VBFA
        FOR ALL ENTRIES IN IT_VTTP
      WHERE VBELN   EQ IT_VTTP-VBELN
        AND VBTYP_N EQ 'J'
        AND VBTYP_V EQ 'C'.

    ENDIF.

    SELECT TP_OPER SAKNR HKONT SHTYP
      FROM ZCOT0001
      INTO TABLE IT_ZCOT0001_P_VENDA
      FOR ALL ENTRIES IN IT_VTTK
    WHERE SHTYP EQ IT_VTTK-SHTYP
      AND TP_OPER EQ '04'.

    IF NOT ( IT_ZCOT0001_P_VENDA[] IS INITIAL ).

      SELECT VBELN PARVW LIFNR
        FROM VBPA
        INTO TABLE IT_VBPA
        FOR ALL ENTRIES IN IT_VBFA
       WHERE VBELN EQ IT_VBFA-VBELV
        AND PARVW  EQ 'Z1'.

      SELECT WERKS LIFNR WERKS_V
        FROM ZSDT_DEPARA_DEPO
        INTO TABLE IT_ZSDT_DEPARA_DEPO
        FOR ALL ENTRIES IN IT_VBPA
      WHERE LIFNR EQ IT_VBPA-LIFNR.

      CLEAR: WA_ZSDT_DEPARA_DEPO.

      LOOP AT IT_ZSDT_DEPARA_DEPO INTO WA_ZSDT_DEPARA_DEPO.

        READ TABLE IT_LIPS INTO WA_LIPS WITH KEY WERKS = WA_ZSDT_DEPARA_DEPO-WERKS.

        IF ( SY-SUBRC EQ 0 ).
          CONTINUE.
        ELSE.

          DELETE  IT_ZSDT_DEPARA_DEPO WHERE WERKS EQ WA_ZSDT_DEPARA_DEPO-WERKS.
        ENDIF.
        CLEAR: WA_ZSDT_DEPARA_DEPO, WA_LIPS.
      ENDLOOP.

    ENDIF.

    "------------------------
    " Select responsavel por selecionar Documento SD: fornecimento: dados de cabeçalho
    "-------------------------
    SELECT VBELN KUNNR LFART
      FROM LIKP
      INTO TABLE IT_LIKP
      FOR ALL ENTRIES IN IT_VTTP
    WHERE VBELN EQ IT_VTTP-VBELN.

    "------------------------
    " Select responsavel por selecionar Fluxo de documentos de vendas e distribuição.
    " COMPRA
    "------------------------

    SELECT TP_OPER SAKNR HKONT SHTYP
       FROM ZCOT0001
       INTO TABLE IT_ZCOT0001_COMPRA
       FOR ALL ENTRIES IN IT_VTTK
     WHERE SHTYP EQ IT_VTTK-SHTYP
       AND TP_OPER EQ '03'.

    IF NOT ( IT_ZCOT0001_COMPRA[] IS INITIAL ).

      SELECT VBELN KUNNR LFART
        FROM LIKP
        INTO TABLE IT_LIKP_COMPRA
        FOR ALL ENTRIES IN IT_VTTP
      WHERE VBELN EQ IT_VTTP-VBELN
        AND LFART EQ 'TFDT'.

    ENDIF.

*    "------------------------
*    " Select responsavel por selecionar Cadastro de Comissão de Frete.
*    "-------------------------
*    SELECT LIFNR KSCHL PERCENTUAL DATA_ATUAL HORA_ATUAL USUARIO
*      FROM ZLEST0050
*      INTO TABLE IT_ZLEST0050
*    WHERE LIFNR EQ P_LIFNR.

    INPUT_ICON( ).
    CAMPOS_TELA( ).
    SAIDA_PAG( ).
    CONTAINER_PAG( ).
    CALL_TELA01( ).

  ENDMETHOD.                    "seleciona_dados

  METHOD SAIDA_PAG.

    CLEAR: WA_ZCTE_CIOT.

    SORT: IT_ZCTE_CIOT BY TKNUM,
          IT_VTTP      BY TKNUM,
          IT_LIPS      BY VBELN,
          IT_MAKT      BY MATNR.

    LOOP AT IT_ZCTE_CIOT INTO WA_ZCTE_CIOT.


      WA_SAIDA_PAG-TKNUM      = WA_ZCTE_CIOT-TKNUM.
      WA_SAIDA_PAG-CD_CIOT    = WA_ZCTE_CIOT-CD_CIOT.
      WA_SAIDA_PAG-DT_TERMIN  = WA_ZCTE_CIOT-DT_TERMIN.

      WA_SAIDA_PAG-LOTE_PGTO  = WA_ZCTE_CIOT-LOTE.
      READ TABLE IT_VTTP INTO WA_VTTP WITH KEY TKNUM = WA_ZCTE_CIOT-TKNUM.
      READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = WA_VTTP-VBELN.

      WA_SAIDA_PAG-LFIMG    = WA_LIPS-LFIMG.
      WA_SAIDA_PAG-MATNR    = WA_LIPS-MATNR.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_LIPS-MATNR.
      WA_SAIDA_PAG-MAKTX = WA_MAKT-MAKTX.

      WA_SAIDA_PAG-VLR_FRETE = WA_ZCTE_CIOT-VLR_FRETE.


      SELECT SINGLE * FROM ZLEST0050 INTO WA_ZLEST0050 WHERE LIFNR EQ P_LIFNR-LOW.

      WA_SAIDA_PAG-VLR_COMISSAO = WA_ZCTE_CIOT-VLR_FRETE * WA_ZLEST0050-PERCENTUAL.
      WA_SAIDA_PAG-PERCENTUAL   = WA_ZLEST0050-PERCENTUAL.


      IF NOT ( WA_ZCTE_CIOT-LOTE IS INITIAL ).
        WA_SAIDA_PAG-SELECT = 'X'.
      ENDIF.


      APPEND WA_SAIDA_PAG TO IT_SAIDA_PAG.

      T_FRETE = T_FRETE + WA_ZCTE_CIOT-VLR_FRETE.
      T_COMI  = T_COMI  + WA_SAIDA_PAG-VLR_COMISSAO.


      CLEAR: WA_ZCTE_CIOT, WA_VTTP, WA_LIPS, WA_MAKT, WA_SAIDA_PAG.

    ENDLOOP.

  ENDMETHOD.                    "saida_pag


  "------------------------
  " Método responsavel por modificar o tipo de dados WERKS.
  "-------------------------

  METHOD MODIFICA_WERKS.

    FIELD-SYMBOLS: <FS_WERKS> TYPE TY_T001W.

    LOOP AT IT_T001W ASSIGNING <FS_WERKS>.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = <FS_WERKS>-WERKS
        IMPORTING
          OUTPUT = <FS_WERKS>-WERKS_AUX.

    ENDLOOP.

    UNASSIGN <FS_WERKS>.

  ENDMETHOD.                    "modifica_werks

  METHOD INPUT_ICON.

    ICON_EMPRESA  = ICON_PLANT.
    ICON_DATA     = ICON_DELIVERY_DATE.
    ICON_FORN     = ICON_DELIVERY_INBOUND.
    ICON_SUM_I    = ICON_SUM.
    ICON_T_FRETE  = ICON_IMPORT_ALL_REQUESTS.
    ICON_T_COMISS = ICON_ACTIVITY_GROUP.
    ICON_P_COMISS = ICON_BUSINESS_AREA.
    ICON_T_LOTE   = ICON_DELIVERY_INBOUND.

    CLEAR: TXT_FORN, WA_LFA1.
    SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ P_LIFNR-LOW.
    TXT_FORN = WA_LFA1-NAME1.

  ENDMETHOD.                    "input_icon


  METHOD CAMPOS_TELA.

    CLEAR: C_EMPRESA,
           C_DATA,
           C_DATA_T,
           C_FORNE.

    C_EMPRESA   = P_VKORG-LOW.


    C_DATA     = P_DATA-LOW.
    C_DATA_T   = P_DATA-HIGH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = P_LIFNR-LOW
      IMPORTING
        OUTPUT = C_FORNE.


  ENDMETHOD.                    "campo_tela

  METHOD CALL_TELA01.
    CALL SCREEN TELA01.
  ENDMETHOD.                    "CALL_TELA01

  METHOD CONTAINER_PAG.

    DATA: GR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.


    IF (  CL_CONTAINER_PAG IS INITIAL ).


      CREATE OBJECT CL_CONTAINER_PAG
        EXPORTING
          CONTAINER_NAME              = 'CONTAINER_PAG'
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5
          OTHERS                      = 6.

      CATALOG_PAG( ).

      CREATE OBJECT CL_GRID_PAG
        EXPORTING
          I_PARENT          = CL_CONTAINER_PAG
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.

      CREATE OBJECT GR_EVENT_HANDLER.
      SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CL_GRID_PAG.
      SET HANDLER GR_EVENT_HANDLER->HANDLE_TOOLBAR FOR CL_GRID_PAG.

      CALL METHOD CL_GRID_PAG->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = WA_LAYOUT
          IS_VARIANT                    = GS_VARIANT_C
          I_SAVE                        = C_A
        CHANGING
          IT_OUTTAB                     = IT_SAIDA_PAG[]
          IT_FIELDCATALOG               = IT_FIELDCATALOG[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.

    ENDIF.

  ENDMETHOD.                    "CONTAINER_PAG

  METHOD CATALOG_PAG.

    CLEAR: WA_FIELDCATALOG.

    WA_FIELDCATALOG-FIELDNAME = 'SELECT'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Seleção'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Seleção'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Seleção'.
    WA_FIELDCATALOG-OUTPUTLEN = '7'.
    WA_FIELDCATALOG-JUST      = 'C'.
    WA_FIELDCATALOG-CHECKBOX  = 'X'.
    WA_FIELDCATALOG-HOTSPOT   = 'X'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'STATUS_PG'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Status (Pg.)'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Status (Pg.)'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Status (Pg.)'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    WA_FIELDCATALOG-JUST      = 'C'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'STATUS_APR'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Status (Aprop.)'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Status (Aprop.)'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Status (Aprop.)'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    WA_FIELDCATALOG-JUST      = 'C'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'LOTE_PGTO'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Lote Pgto.'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Lote Pgto.'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Lote Pgto.'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'DOCNUM'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Doc. Contabil.'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Doc. Contabil.'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Doc. Contabil.'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'TKNUM'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Doc. Transporte.'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Doc. Transporte.'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Doc. Transporte.'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    WA_FIELDCATALOG-NO_ZERO   = 'X'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'CIOT'.
    WA_FIELDCATALOG-SCRTEXT_L = 'CIOT'.
    WA_FIELDCATALOG-SCRTEXT_M = 'CIOT'.
    WA_FIELDCATALOG-SCRTEXT_S = 'CIOT'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    WA_FIELDCATALOG-NO_ZERO   = 'X'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'DT_TERMIN'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Dt.Mov.'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Dt.Mov.'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Dt.Mov.'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'LFIMG'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Quantidade'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Quantidade'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Quantidade'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'VLR_FRETE'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Valor Frete'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Valor Frete'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Valor Frete'.
    WA_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'PERCENTUAL'.
    WA_FIELDCATALOG-SCRTEXT_L = '% Comissão'.
    WA_FIELDCATALOG-SCRTEXT_M = '% Comissão'.
    WA_FIELDCATALOG-SCRTEXT_S = '% Comissão'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'VLR_COMISSAO'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Vlr. Comissão'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Vlr. Comissão'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Vlr. Comissão'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.


    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'WERKS_V'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Centro Aprop.'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Centro Aprop.'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Centro Aprop.'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.
    CLEAR: WA_FIELDCATALOG.

    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'MATNR'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Material'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Material'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Material'.
    WA_FIELDCATALOG-NO_ZERO   = 'X'.
    WA_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

    CLEAR: WA_FIELDCATALOG.
    WA_FIELDCATALOG-FIELDNAME = 'MAKTX'.
    WA_FIELDCATALOG-SCRTEXT_L = 'Desc. Material'.
    WA_FIELDCATALOG-SCRTEXT_M = 'Desc. Material'.
    WA_FIELDCATALOG-SCRTEXT_S = 'Desc. Material'.
    WA_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

    WA_LAYOUT-INFO_FNAME = 'COR_LINHA'.


  ENDMETHOD.                    "catalog_pag
ENDCLASS.                    "lcl_sel_view IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  GERAR_LOTE
*&---------------------------------------------------------------------*
FORM GERAR_LOTE .
  DATA: WA_ZLEST0051 TYPE ZLEST0051,
        VAR_LOTE     TYPE ZCTE_CIOT-LOTE.

  CLEAR: WA_SAIDA_PAG, VAR_LOTE.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZLOTE_PAG'
    IMPORTING
      NUMBER      = VAR_LOTE.

  LOOP AT IT_SAIDA_PAG INTO WA_SAIDA_PAG WHERE SELECT EQ 'X'
                                           AND LOTE_PGTO EQ SPACE.

    WA_ZLEST0051-BUKRS        = P_VKORG-LOW.
    WA_ZLEST0051-LOTE         = VAR_LOTE.
    WA_ZLEST0051-STATUS       = 'L'.
    WA_ZLEST0051-VLR_TOTAL    =  T_LOTE.
    WA_ZLEST0051-VLR_COMISSAO = T_COMI.
    WA_ZLEST0051-USUARIO      = SY-UNAME.
    WA_ZLEST0051-DATA_ATUAL   = SY-DATUM.
    WA_ZLEST0051-HORA_ATUAL   = SY-UZEIT.

    CLEAR: WA_ZCTE_CIOT.
    READ TABLE IT_ZCTE_CIOT INTO WA_ZCTE_CIOT WITH KEY CD_CIOT = WA_SAIDA_PAG-CD_CIOT.

    IF ( SY-SUBRC EQ 0 ).
      UPDATE ZCTE_CIOT SET LOTE = WA_ZLEST0051-LOTE WHERE CD_CIOT = WA_ZCTE_CIOT-CD_CIOT.

      WA_SAIDA_PAG-SELECT      = 'X'.
      WA_SAIDA_PAG-LOTE_PGTO   = VAR_LOTE.

      MODIFY IT_SAIDA_PAG INDEX SY-TABIX FROM WA_SAIDA_PAG TRANSPORTING SELECT LOTE_PGTO.
      INSERT INTO ZLEST0051 VALUES WA_ZLEST0051.
    ENDIF.

    CLEAR: WA_SAIDA_PAG, WA_ZCTE_CIOT.
  ENDLOOP.

  CALL METHOD CL_GRID_PAG->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " GERAR_LOTE
