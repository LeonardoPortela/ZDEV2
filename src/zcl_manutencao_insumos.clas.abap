class ZCL_MANUTENCAO_INSUMOS definition
  public
  final
  create public
  shared memory enabled .

public section.

  class-data:
    R_INCO1 TYPE RANGE OF INCO1 .
  class-data:
    R_WERKS TYPE RANGE OF WERKS_D .
  class-data LV_RESPOSTA type C .

    " Métodos públicos
  class-methods DESMEMBRAMENTO_MASSA
    importing
      !I_CHECK type ABAP_BOOL default 'X'
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods DESMEMBRAMENTO_DEVOLUCAO
    importing
      !I_DESC_ABS type KBETR optional
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods TROCA_MATERIAIS_MASSA
    importing
      !I_INPUT type ZSDS015_T optional
      !I_CHECK type ABAP_BOOL default 'X'
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods TROCA_CENTRO_FORNECEDOR
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_WERKS type WERKS_D
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods DADOSGERAIS
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_CATEGORIA type ZSDED047
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods CHECK_MATERIAL
    importing
      !I_MATNR type MATNR
    exporting
      !E_MAKTX type MAKTX
      !E_MARA type MARA
      !IS_OK type ABAP_BOOL
    exceptions
      ZCX_INSUMOS .
  class-methods CHECK_MATERIAL_ZFER
    importing
      !I_MATNR type MATNR
    exporting
      !IS_OK type ABAP_BOOL
    raising
      ZCX_INSUMOS .
  class-methods GET_CADASTRO_PRECO
    importing
      !I_BUKRS type BUKRS
      !I_WAERK type WAERK
      !I_CULTURA type ZSDED001
      !I_MATNR type MATNR optional
      !I_INCO1 type INCO1 optional
      !I_WERKS_FORNEC type WERKS_D optional
    exporting
      !LS_PRECO type ZSDT0036
      !IS_OK type ABAP_BOOL .
  class-methods CALL_CREATE_ITINERARIO
    importing
      !I_PC type LIFNR
      !I_LR type KUNNR .
  class-methods VERIFICAR_ITINERARIO
    importing
      !I_PC type LIFNR
      !I_LR type KUNNR
      !I_CHECK type ABAP_BOOL optional
    exporting
      !E_ROUTE type ROUTE_VL
      !IS_OK type ABAP_BOOL .
  class-methods GET_ZONA
    importing
      !I_PC type LIFNR optional
      !I_LR type KUNNR optional
    exporting
      !E_ZONA type LZONE .
  class-methods CALL_ORDER_CHANGE
    returning
      value(R_RETURN) type BAPIRET2_T .
  class-methods CALL_ORDER_CREATE
    returning
      value(R_RETURN) type BAPIRET2_T .
  class-methods CHECK_CADASTRO_FORN_SISDEV
    importing
      !I_WERKS type WERKS_D
      !I_KUNNR type KUNNR
    exporting
      !E_ERRO type CHAR01 .
  class-methods SET_DADOS_TROCA
    importing
      !I_TROCA_OLD type ZDE_DADOS_TROCA_T optional
      !I_TROCA_NEW type ZDE_DADOS_TROCA_ADD_T optional
      !I_SELECAO_87 type ZDE_SELECAO_87 optional
      !I_MANUTENCAO type ZDE_MANUTENCAO_OV .
  class-methods EXIBE_MENSAGENS
    importing
      !I_RETURN type BAPIRET2_T .
  class-methods CHK_DESCONTO_ABS
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_VLR_REAL type KBETR optional
    exporting
      !E_DESCONTO_ABS type KBETR .
  class-methods GET_QUANTIDADE_EMBARCADO
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    exporting
      !E_QTDE_EMBARCADA type ZSDED054 .
  class-methods CHK_USER_EXCECAO_PROPORCIONAL
    importing
      !I_USER type SYUNAME
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods CHK_REGRA_PROPORCIONAL
    importing
      !I_MATNR type MATNR
      !I_QUANTIDADE type RFMNG
    exporting
      !E_MSG type BAPI_MSG .
  class-methods GET_PROPORCIONALIDADE_MATERIAL
    importing
      !I_MATNR type MATNR
    exporting
      !E_MEINS type MEINS
      !E_GROES_DEC type ESECOMPAVG
      !IS_OK type ABAP_BOOL .
  class-methods VERIFICAR_ITINERARIO_82
    importing
      !I_ROTA_LR type Z_NR_ROT
      !I_ROTA_PC type Z_NR_ROT
      !IS_BACKGROUND type ABAP_BOOL optional
    exporting
      !IS_OK type ABAP_BOOL
      !E_ROUTE type ROUTE_VL .
  class-methods SET_DADOS_DESMEMBRAMENTO
    importing
      !I_MANUTENCAO type ZDE_MANUTENCAO_OV .
  class-methods FILL_TROCA_NEW
    exporting
      !E_KONV type KONV
      !E_VBKD type VBKD
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T .
  class-methods FILL_DESMEMBRAMENTO_NEW
    exporting
      !E_KONV type KONV
      !E_VBKD type VBKD
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T .
  class-methods CHECK_DADOS_DESMEMBRAMENTO
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods RUN
    importing
      !I_MANUTENCAO type ZDE_MANUTENCAO
      !IS_BACKGROUND type ABAP_BOOL optional
    exporting
      !E_VBELN type VBELN_VA
      !E_POSNR type POSNR_VA
      !R_RETURN type BAPIRET2_T .
  class-methods SET_MENSAGENS
    importing
      !I_MENSAGEM type BAPI_MSG
      !I_TYPE type BAPI_MTYPE
    exporting
      value(R_RETURN) type BAPIRET2_T .
  class-methods GET_TEXT_OV
    importing
      !I_VBELN type TDOBJECT
    exporting
      !T_TEXT type CATSXT_LONGTEXT_ITAB .
  class-methods SEND_APROVADORES_EMBARQUE
    importing
      !I_VBELN type VBELN_VA
      !I_POSNN type POSNR_VA
      !I_VBELV type VBELN_VA
    exporting
      !E_SEQUENCIAL type NUMC10
      !R_RETURN type BAPIRET2_T .
  class-methods REPROCESSA_DESCONTO_ABS
    importing
      !I_VBELN type VBELN_VA
      !I_MATNR type MATNR
      !I_SIMULADOR type ZSDED003
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods ROWBACK_ENCERRAMENTO
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_QTDE_ENCERRADA type ZSDED054 .
  class-methods POPUP_CONFIRM
    importing
      !I_MENSAGEM type FLT_TEXT
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods CANCEL_APROVADORES_EMBARQUE
    importing
      !I_VBELV type VBELN_VA
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods CHK_LATITUDE_LONGITUDE
    importing
      !I_LATITUDE type ZDE_LATITUDE optional
      !I_LONGITUDE type ZDE_LONGITUDE optional
      !I_ORIGEM type ABAP_BOOL optional
    exporting
      !E_MSG type SPO_VALUE .
  class-methods CHK_ID_PROPRIEDADE
    importing
      !I_VBELN type VBELN_VA
      !I_ROTA type Z_NR_ROT
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods REPROCESSA_DESCONTO_ABS_GERAL
    importing
      !I_VBELN type VBELN_VA
      !I_MATNR type MATNR
      !I_SIMULADOR type ZSDED003
      !I_CHECK type ABAP_BOOL optional
    exporting
      !E_DIFFERENT type ABAP_BOOL
      !E_EXIT type ABAP_BOOL
      !R_RETURN type BAPIRET2_T .
  class-methods CHK_DIVERGENCIA_OV_SIMULADOR
    importing
      !I_VBELN type VBELN_VA
      !I_MATNR type MATNR
      !I_SIMULADOR type ZSDED003
    exporting
      !E_ORDEM_VENDA type KBETR
      !E_SIMULADOR type DZWERT
      !E_DIFERENCA_SIMULADOR type KBETR
      !E_DIFERENCA type KBETR .
  class-methods CHECK_DADOS_TROCA
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods SET_DESCONTO_ABS_OV
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA optional
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods SET_DESCONTO_ABS
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_DESCONTO_ABS type KBETR optional
      !I_ZERAR type ABAP_BOOL optional
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods CHK_DESCONTO_ABS_FATURADO
    importing
      !I_VBELN type VBELN_VA
      !I_MATNR type MATNR optional
    exporting
      !IS_OK type ABAP_BOOL
      !IS_FAT type ABAP_BOOL .
  class-methods CHK_FATURAMENTO_SIMULADOR
    importing
      !I_SIMULADOR type ZSDED003
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods CHK_OV_VALOR_MAIOR
    importing
      !I_VBELN_OLD type VBELN_VA
      !I_VBELN_NEW type VBELN_VA
      !I_MATNR_OLD type MATNR
      !I_MATNR_NEW type MATNR
    exporting
      !E_VBELN type VBELN_VA
      !E_MATNR type MATNR .
  class-methods GET_ROTA
    importing
      !I_PC type LIFNR optional
      !I_LR type KUNNR optional
    exporting
      !E_ROTEIRO type Z_NR_ROT .
  class-methods CHK_DESC_APLICADO_SIMULADOR
    importing
      !I_SIMULADOR type ZSDED003
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods CHK_CRIACAO_OV
    importing
      !I_VBELN type VBELN_VA
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods SET_LOG_SOLICITACAO
    importing
      !I_OLD type ZSDT0082
      !I_DIRECAO type CHAR25
      !I_NEW type ZSDT0082 .
  class-methods GET_TOLERANCIA_DESCONTO_ABS
    exporting
      !E_TOLERANCIA type NETPR .
  class-methods CHK_OV_SOLICITADA
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    exporting
      !EXIST type ABAP_BOOL .
  class-methods CHK_OV_DISTRIBUIDA
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    exporting
      !EXIST type ABAP_BOOL .
  class-methods SET_ESTORNAR_SOLICITACAO
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    exporting
      !E_MSG type BAPI_MSG .
  class-methods CHK_CLI_APROVADORES_EMBARQUE
    importing
      !CLIENTE_ORIGEM type KUNNR
      !CLIENTE_DESTINO type KUNNR
    exporting
      !IS_OK type ABAP_BOOL .
  PROTECTED SECTION.

private section.

  types:
    begin of ty_saida_exec,
      inco1    type zsdt0041-werks,
      spart    type zsdt0041-spart,
      auart    type zsdt0041-auart,
      werks    type zsdt0041-werks,
      vbeln    type vbak-vbeln,
      msg(255),
    end of ty_saida_exec .
  types:
    begin of ty_fcat.
      include type slis_fieldcat_main.
      include type slis_fieldcat_alv_spec.
  types: end of ty_fcat .

  constants:
    begin of constantes,
      begin of mtart,
        zfer type c length 4 value 'ZFER',
        zhaw type c length 4 value 'ZHAW',
      end of mtart,
      begin of auart,
        zrfu type c length 4 value 'ZRFU',
        zrem type c length 4 value 'ZREM',
      end of auart,
      begin of spart,
        _02 type c length 2 value '02',
        _03 type c length 2 value '03',
        _04 type c length 2 value '04',
        _13 type c length 2 value '13',
      end of spart,
      begin of type,
        error   type symsgty value 'E',
        warning type symsgty value 'W',
        info    type symsgty value 'I',
        sucess  type symsgty value 'S',
      end of type,
      begin of categoria,
        desmembramento    type c length 1 value 'D',
        troca             type c length 1 value 'M',
        retorno_devolucao type c length 1 value 'K',
        desconto_absoluto type c length 1 value 'O',
      end of categoria,
      begin of itinerario,
        parceiro_pc type c length 6 value 'COD_PC',
        parceiro_lr type c length 7 value 'COD_CLI',
        zles0214    type c length 8 value 'ZLES0214',
      end of itinerario,
      begin of parvw,
        pc type c length 2 value 'PC',
        lr type c length 2 value 'LR',
      end of parvw,
      begin of uf,
        mt type c length 6 value 'MT',
      end of uf,
      begin of status,
        ativo   type c length 1 value 'A',
        inativo type c length 1 value 'I',
      end of status,
      begin of tcode,
        zsdt0087 type c length 8 value 'ZSDT0087',
        zsdt0081 type c length 8 value 'ZSDT0081',
      end of tcode,
      begin of limite,
        qtd_desc_abs type c length 28 value 'ZSDT0087_REPROCESSA_DESC_ABS',
      end of limite,
    end of constantes .
  class-data AT_SALESDOCUMENT type VBELN_VA .
  class-data AT_ORDER_HEADER_IN type BAPISDH1 .
  class-data AT_ORDER_HEADER_INX type BAPISDH1X .
  class-data AT_SALES_HEADER_IN type BAPISDHD1 .
  class-data AT_LOGIC_SWITCH type BAPISDLS .
  class-data AT_ORDER_ITEM_IN type OIL_BAPISDIM_T .
  class-data AT_ORDER_ITEM_INX type OIL_BAPISDIMX_T .
  class-data AT_CONDITIONS_IN type OIT_T_COND .
  class-data AT_CONDITIONS_INX type OIT_T_CONDX .
  class-data AT_SCHEDULE_LINES type OIJ_BAPISCHDL_T .
  class-data AT_SCHEDULE_LINESX type OIJ_BAPISCHDLX_T .
  class-data AT_PARTNERS type OIJ_BAPIPARNR_T .
  class-data AT_ORDER_TEXT type BAPISDTEXT_T .
  class-data AT_EXTENSIONIN type T_BAPIPAREX .
  class-data AT_RETURN type BAPIRET2_T .
  class-data AT_VBAK type VBAK .
  class-data AT_VBAP type VBAP_T .
  class-data AT_VBAK_NEW type VBAK .
  class-data AT_VBAP_NEW type VBAP_T .
  class-data AT_VBEP type VBEP_T .
  class-data AT_BEHAVE_WHEN_ERROR type CHAR1 .
  class-data AT_TROCA_OLD type ZDE_DADOS_TROCA_T .
  class-data AT_TROCA_NEW type ZDE_DADOS_TROCA_ADD_T .
  class-data AT_SELECAO_87 type ZDE_SELECAO_87 .
  class-data AT_NOVA_OV type CHAR01 .
  class-data AT_DADOS_TRATADOS type ZSDS015_T .
  class-data AT_ADITIVOS type ZSDT0090 .
  class-data AT_VBELN_OLD type VBELN_VA .
  class-data AT_VBELN_NEW type VBELN_VA .
  class-data AT_SOLICITACAO type ZSDED003 .
  class-data AT_MANUTENCAO type ZDE_MANUTENCAO_OV .
  class-data:
    at_mensagens type table of ty_saida_exec .
  class-data:
    at_fcat_msg type table of ty_fcat .
  class-data AT_BACKGROUND type ABAP_BOOL .
  class-data AT_CATEGORIA type ZSDED047 .
  class-data AT_PONTO_COLETA type LIFNR .
  class-data AT_PRICE type BAPIKBETR1 .
  data AT_TCODE type TCODE .
  class-data AT_CH_REFERENCIA type ZID_INTEGRACAO .
  class-data AT_ITEM_DISTRIB type ZITEM_DISTRIB .
  class-data AT_BUSINESS_EX type COD_T_SLS_ITEM_DATA .
  class-data AT_DESMEMBRAMENTO_DEVOLUCAO type ABAP_BOOL .
  class-data AT_DESCONTO type BAPIKBETR1 .

  class-methods GET_QUANTIDADE_DISTRIBUIDA
    importing
      !I_VBELN type VBELN_VA
    exporting
      !E_VALOR_DISTRIBUIDA type NETWR_AP .
  class-methods CLEAR_ATRIBUTOS
    importing
      !SET_ALL type ABAP_BOOL optional .
  class-methods CHECK_NOVA_OK .
  class-methods FILL_DADOS_CHANGE
    exporting
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T .
  class-methods FILL_ORDEM_CHANGE
    changing
      !I_VBAK_NEW type VBAK
      !I_VBAP_NEW type VBAP_T
    returning
      value(R_RETURN) type BAPIRET2_T .
  class-methods FILL_DADOS_CREATE
    exporting
      !E_VBKD type VBKD
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T
      !E_KONV type KONV .
  class-methods FILL_ORDEM_CREATE
    changing
      !I_VBKD_NEW type VBKD
      !I_VBAK_NEW type VBAK
      !I_VBAP_NEW type VBAP_T
      !I_KONV_NEW type KONV
    returning
      value(R_RETURN) type BAPIRET2_T .
  class-methods CHECK_DADOS_INCOMPLETOS
    importing
      !I_VBELN type VBELN_VA
    changing
      !R_RETURN type BAPIRET2_T .
  class-methods SET_ADITIVOS
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods GET_DESC_ABS
    importing
      !I_KONV type KONV_TTY
    exporting
      !E_KBETR type KBETR .
  class-methods HEDGE_TROCA_MATERIAL
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods GET_DESCONTO_ABS
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_IMP type ABAP_BOOL optional
    exporting
      !E_DESCONTO_ABS type KBETR .
  class-methods CHK_DESCONTO_ABS_OLD_NEW
    importing
      !I_VBELN_OLD type VBELN_VA
      !I_MATNR_OLD type MATNR
      !I_VBELN_NEW type VBELN_VA
      !I_MATNR_NEW type MATNR
    exporting
      !E_DESCONTO_ABS type KBETR .
  class-methods PROCESSA_DESCONTO_ABS
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods GET_VLR_ADITIVO
    importing
      !I_SIMULADOR type ZSDED003
      !I_CATEGORIA type ZSDED047
    exporting
      !E_VLR type DZWERT .
  class-methods CHK_LIMITE_QUANTIDADE
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_QTDE type KWMENG
    exporting
      !E_MSG type BAPI_MSG .
  class-methods CHECK_DADOS_BACKGROUND
    importing
      !I_MANUTENCAO type ZDE_MANUTENCAO
    exporting
      !R_RETURN type BAPIRET2_T .
  class-methods GET_SIMULADOR
    importing
      !I_VBELN type VBELN_VA
    exporting
      !E_SIMULADOR type ZSDED003 .
  class-methods FILL_TROCA_OLD
    exporting
      !E_KONV type KONV
      !E_VBKD type VBKD
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T .
  class-methods FILL_DESMEMBRAMENTO_OLD
    exporting
      !E_KONV type KONV
      !E_VBKD type VBKD
      !E_VBAK type VBAK
      !E_VBAP type VBAP_T .
  class-methods CHK_SOLICITACAO_EM_CARGA
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
    exporting
      !IS_OK type ABAP_BOOL .
  class-methods CHANGE_SOLICITACAO_APROVADA
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_QTDE_ENCERRADA type ZSDED054 .
  class-methods CHANGE_SOLICITACAO_DISTRIBUIDA
    importing
      !I_VBELN type VBELN_VA
      !I_POSNR type POSNR_VA
      !I_QTDE_ENCERRADA type ZSDED054 .
  class-methods FILL_DELETE_ORDEM
    importing
      !I_VBELN type VBELN_VA
    returning
      value(R_RETURN) type BAPIRET2_T .
  class-methods GET_PC_VIA_ROTEIRO
    importing
      !I_ROTA_PC type Z_NR_ROT
    exporting
      !E_PC type LIFNR .
  class-methods GET_PRECO_LIQUIDO_41
    importing
      !I_SIMULADOR type ZSDED003
      !I_VBELN type VBELN_VA
      !I_MATNR type MATNR
    exporting
      !E_PRECO_LIQUIDO type BAPIKBETR1 .
  class-methods GET_OV_CH_REFERENCIA
    importing
      !I_BSTKD_E type BSTKD_E
    exporting
      !E_VBELN type VBELN_VA
      !E_POSNR type POSNR_VA .
  class-methods GET_QUANTIDADE_DEVOLVIDA
    importing
      !I_VBELN type VBELN_VA
    exporting
      !E_VALOR_DEVOLVIDO type NETWR_AP .
  class-methods CHECK_NOVA_OV_81
    exporting
      !E_NOVA_OV type ABAP_BOOL
      !E_AGRUPAR type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_MANUTENCAO_INSUMOS IMPLEMENTATION.


  METHOD CALL_CREATE_ITINERARIO.

    CHECK I_PC IS NOT INITIAL.
    CHECK I_LR IS NOT INITIAL.

    CALL METHOD ZCL_MANUTENCAO_INSUMOS=>VERIFICAR_ITINERARIO
      EXPORTING
        I_PC    = I_PC
        I_LR    = I_LR
      IMPORTING
        E_ROUTE = DATA(LV_ROUTE)
        IS_OK   = DATA(IS_OK).

    IF IS_OK IS NOT INITIAL.

      MESSAGE |Itinerário para parceiro PC: { I_PC }, LR: { I_LR } já Cadastrado: { LV_ROUTE }| TYPE 'I'.

*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          TEXT_QUESTION         = TEXT-001 "//'Itinerário Existente, Deseja Continuar!'
*          TEXT_BUTTON_1         = 'Sim'
*          ICON_BUTTON_1         = 'ICON_OKAY '
*          TEXT_BUTTON_2         = 'Não'
*          ICON_BUTTON_2         = 'ICON_CANCEL'
*          DEFAULT_BUTTON        = '1'
*          DISPLAY_CANCEL_BUTTON = ' '
*          START_COLUMN          = 25
*          START_ROW             = 6
*        IMPORTING
*          ANSWER                = LV_RESPOSTA
*        EXCEPTIONS
*          TEXT_NOT_FOUND        = 1
*          OTHERS                = 2.

      RETURN.

    ENDIF.

    SET PARAMETER ID CONSTANTES-ITINERARIO-PARCEIRO_PC FIELD I_PC.
    SET PARAMETER ID CONSTANTES-ITINERARIO-PARCEIRO_LR FIELD I_LR.
    CALL TRANSACTION CONSTANTES-ITINERARIO-ZLES0214.

  ENDMETHOD.


  METHOD CHECK_MATERIAL.

    IS_OK = ABAP_FALSE.

    SELECT SINGLE *
      FROM MARA
      INTO E_MARA
      WHERE MATNR EQ I_MATNR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE MAKTX
      FROM MAKT
      INTO E_MAKTX
      WHERE MATNR EQ I_MATNR
        AND SPRAS EQ SY-LANGU.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHECK_MATERIAL_ZFER.

    IS_OK = ABAP_FALSE.

    CALL METHOD CHECK_MATERIAL
      EXPORTING
        I_MATNR = I_MATNR
      IMPORTING
        E_MARA  = DATA(LS_MARA)
        IS_OK   = DATA(LV_EXISTS).

    CHECK LS_MARA-MTART = CONSTANTES-MTART-ZFER.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD DESMEMBRAMENTO_MASSA.

    IF I_CHECK IS NOT INITIAL.
      CALL METHOD CHECK_DADOS_DESMEMBRAMENTO
        IMPORTING
          R_RETURN = R_RETURN.
    ENDIF.

    IF R_RETURN IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD SET_MENSAGENS
      EXPORTING
        I_TYPE     = CONSTANTES-TYPE-INFO
        I_MENSAGEM = |{ TEXT-022 }|
      IMPORTING
        R_RETURN   = DATA(E_RETURN).

    APPEND LINES OF E_RETURN TO R_RETURN.

    CALL METHOD FILL_DADOS_CREATE
      IMPORTING
        E_VBAK = DATA(LS_VBAK)
        E_VBAP = DATA(LT_VBAP)
        E_VBKD = DATA(LS_VBKD)
        E_KONV = DATA(LS_KONV).

    CALL METHOD FILL_ORDEM_CREATE
      CHANGING
        I_VBAK_NEW = LS_VBAK
        I_VBAP_NEW = LT_VBAP
        I_VBKD_NEW = LS_VBKD
        I_KONV_NEW = LS_KONV
      RECEIVING
        R_RETURN   = E_RETURN.

    APPEND LINES OF E_RETURN TO R_RETURN.

    IF NOT LINE_EXISTS( R_RETURN[ TYPE = CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR ] ).

*      CALL METHOD CLEAR_ATRIBUTOS.
*
*      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>SET_DESCONTO_ABS_OV
*        EXPORTING
*          I_VBELN  = AT_ADITIVOS-VBELV
*          I_POSNR  = AT_ADITIVOS-POSNV
*        IMPORTING
*          R_RETURN = E_RETURN.
*
*      APPEND LINES OF E_RETURN TO R_RETURN.

      CALL METHOD CLEAR_ATRIBUTOS.

      CALL METHOD PROCESSA_DESCONTO_ABS
        IMPORTING
          R_RETURN = E_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

*      CALL METHOD CLEAR_ATRIBUTOS.

      CALL METHOD CHK_OV_VALOR_MAIOR
        EXPORTING
          I_VBELN_OLD = AT_ADITIVOS-VBELV
          I_MATNR_OLD = AT_ADITIVOS-MATNRV
          I_VBELN_NEW = AT_ADITIVOS-VBELN
          I_MATNR_NEW = AT_ADITIVOS-MATNR
        IMPORTING
          E_VBELN     = DATA(E_VBELN)
          E_MATNR     = DATA(E_MATNR).

      CALL METHOD REPROCESSA_DESCONTO_ABS
        EXPORTING
          I_SIMULADOR = AT_ADITIVOS-DOC_SIMULACAO
          I_VBELN     = E_VBELN
          I_MATNR     = E_MATNR
        IMPORTING
          R_RETURN    = E_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

      CALL METHOD SEND_APROVADORES_EMBARQUE
        EXPORTING
          I_VBELN  = AT_ADITIVOS-VBELN
          I_POSNN  = AT_ADITIVOS-POSNN
          I_VBELV  = AT_ADITIVOS-VBELV
        IMPORTING
          R_RETURN = E_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ELSE.

      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>FILL_DELETE_ORDEM
        EXPORTING
          I_VBELN  = AT_MANUTENCAO-VBELN_NEW
        RECEIVING
          R_RETURN = E_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    CALL METHOD SET_MENSAGENS
      EXPORTING
        I_TYPE     = CONSTANTES-TYPE-INFO
        I_MENSAGEM = |{ TEXT-023 }|
      IMPORTING
        R_RETURN   = E_RETURN.

    APPEND LINES OF E_RETURN TO R_RETURN.
    APPEND INITIAL LINE TO R_RETURN.

*    IF AT_BACKGROUND IS INITIAL.
*      CALL METHOD EXIBE_MENSAGENS
*        EXPORTING
*          I_RETURN = R_RETURN.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_CADASTRO_PRECO.

    IS_OK = ABAP_FALSE.

    FREE: R_INCO1, R_WERKS.

    IF I_INCO1 IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = I_INCO1 ) TO R_INCO1.
    ENDIF.

    IF I_WERKS_FORNEC IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = I_WERKS_FORNEC ) TO R_WERKS.
    ENDIF.

    SELECT SINGLE *
       FROM ZSDT0036
      INTO LS_PRECO
       WHERE BUKRS        EQ I_BUKRS
        AND  WAERK        EQ I_WAERK
        AND  CULTURA      EQ I_CULTURA
        AND  MATNR        EQ I_MATNR
        AND  INCO1        IN R_INCO1
        AND  WERKS_FORNEC IN R_WERKS
        AND  ELIMINADO    NE ABAP_TRUE
        AND  VAL_ATE      GE SY-DATUM.

    CHECK SY-SUBRC EQ 0.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD GET_ZONA.

    IF I_LR IS NOT INITIAL.
      SELECT SINGLE LZONE
        FROM KNA1
        INTO E_ZONA
      WHERE KUNNR EQ I_LR.
    ENDIF.

    IF I_PC IS NOT INITIAL.
      SELECT SINGLE LZONE
        FROM LFA1
        INTO E_ZONA
      WHERE LIFNR EQ I_PC.
    ENDIF.

  ENDMETHOD.


  method troca_materiais_massa.

    free r_return.

    if i_check is not initial.
      call method check_dados_troca
        importing
          r_return = r_return.
    endif.

    if r_return is not initial.

      call method exibe_mensagens
        exporting
          i_return = r_return.

      if at_background is initial.
        call method set_mensagens
          exporting
            i_type     = '#'
            i_mensagem = |Validação Interna|
          importing
            r_return   = r_return.
      endif.

      return.
    endif.

    call method set_mensagens
      exporting
        i_type     = constantes-type-info
        i_mensagem = |{ text-022 }|
      importing
        r_return   = data(e_return).

    append lines of r_return to at_return.

    if at_background is initial.
      call method check_nova_ok.
    else.
      call method check_nova_ov_81
        importing
          e_nova_ov = data(e_nova_ov)
          e_agrupar = data(e_agrupar).
    endif.

    if at_nova_ov is initial.

      call method fill_dados_change
        importing
          e_vbak = data(ls_vbak)
          e_vbap = data(lt_vbap).

      call method set_aditivos
        importing
          r_return = r_return.

      append lines of r_return to at_return.

      call method fill_ordem_change
        changing
          i_vbak_new = ls_vbak
          i_vbap_new = lt_vbap
        receiving
          r_return   = r_return.

      append lines of r_return to at_return.

      at_manutencao-troca-ordem_new-vbeln = at_manutencao-troca-ordem_old-vbeln.
      at_manutencao-vbeln_new = at_manutencao-troca-ordem_old-vbeln.

      call method set_aditivos
        importing
          r_return = r_return.

      append lines of r_return to at_return.

    else.

      free lt_vbap.
      call method fill_dados_create
        importing
          e_vbak = ls_vbak
          e_vbap = lt_vbap
          e_vbkd = data(ls_vbkd)
          e_konv = data(ls_konv).

      call method fill_ordem_create
        changing
          i_vbak_new = ls_vbak
          i_vbap_new = lt_vbap
          i_vbkd_new = ls_vbkd
          i_konv_new = ls_konv
        receiving
          r_return   = r_return.

      append lines of r_return to at_return.

    endif.

    if not line_exists( r_return[ type = cl_abap_aab_utilities=>category_error ] ).

      call method clear_atributos.

      call method processa_desconto_abs
        importing
          r_return = r_return.

      append lines of r_return to at_return.

      call method hedge_troca_material
        importing
          r_return = r_return.

      append lines of r_return to at_return.

      call method chk_ov_valor_maior
        exporting
          i_vbeln_old = at_aditivos-vbelv
          i_matnr_old = at_aditivos-matnrv
          i_vbeln_new = at_aditivos-vbeln
          i_matnr_new = at_aditivos-matnr
        importing
          e_vbeln     = data(e_vbeln)
          e_matnr     = data(e_matnr).

      call method reprocessa_desconto_abs
        exporting
          i_simulador = at_aditivos-doc_simulacao
          i_vbeln     = e_vbeln
          i_matnr     = e_matnr
        importing
          r_return    = r_return.

      append lines of r_return to at_return.

      call method send_aprovadores_embarque
        exporting
          i_vbeln  = at_aditivos-vbeln
          i_posnn  = at_aditivos-posnn
          i_vbelv  = at_aditivos-vbelv
        importing
          r_return = r_return.

      append lines of r_return to at_return.

    endif.

    call method set_mensagens
      exporting
        i_type     = constantes-type-info
        i_mensagem = |{ text-023 }|
      importing
        r_return   = r_return.

    append lines of r_return to at_return.

    if at_background is initial.
      call method exibe_mensagens
        exporting
          i_return = at_return.
    endif.

    free r_return.
    append lines of at_return to r_return.

  endmethod.


  METHOD VERIFICAR_ITINERARIO.

    IF AT_BACKGROUND IS NOT INITIAL.
      IS_OK = ABAP_TRUE.
      RETURN.
    ENDIF.

    CHECK I_PC IS NOT INITIAL.
    CHECK I_LR IS NOT INITIAL.

    IS_OK = ABAP_FALSE.

    CALL METHOD GET_ZONA
      EXPORTING
        I_PC   = CONV #( |{ I_PC ALPHA = IN }| )
      IMPORTING
        E_ZONA = DATA(E_ZONA_PC).

    CALL METHOD GET_ZONA
      EXPORTING
        I_LR   = CONV #( |{ I_LR ALPHA = IN }| )
      IMPORTING
        E_ZONA = DATA(E_ZONA_LR).

    IF E_ZONA_PC IS INITIAL.
      IF I_CHECK IS INITIAL.
        MESSAGE |Parceiro: { I_PC ALPHA = OUT } sem Zona de Transporte!| TYPE 'I'.
      ENDIF.
      RETURN.
    ENDIF.

    IF E_ZONA_LR IS INITIAL.
      IF I_CHECK IS INITIAL.
        MESSAGE |Parceiro: { I_LR ALPHA = OUT } sem Zona de Transporte!| TYPE 'I'.
      ENDIF.
      RETURN.
    ENDIF.

    SELECT SINGLE A~ROUTE
        FROM TROLZ AS A
      INNER JOIN TVROT AS B ON A~ROUTE = B~ROUTE
            INTO @DATA(LV_ROUTE)
      WHERE A~LZONE EQ @E_ZONA_LR
        AND A~AZONE EQ @E_ZONA_PC
        AND B~SPRAS EQ @SY-LANGU.

    CHECK LV_ROUTE IS NOT INITIAL.

    E_ROUTE = LV_ROUTE.
    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CALL_ORDER_CHANGE.

    FREE R_RETURN.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        SALESDOCUMENT     = AT_SALESDOCUMENT
        ORDER_HEADER_IN   = AT_ORDER_HEADER_IN
        ORDER_HEADER_INX  = AT_ORDER_HEADER_INX
        LOGIC_SWITCH      = AT_LOGIC_SWITCH
        BEHAVE_WHEN_ERROR = AT_BEHAVE_WHEN_ERROR
      TABLES
        ORDER_ITEM_IN     = AT_ORDER_ITEM_IN
        ORDER_ITEM_INX    = AT_ORDER_ITEM_INX
        CONDITIONS_IN     = AT_CONDITIONS_IN
        CONDITIONS_INX    = AT_CONDITIONS_INX
        SCHEDULE_LINES    = AT_SCHEDULE_LINES
        SCHEDULE_LINESX   = AT_SCHEDULE_LINESX
        RETURN            = R_RETURN.

    IF NOT LINE_EXISTS( R_RETURN[ TYPE = CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.

      CHECK LINE_EXISTS( R_RETURN[ TYPE = CONSTANTES-TYPE-SUCESS NUMBER = 311 ] ).

      DELETE R_RETURN WHERE TYPE NE CONSTANTES-TYPE-SUCESS.
      DELETE R_RETURN WHERE NUMBER NE 311.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.


  METHOD CALL_ORDER_CREATE.

    DATA: AT_CONTADOR TYPE I.

    FREE R_RETURN.

    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
      EXPORTING
        SALES_HEADER_IN     = AT_SALES_HEADER_IN
      IMPORTING
        SALESDOCUMENT_EX    = AT_SALESDOCUMENT
      TABLES
        RETURN              = R_RETURN
        SALES_PARTNERS      = AT_PARTNERS
        SALES_ITEMS_IN      = AT_ORDER_ITEM_IN
        SALES_SCHEDULES_IN  = AT_SCHEDULE_LINES
        SALES_CONDITIONS_IN = AT_CONDITIONS_IN
        SALES_TEXT          = AT_ORDER_TEXT
        EXTENSIONIN         = AT_EXTENSIONIN.

    IF NOT LINE_EXISTS( R_RETURN[ TYPE = CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.

      CHECK LINE_EXISTS( R_RETURN[ TYPE = CONSTANTES-TYPE-SUCESS NUMBER = 311 ] ).

      DELETE R_RETURN WHERE TYPE NE CONSTANTES-TYPE-SUCESS.
      DELETE R_RETURN WHERE NUMBER NE 311.

      DO.

        IF AT_CONTADOR EQ 30.
          EXIT.
        ENDIF.

        CALL METHOD CHK_CRIACAO_OV
          EXPORTING
            I_VBELN = AT_SALESDOCUMENT
          IMPORTING
            IS_OK   = DATA(IS_OK).

        IF IS_OK IS NOT INITIAL.
          EXIT.
        ENDIF.

        WAIT UP TO '0.2' SECONDS.

        ADD 1 TO AT_CONTADOR.

      ENDDO.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.



  ENDMETHOD.


  method check_cadastro_forn_sisdev.

**  Projeto Insumo CS2025000249 - AOENNING.
    "Verificar Região do centro fornecedor.
    select single regio
      from  t001w
    into @data(vg_regio)
    where werks eq @i_werks.

    select single *
      from kna1
      into @data(wa_kna1)
    where kunnr eq @i_kunnr.

    check sy-subrc is initial.
    check wa_kna1-regio eq 'MT' and vg_regio eq 'MT'.

    select count(*)
    from zsdt0205
      into @data(vl_count)
    where cpfcnpj eq @wa_kna1-stcd1
       or cpfcnpj eq @wa_kna1-stcd2.

    check sy-subrc ne 0.

    select count(*)
      from zsdt0206
    into @data(vl_count_)
  where cnpj eq @wa_kna1-stcd1
     or cnpj eq @wa_kna1-stcd2.

    check sy-subrc ne 0.

    select count(*)
      from zsdt0216
    into @data(vl_count__)
  where kunnr eq @i_kunnr.
    check sy-subrc ne 0.

    e_erro = abap_true.

    clear: vg_regio.
**  Projeto Insumo CS2025000249 - AOENNING.
  endmethod.


  method check_dados_troca.

    free r_return.

    if at_manutencao-troca-ordem_old-auart ne constantes-auart-zrfu and
       at_manutencao-troca-ordem_old-auart ne constantes-auart-zrem.
    else.

      if at_manutencao-troca-ordem_old-werks <> at_manutencao-troca-ordem_new-werks.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ text-e07 } { at_manutencao-troca-ordem_old-auart }, { text-e08 }|
          importing
            r_return   = data(e_return).
        append lines of e_return to r_return.
      endif.

      if at_manutencao-troca-ordem_old-matkl <> at_manutencao-troca-ordem_new-matkl.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ text-e06 }|
          importing
            r_return   = e_return.
        append lines of e_return to r_return.
      endif.

      if at_manutencao-troca-ordem_old-inco1 <> at_manutencao-troca-ordem_new-inco1.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ text-e07 } { at_manutencao-troca-ordem_old-auart }, { text-e09 }|
          importing
            r_return   = e_return.
        append lines of e_return to r_return.
      endif.

      select single count(*)
        from vbap
      where vbeln eq at_manutencao-troca-ordem_old-vbeln
        and matnr eq at_manutencao-troca-ordem_new-matnr.

      if sy-subrc is initial.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ text-e11 } { at_manutencao-troca-ordem_new-matnr }|
          importing
            r_return   = e_return.
        append lines of e_return to r_return.
      endif.

      call method zcl_manutencao_insumos=>verificar_itinerario
        exporting
          i_pc  = conv #( at_manutencao-troca-ordem_new-werks )
          i_lr  = at_manutencao-troca-ordem_old-kunnr
        importing
          is_ok = data(is_ok).

      if is_ok is initial.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ text-e12 } { at_manutencao-troca-ordem_new-matnr alpha = out }/{ at_manutencao-troca-ordem_old-kunnr alpha = out }|
          importing
            r_return   = e_return.
        append lines of e_return to r_return.
        return.
      endif.
    endif.

    if at_manutencao-troca-ordem_old-qtd_removida is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-e01
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    if at_manutencao-troca-ordem_new-matnr is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-e02
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    if at_manutencao-troca-ordem_new-qtd_recebida is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-e03
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    if at_manutencao-troca-ordem_new-lgort is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-e04
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    select count(*)
      from mard
       where matnr eq at_manutencao-troca-ordem_new-matnr
         and werks eq at_manutencao-troca-ordem_new-werks
         and lgort eq at_manutencao-troca-ordem_new-lgort.

    if sy-subrc is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-e05
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    select single count(*)
     from zsdt0087
     where matkl eq @at_manutencao-troca-ordem_new-matkl
       and tpsim eq @at_manutencao-troca-ordem_old-tpsim
       and inco1 eq @at_manutencao-troca-ordem_new-inco1.

    if sy-subrc is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |Grp. Material: { at_manutencao-troca-ordem_new-matkl }, | &&
                       |Tp Simulador: { at_manutencao-troca-ordem_old-tpsim }, | &&
                       |Incoterms: { at_manutencao-troca-ordem_new-inco1 }. | &&
                       |Combinação não parametrizada na Transação ZSDT0085.|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    call method zcl_manutencao_insumos=>verificar_itinerario
      exporting
        i_pc  = conv #( at_manutencao-troca-ordem_new-werks )
        i_lr  = at_manutencao-troca-ordem_old-kunnr
      importing
        is_ok = is_ok.

    if is_ok is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |{ text-e12 } { at_manutencao-troca-ordem_new-werks }/{ at_manutencao-troca-ordem_old-kunnr }|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    if at_manutencao-saldo_ov ne at_manutencao-troca-ordem_old-qtd_removida.

      call method chk_desconto_abs_faturado
        exporting
          i_vbeln = at_manutencao-troca-ordem_old-vbeln
          i_matnr = at_manutencao-troca-ordem_old-matnr
        importing
          is_ok   = is_ok.

      if is_ok is not initial.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = text-e13
          importing
            r_return   = e_return.
        append lines of e_return to r_return.
      endif.

    endif.

    call method chk_regra_proporcional
      exporting
        i_matnr      = at_manutencao-troca-ordem_old-matnr
        i_quantidade = at_manutencao-troca-ordem_old-qtd_removida
      importing
        e_msg        = data(e_msg).

    if e_msg is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |Origem: { e_msg }|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    clear e_msg.
    call method chk_regra_proporcional
      exporting
        i_matnr      = at_manutencao-troca-ordem_new-matnr
        i_quantidade = at_manutencao-troca-ordem_new-qtd_recebida
      importing
        e_msg        = e_msg.

    if e_msg is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |Destino: { e_msg }|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    clear e_msg.
    call method chk_limite_quantidade
      exporting
        i_vbeln = at_manutencao-troca-ordem_old-vbeln
        i_posnr = at_manutencao-troca-ordem_old-posnr
        i_qtde  = at_manutencao-troca-ordem_old-qtd_removida
      importing
        e_msg   = e_msg.

    if e_msg is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = e_msg
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    call method zcl_manutencao_insumos=>chk_divergencia_ov_simulador
      exporting
        i_vbeln               = at_manutencao-troca-ordem_old-vbeln
        i_matnr               = at_manutencao-troca-ordem_old-matnr
        i_simulador           = at_manutencao-doc_simulacao
      importing
        e_ordem_venda         = data(e_ordem_venda)
        e_simulador           = data(e_simulador)
        e_diferenca_simulador = data(e_diferenca).

    if e_diferenca is not initial.

      call method zcl_manutencao_insumos=>get_tolerancia_desconto_abs
        importing
          e_tolerancia = data(e_tolerancia).

      data(e_tolerancia_negativo) = e_tolerancia * -1.

      if e_diferenca not between e_tolerancia_negativo and e_tolerancia.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
*           i_mensagem = |{ text-024 } # { e_diferenca } #|
            i_mensagem = |Totais: Ordem de Venda: { e_ordem_venda }, Simulador: { e_simulador }, Diferença: { e_diferenca }|
          importing
            r_return   = e_return.

        append lines of e_return to r_return.
      endif.

    endif.

    clear is_ok.
    call method chk_desc_aplicado_simulador
      exporting
        i_simulador = at_manutencao-doc_simulacao
      importing
        is_ok       = is_ok.

    if is_ok is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |{ text-027 } { at_manutencao-doc_simulacao }|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

  endmethod.


  METHOD CHECK_NOVA_OK.

    CLEAR AT_NOVA_OV.

    IF AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-MATKL AND
       AT_MANUTENCAO-TROCA-ORDEM_NEW-WERKS EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-WERKS AND
       AT_MANUTENCAO-TROCA-ORDEM_NEW-INCO1 EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-INCO1.

      IF AT_MANUTENCAO-TROCA-ORDEM_NEW-SPART EQ CONSTANTES-SPART-_02.
        CASE AT_MANUTENCAO-TROCA-ORDEM_OLD-AUART.
          WHEN CONSTANTES-AUART-ZRFU OR CONSTANTES-AUART-ZREM.
            RETURN.
          WHEN OTHERS.
            IF AT_MANUTENCAO-TROCA-ORDEM_NEW-MTART EQ CONSTANTES-MTART-ZHAW.
              AT_NOVA_OV = ABAP_TRUE.
              RETURN.
            ENDIF.

            SELECT SINGLE COUNT(*)
              FROM MARA
            WHERE MATNR EQ @AT_MANUTENCAO-TROCA-ORDEM_OLD-MATNR
              AND MTART EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-MTART.

            IF SY-SUBRC IS INITIAL.
              AT_NOVA_OV = ABAP_TRUE.
              RETURN.
            ENDIF.

        ENDCASE.
      ENDIF.

      SELECT COUNT(*)
        FROM VBAP
      WHERE VBELN EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN
        AND MATNR EQ AT_MANUTENCAO-TROCA-ORDEM_NEW-MATNR.

      IF SY-SUBRC IS INITIAL.
        AT_NOVA_OV = ABAP_TRUE.
        RETURN.
      ENDIF.

    ELSE.

      IF ( AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-MATKL ) AND
         ( AT_MANUTENCAO-TROCA-ORDEM_NEW-INCO1 NE AT_MANUTENCAO-TROCA-ORDEM_OLD-INCO1 ) AND
         ( AT_MANUTENCAO-TROCA-ORDEM_NEW-SPART EQ CONSTANTES-SPART-_02 ) AND
         ( AT_MANUTENCAO-TROCA-ORDEM_OLD-AUART EQ CONSTANTES-AUART-ZRFU OR
           AT_MANUTENCAO-TROCA-ORDEM_OLD-AUART EQ CONSTANTES-AUART-ZREM ).
        RETURN.
      ELSE.
        AT_NOVA_OV = ABAP_TRUE.
        RETURN.
      ENDIF.

    ENDIF.

    IF AT_PONTO_COLETA IS NOT INITIAL.

      SELECT COUNT(*)
       FROM VBPA
     WHERE VBELN EQ @AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN
       AND LIFNR EQ @AT_PONTO_COLETA
       AND PARVW EQ @CONSTANTES-PARVW-PC.

      IF SY-SUBRC IS NOT INITIAL.
        AT_NOVA_OV = ABAP_TRUE.
        RETURN.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD CLEAR_ATRIBUTOS.

    FREE:
    AT_VBAK,
    AT_VBAP,
    AT_ORDER_TEXT,
    AT_SALES_HEADER_IN,
    AT_PARTNERS,
    AT_EXTENSIONIN,
    AT_ORDER_ITEM_IN,
    AT_ORDER_ITEM_INX,
    AT_CONDITIONS_IN,
    AT_CONDITIONS_INX,
    AT_SCHEDULE_LINES,
    AT_SCHEDULE_LINESX,
    AT_ORDER_HEADER_IN,
    AT_ORDER_HEADER_INX,
    AT_LOGIC_SWITCH,
    AT_BEHAVE_WHEN_ERROR.

    CHECK SET_ALL IS NOT INITIAL.

    FREE:
    AT_ADITIVOS,
    AT_RETURN,
    AT_CATEGORIA.

    CHECK AT_BACKGROUND IS INITIAL.

    FREE:
    AT_MANUTENCAO,
    AT_CH_REFERENCIA,
    AT_ITEM_DISTRIB.

  ENDMETHOD.


  METHOD SET_DADOS_TROCA.

    DATA: LV_MATNR TYPE C LENGTH 18.

    CALL METHOD CLEAR_ATRIBUTOS
      EXPORTING
        SET_ALL = ABAP_TRUE.

    CALL METHOD ZCL_MANUTENCAO_INSUMOS=>CLEAR_ATRIBUTOS
      EXPORTING
        SET_ALL = ABAP_TRUE.

    AT_MANUTENCAO = I_MANUTENCAO.

    IF AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL IS INITIAL.

      LV_MATNR = |{ AT_MANUTENCAO-TROCA-ORDEM_NEW-MATNR ALPHA = IN }|.

      SELECT SINGLE MATKL
        FROM MARA
        INTO AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL
        WHERE MATNR EQ LV_MATNR.

    ENDIF.

    IF AT_MANUTENCAO-TROCA-ORDEM_NEW-AUART IS INITIAL
    OR AT_MANUTENCAO-TROCA-ORDEM_NEW-SPART IS INITIAL.

      SELECT SINGLE AUART, SPART
          FROM ZSDT0087
        INTO ( @AT_MANUTENCAO-TROCA-ORDEM_NEW-AUART, @AT_MANUTENCAO-TROCA-ORDEM_NEW-SPART )
          WHERE MATKL EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL
            AND TPSIM EQ @AT_MANUTENCAO-TROCA-ORDEM_OLD-TPSIM
            AND INCO1 EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-INCO1.

    ENDIF.

    AT_MANUTENCAO-DOC_SIMULACAO = AT_MANUTENCAO-TROCA-DOC_SIMULACAO.
    AT_MANUTENCAO-VBELN_OLD     = AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.
    AT_MANUTENCAO-MATNR_OLD     = AT_MANUTENCAO-TROCA-ORDEM_OLD-MATNR.
    AT_MANUTENCAO-QTD_REMOVIDA  = AT_MANUTENCAO-TROCA-ORDEM_OLD-QTD_REMOVIDA.

    AT_CATEGORIA = CONSTANTES-CATEGORIA-TROCA.

    CALL METHOD GET_PC_VIA_ROTEIRO
      EXPORTING
        I_ROTA_PC = AT_MANUTENCAO-TROCA-ORDEM_NEW-NR_ROT_PC_81
      IMPORTING
        E_PC      = AT_PONTO_COLETA.

  ENDMETHOD.


  METHOD CHECK_DADOS_INCOMPLETOS.

    DATA: E_DDTXT TYPE RMDI_DDTXT.

    FREE: R_RETURN.

    SELECT FDNAM
      FROM VBUV
      INTO TABLE @DATA(LT_VBUV)
    WHERE VBELN EQ @I_VBELN.

    CHECK LT_VBUV IS NOT INITIAL.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).
    <FS_RETURN>-TYPE    = CONSTANTES-TYPE-SUCESS.
    <FS_RETURN>-MESSAGE = |************ ORDEM IMCOMPLETA ************|.

    LOOP AT LT_VBUV INTO DATA(LS_VBUV).

      CALL FUNCTION 'RM_DDIC_TEXTS_GET'
        EXPORTING
          I_NAME                = LS_VBUV-FDNAM
          I_TYPE                = 'DTEL'
          I_LANGU               = SY-LANGU
        IMPORTING
          E_DDTXT               = E_DDTXT
        EXCEPTIONS
          OBJTYPE_NOT_SUPPORTED = 1
          ILLEGAL_INPUT         = 2
          OTHERS                = 3.

      APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
      <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR.
      <FS_RETURN>-MESSAGE = COND #( WHEN E_DDTXT IS INITIAL
                                       THEN |{ TEXT-003 } { LS_VBUV-FDNAM }|
                                       ELSE |{ TEXT-003 } { E_DDTXT }| ).

    ENDLOOP.

  ENDMETHOD.


  method fill_dados_change.

    call method fill_troca_old
      importing
        e_konv = data(e_konv)
        e_vbkd = data(e_vbkd)
        e_vbak = e_vbak
        e_vbap = e_vbap.

    call method fill_desmembramento_old
      importing
        e_konv = e_konv
        e_vbkd = e_vbkd
        e_vbak = e_vbak
        e_vbap = e_vbap.

  endmethod.


  METHOD FILL_DADOS_CREATE.

    FREE AT_PRICE.

    CALL METHOD FILL_TROCA_NEW
      IMPORTING
        E_VBKD = E_VBKD
        E_VBAK = E_VBAK
        E_VBAP = E_VBAP
        E_KONV = E_KONV.

    CALL METHOD FILL_DESMEMBRAMENTO_NEW
      IMPORTING
        E_VBKD = E_VBKD
        E_VBAK = E_VBAK
        E_VBAP = E_VBAP
        E_KONV = E_KONV.

  ENDMETHOD.


  method fill_ordem_change.

    data: wl_tabix      type sy-tabix,
          v_saldo       type vbap-kwmeng,
          wl_posnr      type posnr_va,
          wl_vlr_covert type dzmeng,
          cont          type c,
          v_check       type c,
          lt_return     type bapiret2_t,
          lv_posnr      type posnr_va.

    free: at_vbak, at_vbap, at_vbep, r_return, lt_return.

    sort i_vbap_new by posnr.

    data(lt_vbap_old) = i_vbap_new.
    delete lt_vbap_old where posnr is initial.

    loop at i_vbap_new into data(ls_vbap_new).

      read table lt_return transporting no fields with key type = cl_abap_aab_utilities=>category_error.
      if sy-subrc is initial.
        continue.
      endif.

      clear lv_posnr.

      select *
        from vbap
        into table at_vbap
        where vbeln eq ls_vbap_new-vbeln
         and  posnr eq ls_vbap_new-posnr.

      if at_vbap[] is not initial.

        select single *
          from vbak
          into at_vbak
          where vbeln eq ls_vbap_new-vbeln.

        if sy-subrc is initial.

          select *
            from v_konv
          into table @data(it_konv)
            where knumv eq @at_vbak-knumv
              and kschl eq 'PR00'.

          clear at_vbep.
          select *
            from vbep
            into table at_vbep
             for all entries in at_vbap
              where vbeln eq at_vbap-vbeln
                and posnr eq at_vbap-posnr
                and wmeng ne 0.

          at_salesdocument = ls_vbap_new-vbeln.
          at_order_header_inx-updateflag = 'U'.

          at_logic_switch =
          value #(
                    cond_handl = abap_true
                    pricing    = 'G'
                 ).

          loop at at_vbap into data(ls_vbap).

            free: at_order_item_in, at_order_item_inx, at_schedule_lines, at_schedule_linesx, at_conditions_in, at_conditions_inx.

            if sy-subrc is initial.
              wl_tabix = sy-tabix.
              v_saldo = ls_vbap_new-zmeng.

              if v_saldo eq 0.
                v_saldo = 1.
                v_check = abap_true.
              endif.

              data(saldo) = ls_vbap_new-zmeng.
              if saldo eq 0.
                saldo = 1.
              endif.

              loop at at_vbep assigning field-symbol(<vbep>) where vbeln eq ls_vbap-vbeln and
                                                                   posnr eq ls_vbap-posnr.

                if saldo is initial.
                  <vbep>-wmeng = 0.
                  continue.
                endif.

                if saldo > <vbep>-wmeng.
                  saldo = saldo - <vbep>-wmeng.
                else.
                  <vbep>-wmeng = saldo.
                  saldo = 0.
                endif.

              endloop.

              append initial line to at_order_item_in assigning field-symbol(<fs_order_item_in>).
              <fs_order_item_in>-itm_number = ls_vbap-posnr.
              <fs_order_item_in>-target_qty = v_saldo.

              append initial line to at_order_item_inx assigning field-symbol(<fs_order_item_inx>).
              <fs_order_item_inx>-updateflag = 'U'.
              <fs_order_item_inx>-itm_number = ls_vbap-posnr.
              <fs_order_item_inx>-target_qty = abap_true.

              if v_check eq abap_true.

                clear at_logic_switch-pricing.

                append initial line to at_conditions_in assigning field-symbol(<fs_conditions_in>).
                if line_exists( it_konv[ kposn = ls_vbap-posnr ] ).
                  <fs_conditions_in>-cond_unit  = switch #( it_konv[ kposn = ls_vbap-posnr ]-kmein when 'TO' then 'KG' ).
                endif.

                <fs_conditions_in>-itm_number = ls_vbap-posnr.
                <fs_conditions_in>-cond_count = '01'.
                <fs_conditions_in>-cond_type  = 'PR00'.
                <fs_conditions_in>-cond_value = '0.1'.

                append initial line to at_conditions_inx assigning field-symbol(<fs_conditions_inx>).
                if line_exists( it_konv[ kposn = ls_vbap-posnr ] ).
                  <fs_conditions_inx>-cond_unit = switch #( it_konv[ kposn = ls_vbap-posnr ]-kmein when 'TO' then abap_true ).
                endif.

                <fs_conditions_inx>-itm_number = ls_vbap-posnr.
                <fs_conditions_inx>-cond_count = '01'.
                <fs_conditions_inx>-cond_type  = 'PR00'.
                <fs_conditions_inx>-cond_value = abap_true.
                <fs_conditions_inx>-updateflag = 'U'.

                append initial line to at_conditions_in assigning <fs_conditions_in>.
                <fs_conditions_in>-itm_number = ls_vbap-posnr.
                <fs_conditions_in>-cond_count = '01'.
                <fs_conditions_in>-cond_type  = 'RB00'.
                <fs_conditions_in>-cond_value = 0.
                <fs_conditions_in>-cond_unit  = ls_vbap_new-vrkme.
                <fs_conditions_in>-currency   = at_vbak-waerk.

                append initial line to at_conditions_inx assigning <fs_conditions_inx>.
                <fs_conditions_inx>-itm_number = ls_vbap-posnr.
                <fs_conditions_inx>-cond_count = '01'.
                <fs_conditions_inx>-cond_type  = 'RB00'.
                <fs_conditions_inx>-cond_value = abap_true.
                <fs_conditions_inx>-cond_unit  = abap_true.
                <fs_conditions_inx>-currency   = abap_true.
                <fs_conditions_inx>-updateflag = 'U'.

              endif.

              loop at at_vbep into data(ls_vbep) where vbeln eq ls_vbap-vbeln
                                                  and  posnr eq ls_vbap-posnr.

                append initial line to at_schedule_lines assigning field-symbol(<fs_schedule_lines>).
                <fs_schedule_lines>-itm_number = ls_vbap-posnr.
                <fs_schedule_lines>-sched_line = ls_vbep-etenr.
                <fs_schedule_lines>-req_qty    = ls_vbep-wmeng.

                if v_check eq abap_true.
                  <fs_schedule_lines>-req_dlv_bl    = '12'.
                endif.

                append initial line to at_schedule_linesx assigning field-symbol(<fs_schedule_linesx>).
                <fs_schedule_linesx>-updateflag = 'U'.
                <fs_schedule_linesx>-itm_number = ls_vbap-posnr.
                <fs_schedule_linesx>-sched_line = ls_vbep-etenr.
                <fs_schedule_linesx>-req_qty    = abap_true.

                if v_check eq abap_true.
                  <fs_schedule_linesx>-req_dlv_bl    = abap_true.
                endif.

              endloop.

              call method call_order_change
                receiving
                  r_return = data(e_return).

              append lines of e_return to r_return.

              call method clear_atributos.

              if not line_exists( r_return[ type = cl_abap_aab_utilities=>category_error ] ).

                call method set_desconto_abs
                  exporting
                    i_vbeln        = ls_vbap-vbeln
                    i_posnr        = ls_vbap-posnr
                    i_desconto_abs = 0
                    i_zerar        = abap_true.

                call method clear_atributos.

                call method set_desconto_abs_ov
                  exporting
                    i_vbeln = ls_vbap-vbeln
                    i_posnr = ls_vbap-posnr.
              else.
                call function 'BAPI_TRANSACTION_ROLLBACK'.
              endif.

              append initial line to lt_return assigning field-symbol(<fs_return>).
              <fs_return>-type    = constantes-type-sucess.
              <fs_return>-message = |************ CHANGE QUANTIDADE OV ************ |.

              append lines of r_return to lt_return.

*              CLEAR:WL_RETURN.
*              READ TABLE AT_RETURN INTO WL_RETURN WITH KEY TYPE = 'E'.
*              IF SY-SUBRC IS NOT INITIAL.
*                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                  EXPORTING
*                    WAIT = 'X'.
*
*                READ TABLE AT_RETURN INTO WL_RETURN WITH KEY TYPE   = CONSTANTES-TYPE-SUCESS
*                                                             NUMBER = 311.
*                IF SY-SUBRC IS INITIAL.
*                  APPEND WL_RETURN TO R_RETURN.
*                ENDIF.
*              ELSE.
*                APPEND LINES OF AT_RETURN TO R_RETURN.
*
*                SELECT SINGLE *
*                  FROM VBAP
*                  INTO @DATA(WVBAP)
*                  WHERE VBELN EQ @NVBELN AND
*                        POSNR EQ @NPOSNR.
*
*                DATA(ORDER_HEADER_INX) = VALUE BAPISDH1X( UPDATEFLAG = 'U' ).
*                DATA(ORDER_ITEM_IN) = VALUE WISO_T_SDITM( (
*                                                            ITM_NUMBER = WVBAP-POSNR
*                                                            MATERIAL   = WVBAP-MATNR
*                                                        ) ).
*
*                DATA(ORDER_ITEM_INX) = VALUE WISO_T_SDITMX( (
*                                                              ITM_NUMBER = WVBAP-POSNR
*                                                              MATERIAL   = WVBAP-MATNR
*                                                              UPDATEFLAG = 'D'
*                                                          ) ).
*
*                CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*                  EXPORTING
*                    SALESDOCUMENT    = WVBAP-VBELN
*                    ORDER_HEADER_INX = ORDER_HEADER_INX
*                  TABLES
*                    ORDER_ITEM_IN    = ORDER_ITEM_IN
*                    ORDER_ITEM_INX   = ORDER_ITEM_INX
*                    RETURN           = AT_RETURN.
*
*                IF NOT LINE_EXISTS( AT_RETURN[ TYPE = 'E' ] ).
*                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                    EXPORTING
*                      WAIT = ABAP_TRUE.
**                  APPEND LINES OF AT_RETURN TO TE_RETURN.
*                ELSE.
**                  APPEND LINES OF AT_RETURN TO TE_RETURN.
*                ENDIF.
*              ENDIF.
            endif.

          endloop.
        endif.
      else.

        refresh: at_order_item_in, at_order_item_inx, at_schedule_lines, at_schedule_linesx,
                 at_conditions_in.

        clear:   at_order_item_in, at_order_item_inx, at_conditions_in, at_conditions_inx.

        select single *
          from vbak
          into @data(_vbak)
          where vbeln eq @ls_vbap_new-vbeln.

        select *
          up to 1 rows
          from vbap
          into table at_vbap
            where vbeln eq ls_vbap_new-vbeln
            order by posnr descending.

        read table at_vbap into ls_vbap index 1.

        at_salesdocument = ls_vbap_new-vbeln.

        append initial line to at_order_item_in assigning <fs_order_item_in>.
        <fs_order_item_in>-store_loc    = ls_vbap_new-lgort.
        lv_posnr = ls_vbap-posnr + 10.
        <fs_order_item_in>-itm_number   = lv_posnr.

        at_manutencao-troca-ordem_new-posnr = <fs_order_item_in>-itm_number.
        at_manutencao-vbeln_new = ls_vbap_new-vbeln.
        at_manutencao-matnr_new = ls_vbap_new-matnr.

        <fs_order_item_in>-target_qty   = ls_vbap_new-zmeng.
*        <FS_ORDER_ITEM_IN>-TARGET_QU    = LS_VBAP_NEW-VRKME.
*        <FS_ORDER_ITEM_IN>-SALES_UNIT   = LS_VBAP_NEW-VRKME.

        case ls_vbap-spart.
          when '02' or '04'.
            if ls_vbap_new-vrkme eq 'TO'.
              <fs_order_item_in>-target_qty = ls_vbap_new-zmeng * 1000.
              <fs_order_item_in>-target_qu  = 'KG'.
              <fs_order_item_in>-sales_unit = 'KG'.
            endif.
        endcase.

        case _vbak-auart.
          when 'ZFTE' or 'ZSEM' or 'ZOSM' or 'ZOFE'.
            if ls_vbap_new-vrkme eq 'TO'.
              data(lv_vrkme) = 'KG'.
            else.
              lv_vrkme = ''.
            endif.
          when others.
            lv_vrkme = ''.
        endcase.

        <fs_order_item_in>-material     = |{ ls_vbap_new-matnr alpha = in }|.
        <fs_order_item_in>-usage_ind    = 'I'.
        <fs_order_item_in>-plant        = ls_vbap_new-werks.
        <fs_order_item_in>-batch        = ls_vbap_new-charg.
        <fs_order_item_in>-ship_point   = ls_vbap_new-werks.
        <fs_order_item_in>-matfrgtgrp   = '00000001'.

        if at_ch_referencia is not initial.
          <fs_order_item_in>-purch_no_s = |{ at_ch_referencia }-{ at_item_distrib }|.
        endif.

        append initial line to at_order_item_inx assigning <fs_order_item_inx>.
        <fs_order_item_inx>-updateflag   = 'I'.
        <fs_order_item_inx>-itm_number   = <fs_order_item_in>-itm_number.
        <fs_order_item_inx>-store_loc    = abap_true.
        <fs_order_item_inx>-material     = abap_true.
        <fs_order_item_inx>-target_qty   = abap_true.
*        <FS_ORDER_ITEM_INX>-TARGET_QU    = ABAP_TRUE.
*        <FS_ORDER_ITEM_INX>-SALES_UNIT   = ABAP_TRUE.
        <fs_order_item_inx>-usage_ind    = abap_true.
        <fs_order_item_inx>-plant        = abap_true.
        <fs_order_item_inx>-batch        = abap_true.
        <fs_order_item_inx>-ship_point   = abap_true.
        <fs_order_item_inx>-matfrgtgrp   = abap_true.

        if <fs_order_item_inx>-purch_no_s is not initial.
          <fs_order_item_inx>-purch_no_s = abap_true.
        endif.

        append initial line to at_schedule_lines assigning <fs_schedule_lines>.
        <fs_schedule_lines>-itm_number = <fs_order_item_in>-itm_number.
        <fs_schedule_lines>-req_qty    = <fs_order_item_in>-target_qty.
        <fs_schedule_lines>-req_dlv_bl = '10'.

        append initial line to at_schedule_linesx assigning <fs_schedule_linesx>.
        <fs_schedule_linesx>-updateflag  = 'I'.
        <fs_schedule_linesx>-itm_number  = <fs_schedule_lines>-itm_number.
        <fs_schedule_linesx>-req_qty     = abap_true.
        <fs_schedule_linesx>-req_dlv_bl  = abap_true.


        append initial line to at_conditions_in assigning field-symbol(<ls_conditions_in>).
        <ls_conditions_in>-itm_number  = <fs_order_item_in>-itm_number.
        <ls_conditions_in>-currency    = ls_vbap-waerk.

        if lv_vrkme is not initial.
          <ls_conditions_in>-cond_unit   = ls_vbap_new-vrkme.
        endif.

** US-169490-ZSDT0087 WBARBOSA 27/08/2025 INICIO
        if at_categoria ne constantes-categoria-troca.
          read table lt_vbap_old into data(ls_vbap_old) index 1.
          call method get_preco_liquido_41
            exporting
              i_simulador     = at_manutencao-doc_simulacao
              i_vbeln         = ls_vbap_old-vbeln
              i_matnr         = ls_vbap_old-matnr
            importing
              e_preco_liquido = data(e_preco_liquido).
        endif.
* US-169490-ZSDT0087 WBARBOSA 27/08/2025 INICIO

        if e_preco_liquido is not initial.
          <ls_conditions_in>-cond_value  = e_preco_liquido.
        else.

          call method zcl_solicitacao_ov=>get_imposto_v2
            exporting
              i_direcao     = 'D'
              i_cliente     = _vbak-kunnr
              i_fornecedor  = conv #( |{ ls_vbap_new-werks alpha = in }| )
              i_material    = ls_vbap_new-matnr
              i_tipo_ordem  = _vbak-auart
              i_werks       = ls_vbap_new-werks
            receiving
              i_coeficiente = data(coeficiente_d).

          <ls_conditions_in>-cond_value = ls_vbap_new-netpr * coeficiente_d.

        endif.

        <ls_conditions_in>-cond_type   = 'PR00'.

        append initial line to at_conditions_inx assigning <fs_conditions_inx>.
        <fs_conditions_inx>-itm_number  = <fs_order_item_in>-itm_number.
        <fs_conditions_inx>-currency    = abap_true.
        <fs_conditions_inx>-cond_value  = abap_true.

        if lv_vrkme is not initial.
          <fs_conditions_inx>-cond_unit   = abap_true.
        endif.

        <fs_conditions_inx>-cond_type   = abap_true.

*        IF NOT I_VBAP_NEW-DIF_DESC IS INITIAL.
*
*          IT_0041-VBELN = AT_VBAP-VBELN.
*          IT_0041-POSNR = AT_ORDER_ITEM_IN-ITM_NUMBER.
*          IT_0041-MATNR = AT_ORDER_ITEM_IN-MATERIAL.
*          IT_0041-DESC_ABSOLUTO = I_VBAP_NEW-DIF_DESC.
*
*          APPEND IT_0041.
*
*        ENDIF.

        at_order_header_inx-updateflag = 'U'.
        at_behave_when_error = 'P'.

        call method call_order_change
          receiving
            r_return = e_return.

        append lines of e_return to r_return.

        call method clear_atributos.

        call method zcl_manutencao_insumos=>set_desconto_abs_ov
          exporting
            i_vbeln  = ls_vbap_new-vbeln
            i_posnr  = lv_posnr
          importing
            r_return = e_return.

        append lines of e_return to r_return.

        append initial line to lt_return assigning <fs_return>.
        <fs_return>-type    = constantes-type-sucess.
        <fs_return>-message = |************ CHANGE ITEM OV ************ |.
        append lines of r_return to lt_return.

      endif.
    endloop.

    r_return = lt_return.

  endmethod.


  method fill_ordem_create.

    data: obj type ref to zcl_util_sd.
    create object obj.

    data: lv_vbeln type vbeln_va.

    free: r_return.

    call method get_text_ov
      exporting
        i_vbeln = i_vbak_new-vbeln
      importing
        t_text  = data(lt_texto).

    at_order_text =
   value #( for ls in lt_texto (
     text_line  = ls
     text_id    = '0002'
     langu      = sy-langu
     format_col = '/' ) ).

    read table i_vbap_new into data(ls_vbap) index 1.
    at_sales_header_in-sales_org  = ls_vbap-vkorg_ana.
    at_sales_header_in-distr_chan = ls_vbap-vtweg_ana.
    at_sales_header_in-sales_off  = ls_vbap-vkbur_ana.
    at_sales_header_in-sales_grp  = i_vbak_new-vkgrp.
    at_sales_header_in-purch_no_c = i_vbak_new-bstnk.
    at_sales_header_in-purch_date = sy-datum.
    at_sales_header_in-currency   = ls_vbap-waerk.
    at_sales_header_in-pymt_meth  = i_vbkd_new-zlsch.
    at_sales_header_in-fix_val_dy = i_vbkd_new-valdt.
    at_sales_header_in-exrate_fi  = i_vbkd_new-kurrf.
    at_sales_header_in-pmnttrms   = i_vbkd_new-zterm.
    at_sales_header_in-incoterms1 = i_vbkd_new-inco1.
    at_sales_header_in-incoterms2 = i_vbkd_new-inco1.
    at_sales_header_in-division   = ls_vbap-spart.
    at_sales_header_in-doc_type   = i_vbak_new-auart.
    at_sales_header_in-ref_doc_l  = i_vbak_new-vbeln.

    at_partners = value #(
      ( partn_role = 'AG' partn_numb = i_vbak_new-kunnr )
      ( partn_role = 'PC' partn_numb = cond #( when at_ponto_coleta is initial then |{ ls_vbap-werks alpha = in }| else |{ at_ponto_coleta }| ) )
      ( partn_role = 'LR' partn_numb = i_vbak_new-kunnr )
      ( partn_role = 'RE' partn_numb = i_vbak_new-kunnr )
      ( partn_role = 'RG' partn_numb = i_vbak_new-kunnr )
      ( partn_role = 'WE' partn_numb = i_vbak_new-kunnr )
    ).

    call method obj->set_tp_pesagem_ov_simulador
      exporting
        centro  = ls_vbap-werks
        matnr   = ls_vbap-matnr
      receiving
        pesagem = data(lv_pesagem).

    data(ls_bape_vbak) = value bape_vbak( zpesagem = lv_pesagem ).

    append initial line to at_extensionin assigning field-symbol(<fs_extensionin>).
    <fs_extensionin>-structure = 'BAPE_VBAK'.
    <fs_extensionin>-valuepart1 = ls_bape_vbak.

    data(ls_bape_vbakx) = value bape_vbakx( zpesagem = abap_true ).

    append initial line to at_extensionin assigning <fs_extensionin>.
    <fs_extensionin>-structure = 'BAPE_VBAKX'.
    <fs_extensionin>-valuepart1 = ls_bape_vbakx.

    append initial line to at_order_item_in assigning field-symbol(<fs_order_item_in>).
    <fs_order_item_in>-store_loc  = ls_vbap-lgort.
    <fs_order_item_in>-itm_number = '000010'.

    <fs_order_item_in>-sales_unit = ls_vbap-vrkme.
    <fs_order_item_in>-target_qu  = ls_vbap-vrkme.
    <fs_order_item_in>-target_qty = ls_vbap-kwmeng.

    <fs_order_item_in>-usage_ind  = 'I'.
    <fs_order_item_in>-plant      = ls_vbap-werks.
    <fs_order_item_in>-batch      = ls_vbap-charg.
    <fs_order_item_in>-ship_point = ls_vbap-werks.
    <fs_order_item_in>-matfrgtgrp = '00000001'.
    <fs_order_item_in>-material   = ls_vbap-matnr.
    <fs_order_item_in>-route      = ls_vbap-route.

    if at_ch_referencia is not initial.
      <fs_order_item_in>-purch_no_s = |{ at_ch_referencia }-{ at_item_distrib }|.
    endif.

    if strlen( ls_vbap-matnr ) > 18.
      <fs_order_item_in>-material_long = ls_vbap-matnr.
    endif.

    append initial line to at_conditions_in assigning field-symbol(<fs_conditions_in>).
    <fs_conditions_in>-itm_number  = <fs_order_item_in>-itm_number.
    <fs_conditions_in>-currency    = ls_vbap-waerk.
    <fs_conditions_in>-cond_value  = at_price.
    <fs_conditions_in>-cond_unit   = ls_vbap-kmein.
    <fs_conditions_in>-conexchrat  = i_vbkd_new-kursk.
    <fs_conditions_in>-cond_type   = 'PR00'.

    append initial line to at_schedule_lines assigning field-symbol(<fs_schedule_lines>).
    <fs_schedule_lines>-itm_number = <fs_order_item_in>-itm_number.
    <fs_schedule_lines>-req_qty    = ls_vbap-kwmeng.
    <fs_schedule_lines>-req_dlv_bl = '10'.

    call method zcl_manutencao_insumos=>call_order_create
      receiving
        r_return = data(e_return).

    if at_salesdocument is initial.
      append initial line to r_return assigning field-symbol(<fs_return>).
      <fs_return>-type    = 'E'.
      <fs_return>-message = |Ov não foi Criado|.

      append lines of e_return to r_return.
      return.
    endif.

    if line_exists( e_return[ type = constantes-type-sucess number = 311 ] ).
      delete e_return where type ne constantes-type-sucess.
      delete e_return where number ne 311.
    endif.

    append initial line to r_return assigning <fs_return>.
    <fs_return>-type    = constantes-type-sucess.
    <fs_return>-message = |************ CRIANDO ORDEM ************|.

    append lines of e_return to r_return.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = abap_true.

    call method zcl_manutencao_insumos=>check_dados_incompletos
      exporting
        i_vbeln  = at_salesdocument
      changing
        r_return = e_return.

    if e_return is not initial.

      append lines of e_return to r_return.

      call method zcl_manutencao_insumos=>fill_delete_ordem
        exporting
          i_vbeln  = at_salesdocument
        receiving
          r_return = e_return.

      append lines of e_return to r_return.

    else.

      at_manutencao-troca-ordem_new-vbeln = at_salesdocument.
      at_manutencao-vbeln_new = at_salesdocument.

      call method clear_atributos.

      if at_desmembramento_devolucao is initial.

        call method zcl_manutencao_insumos=>set_desconto_abs_ov
          exporting
            i_vbeln  = at_salesdocument
          importing
            r_return = e_return.

        append lines of e_return to r_return.
      else.

        if at_desconto is not initial.

          call method set_desconto_abs
            exporting
              i_vbeln        = at_salesdocument
              i_posnr        = 10
              i_desconto_abs = conv #( at_desconto ).

        endif.
      endif.

      call method set_aditivos
        importing
          r_return = e_return.

      append lines of e_return to r_return.

      call method clear_atributos.

*"// Processo de Desmembramento proveniente de uma Devolução não realiza os processo abaixo
      check at_desmembramento_devolucao is initial.

      call method fill_dados_change
        importing
          e_vbak = data(ls_vbak)
          e_vbap = data(lt_vbap).

      delete lt_vbap where posnr is initial.

      call method fill_ordem_change
        changing
          i_vbak_new = ls_vbak
          i_vbap_new = lt_vbap
        receiving
          r_return   = e_return.

      append lines of e_return to r_return.

      if not line_exists( r_return[ type = cl_abap_aab_utilities=>category_error ] ).
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = abap_true.
      else.

        call function 'BAPI_TRANSACTION_ROLLBACK'.

        if at_manutencao-vbeln_old ne at_manutencao-vbeln_new.

          call method zcl_manutencao_insumos=>fill_delete_ordem
            exporting
              i_vbeln  = at_manutencao-vbeln_new
            receiving
              r_return = e_return.

          append lines of e_return to r_return.

        endif.
      endif.
    endif.
  endmethod.


  METHOD CHK_DESCONTO_ABS.

    DATA: T_KONV     TYPE KONV_TTY,
          VALOR_REAL TYPE KBETR,
          VALOR_VBAP TYPE KBETR,
          DIFERENCA  TYPE KBETR.

    FREE E_DESCONTO_ABS.

    SELECT SINGLE *
         FROM VBAK
         INTO @DATA(E_VBAK)
         WHERE VBELN EQ @I_VBELN.

    SELECT SINGLE *
         FROM VBAP
         INTO @DATA(E_VBAP)
         WHERE VBELN EQ @I_VBELN
           AND POSNR EQ @I_POSNR.

    SELECT COUNT(*)
      FROM VBEP
        WHERE VBELN EQ E_VBAP-VBELN
          AND POSNR EQ E_VBAP-POSNR
          AND LIFSP EQ '12'.

    CHECK SY-SUBRC IS NOT INITIAL.

    SELECT * FROM V_KONV
      INTO CORRESPONDING FIELDS OF TABLE @T_KONV
        WHERE KNUMV EQ @E_VBAK-KNUMV
          AND KSCHL IN ( 'PR00', 'RB00', 'ICVA', 'ICBS', 'ICMI' )
          AND KPOSN EQ @E_VBAP-POSNR.

*    IF SY-UNAME EQ 'WBARBOSA'.

*      BREAK-POINT.

    CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
      EXPORTING
        I_DIRECAO     = 'O'
        I_VBELN       = I_VBELN
        I_POSNR       = I_POSNR
      RECEIVING
        I_COEFICIENTE = DATA(COEFICIENTE_O).

    READ TABLE T_KONV INTO DATA(LS_PR00) WITH KEY KSCHL = 'PR00'.
    IF SY-SUBRC IS INITIAL.

      DATA(E_KBETRV_1) = LS_PR00-KBETR / COEFICIENTE_O.
      DATA(E_KMEIN) = LS_PR00-KMEIN.

    ENDIF.


*    ELSE.

*      READ TABLE T_KONV INTO LS_PR00 WITH KEY KSCHL = 'PR00'.
*      IF SY-SUBRC IS INITIAL.
*
*        DATA(E_KBETR) = LS_PR00-KBETR.
*        E_KMEIN = LS_PR00-KMEIN.
*
*        IF E_VBAP-MWSBP IS NOT INITIAL.
*          CALL METHOD GET_DESC_ABS
*            EXPORTING
*              I_KONV  = T_KONV
*            IMPORTING
*              E_KBETR = E_KBETR.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.

*   "// multiplicação da quantidade(KWMENG) com Preço(_KBETR) Convertendo a quantidade  na unidade de medida referente ao do Preço (_KMEIN)
*    IF SY-UNAME NE 'WBARBOSA'.
*
*      VALOR_REAL = COND #( WHEN I_VLR_REAL IS INITIAL THEN
*                  ( COND #(
*                            WHEN E_KMEIN EQ E_VBAP-VRKME
*                                  THEN E_VBAP-KWMENG
*                                  ELSE SWITCH #( E_VBAP-VRKME
*                                                  WHEN 'TO'
*                                                    THEN ( E_VBAP-KWMENG * 1000 )
*                                                  WHEN 'KG'
*                                                    THEN ( E_VBAP-KWMENG / 1000 )
*                                             )
*                          ) * E_KBETR
*                   ) ELSE I_VLR_REAL ).
*    ELSE.
    VALOR_REAL = COND #( WHEN I_VLR_REAL IS INITIAL THEN
          ( COND #(
                    WHEN E_KMEIN EQ E_VBAP-VRKME
                          THEN E_VBAP-KWMENG
                          ELSE SWITCH #( E_VBAP-VRKME
                                          WHEN 'TO'
                                            THEN ( E_VBAP-KWMENG * 1000 )
                                          WHEN 'KG'
                                            THEN ( E_VBAP-KWMENG / 1000 )
                                     )
                  ) * E_KBETRV_1
           ) ELSE I_VLR_REAL ).
*    ENDIF.
*   "// Soma valor Liquido(NETWR) com Imposto do iten(MWSBP)
    VALOR_VBAP = E_VBAP-NETWR + E_VBAP-MWSBP.

*   "// Subtração entre Valor da VBAP com Valor real
    DIFERENCA = VALOR_REAL - VALOR_VBAP.

*    IF I_VLR_REAL IS INITIAL.
*      DIFERENCA *= COEFICIENTE_O.
*    ENDIF.

    E_DESCONTO_ABS = DIFERENCA.

  ENDMETHOD.


  METHOD CHK_DESCONTO_ABS_FATURADO.

    IS_OK = ABAP_FALSE.
    IS_FAT = ABAP_FALSE.

    SELECT SINGLE KNUMV
      FROM VBAK
      INTO @DATA(LV_KNUMV)
      WHERE VBELN EQ @I_VBELN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE POSNR
      FROM VBAP
      INTO @DATA(LV_POSNR)
      WHERE VBELN EQ @I_VBELN
        AND MATNR EQ @I_MATNR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT COUNT(*)
      FROM VBFA
      WHERE VBELV   EQ I_VBELN
        AND POSNV   EQ LV_POSNR
        AND VBTYP_N EQ 'M'
        AND VBTYP_V EQ 'C'.

    CHECK SY-SUBRC IS INITIAL.

    IS_FAT = ABAP_TRUE.

    SELECT SUM( ABS( KBETR ) )
      FROM V_KONV
      INTO @DATA(LV_KBETR)
    WHERE KNUMV EQ @LV_KNUMV
      AND KPOSN EQ @LV_POSNR
      AND KSCHL IN ( 'RB00' ).

    CHECK LV_KBETR IS NOT INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHK_DESCONTO_ABS_OLD_NEW.

    CHECK AT_CATEGORIA EQ CONSTANTES-CATEGORIA-TROCA.

    DATA CHG_TOTAL TYPE NETWR.
    DATA NEW_TOTAL TYPE NETWR.
    DATA OLD_TOTAL TYPE NETWR.
    DATA VLR TYPE NETWR.

    FREE E_DESCONTO_ABS.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(E_VBAP_OLD)
      WHERE VBELN EQ @I_VBELN_OLD
        AND MATNR EQ @I_MATNR_OLD.

    SELECT COUNT(*)
      FROM VBEP
        WHERE VBELN EQ E_VBAP_OLD-VBELN
          AND POSNR EQ E_VBAP_OLD-POSNR
          AND LIFSP EQ '12'.

    IF SY-SUBRC IS INITIAL.
      FREE E_VBAP_OLD.
    ENDIF.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(E_VBAP_NEW)
      WHERE VBELN EQ @I_VBELN_NEW
        AND MATNR EQ @I_MATNR_NEW.

    SELECT COUNT(*)
      FROM VBEP
        WHERE VBELN EQ E_VBAP_NEW-VBELN
          AND POSNR EQ E_VBAP_NEW-POSNR
          AND LIFSP EQ '12'.

    IF SY-SUBRC IS INITIAL.
      FREE E_VBAP_NEW.
    ENDIF.

    ADD E_VBAP_OLD-NETWR TO OLD_TOTAL.
    ADD E_VBAP_OLD-MWSBP TO OLD_TOTAL.
    ADD E_VBAP_NEW-NETWR TO NEW_TOTAL.
    ADD E_VBAP_NEW-MWSBP TO NEW_TOTAL.
    CHG_TOTAL = OLD_TOTAL + NEW_TOTAL.

* "//  verifica se há diferenças
    IF AT_MANUTENCAO-TROCA-NETWR NE CHG_TOTAL.
      VLR = AT_MANUTENCAO-TROCA-NETWR - CHG_TOTAL.
      ADD VLR TO NEW_TOTAL.

      E_DESCONTO_ABS = NEW_TOTAL.
    ENDIF.

  ENDMETHOD.


  METHOD exibe_mensagens.

    CHECK at_background IS INITIAL.
    CHECK i_return IS NOT INITIAL.

    IF at_manutencao-troca IS NOT INITIAL.

      "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP --->>
      IF at_manutencao-troca-troca_massa EQ abap_true.
        at_mensagens = VALUE #(
           FOR ls IN i_return (
              msg = ls-message )
              ).
      ELSE.
        "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP <<---

        at_mensagens = VALUE #(
        FOR ls IN i_return (
            inco1 = at_manutencao-troca-ordem_old-inco1
            spart = at_manutencao-troca-ordem_new-spart
            auart = at_manutencao-troca-ordem_old-auart
            werks = at_manutencao-troca-ordem_old-werks
            vbeln = at_manutencao-troca-ordem_old-vbeln
            msg = ls-message ) ).

      ENDIF. "SD - Ajuste Troca em Massa ZSDT0087 - US 191708 - WPP
    ENDIF.

    IF at_manutencao-desmembramento IS NOT INITIAL.
      at_mensagens = VALUE #(
      FOR ls IN i_return (
          vbeln = at_manutencao-desmembramento-vbeln
          msg = ls-message ) ).
    ENDIF.

    IF at_mensagens IS INITIAL.
      at_mensagens = VALUE #(
      FOR ls IN i_return (
          msg = ls-message ) ).
    ENDIF.

    at_fcat_msg = VALUE #(
    ( col_pos = 1 fieldname = 'INCO1' )
    ( col_pos = 2 fieldname = 'SPART' )
    ( col_pos = 3 fieldname = 'AUART' )
    ( col_pos = 4 fieldname = 'WERKS' )
    ( col_pos = 5 fieldname = 'VBELN' )
    ( col_pos = 6 fieldname = 'MSG'   )
    ).

    LOOP AT at_fcat_msg ASSIGNING FIELD-SYMBOL(<fs_fcat_msg>).
      <fs_fcat_msg>-ref_tabname   = 'ZSDT0041'.
      <fs_fcat_msg>-tabname       = 'TL_SAIDA_EXEC'.
      <fs_fcat_msg>-seltext_l     = ' '.
      <fs_fcat_msg>-ref_fieldname = <fs_fcat_msg>-fieldname.

      IF <fs_fcat_msg>-fieldname EQ 'MSG'.
        <fs_fcat_msg>-seltext_l = 'Msg de bapi'.
        <fs_fcat_msg>-outputlen = '80'.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat           = at_fcat_msg
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = at_mensagens.

  ENDMETHOD.


  METHOD GET_DESCONTO_ABS.
*   "// get dos Descontos aplicado pelo usuario

    FREE E_DESCONTO_ABS.

    SELECT SUM( DESC_ABSOLUTO )
      FROM ZSDT0090
      INTO E_DESCONTO_ABS
      WHERE VBELV EQ I_VBELN
        AND POSNV EQ I_POSNR
        AND CATEGORIA EQ 'O'
        AND ESTORNO EQ ABAP_FALSE.

    CHECK I_IMP IS NOT INITIAL.

*   "// get do coeficiente o Iten da OV
    CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
      EXPORTING
        I_DIRECAO     = 'O'
        I_VBELN       = I_VBELN
        I_POSNR       = I_POSNR
      RECEIVING
        I_COEFICIENTE = DATA(COEFICIENTE_DIFERENCA).

    CHECK COEFICIENTE_DIFERENCA IS NOT INITIAL.

*   "// aplicando o coeficiente no desconto
    MULTIPLY E_DESCONTO_ABS BY COEFICIENTE_DIFERENCA.

  ENDMETHOD.


  METHOD GET_DESC_ABS.

    DATA: COEFICIENTE   TYPE P DECIMALS 5,
          DESCONTOABSLQ TYPE P DECIMALS 3.

    DATA(DESCONTOABS) = E_KBETR.

    TRY .
        DATA(V_ICVA) = I_KONV[ KSCHL = 'ICVA' ]-KBETR.
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    TRY .
        DATA(V_ICBS) = I_KONV[ KSCHL = 'ICBS' ]-KBETR.
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    TRY .
        V_ICVA = V_ICVA / I_KONV[ KSCHL = 'ICVA' ]-KAWRT.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    TRY .
        V_ICBS = V_ICBS / I_KONV[ KSCHL = 'ICBS' ]-KAWRT.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    TRY .
        COEFICIENTE = 1 - ( ( V_ICBS * ( V_ICVA / 100 ) ) / 100 ).
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

*    IF DIR IS INITIAL.
*      _DESCONTOABSLQ  = _DESCONTOABS * COEFICIENTE. "Remove o Imposto
*    ELSE.
    DESCONTOABSLQ  = DESCONTOABS / COEFICIENTE. "Adiciona o Imposto
*    ENDIF.

    CHECK DESCONTOABSLQ IS NOT INITIAL.

    E_KBETR = DESCONTOABSLQ.

  ENDMETHOD.


  method get_vlr_aditivo.

    free e_vlr.

    data: vlr_aux type dzwert,
          vlr_old type dzwert,
          vlr_new type dzwert.

    select *
      from zsdt0090
      into table @data(t_aditivo)
      where doc_simulacao eq @i_simulador
        and categoria     eq @i_categoria
        and estorno       eq @abap_false.

    loop at t_aditivo into data(ls_aditivo).

      case i_categoria.
        when 'R'.

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_old = ls_aditivo-zmengv * ls_aditivo-netprv.
          else.
            vlr_old = cond #( when ls_aditivo-ziemev eq 'TO' then ( ls_aditivo-zmengv * 1000 ) * ls_aditivo-netprv
                                                             else ( ls_aditivo-zmengv / 1000 ) * ls_aditivo-netprv ).
          endif.

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_new = ls_aditivo-zmeng * ls_aditivo-netpr.
          else.
            vlr_new = cond #( when ls_aditivo-zieme  eq 'TO' then ( ls_aditivo-zmeng  * 1000 ) * ls_aditivo-netpr
                                                             else ( ls_aditivo-zmeng  / 1000 ) * ls_aditivo-netpr  ).
          endif.

          vlr_aux = vlr_old + vlr_new.

        when 'A'.

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_aux =  ls_aditivo-zmengv * ls_aditivo-netprv.
          else.
            vlr_aux = switch #( ls_aditivo-kmeinv when 'TO'
                                    then ( ls_aditivo-zmengv / 1000 ) * ls_aditivo-netprv
                                    else ( ls_aditivo-zmengv * 1000 ) * ls_aditivo-netprv ).
          endif.

        when 'Y'.

          ls_aditivo-zmengv = abs( ls_aditivo-zmeng ).

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_aux =  ls_aditivo-zmengv * ls_aditivo-netprv.
          else.
            vlr_aux = switch #( ls_aditivo-kmeinv when 'TO'
                                    then ( ls_aditivo-zmengv / 1000 ) * ls_aditivo-netprv
                                    else ( ls_aditivo-zmengv * 1000 ) * ls_aditivo-netprv ).

          endif.

        when 'K'.

          ls_aditivo-zmengv = abs( ls_aditivo-zmengv ).

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_aux =  ls_aditivo-zmengv * ls_aditivo-netprv.
          else.
            vlr_aux = switch #( ls_aditivo-kmeinv when 'TO'
                                    then ( ls_aditivo-zmengv / 1000 ) * ls_aditivo-netprv
                                    else ( ls_aditivo-zmengv * 1000 ) * ls_aditivo-netprv ).
          endif.

        when others.

          if ls_aditivo-ziemev eq ls_aditivo-kmeinv.
            vlr_aux =  ls_aditivo-zmengv * ls_aditivo-netprv.
          else.
            vlr_aux = switch #( ls_aditivo-kmeinv when 'TO'
                                    then ( ls_aditivo-zmengv / 1000 ) * ls_aditivo-netprv
                                    else ( ls_aditivo-zmengv * 1000 ) * ls_aditivo-netprv ).
          endif.

      endcase.

      add vlr_aux to e_vlr.

    endloop.

  endmethod.


  METHOD HEDGE_TROCA_MATERIAL.

    DATA: VL_TRAVA_CAMBIO  TYPE C,
          VL_ORIG_TRV_CAM  TYPE C,
          VL_OV_PRECEDENTE TYPE VBELN,
          VL_QTDE_EQUAL    TYPE C,
          LV_SEND_VD       TYPE C,
          LV_SEND_FR       TYPE C,
          LV_TROCA         TYPE C.

    FREE R_RETURN.

    SELECT SINGLE DOC_SIMULACAO, WAERK, TPSIM
      FROM ZSDT0040
      INTO @DATA(LS_SIMULADOR)
    WHERE DOC_SIMULACAO EQ @AT_MANUTENCAO-TROCA-DOC_SIMULACAO.

    CHECK LS_SIMULADOR-TPSIM NE 'PM'.

    IF LS_SIMULADOR-TPSIM NE 'BN'.

      FREE: VL_TRAVA_CAMBIO, VL_ORIG_TRV_CAM, VL_OV_PRECEDENTE.
      IF LS_SIMULADOR-WAERK EQ 'USD'.
        CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
          EXPORTING
            I_DOC_SIMULACAO     = AT_ADITIVOS-DOC_SIMULACAO
            I_VBELN             = AT_ADITIVOS-VBELV
          CHANGING
            C_TRAVA_CAMBIO      = VL_TRAVA_CAMBIO
            C_TRAVA_CAMBIO_PREC = VL_ORIG_TRV_CAM
            C_OV_PRECEDENTE     = VL_OV_PRECEDENTE.
      ENDIF.

      CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
        EXPORTING
          I_MATNR_01 = AT_ADITIVOS-MATNR
          I_MENGE_01 = AT_ADITIVOS-ZMENG
          I_MATNR_02 = AT_ADITIVOS-MATNRV
          I_MENGE_02 = AT_ADITIVOS-ZMENGV
        IMPORTING
          E_EQUAL    = VL_QTDE_EQUAL.

      IF ( LS_SIMULADOR-WAERK EQ 'BRL' ) OR
       ( ( LS_SIMULADOR-WAERK EQ 'USD' ) AND
         ( VL_TRAVA_CAMBIO IS NOT INITIAL ) ).

        IF ( AT_ADITIVOS-MATKLV EQ AT_ADITIVOS-MATKL ) AND
           ( AT_ADITIVOS-MATKLV NE '658445'          ) AND
           ( VL_QTDE_EQUAL      EQ ABAP_FALSE        ).
          LV_TROCA = 'M'.
          LV_SEND_VD = ABAP_TRUE.
        ENDIF.

        IF AT_ADITIVOS-MATKLV NE AT_ADITIVOS-MATKL.
          LV_SEND_VD = ABAP_TRUE.
        ENDIF.

      ENDIF.
    ENDIF.

    IF ( AT_ADITIVOS-MATKLV EQ AT_ADITIVOS-MATKL ) AND
       ( AT_ADITIVOS-MATKLV NE '658445'          ) AND
       ( VL_QTDE_EQUAL      EQ ABAP_FALSE        ).
      LV_TROCA = 'M'.
      LV_SEND_FR = ABAP_TRUE.
    ELSEIF ( AT_ADITIVOS-MATKLV NE AT_ADITIVOS-MATKL ) OR
           ( AT_ADITIVOS-INCO1V NE AT_ADITIVOS-INCO1 ).
      LV_SEND_FR = ABAP_TRUE.
    ENDIF.

    IF LV_SEND_VD IS INITIAL AND LV_SEND_FR IS INITIAL.
      RETURN.
    ENDIF.

    IF LV_SEND_VD IS NOT INITIAL.
      CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
        EXPORTING
          I_ACAO = 'TROCA'
          I_0090 = AT_ADITIVOS
          I_TIPO = 'VDI'
          I_DIR  = CONV #( LV_TROCA ).
    ENDIF.

    IF LV_SEND_FR IS NOT INITIAL.
      CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
        EXPORTING
          I_ACAO = 'TROCA'
          I_0090 = AT_ADITIVOS
          I_TIPO = 'FRI'
          I_DIR  = CONV #( LV_TROCA ).
    ENDIF.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).
    <FS_RETURN>-TYPE    = CONSTANTES-TYPE-SUCESS.
    <FS_RETURN>-MESSAGE = |************ HEDGE ************ |.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
    <FS_RETURN>-TYPE    = CONSTANTES-TYPE-SUCESS.
    <FS_RETURN>-MESSAGE = |Hedge do simulador { AT_ADITIVOS-DOC_SIMULACAO } efetuado!|.

  ENDMETHOD.


  METHOD PROCESSA_DESCONTO_ABS.

    FREE R_RETURN.

    CALL METHOD SET_DESCONTO_ABS
      EXPORTING
        I_VBELN        = AT_ADITIVOS-VBELV
        I_POSNR        = AT_ADITIVOS-POSNV
        I_DESCONTO_ABS = 0
        I_ZERAR        = ABAP_TRUE
      IMPORTING
        R_RETURN       = DATA(E_RETURN).

    APPEND LINES OF E_RETURN TO R_RETURN.

    CALL METHOD GET_DESCONTO_ABS
      EXPORTING
        I_VBELN        = AT_ADITIVOS-VBELV
        I_POSNR        = AT_ADITIVOS-POSNV
        I_IMP          = ABAP_TRUE
      IMPORTING
        E_DESCONTO_ABS = DATA(E_DESCONTO_ABS).

    CALL METHOD SET_DESCONTO_ABS
      EXPORTING
        I_VBELN        = AT_ADITIVOS-VBELV
        I_POSNR        = AT_ADITIVOS-POSNV
        I_DESCONTO_ABS = E_DESCONTO_ABS
      IMPORTING
        R_RETURN       = E_RETURN.

    APPEND LINES OF E_RETURN TO R_RETURN.

    CALL METHOD CHK_DESCONTO_ABS
      EXPORTING
        I_VBELN        = AT_ADITIVOS-VBELV
        I_POSNR        = AT_ADITIVOS-POSNV
      IMPORTING
        E_DESCONTO_ABS = E_DESCONTO_ABS.

    CALL METHOD SET_DESCONTO_ABS
      EXPORTING
        I_VBELN        = AT_ADITIVOS-VBELV
        I_POSNR        = AT_ADITIVOS-POSNV
        I_DESCONTO_ABS = E_DESCONTO_ABS
      IMPORTING
        R_RETURN       = E_RETURN.

    APPEND LINES OF E_RETURN TO R_RETURN.

*    CALL METHOD CHK_DESCONTO_ABS_OLD_NEW
*      EXPORTING
*        I_VBELN_OLD    = AT_ADITIVOS-VBELV
*        I_MATNR_OLD    = AT_ADITIVOS-MATNRV
*        I_VBELN_NEW    = AT_ADITIVOS-VBELN
*        I_MATNR_NEW    = AT_ADITIVOS-MATNR
*      IMPORTING
*        E_DESCONTO_ABS = DATA(I_VLR_REAL).
*
*    APPEND LINES OF E_RETURN TO R_RETURN.
*
*    CALL METHOD CHK_DESCONTO_ABS
*      EXPORTING
*        I_VBELN        = AT_ADITIVOS-VBELN
*        I_POSNR        = AT_ADITIVOS-POSNN
*        I_VLR_REAL     = I_VLR_REAL
*      IMPORTING
*        E_DESCONTO_ABS = E_DESCONTO_ABS.

*    CALL METHOD SET_DESCONTO_ABS
*      EXPORTING
*        I_VBELN        = AT_ADITIVOS-VBELN
*        I_POSNR        = AT_ADITIVOS-POSNN
*        I_DESCONTO_ABS = E_DESCONTO_ABS
*      IMPORTING
*        R_RETURN       = E_RETURN.
*
*    APPEND LINES OF E_RETURN TO R_RETURN.

  ENDMETHOD.


  method reprocessa_desconto_abs.

    data(at_contador) = 0.

    free r_return.

    call method chk_desconto_abs_faturado
      exporting
        i_vbeln = i_vbeln
        i_matnr = i_matnr
      importing
        is_fat  = data(is_ok).

    check is_ok is initial.

    select single low
      from tvarvc
      into @data(limite_desc_abs)
      where name eq @constantes-limite-qtd_desc_abs.

    do.
      if at_contador eq limite_desc_abs.
        exit.
      endif.

      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          text = |Processando 40 { at_contador }|.

      call method reprocessa_desconto_abs_geral
        exporting
          i_vbeln     = i_vbeln
          i_matnr     = i_matnr
          i_simulador = i_simulador
        importing
          e_exit      = data(e_exit)
          r_return    = data(e_return).

      append lines of e_return to r_return.

      if e_exit is not initial.
        exit.
      endif.

      add 1 to at_contador.
    enddo.

  endmethod.


  METHOD REPROCESSA_DESCONTO_ABS_GERAL.

    FREE: R_RETURN.

    E_EXIT = ABAP_TRUE.

    SELECT SINGLE POSNR
      FROM VBAP
      INTO @DATA(VL_POSNR)
      WHERE VBELN EQ @I_VBELN
        AND MATNR EQ @I_MATNR.

    CALL METHOD CHK_FATURAMENTO_SIMULADOR
      EXPORTING
        I_SIMULADOR = I_SIMULADOR
      IMPORTING
        IS_OK       = DATA(IS_OK).

    IF IS_OK IS NOT INITIAL.

      CALL METHOD SET_DESCONTO_ABS
        EXPORTING
          I_VBELN        = I_VBELN
          I_POSNR        = VL_POSNR
          I_DESCONTO_ABS = 0
          I_ZERAR        = ABAP_TRUE.

    ENDIF.

    CALL METHOD CHK_DIVERGENCIA_OV_SIMULADOR
      EXPORTING
        I_VBELN     = I_VBELN
        I_MATNR     = I_MATNR
        I_SIMULADOR = I_SIMULADOR
      IMPORTING
        E_DIFERENCA = DATA(E_DIFERENCA).

    CHECK E_DIFERENCA IS NOT INITIAL.

    CALL METHOD SET_DESCONTO_ABS
      EXPORTING
        I_VBELN        = I_VBELN
        I_POSNR        = VL_POSNR
        I_DESCONTO_ABS = E_DIFERENCA
      IMPORTING
        R_RETURN       = DATA(E_RETURN).

    APPEND LINES OF E_RETURN TO R_RETURN.

    FREE E_EXIT.

  ENDMETHOD.


  method send_aprovadores_embarque.

    data: sequencia    type numc10,
          ls_0116_new  type zsdt0116,
          lv_netwr     type netwr_ap,
          lv_netwr_add type netwr_ap,
          lv_netpr     type netpr,
          lv_old       type c,
          is_ok        type c,
          at_contador  type i.

    free r_return.
    clear: lv_old, e_sequencial, is_ok.

    if at_categoria eq constantes-categoria-desmembramento.
*"// US-192060 WBARBOSA 01/10/2025
*"// Verifica se o Cliente de Origem e Destino é o mesmo CPF ou o mesmo CNPJ Raiz
      call method chk_cli_aprovadores_embarque
        exporting
          cliente_origem  = at_aditivos-kunnr
          cliente_destino = at_aditivos-kunnrv
        importing
          is_ok           = data(lv_ok).

      check lv_ok is not initial.
*"// US-192060 WBARBOSA 01/10/2025
    endif.

    select single *
      from zsdt0116
      into @data(ls_0116)
      where vbeln   eq @i_vbeln
        and status  eq @abap_false
        and status_workflow in ( ' ', 'A' ).

    if sy-subrc is initial..
      e_sequencial = ls_0116-seq.
      return.
    endif.

    select *
      from zsdt0116
      into table @data(lt_zsdt0116)
      where vbeln   eq @i_vbelv
        and status  eq @abap_false
        and status_workflow in ( ' ', 'A' ).

    check sy-subrc is initial.

    loop at lt_zsdt0116 into data(ls_zsdt0116).
      if ls_zsdt0116-posnr is not initial.
        lv_old = abap_true.
        exit.
      endif.
    endloop.

    if lv_old is not initial.
*   "// Processo Antigo de Inserção na ZSDT0116

      call function 'NUMBER_GET_NEXT'
        exporting
          nr_range_nr = '01'
          object      = 'ZSEQ_0116_'
        importing
          number      = sequencia.

      if sequencia is initial.
        rollback work.
        message 'Objeto numeração ZSEQ_0116_ não configurado!' type 'S'.
        return.
      endif.

      ls_0116_new =
      value zsdt0116(
                      seq               = sequencia
                      vbeln             = i_vbeln
                      posnr             = i_posnn
                      user_apv          = ls_zsdt0116-user_apv
                      dt_apv            = ls_zsdt0116-dt_apv
                      hr_apv            = ls_zsdt0116-hr_apv
                      user_solicitante  = sy-uname
                      data_solicitante  = sy-datum
                      hora_solicitante  = sy-uzeit
                      aprov_por_ref     = abap_true
                      seq_ref           = ls_zsdt0116-seq
                      vbeln_ref         = ls_zsdt0116-vbeln
                      posnr_ref         = ls_zsdt0116-posnr
                      saldo_origem      = ls_zsdt0116-saldo_origem
                      id_limite         = ls_zsdt0116-id_limite
                      waerk             = ls_zsdt0116-waerk
                      kursf             = ls_zsdt0116-kursf
                    ).

      insert into zsdt0116 values ls_0116_new.
      check sy-subrc is initial.
      commit work.

      is_ok = abap_true.

      append initial line to r_return assigning field-symbol(<fs_return>).
      <fs_return>-type    = constantes-type-sucess.
      <fs_return>-message = |************ ZSDT0100 ************ |.

      append initial line to r_return assigning <fs_return>.
      <fs_return>-type    = constantes-type-sucess.
      <fs_return>-message = |OV: { i_vbeln }, Seq: { sequencia } Processado automaticamente a Transação ZSDT0100.|.

    else.

      delete lt_zsdt0116 where vlr_liberado_moeda is initial.

      check lt_zsdt0116 is not initial.

      if i_vbelv ne i_vbeln.

        call method get_quantidade_distribuida
          exporting
            i_vbeln             = i_vbelv
          importing
            e_valor_distribuida = data(e_valor_distribuida).

        call method get_quantidade_devolvida
          exporting
            i_vbeln           = i_vbelv
          importing
            e_valor_devolvido = data(e_valor_devolvido).

        if e_valor_devolvido is not initial.
          e_valor_distribuida -= e_valor_devolvido.
        endif.

        select *
           from vbap
         into table @data(lt_vbap)
           where vbeln eq @i_vbeln.

        lv_netwr = 0.
        lv_netwr_add = 0.

        loop at lt_vbap into data(ls_vbap).
          lv_netwr += ( ls_vbap-netwr + ls_vbap-mwsbp ).
        endloop.

        loop at lt_zsdt0116 assigning field-symbol(<fs_0116>).

          if <fs_0116>-vlr_liberado_moeda >= e_valor_distribuida.
            <fs_0116>-vlr_liberado_moeda -= e_valor_distribuida.

*            IF SY-TCODE NE CONSTANTES-TCODE-ZSDT0087.
            e_valor_distribuida = 0.
*            ENDIF.

          else.
            e_valor_distribuida -= <fs_0116>-vlr_liberado_moeda.
            <fs_0116>-vlr_liberado_moeda = 0.
          endif.

        endloop.

        data(lv_liberado_moeda) = reduce netwr_ak( init p = 0
                                for s_0116 in lt_zsdt0116
                                next p = p + s_0116-vlr_liberado_moeda ).

        check lv_liberado_moeda is not initial.

        clear e_valor_distribuida.
        call method get_quantidade_distribuida
          exporting
            i_vbeln             = i_vbelv
          importing
            e_valor_distribuida = e_valor_distribuida.

        if e_valor_devolvido is not initial.
          e_valor_distribuida -= e_valor_devolvido.
        endif.

        loop at lt_zsdt0116 into ls_0116.

          if ls_0116-vlr_liberado_moeda is initial.

            update zsdt0116 set vlr_liberado_moeda = 0
                                vlr_liberado       = 0
            where seq   eq ls_0116-seq
              and vbeln eq ls_0116-vbeln
              and posnr eq ls_0116-posnr.

            continue.
          endif.

          if lv_netwr is initial.
            continue.
          endif.

          if lv_netwr >= ls_0116-vlr_liberado_moeda.

            lv_netwr -= ls_0116-vlr_liberado_moeda.
            lv_netwr_add += ls_0116-vlr_liberado_moeda.

*"// Retornar o Valor Removido da Liberação de Embarque
*            LS_0116-VLR_LIBERADO_MOEDA = E_VALOR_DISTRIBUIDA.
            ls_0116-vlr_liberado_moeda -= ls_0116-vlr_liberado_moeda.
            ls_0116-vlr_liberado_moeda += e_valor_distribuida.
            clear e_valor_distribuida.

            if ls_0116-waerk eq 'USD'.
              ls_0116-vlr_liberado = ls_0116-vlr_liberado_moeda.
            else.
              ls_0116-vlr_liberado = ls_0116-vlr_liberado_moeda / ls_0116-kursf.
            endif.

            update zsdt0116 set vlr_liberado_moeda = ls_0116-vlr_liberado_moeda
                                vlr_liberado       = ls_0116-vlr_liberado
            where seq   eq ls_0116-seq
              and vbeln eq ls_0116-vbeln
              and posnr eq ls_0116-posnr.

          else.

            ls_0116-vlr_liberado_moeda -= lv_netwr.
            lv_netwr_add += lv_netwr.
            lv_netwr = 0.
*"// Retornar o Valor Removido da Liberação de Embarque
            ls_0116-vlr_liberado_moeda += e_valor_distribuida.
            clear e_valor_distribuida.

            if ls_0116-waerk eq 'USD'.
              ls_0116-vlr_liberado = ls_0116-vlr_liberado_moeda.
            else.
              ls_0116-vlr_liberado = ls_0116-vlr_liberado_moeda / ls_0116-kursf.
            endif.

            update zsdt0116 set vlr_liberado_moeda = ls_0116-vlr_liberado_moeda
                                vlr_liberado       = ls_0116-vlr_liberado
            where seq   eq ls_0116-seq
              and vbeln eq ls_0116-vbeln.

          endif.

          if lv_netwr_add is initial.
            continue.
          endif.

          call function 'NUMBER_GET_NEXT'
            exporting
              nr_range_nr = '01'
              object      = 'ZSEQ_0116_'
            importing
              number      = sequencia.

          if sequencia is initial.
            rollback work.

            append initial line to r_return assigning <fs_return>.
            <fs_return>-type    = cl_abap_aab_utilities=>category_error.
            <fs_return>-message = |************ ZSDT0116 ************ |.

            append initial line to r_return assigning <fs_return>.
            <fs_return>-type    = cl_abap_aab_utilities=>category_error.
            <fs_return>-message = 'Objeto numeração ZSEQ_0116_ não configurado!'.

            message 'Objeto numeração ZSEQ_0116_ não configurado!' type constantes-type-sucess.
            continue.
          endif.

          ls_0116_new =
          value zsdt0116(
                          seq               =  sequencia
                          vbeln             =  i_vbeln
                          user_apv          =  ls_0116-user_apv
                          dt_apv            =  ls_0116-dt_apv
                          hr_apv            =  ls_0116-hr_apv
                          user_solicitante  = sy-uname
                          data_solicitante  = sy-datum
                          hora_solicitante  = sy-uzeit
                          aprov_por_ref     = abap_true
                          seq_ref           = ls_0116-seq
                          vbeln_ref         = ls_0116-vbeln
                          posnr_ref         = ls_0116-posnr
                          saldo_origem      = ls_0116-saldo_origem
                          id_limite         = ls_0116-id_limite
                          waerk             = ls_0116-waerk
                          kursf             = ls_0116-kursf
                        ).

          ls_0116_new-vlr_liberado_moeda = lv_netwr_add.

          if ls_0116_new-waerk eq 'USD'.
            ls_0116_new-vlr_liberado       = lv_netwr_add.
          else.
            ls_0116_new-vlr_liberado       = lv_netwr_add / ls_0116_new-kursf.
          endif.

          ls_0116_new-vlr_original       = ls_0116_new-vlr_liberado.
          ls_0116_new-vlr_original_moeda = ls_0116_new-vlr_liberado_moeda.

          insert into zsdt0116 values ls_0116_new.
          check  sy-subrc is initial.
          commit work.

          is_ok = abap_true.

          append initial line to r_return assigning <fs_return>.
          <fs_return>-type    = constantes-type-sucess.
          <fs_return>-message = |************ ZSDT0116 ************ |.

          append initial line to r_return assigning <fs_return>.
          <fs_return>-type    = constantes-type-sucess.
          <fs_return>-message = |OV: { i_vbeln }, Seq: { sequencia } Processado automaticamente a Transação ZSDT0100.|.

          clear: lv_netwr_add.

        endloop.


      endif.

    endif.

    check is_ok is not initial.

    clear: ls_0116, e_sequencial.

    do.
      if at_contador eq 30.
        exit.
      endif.

      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          text = |Processando inclusão na ZSDT0100 { at_contador }|.

      select single *
        from zsdt0116
        into ls_0116
        where vbeln   eq i_vbeln
          and status  eq abap_false
          and status_workflow in ( ' ', 'A' ).

      if sy-subrc is initial and ls_0116-seq is not initial.
        e_sequencial = ls_0116-seq.
        exit.
      else.
        wait up to '0.5' seconds.
      endif.

      add 1 to at_contador.
    enddo.

  endmethod.


  METHOD SET_ADITIVOS.

    DATA: T_KONV  TYPE KONV_TTY,
          SEQ     TYPE ZSDT0090-SEQUENCIA,
          LV_EXIT TYPE SY-TABIX.

    FREE R_RETURN.

    SELECT COUNT(*)
      FROM ZSDT0090
      INTO SEQ
    WHERE DOC_SIMULACAO EQ AT_MANUTENCAO-DOC_SIMULACAO.

    SELECT SINGLE *
      FROM VBAK
      INTO @DATA(W_VBAK)
    WHERE VBELN EQ @AT_MANUTENCAO-VBELN_OLD.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(W_VBAP)
        WHERE VBELN EQ @W_VBAK-VBELN
          AND MATNR EQ @AT_MANUTENCAO-MATNR_OLD.

    SELECT SINGLE *
      FROM VBKD
      INTO @DATA(W_VBKD)
    WHERE VBELN EQ @W_VBAK-VBELN.

    SELECT * FROM V_KONV
      INTO CORRESPONDING FIELDS OF TABLE @T_KONV
        WHERE KNUMV EQ @W_VBAK-KNUMV
          AND KSCHL IN ( 'PR00', 'RB00', 'ICVA', 'ICBS' )
          AND KPOSN EQ @W_VBAP-POSNR.

    TRY .
        DATA(W_PR00) = T_KONV[ KSCHL = 'PR00' ].
        DATA(W_RB00) = T_KONV[ KSCHL = 'RB00' ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    ADD 1 TO SEQ.

    IF AT_ADITIVOS-DOC_SIMULACAO IS INITIAL.

      AT_ADITIVOS-DOC_SIMULACAO = AT_MANUTENCAO-DOC_SIMULACAO.
      AT_ADITIVOS-SEQUENCIA     = SEQ.
      AT_ADITIVOS-AUARTV        = W_VBAK-AUART.
      AT_ADITIVOS-VBELV         = W_VBAP-VBELN.
      AT_ADITIVOS-POSNV         = W_VBAP-POSNR.
      AT_ADITIVOS-SPARTV        = W_VBAP-SPART.
      AT_ADITIVOS-ZMENGV        = AT_MANUTENCAO-QTD_REMOVIDA * -1.
      AT_ADITIVOS-ZIEMEV        = W_VBAP-ZIEME.
      AT_ADITIVOS-NETPRV        = W_PR00-KBETR.
      AT_ADITIVOS-KMEINV        = W_PR00-KMEIN.

      IF W_VBAP-MWSBP IS NOT INITIAL.
        CALL METHOD GET_DESC_ABS
          EXPORTING
            I_KONV  = T_KONV
          IMPORTING
            E_KBETR = AT_ADITIVOS-NETPRV.
      ENDIF.

      AT_ADITIVOS-CHARGV        = W_VBAP-CHARG.
      AT_ADITIVOS-MATNRV        = W_VBAP-MATNR.
      AT_ADITIVOS-MATKLV        = W_VBAP-MATKL.
      AT_ADITIVOS-INCO1V        = W_VBKD-INCO1.
      AT_ADITIVOS-WERKSV        = W_VBAP-WERKS.
      AT_ADITIVOS-KUNNRV        = W_VBAK-KUNNR.
      AT_ADITIVOS-CATEGORIA     = AT_CATEGORIA.
      AT_ADITIVOS-KURRF         = W_VBKD-KURRF.
      AT_ADITIVOS-USNAM         = SY-UNAME.
      AT_ADITIVOS-DATA_ATUAL    = SY-DATUM.
      AT_ADITIVOS-HORA_ATUAL    = SY-UZEIT.

    ENDIF.

    FREE: T_KONV, W_VBAK, W_VBAP, W_VBKD, W_PR00, W_RB00.

    CHECK AT_MANUTENCAO-VBELN_NEW IS NOT INITIAL.

    LV_EXIT = 0.

    DO.

      ADD 1 TO LV_EXIT.

      SELECT SINGLE *
        FROM VBAK
        INTO W_VBAK
      WHERE VBELN EQ AT_MANUTENCAO-VBELN_NEW.

      IF SY-SUBRC IS INITIAL.
        EXIT.
      ENDIF.

      IF LV_EXIT EQ 50.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          TEXT = |Wait Aditivo-> { LV_EXIT }|.

    ENDDO.

    IF LV_EXIT EQ 50.
      APPEND INITIAL LINE TO R_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).
      <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_WARNING.
      <FS_RETURN>-MESSAGE = |************ ADITIVOS ************ |.

      APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
      <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR.
      <FS_RETURN>-MESSAGE = |Problema na Inclusão do Aditivo da OV. { AT_MANUTENCAO-VBELN_NEW }!|.
      RETURN.
    ENDIF.


    CLEAR W_VBAP.

    SELECT SINGLE *
      FROM VBAP
      INTO W_VBAP
        WHERE VBELN EQ AT_MANUTENCAO-VBELN_NEW
          AND MATNR EQ AT_MANUTENCAO-MATNR_NEW.
    IF SY-SUBRC IS NOT INITIAL.
      SELECT SINGLE *
        FROM VBAP
        INTO W_VBAP
          WHERE VBELN EQ AT_MANUTENCAO-VBELN_NEW.
    ENDIF.

    SELECT SINGLE *
      FROM VBKD
      INTO W_VBKD
    WHERE VBELN EQ AT_MANUTENCAO-VBELN_NEW.


    SELECT * FROM V_KONV
      INTO CORRESPONDING FIELDS OF TABLE @T_KONV
        WHERE KNUMV EQ @W_VBAK-KNUMV
          AND KSCHL IN ( 'PR00', 'RB00', 'ICVA', 'ICBS' )
          AND KPOSN EQ @W_VBAP-POSNR.

    TRY .
        W_PR00 = T_KONV[ KSCHL = 'PR00' ].
        W_RB00 = T_KONV[ KSCHL = 'RB00' ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    AT_ADITIVOS-AUART         = W_VBAK-AUART.
    AT_ADITIVOS-VBELN         = W_VBAP-VBELN.
    AT_ADITIVOS-POSNN         = W_VBAP-POSNR.
    AT_ADITIVOS-SPART         = W_VBAP-SPART.

    AT_ADITIVOS-ZMENG         = COND #( WHEN W_VBAP-MEINS EQ 'TO'
                                        THEN W_VBAP-KWMENG * 1000
                                        ELSE W_VBAP-KWMENG ).

    AT_ADITIVOS-ZIEME         = COND #( WHEN W_VBAP-MEINS EQ 'TO'
                                        THEN 'KG'
                                        ELSE W_VBAP-MEINS ).

    AT_ADITIVOS-NETPR        = W_PR00-KBETR.

    IF W_VBAP-MWSBP IS NOT INITIAL.
      CALL METHOD GET_DESC_ABS
        EXPORTING
          I_KONV  = T_KONV
        IMPORTING
          E_KBETR = AT_ADITIVOS-NETPR.
    ENDIF.

*    AT_ADITIVOS-NETPR         = W_VBAP-NETPR.
    AT_ADITIVOS-KMEIN         = W_VBAP-MEINS.
    AT_ADITIVOS-NETWR         = W_VBAP-NETWR + W_VBAP-MWSBP.
    AT_ADITIVOS-CHARG         = W_VBAP-CHARG.
    AT_ADITIVOS-MATNR         = W_VBAP-MATNR.
    AT_ADITIVOS-MATKL         = W_VBAP-MATKL.
    AT_ADITIVOS-INCO1         = W_VBKD-INCO1.
    AT_ADITIVOS-WERKS         = W_VBAP-WERKS.
    AT_ADITIVOS-KUNNR         = W_VBAK-KUNNR.
    AT_ADITIVOS-CATEGORIA     = AT_CATEGORIA.
    AT_ADITIVOS-KURRF         = W_VBKD-KURRF.
    AT_ADITIVOS-USNAM         = SY-UNAME.
    AT_ADITIVOS-DATA_ATUAL    = SY-DATUM.
    AT_ADITIVOS-HORA_ATUAL    = SY-UZEIT.

    MODIFY ZSDT0090 FROM AT_ADITIVOS.
    COMMIT WORK.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
    <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_WARNING.
    <FS_RETURN>-MESSAGE = |************ ADITIVOS ************ |.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
    <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_WARNING.
    <FS_RETURN>-MESSAGE = |OV. { AT_ADITIVOS-VBELV }-{ AT_ADITIVOS-MATNRV } foi Incluida na Tabela de Aditivos!|.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING <FS_RETURN>.
    <FS_RETURN>-TYPE    = CL_ABAP_AAB_UTILITIES=>CATEGORY_WARNING.
    <FS_RETURN>-MESSAGE = |OV. { AT_ADITIVOS-VBELN }-{ AT_ADITIVOS-MATNR } foi Incluida na Tabela de Aditivos!|.

  ENDMETHOD.


  method set_desconto_abs.

    if i_zerar is initial.
      check i_desconto_abs is not initial.
    endif.

    free: r_return, at_conditions_in, at_conditions_inx, at_logic_switch, at_order_header_inx.

    select single *
      from vbap
      into @data(ls_vbap)
      where vbeln eq @i_vbeln
        and posnr eq @i_posnr.

    check sy-subrc is initial.

    call method chk_desconto_abs_faturado
      exporting
        i_vbeln = ls_vbap-vbeln
        i_matnr = ls_vbap-matnr
      importing
        is_fat  = data(is_fat).

    check is_fat is initial.

    if i_zerar is not initial.

      select single knumv
        from vbak
        into @data(lv_knumv)
        where vbeln eq @i_vbeln.

      if sy-subrc is initial.

        select single kbetr
           from v_konv
          into @data(lv_kbter)
           where knumv eq @lv_knumv
             and kposn eq @i_posnr
             and kschl eq 'RB00'.

        if lv_kbter is initial.
          return.
        endif.

      endif.
    endif.

    at_salesdocument = i_vbeln.

    at_order_header_inx-updateflag = 'U'.

    append initial line to at_conditions_in assigning field-symbol(<fs_conditions_in>).
    <fs_conditions_in>-itm_number = ls_vbap-posnr.
    <fs_conditions_in>-cond_count = '01'.
    <fs_conditions_in>-cond_type  = 'RB00'.
    <fs_conditions_in>-cond_value = i_desconto_abs.
    <fs_conditions_in>-currency   = ls_vbap-waerk.

    append initial line to at_conditions_inx assigning field-symbol(<fs_conditions_inx>).
    <fs_conditions_inx>-itm_number = ls_vbap-posnr.
    <fs_conditions_inx>-cond_count = '01'.
    <fs_conditions_inx>-cond_type  = 'RB00'.
    <fs_conditions_inx>-cond_value = abap_true.
    <fs_conditions_inx>-currency   = abap_true.
    <fs_conditions_inx>-updateflag = 'U'.

    at_logic_switch-cond_handl = abap_true.

    call method call_order_change
      receiving
        r_return = data(e_return).

    if i_zerar is not initial.
      append initial line to r_return assigning field-symbol(<fs_return>).
      <fs_return>-type    = constantes-type-sucess.
      <fs_return>-message = |************ CHANGE ZERAR DESCONTO ABS ************|.
    else.
      append initial line to r_return assigning <fs_return>.
      <fs_return>-type    = constantes-type-sucess.
      <fs_return>-message = |************ CHANGE DESCONTO ABS ************|.
    endif.

    append lines of e_return to r_return.

  endmethod.


  METHOD GET_QUANTIDADE_EMBARCADO.

    FREE E_QTDE_EMBARCADA.

    CHECK SY-TCODE NE CONSTANTES-TCODE-ZSDT0081.

    SELECT SUM( QTE_SOL )
      FROM ZSDT0082
      INTO E_QTDE_EMBARCADA
        WHERE SEQ    EQ 1
          AND VBELN  EQ I_VBELN
          AND POSNR  EQ I_POSNR
          AND STATUS EQ 1.

  ENDMETHOD.


  METHOD CHANGE_SOLICITACAO_APROVADA.

    DATA: LV_NETWR     TYPE NETWR_AP,
          LV_NETPR     TYPE NETPR,
          LV_NETWR_ADD TYPE NETWR_AP,
          LV_OLD       TYPE C.

    SELECT *
      FROM ZSDT0116
    INTO TABLE @DATA(LT_0116)
      WHERE VBELN   EQ @I_VBELN
        AND STATUS  EQ @ABAP_FALSE
        AND STATUS_WORKFLOW IN ( ' ', 'A' ).

    CHECK SY-SUBRC IS INITIAL.

    CLEAR LV_OLD.
    LOOP AT LT_0116 INTO DATA(LS_0116).
      IF LS_0116-POSNR IS NOT INITIAL.
        LV_OLD = ABAP_TRUE.
      ENDIF.
    ENDLOOP.

    IF LV_OLD IS NOT INITIAL.
      RETURN.
    ENDIF.

    CHECK LT_0116 IS NOT INITIAL.

    SELECT SINGLE
           VBELN,
           SUM( KWMENG ) AS KWMENG,
           SUM( MWSBP ) AS MWSBP,
           SUM( NETWR ) AS NETWR
      FROM VBAP
      INTO @DATA(LS_VBAP)
    WHERE VBELN EQ @I_VBELN
    GROUP BY VBELN.

    LV_NETPR = ( LS_VBAP-MWSBP + LS_VBAP-NETWR ) / LS_VBAP-KWMENG.
    LV_NETWR = I_QTDE_ENCERRADA * LV_NETPR.

    LOOP AT LT_0116 INTO LS_0116.

      IF LV_NETWR IS INITIAL.
        CONTINUE.
      ENDIF.

      IF LV_NETWR >= LS_0116-VLR_LIBERADO_MOEDA.

        LV_NETWR -= LS_0116-VLR_LIBERADO_MOEDA.
        LV_NETWR_ADD += LS_0116-VLR_LIBERADO_MOEDA.

        UPDATE ZSDT0116
           SET STATUS = ABAP_TRUE
               VLR_LIBERADO_MOEDA = 0
               VLR_LIBERADO = 0
        WHERE SEQ   EQ LS_0116-SEQ
          AND VBELN EQ LS_0116-VBELN
          AND POSNR EQ LS_0116-POSNR.

      ELSE.

        LS_0116-VLR_LIBERADO_MOEDA -= LV_NETWR.
        LV_NETWR_ADD += LV_NETWR.
        LV_NETWR = 0.

        IF LS_0116-WAERK EQ 'USD'.
          LS_0116-VLR_LIBERADO = LS_0116-VLR_LIBERADO_MOEDA.
        ELSE.
          LS_0116-VLR_LIBERADO = LS_0116-VLR_LIBERADO_MOEDA / LS_0116-KURSF.
        ENDIF.

        UPDATE ZSDT0116
          SET VLR_LIBERADO_MOEDA = LS_0116-VLR_LIBERADO_MOEDA
              VLR_LIBERADO       = LS_0116-VLR_LIBERADO
        WHERE SEQ   EQ LS_0116-SEQ
          AND VBELN EQ LS_0116-VBELN
          AND POSNR EQ LS_0116-POSNR.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD CHANGE_SOLICITACAO_DISTRIBUIDA.

    DATA: LV_SALDO  TYPE ZSDED054.
    DATA: LS_0082_OLD TYPE ZSDT0082,
          LS_0082_NEW TYPE ZSDT0082.

    SELECT *
      FROM ZSDT0082
      INTO TABLE @DATA(LT_0082)
    WHERE VBELN EQ @I_VBELN
      AND POSNR EQ @I_POSNR
      AND STATUS NOT IN ( 3, 4, 5 ).

    CHECK SY-SUBRC IS INITIAL.

    CALL METHOD GET_QUANTIDADE_EMBARCADO
      EXPORTING
        I_VBELN          = I_VBELN
        I_POSNR          = I_POSNR
      IMPORTING
        E_QTDE_EMBARCADA = DATA(E_QTDE_EMBARCADA).

    IF I_QTDE_ENCERRADA >= E_QTDE_EMBARCADA.

      LOOP AT LT_0082 ASSIGNING FIELD-SYMBOL(<FS_0082>).
        LS_0082_OLD = <FS_0082>.
        LS_0082_NEW = <FS_0082>.

        DATA(LV_STATUS_NEW) =
        SWITCH ZSDED045( <FS_0082>-STATUS
                             WHEN 1 THEN 3
                             WHEN 2 THEN 4 ).

        LS_0082_NEW-STATUS = LV_STATUS_NEW.

        UPDATE ZSDT0082 SET STATUS    = LV_STATUS_NEW
                            DT_CANC   = SY-DATUM
                            USER_CANC = SY-UNAME
              WHERE NRO_SOL EQ <FS_0082>-NRO_SOL
                AND SEQ     EQ <FS_0082>-SEQ
                AND VBELN   EQ <FS_0082>-VBELN
                AND POSNR   EQ <FS_0082>-POSNR.

        CALL METHOD SET_LOG_SOLICITACAO
          EXPORTING
            I_OLD     = LS_0082_OLD
            I_NEW     = LS_0082_NEW
            I_DIRECAO = 'Encerramento'.

      ENDLOOP.

    ELSE.

      LV_SALDO = I_QTDE_ENCERRADA.

      LOOP AT LT_0082 ASSIGNING <FS_0082> WHERE STATUS EQ 2.

        LS_0082_OLD = <FS_0082>.
        LS_0082_NEW = <FS_0082>.

        IF LV_SALDO IS INITIAL.
          CONTINUE.
        ENDIF.

        IF <FS_0082>-QTE_LIB <= LV_SALDO.
          LV_SALDO -= <FS_0082>-QTE_LIB.

          LS_0082_NEW-STATUS = 4.

          UPDATE ZSDT0082 SET STATUS    = 4
                              DT_CANC   = SY-DATUM
                              USER_CANC = SY-UNAME
            WHERE NRO_SOL EQ <FS_0082>-NRO_SOL
              AND SEQ     EQ <FS_0082>-SEQ
              AND VBELN   EQ <FS_0082>-VBELN
              AND POSNR   EQ <FS_0082>-POSNR.

          CALL METHOD SET_LOG_SOLICITACAO
            EXPORTING
              I_OLD     = LS_0082_OLD
              I_NEW     = LS_0082_NEW
              I_DIRECAO = 'Encerramento'.

        ELSE.

          <FS_0082>-QTE_LIB -= LV_SALDO.
          LV_SALDO = 0.

          LS_0082_NEW-QTE_LIB = <FS_0082>-QTE_LIB.

          UPDATE ZSDT0082 SET QTE_LIB = <FS_0082>-QTE_LIB
            WHERE NRO_SOL EQ <FS_0082>-NRO_SOL
              AND SEQ     EQ <FS_0082>-SEQ
              AND VBELN   EQ <FS_0082>-VBELN
              AND POSNR   EQ <FS_0082>-POSNR.

          CALL METHOD SET_LOG_SOLICITACAO
            EXPORTING
              I_OLD     = LS_0082_OLD
              I_NEW     = LS_0082_NEW
              I_DIRECAO = 'Encerramento'.

        ENDIF.
      ENDLOOP.

      LV_SALDO = I_QTDE_ENCERRADA.

      LOOP AT LT_0082 ASSIGNING <FS_0082> WHERE STATUS EQ 1.

        LS_0082_OLD = <FS_0082>.
        LS_0082_NEW = <FS_0082>.

        IF LV_SALDO IS INITIAL.
          CONTINUE.
        ENDIF.

        IF <FS_0082>-QTE_SOL <= LV_SALDO.
          LV_SALDO -= <FS_0082>-QTE_SOL.

          LS_0082_NEW-STATUS = 3.

          UPDATE ZSDT0082 SET STATUS    = 3
                              DT_CANC   = SY-DATUM
                              USER_CANC = SY-UNAME
            WHERE NRO_SOL EQ <FS_0082>-NRO_SOL
              AND SEQ     EQ <FS_0082>-SEQ
              AND VBELN   EQ <FS_0082>-VBELN
              AND POSNR   EQ <FS_0082>-POSNR.

          CALL METHOD SET_LOG_SOLICITACAO
            EXPORTING
              I_OLD     = LS_0082_OLD
              I_NEW     = LS_0082_NEW
              I_DIRECAO = 'Encerramento'.

        ELSE.

          <FS_0082>-QTE_SOL -= LV_SALDO.
          LV_SALDO = 0.

          LS_0082_NEW-QTE_SOL = <FS_0082>-QTE_SOL.

          UPDATE ZSDT0082 SET QTE_SOL = <FS_0082>-QTE_SOL
            WHERE NRO_SOL EQ <FS_0082>-NRO_SOL
              AND SEQ     EQ <FS_0082>-SEQ
              AND VBELN   EQ <FS_0082>-VBELN
              AND POSNR   EQ <FS_0082>-POSNR.

          CALL METHOD SET_LOG_SOLICITACAO
            EXPORTING
              I_OLD     = LS_0082_OLD
              I_NEW     = LS_0082_NEW
              I_DIRECAO = 'Encerramento'.

        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD CHECK_DADOS_BACKGROUND.

    IF I_MANUTENCAO-VBELN_OLD-VBELN IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-014 "'O.V é um campo Obrigatório!'
        IMPORTING
          R_RETURN   = DATA(E_RETURN).

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    IF I_MANUTENCAO-VBELN_OLD-POSNR IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-015 "'Item da O.V é um campo Obrigatório!'
        IMPORTING
          R_RETURN   = R_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-QUANTIDADE IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-012 "'É Obrigatorio o preenchimento da Quantidade!'
        IMPORTING
          R_RETURN   = R_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-ITINERARIO IS INITIAL.

      IF I_MANUTENCAO-VBELN_NEW-NR_ROT_LR IS INITIAL.

        CALL METHOD SET_MENSAGENS
          EXPORTING
            I_TYPE     = CONSTANTES-TYPE-ERROR
            I_MENSAGEM = TEXT-020 "'Rota do Local de Recebimento é Obrigatório!'
          IMPORTING
            R_RETURN   = R_RETURN.

        APPEND LINES OF E_RETURN TO R_RETURN.

      ENDIF.

      IF I_MANUTENCAO-VBELN_NEW-NR_ROT_PC IS INITIAL.

        CALL METHOD SET_MENSAGENS
          EXPORTING
            I_TYPE     = CONSTANTES-TYPE-ERROR
            I_MENSAGEM = TEXT-021 "'Rota do Ponto de Coleta é Obrigatório!'
          IMPORTING
            R_RETURN   = R_RETURN.

        APPEND LINES OF E_RETURN TO R_RETURN.

      ENDIF.

      CALL METHOD VERIFICAR_ITINERARIO_82
        EXPORTING
          I_ROTA_LR = I_MANUTENCAO-VBELN_NEW-NR_ROT_LR
          I_ROTA_PC = I_MANUTENCAO-VBELN_NEW-NR_ROT_PC
        IMPORTING
          IS_OK     = DATA(IS_OK).

      IF IS_OK IS INITIAL.
        CALL METHOD SET_MENSAGENS
          EXPORTING
            I_TYPE     = CONSTANTES-TYPE-ERROR
            I_MENSAGEM = |{ TEXT-E12 } Rota LR: { I_MANUTENCAO-VBELN_NEW-NR_ROT_LR }, PC:{ I_MANUTENCAO-VBELN_NEW-NR_ROT_PC }|
          IMPORTING
            R_RETURN   = R_RETURN.

        APPEND LINES OF E_RETURN TO R_RETURN.
      ENDIF.

    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-KUNNR IS NOT INITIAL.
      SELECT COUNT(*)
        FROM VBAK
        WHERE VBELN EQ I_MANUTENCAO-VBELN_OLD-VBELN
          AND KUNNR EQ I_MANUTENCAO-VBELN_NEW-KUNNR.
      IF SY-SUBRC IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-WERKS IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-009 "'É Obrigatorio o preenchimento do Centro!'
        IMPORTING
          R_RETURN   = R_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-MATNR IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-010 "'É Obrigatorio o preenchimento do Material!'
        IMPORTING
          R_RETURN   = R_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

    IF I_MANUTENCAO-VBELN_NEW-LGORT IS INITIAL.

      CALL METHOD SET_MENSAGENS
        EXPORTING
          I_TYPE     = CONSTANTES-TYPE-ERROR
          I_MENSAGEM = TEXT-011 "'É Obrigatorio o preenchimento do Deposito!'
        IMPORTING
          R_RETURN   = R_RETURN.

      APPEND LINES OF E_RETURN TO R_RETURN.

    ENDIF.

  ENDMETHOD.


  method check_dados_desmembramento.

    if at_manutencao-desmembramento-vbeln is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-014 " O.V é um campo Obrigatório!'
        importing
          r_return   = data(e_return).

      append lines of e_return to r_return.
    endif.

    if at_manutencao-desmembramento-posnr is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-015 " Item da O.V é um campo Obrigatório!'
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    if at_manutencao-desmembramento-matnr is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-016 " Material é um campo Obrigatório!'
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    if at_manutencao-desmembramento-kunnr is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-017 " Cliente é um campo Obrigatório!'
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    else.

      select single count(*)
        from kna1
      where kunnr eq at_manutencao-desmembramento-kunnr.

      if sy-subrc is not initial.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
            i_mensagem = |{ at_manutencao-desmembramento-kunnr }-{ text-019 }| "Cliente não cadastrado!'
          importing
            r_return   = e_return.

        append lines of e_return to r_return.
      endif.

    endif.

    if at_manutencao-desmembramento-qtd_recebida is initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-018 " A Quantidade é um campo Obrigatório!'
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    select single count(*)
      from vbak
      where vbeln eq at_manutencao-desmembramento-vbeln
      and auart in ( 'ZDEF', 'ZFTE', 'ZSEM', 'ZODF', 'ZOFE', 'ZOSM', 'ZBON' ).

    if sy-subrc is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = text-013 "'Tipo de OV não permitida!'
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    call method chk_limite_quantidade
      exporting
        i_vbeln = at_manutencao-desmembramento-vbeln
        i_posnr = at_manutencao-desmembramento-posnr
        i_qtde  = at_manutencao-desmembramento-qtd_recebida
      importing
        e_msg   = data(e_msg).

    if e_msg is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = e_msg
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    call method zcl_manutencao_insumos=>chk_divergencia_ov_simulador
      exporting
        i_vbeln               = at_manutencao-desmembramento-vbeln
        i_matnr               = at_manutencao-desmembramento-matnr
        i_simulador           = at_manutencao-doc_simulacao
      importing
        e_ordem_venda         = data(e_ordem_venda)
        e_simulador           = data(e_simulador)
        e_diferenca_simulador = data(e_diferenca).

    if e_diferenca is not initial.

      call method zcl_manutencao_insumos=>get_tolerancia_desconto_abs
        importing
          e_tolerancia = data(e_tolerancia).

      data(e_tolerancia_negativo) = e_tolerancia * -1.

      if e_diferenca not between e_tolerancia_negativo and e_tolerancia.
        call method set_mensagens
          exporting
            i_type     = constantes-type-error
*            i_mensagem = 'Totais: Ordem de Venda: ' && e_ordem_venda && ', Simulador: ' && e_simulador && ' Diferença: ' && e_diferenca
            i_mensagem = |Totais: Ordem de Venda: { e_ordem_venda }, Simulador: { e_simulador }, Diferença: { e_diferenca }|
          importing
            r_return   = e_return.

        append lines of e_return to r_return.
      endif.

    endif.

    call method chk_regra_proporcional
      exporting
        i_matnr      = at_manutencao-desmembramento-matnr
        i_quantidade = at_manutencao-desmembramento-qtd_recebida
      importing
        e_msg        = e_msg.

    if e_msg is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |Origem: { e_msg }|
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

    call method chk_desc_aplicado_simulador
      exporting
        i_simulador = at_manutencao-doc_simulacao
      importing
        is_ok       = data(is_ok).

    if is_ok is not initial.
      call method set_mensagens
        exporting
          i_type     = constantes-type-error
          i_mensagem = |{ text-027 } { at_manutencao-doc_simulacao }|
        importing
          r_return   = e_return.
      append lines of e_return to r_return.
    endif.

    if r_return is not initial.
      call method set_mensagens
        exporting
          i_type     = '#'
          i_mensagem = |Validação Interna|
        importing
          r_return   = e_return.

      append lines of e_return to r_return.
    endif.

  endmethod.


  METHOD CHK_LIMITE_QUANTIDADE.

    DATA LV_QTDE TYPE ZSDED054.
    DATA LV_SALDO TYPE ZSDED054.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(LS_VBAP)
      WHERE VBELN = @I_VBELN
        AND POSNR = @I_POSNR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SUM( RFMNG )
      FROM VBFA
    INTO @DATA(LV_FATURADO)
      WHERE VBELV EQ @LS_VBAP-VBELN
        AND POSNV EQ @LS_VBAP-POSNR
        AND VBTYP_N EQ 'J'
        AND VBTYP_V EQ 'C'
        AND BWART NE @ABAP_FALSE.

    CALL METHOD GET_QUANTIDADE_EMBARCADO
      EXPORTING
        I_VBELN          = LS_VBAP-VBELN
        I_POSNR          = LS_VBAP-POSNR
      IMPORTING
        E_QTDE_EMBARCADA = DATA(E_QTDE_EMBARCADA).

    LV_SALDO = LS_VBAP-KWMENG - LV_FATURADO.
    LV_QTDE = LS_VBAP-KWMENG - I_QTDE.

    IF LV_SALDO LT I_QTDE.
      E_MSG = TEXT-005. "//Quantidade Solicitada é Superior ao Saldo!
      RETURN.
    ENDIF.

    IF LV_QTDE LT E_QTDE_EMBARCADA.
      E_MSG = TEXT-006. "//Quantidade Solicitada é Superior a Quantidade Embarcado!
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD CHK_REGRA_PROPORCIONAL.

    DATA: LV_GROES_DEC TYPE ESECOMPAVG,
          LV_GROES_STR TYPE CHAR30,
          LV_QTD_PROP  TYPE DZMENG.

    CLEAR E_MSG.

    CHECK I_MATNR IS NOT INITIAL.

    SELECT SINGLE COUNT(*)
      FROM MARA
      WHERE MATNR EQ @I_MATNR
        AND SPART IN ( @CONSTANTES-SPART-_03, @CONSTANTES-SPART-_13 ).

    CHECK SY-SUBRC IS INITIAL.

    CALL METHOD CHK_USER_EXCECAO_PROPORCIONAL
      EXPORTING
        I_USER = SY-UNAME
      IMPORTING
        IS_OK  = DATA(IS_USER_OK).

    CHECK IS_USER_OK IS INITIAL.

    CALL METHOD GET_PROPORCIONALIDADE_MATERIAL
      EXPORTING
        I_MATNR     = I_MATNR
      IMPORTING
        E_MEINS     = DATA(E_MEINS)
        E_GROES_DEC = DATA(E_GROES_DEC)
        IS_OK       = DATA(IS_OK).

    IF IS_OK IS INITIAL.
      E_MSG = |A quantidade da embalagem do material { I_MATNR } informada no cadastro mestre, deve ser numérica.|.
      RETURN.
    ENDIF.

    IF E_GROES_DEC IS INITIAL.
      E_MSG = |O Cadastro do Material { I_MATNR } está sem a quantidade da embalagem no cadastro mestre.|.
      RETURN.
    ENDIF.

    LV_QTD_PROP = FRAC( I_QUANTIDADE / E_GROES_DEC ).

    CHECK LV_QTD_PROP IS NOT INITIAL.

    E_MSG = |A quantidade informada para o item, não é proporcional à quantidade de { E_GROES_DEC } { E_MEINS } da embalagem do material.|.

  ENDMETHOD.


  METHOD CHK_SOLICITACAO_EM_CARGA.

    IS_OK = ABAP_FALSE.

    SELECT SINGLE COUNT(*)
      FROM ZSDT0082
    WHERE VBELN  EQ @I_VBELN
      AND POSNR  EQ @I_POSNR
      AND STATUS EQ 5.

    CHECK SY-SUBRC IS INITIAL.

*// Sementes
    SELECT SINGLE NRO_LOTE
      FROM ZSDT0131
        INTO @DATA(LV_NRO_LOTE)
      WHERE VBELN EQ @I_VBELN
        AND POSNR EQ @I_POSNR
        AND STATUS NE @ABAP_TRUE.

    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE NRO_CG
        FROM ZSDT0129
          INTO @DATA(LV_NRO_CG)
        WHERE NRO_LOTE EQ @LV_NRO_LOTE.

      CHECK SY-SUBRC IS INITIAL.
      MESSAGE |Ordem de Venda { I_VBELN }/{ I_POSNR }, esta vinculado na Carga { LV_NRO_CG }!| TYPE 'S' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

*// Defensivos
    SELECT SINGLE NRO_CGD
      FROM ZSDT0140
        INTO @DATA(LV_NRO_CGD)
      WHERE VBELN  EQ @I_VBELN
        AND POSNR  EQ @I_POSNR
        AND STATUS NE @ABAP_TRUE.
    IF SY-SUBRC IS INITIAL.
      MESSAGE |Ordem de Venda { I_VBELN }/{ I_POSNR }, esta vinculado na Carga { LV_NRO_CGD }!| TYPE 'S' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

*// Fertilizante
    SELECT COUNT(*)
      FROM ZPPT0008
      WHERE VBELN EQ I_VBELN
        AND POSNR EQ I_POSNR.
    IF SY-SUBRC IS INITIAL.
      MESSAGE |Ordem de Venda { I_VBELN }/{ I_POSNR }, esta vinculado em um Planejamento!| TYPE 'S' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHK_USER_EXCECAO_PROPORCIONAL.

    FREE IS_OK.

    SELECT SINGLE COUNT(*)
      FROM ZSDT0408
      WHERE USUARIO = I_USER.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD DADOSGERAIS.
*
*    SELECT *
*      FROM VBAP
*      INTO TABLE @DATA(LT_VBAP)
*    WHERE VBELN EQ @I_VBELN
*      AND POSNR EQ @I_POSNR.
*
*    SELECT SINGLE *
*      FROM VBPA
*      INTO @DATA(WA_VBPA)
*      WHERE VBELN EQ @I_VBELN
*        AND POSNR EQ @I_POSNR
*        AND PARVW EQ 'PC'.
*
*    _LOGIC_SWITCH-COND_HANDL   = ABAP_TRUE.
*
** Atualiza o Incoterms1
*    _ORDERHEADERIN  = VALUE #(
*                               INCOTERMS1 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN WA_ALT_GERAIS-INCO1 ELSE ABAP_FALSE )
*                               INCOTERMS2 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN WA_ALT_GERAIS-INCO1 ELSE ABAP_FALSE )
*                             ).
*    _ORDERHEADERINX = VALUE #(
*                               UPDATEFLAG = 'U'
*                               INCOTERMS1 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                               INCOTERMS2 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                             ).
*
**  Atualiza a ZPESAGEM
*    IF ZPESAGEM_ EQ ABAP_TRUE.
*      _BAPE_VBAK = VALUE #(
*                             VBELN = VBELN
*                             ZPESAGEM = WA_ALT_GERAIS-ZPESAGEM
*                          ).
*      _BAPE_VBAKX = VALUE #(
*                           VBELN = VBELN
*                           ZPESAGEM = ABAP_TRUE
*                         ).
*      _BAPIPAREX = VALUE #(
*                            ( STRUCTURE = 'BAPE_VBAK'  VALUEPART1 = _BAPE_VBAK  )
*                            ( STRUCTURE = 'BAPE_VBAKX' VALUEPART1 = _BAPE_VBAKX )
*                          ).
*    ENDIF.
*
** Atualiza a Rota
*    IF ROUTE_ EQ ABAP_TRUE OR INCO1_ EQ ABAP_TRUE.
*      _BAPISDITM = VALUE #( FOR LS IN LT_VBAP
*                              (
*                                ITM_NUMBER = LS-POSNR
*                                ROUTE      = COND #( WHEN ROUTE_ EQ ABAP_TRUE THEN WA_ALT_GERAIS-ROUTE )
*                                INCOTERMS1 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN WA_ALT_GERAIS-INCO1 )
*                                INCOTERMS2 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN WA_ALT_GERAIS-INCO1 )
*                              )
*                          ).
*      _BAPISDITMX = VALUE #( FOR LS1 IN LT_VBAP
*                              (
*                                UPDATEFLAG = 'U'
*                                ITM_NUMBER = LS1-POSNR
*                                ROUTE      = COND #( WHEN ROUTE_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                                INCOTERMS1 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                                INCOTERMS2 = COND #( WHEN INCO1_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                              )
*                           ).
*    ELSEIF  DEP_LOTE_ IS NOT INITIAL.
** Alteração do Deposito e Lote
*      _BAPISDITM = VALUE #( FOR LS3 IN IT_DEP_LOTE2
*                              (
*                                ITM_NUMBER = LS3-POSNR
*                                "*---> 19/07/2023 - Migração S4 - LO
*                                MATERIAL        = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN LS3-MATNR )
**                              material_long   = COND #( WHEN dep_lote_ EQ abap_true THEN |{ ls3-matnr ALPHA = IN  }| )
*                                "*---> 19/07/2023 - Migração S4 - LO
*                                PLANT           = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN LS3-WERKS )
*                                STORE_LOC       = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN LS3-LGORT )
*                                BATCH           = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN LS3-CHARG )
*
*                              )
*                          ).
*      _BAPISDITMX = VALUE #( FOR LS4 IN IT_DEP_LOTE2
*                              (
*                                UPDATEFLAG = 'U'
*                                ITM_NUMBER = LS4-POSNR
*                                "*---> 19/07/2023 - Migração S4 - LO
*                                MATERIAL   = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
**                              material_long   = COND #( WHEN dep_lote_ EQ abap_true THEN abap_true ELSE abap_false )
*                                "*---> 19/07/2023 - Migração S4 - LO
*                                PLANT          = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                                STORE_LOC      = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                                BATCH          = COND #( WHEN DEP_LOTE_ EQ ABAP_TRUE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                              )
*                           ).
*
*    ENDIF.
*
** Atualiza o Parceiro PC
*    IF COD_PARC_ EQ ABAP_TRUE.
*
*      _PARTNERSC = VALUE #( (
*                              DOCUMENT = WA_VBPA-VBELN
*                              ITM_NUMBER = WA_VBPA-POSNR
*                              UPDATEFLAG = 'U'
*                              PARTN_ROLE = WA_VBPA-PARVW
*                              P_NUMB_OLD = WA_VBPA-LIFNR
*                              P_NUMB_NEW = WA_ALT_GERAIS-COD_PARC
*                          ) ).
*
*      " 20.12.2022 - RAMON --- 74130 -->> Comentando para subir os ajustes do <RIM-SKM-IR120585-23.12.22
*
*      SELECT SINGLE KUNNR FROM VBAK
*        INTO @DATA(LV_KUNNR)
*      WHERE VBELN = @WA_ALT_GERAIS-VBELN.
*
*      PERFORM F_GET_ROUTE
*        USING WA_ALT_GERAIS-COD_PARC
*              LV_KUNNR
*     CHANGING WA_ALT_GERAIS-ROUTE.
*
*      IF V_ERRO = 'X'.
*        MESSAGE 'Não existe itinerário para zona de transporte do cliente' TYPE 'I'.
*        EXIT.
*      ENDIF.
*
*      _BAPISDITM = VALUE #( FOR LS IN LT_VBAP
*                          (
*                            ITM_NUMBER = LS-POSNR
*                            ROUTE      =  WA_ALT_GERAIS-ROUTE
*                            "incoterms1 = COND #( WHEN inco1_ EQ abap_true THEN wa_alt_gerais-inco1 )
*                            "incoterms2 = COND #( WHEN inco1_ EQ abap_true THEN wa_alt_gerais-inco1 )
*                          )
*                      ).
*      _BAPISDITMX = VALUE #( FOR LS1 IN LT_VBAP
*                              (
*                                UPDATEFLAG = 'U'
*                                ITM_NUMBER = LS1-POSNR
*                                ROUTE      = 'X'
*                                "incoterms1 = COND #( WHEN inco1_ EQ abap_true THEN abap_true ELSE abap_false )
*                                "incoterms2 = COND #( WHEN inco1_ EQ abap_true THEN abap_true ELSE abap_false )
*                              )
*                           ).
*
*      " 20.12.2022 - RAMON --- 74130 --<<
*
*    ENDIF.

  ENDMETHOD.


  method fill_desmembramento_new.

    check at_manutencao-desmembramento-vbeln is not initial.
    check at_manutencao-desmembramento-posnr is not initial.

    select single *
      from vbak
      into e_vbak
      where vbeln eq at_manutencao-desmembramento-vbeln.

    e_vbak-kunnr = at_manutencao-desmembramento-kunnr.

    select *
      from vbap
      into table e_vbap
      where vbeln eq at_manutencao-desmembramento-vbeln
        and posnr eq at_manutencao-desmembramento-posnr.

    read table e_vbap assigning field-symbol(<fs_vbap>) index 1.

    <fs_vbap>-kwmeng = at_manutencao-desmembramento-qtd_recebida.
    <fs_vbap>-matnr  = at_manutencao-desmembramento-matnr.

    select single *
      from vbkd
      into e_vbkd
      where vbeln eq at_manutencao-desmembramento-vbeln.

    select single nr_rot
      from zsdt0082
      into @data(lv_nr_rot)
      where vbeln eq @at_manutencao-desmembramento-vbeln
        and posnr eq @at_manutencao-desmembramento-posnr.

    if lv_nr_rot is not initial.

      select single lzone
        from zsdt0132
        into @data(lv_lzone)
        where nr_rot eq @lv_nr_rot
          and status eq @constantes-status-ativo.

    endif.

    select single lzone
      from vbpa
      into @data(lv_azone)
      where vbeln eq @at_manutencao-desmembramento-vbeln
        and parvw eq 'PC'.

    if lv_azone is not initial and
       lv_lzone is not initial.

      select single route
        from trolz
      into @data(lv_route)
      where azone eq @lv_azone
        and lzone eq @lv_lzone.

    endif.

    if at_manutencao-desmembramento-itinerario is not initial.
      <fs_vbap>-route = at_manutencao-desmembramento-itinerario.
    endif.

    call method zcl_solicitacao_ov=>get_imposto_v2
      exporting
        i_direcao     = 'O'
        i_vbeln       = at_manutencao-desmembramento-vbeln
        i_posnr       = at_manutencao-desmembramento-posnr
      receiving
        i_coeficiente = data(coeficiente_o).

    call method zcl_solicitacao_ov=>get_imposto_v2
      exporting
        i_direcao     = 'D'
        i_cliente     = at_manutencao-desmembramento-kunnr
        i_fornecedor  = conv #( |{ <fs_vbap>-werks alpha = in }| )
        i_werks       = <fs_vbap>-werks
        i_material    = <fs_vbap>-matnr
        i_tipo_ordem  = e_vbak-auart
      receiving
        i_coeficiente = data(coeficiente_d).

    select single *
      from v_konv
    into corresponding fields of @e_konv
      where knumv eq @e_vbak-knumv
        and kposn eq @<fs_vbap>-posnr
        and kschl eq 'PR00'.

    at_price = e_konv-kbetr.

*    if at_desmembramento_devolucao is not initial.
*      clear at_desconto.
*      select single *
*        from v_konv
*      into @data(e_konv_br00)
*        where knumv eq @e_vbak-knumv
*          and kposn eq @<fs_vbap>-posnr
*          and kschl eq 'RB00'.
*
*      at_desconto = e_konv_br00-kbetr.
*    endif.

    if  coeficiente_o ne coeficiente_d.

*      IF COEFICIENTE_D IS NOT INITIAL.
*        E_KONV-KBETR *= COEFICIENTE_D.
*      ENDIF.

      if coeficiente_o is not initial and coeficiente_d is initial.
        at_price /= coeficiente_o.
        at_desconto /= coeficiente_o.
      endif.

      if coeficiente_o is not initial and coeficiente_d is not initial.
        at_price /= coeficiente_o.
        at_price *= coeficiente_d.

        at_desconto /= coeficiente_o.
        at_desconto *= coeficiente_d.
      endif.

      if coeficiente_o is initial and coeficiente_d is not initial.
        at_price *= coeficiente_d.
        at_desconto *= coeficiente_d.
      endif.

    endif.

    <fs_vbap>-kmein = e_konv-kmein.

  endmethod.


  METHOD FILL_DESMEMBRAMENTO_OLD.

    CHECK AT_CATEGORIA EQ CONSTANTES-CATEGORIA-DESMEMBRAMENTO.

    FREE: AT_DADOS_TRATADOS, E_VBAP.

    CHECK AT_MANUTENCAO-VBELN_OLD IS NOT INITIAL.
    CHECK AT_MANUTENCAO-MATNR_OLD IS NOT INITIAL.

    SELECT SINGLE *
      FROM VBAK
      INTO @DATA(LS_VBAK)
    WHERE VBELN EQ @AT_MANUTENCAO-VBELN_OLD.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(LS_VBAP)
    WHERE VBELN EQ @AT_MANUTENCAO-VBELN_OLD
      AND MATNR EQ @AT_MANUTENCAO-MATNR_OLD.

    APPEND INITIAL LINE TO AT_DADOS_TRATADOS ASSIGNING FIELD-SYMBOL(<FS_DADOS_TRATADOS>).
    <FS_DADOS_TRATADOS>-VBELN = LS_VBAP-VBELN.
    <FS_DADOS_TRATADOS>-POSNR = LS_VBAP-POSNR.
    <FS_DADOS_TRATADOS>-ZMENG = LS_VBAP-KWMENG - AT_MANUTENCAO-QTD_REMOVIDA.
    <FS_DADOS_TRATADOS>-MATNR = LS_VBAP-MATNR.

    APPEND INITIAL LINE TO E_VBAP ASSIGNING FIELD-SYMBOL(<FS_VBAP>).
    <FS_VBAP>-VBELN = LS_VBAP-VBELN.
    <FS_VBAP>-POSNR = LS_VBAP-POSNR.
    <FS_VBAP>-ZMENG = LS_VBAP-KWMENG - AT_MANUTENCAO-QTD_REMOVIDA.
    <FS_VBAP>-MATNR = LS_VBAP-MATNR.

    SELECT SINGLE *
      FROM V_KONV
    INTO CORRESPONDING FIELDS OF @E_KONV
      WHERE KNUMV EQ @E_VBAK-KNUMV
        AND KPOSN EQ @LS_VBAP-POSNR
        AND KSCHL EQ 'PR00'.

    APPEND INITIAL LINE TO AT_DADOS_TRATADOS ASSIGNING <FS_DADOS_TRATADOS>.
    <FS_DADOS_TRATADOS>-VBELN = LS_VBAP-VBELN.
    <FS_DADOS_TRATADOS>-ZMENG = AT_MANUTENCAO-QTD_REMOVIDA.
    <FS_DADOS_TRATADOS>-MATNR = LS_VBAP-MATNR.
    <FS_DADOS_TRATADOS>-CHARG = LS_VBAP-CHARG.
    <FS_DADOS_TRATADOS>-VRKME = LS_VBAP-MEINS.
    <FS_DADOS_TRATADOS>-WERKS = LS_VBAP-WERKS.
    <FS_DADOS_TRATADOS>-LGORT = LS_VBAP-LGORT.
    <FS_DADOS_TRATADOS>-NETPR = E_KONV-KBETR.

    APPEND INITIAL LINE TO E_VBAP ASSIGNING <FS_VBAP>.
    <FS_VBAP>-VBELN = LS_VBAP-VBELN.
    <FS_VBAP>-ZMENG = AT_MANUTENCAO-QTD_REMOVIDA.
    <FS_VBAP>-MATNR = LS_VBAP-MATNR.
    <FS_VBAP>-CHARG = LS_VBAP-CHARG.
    <FS_VBAP>-VRKME = LS_VBAP-MEINS.
    <FS_VBAP>-WERKS = LS_VBAP-WERKS.
    <FS_VBAP>-LGORT = LS_VBAP-LGORT.
    <FS_VBAP>-NETPR = E_KONV-KBETR.

  ENDMETHOD.


  METHOD FILL_TROCA_NEW.

    CHECK AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN IS NOT INITIAL.
    CHECK AT_MANUTENCAO-TROCA-ORDEM_OLD-POSNR IS NOT INITIAL.

    SELECT SINGLE *
      FROM VBAK
      INTO E_VBAK
      WHERE VBELN EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.

    E_VBAK-BSTNK = |{ AT_MANUTENCAO-TROCA-CULTURA }-{ AT_MANUTENCAO-TROCA-SAFRA }-{ AT_MANUTENCAO-TROCA-DOC_SIMULACAO }|.
    E_VBAK-KUNNR = AT_MANUTENCAO-TROCA-ORDEM_OLD-KUNNR.

    SELECT *
      FROM VBAP
      INTO TABLE E_VBAP
      WHERE VBELN EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN
        AND POSNR EQ AT_MANUTENCAO-TROCA-ORDEM_OLD-POSNR.

    READ TABLE E_VBAP ASSIGNING FIELD-SYMBOL(<FS_VBAP>) INDEX 1.

    <FS_VBAP>-VBELN      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-VBELN.
    <FS_VBAP>-SPART      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-SPART.
    <FS_VBAP>-WERKS      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-WERKS.
    <FS_VBAP>-LGORT      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-LGORT.
    <FS_VBAP>-VRKME      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-MEINS.
    <FS_VBAP>-KWMENG     =    AT_MANUTENCAO-TROCA-ORDEM_NEW-QTD_RECEBIDA.
    <FS_VBAP>-CHARG      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-LOTE.
    <FS_VBAP>-MATNR      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-MATNR.
    <FS_VBAP>-FKBER      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-VLR_VENDA.
    <FS_VBAP>-KMEIN      =    AT_MANUTENCAO-TROCA-ORDEM_NEW-MEINS.

    SELECT SINGLE AUART, SPART
      FROM ZSDT0087
      INTO ( @E_VBAK-AUART, @<FS_VBAP>-SPART )
    WHERE MATKL EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-MATKL
      AND TPSIM EQ @AT_MANUTENCAO-TROCA-ORDEM_OLD-TPSIM
      AND INCO1 EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-INCO1.

    SELECT SINGLE *
       FROM V_KONV
     INTO CORRESPONDING FIELDS OF @E_KONV
       WHERE KNUMV EQ @E_VBAK-KNUMV
         AND KPOSN EQ @<FS_VBAP>-POSNR
         AND KSCHL EQ 'PR00'.

    CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
      EXPORTING
        I_DIRECAO     = 'D'
        I_CLIENTE     = E_VBAK-KUNNR
        I_FORNECEDOR  = CONV #( |{ <FS_VBAP>-WERKS ALPHA = IN }| )
        I_MATERIAL    = <FS_VBAP>-MATNR
        I_TIPO_ORDEM  = E_VBAK-AUART
        I_WERKS       = <FS_VBAP>-WERKS
      RECEIVING
        I_COEFICIENTE = DATA(COEFICIENTE_D).

    AT_PRICE = AT_MANUTENCAO-TROCA-ORDEM_NEW-VLR_VENDA.

    IF COEFICIENTE_D IS NOT INITIAL.
      AT_PRICE *= COEFICIENTE_D.
    ENDIF.

    CASE <FS_VBAP>-SPART.
      WHEN '03'.

        SELECT SINGLE LGORT
          FROM ZSDT0048
          INTO <FS_VBAP>-LGORT
            WHERE BUKRS EQ <FS_VBAP>-VKORG_ANA
              AND VTWEG EQ <FS_VBAP>-VTWEG_ANA
              AND SPART EQ <FS_VBAP>-SPART
              AND VKBUR EQ <FS_VBAP>-VKBUR_ANA.

      WHEN '04'.

        SELECT SINGLE LGORT
          FROM ZSDT0048
          INTO @DATA(LV_LGORT)
            WHERE BUKRS   EQ @<FS_VBAP>-VKORG_ANA
              AND VTWEG   EQ @<FS_VBAP>-VTWEG_ANA
              AND SPART   EQ @<FS_VBAP>-SPART
              AND WERKS_D EQ @<FS_VBAP>-WERKS.

        IF <FS_VBAP>-LGORT IS INITIAL.
          <FS_VBAP>-LGORT = LV_LGORT.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    CASE E_VBAK-AUART.
      WHEN 'ZFTE' OR 'ZSEM' OR 'ZOSM' OR 'ZOFE'.
        IF <FS_VBAP>-VRKME EQ 'TO'.
          <FS_VBAP>-KWMENG *= 1000.
          <FS_VBAP>-VRKME   = 'KG'.
        ELSE.
          CLEAR: <FS_VBAP>-VRKME, <FS_VBAP>-KMEIN.
        ENDIF.
      WHEN OTHERS.
        CLEAR: <FS_VBAP>-VRKME, <FS_VBAP>-KMEIN.
    ENDCASE.

    SELECT SINGLE *
      FROM VBKD
      INTO E_VBKD
      WHERE VBELN EQ E_VBAK-VBELN.

    E_VBKD-ZTERM = SWITCH #( AT_MANUTENCAO-TROCA-ORDEM_OLD-TPSIM
                               WHEN 'TS' THEN 'I001'
                               WHEN 'AD' THEN 'I002'
                               WHEN 'TT' THEN 'I008'
                               WHEN 'VV' OR
                                    'VF' OR
                                    'BN' THEN 'I003'
                               WHEN 'TV' THEN 'I004'
                               WHEN 'VP' THEN 'I005'
                               WHEN 'PM' THEN 'I006' ).

    E_VBKD-INCO1 = AT_MANUTENCAO-TROCA-ORDEM_NEW-INCO1.

    IF AT_BACKGROUND IS NOT INITIAL.

      CALL METHOD VERIFICAR_ITINERARIO_82
        EXPORTING
          I_ROTA_LR     = AT_MANUTENCAO-TROCA-ORDEM_NEW-NR_ROT_LR_81
          I_ROTA_PC     = AT_MANUTENCAO-TROCA-ORDEM_NEW-NR_ROT_PC_81
          IS_BACKGROUND = AT_BACKGROUND
        IMPORTING
          E_ROUTE       = DATA(E_ROUTE).

      <FS_VBAP>-ROUTE = E_ROUTE.

    ENDIF.

  ENDMETHOD.


  METHOD FILL_TROCA_OLD.

    CHECK AT_CATEGORIA EQ CONSTANTES-CATEGORIA-TROCA.

    FREE: AT_DADOS_TRATADOS, E_VBAP.

    CHECK AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN IS NOT INITIAL.
    CHECK AT_MANUTENCAO-TROCA-ORDEM_OLD-POSNR IS NOT INITIAL.

    APPEND INITIAL LINE TO AT_DADOS_TRATADOS ASSIGNING FIELD-SYMBOL(<FS_DADOS_TRATADOS>).
    <FS_DADOS_TRATADOS>-VBELN = AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.
    <FS_DADOS_TRATADOS>-POSNR = AT_MANUTENCAO-TROCA-ORDEM_OLD-POSNR.
    <FS_DADOS_TRATADOS>-ZMENG = AT_MANUTENCAO-TROCA-ORDEM_OLD-KWMENG - AT_MANUTENCAO-TROCA-ORDEM_OLD-QTD_REMOVIDA.
    <FS_DADOS_TRATADOS>-MATNR = AT_MANUTENCAO-TROCA-ORDEM_OLD-MATNR.

    APPEND INITIAL LINE TO E_VBAP ASSIGNING FIELD-SYMBOL(<FS_VBAP>).
    <FS_VBAP>-VBELN = AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.
    <FS_VBAP>-POSNR = AT_MANUTENCAO-TROCA-ORDEM_OLD-POSNR.
    <FS_VBAP>-ZMENG = AT_MANUTENCAO-TROCA-ORDEM_OLD-KWMENG - AT_MANUTENCAO-TROCA-ORDEM_OLD-QTD_REMOVIDA.
    <FS_VBAP>-MATNR = AT_MANUTENCAO-TROCA-ORDEM_OLD-MATNR.

    APPEND INITIAL LINE TO AT_DADOS_TRATADOS ASSIGNING <FS_DADOS_TRATADOS>.
    <FS_DADOS_TRATADOS>-VBELN = AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.
    <FS_DADOS_TRATADOS>-ZMENG = AT_MANUTENCAO-TROCA-ORDEM_NEW-QTD_RECEBIDA.
    <FS_DADOS_TRATADOS>-MATNR = AT_MANUTENCAO-TROCA-ORDEM_NEW-MATNR.
    <FS_DADOS_TRATADOS>-CHARG = AT_MANUTENCAO-TROCA-ORDEM_NEW-LOTE.
    <FS_DADOS_TRATADOS>-VRKME = AT_MANUTENCAO-TROCA-ORDEM_NEW-MEINS.
    <FS_DADOS_TRATADOS>-WERKS = AT_MANUTENCAO-TROCA-ORDEM_NEW-WERKS.
    <FS_DADOS_TRATADOS>-LGORT = AT_MANUTENCAO-TROCA-ORDEM_NEW-LGORT.
    <FS_DADOS_TRATADOS>-NETPR = AT_MANUTENCAO-TROCA-ORDEM_NEW-VLR_VENDA.

    APPEND INITIAL LINE TO E_VBAP ASSIGNING <FS_VBAP>.
    <FS_VBAP>-VBELN = AT_MANUTENCAO-TROCA-ORDEM_OLD-VBELN.
    <FS_VBAP>-ZMENG = AT_MANUTENCAO-TROCA-ORDEM_NEW-QTD_RECEBIDA.
    <FS_VBAP>-MATNR = AT_MANUTENCAO-TROCA-ORDEM_NEW-MATNR.
    <FS_VBAP>-CHARG = AT_MANUTENCAO-TROCA-ORDEM_NEW-LOTE.
    <FS_VBAP>-VRKME = AT_MANUTENCAO-TROCA-ORDEM_NEW-MEINS.
    <FS_VBAP>-WERKS = AT_MANUTENCAO-TROCA-ORDEM_NEW-WERKS.
    <FS_VBAP>-LGORT = AT_MANUTENCAO-TROCA-ORDEM_NEW-LGORT.
    <FS_VBAP>-NETPR = AT_MANUTENCAO-TROCA-ORDEM_NEW-VLR_VENDA.

  ENDMETHOD.


  METHOD GET_PROPORCIONALIDADE_MATERIAL.

    DATA: LV_GROES_DEC TYPE ESECOMPAVG,
          LV_GROES_STR TYPE CHAR30.

    CHECK I_MATNR IS NOT INITIAL.

    FREE IS_OK.

    SELECT SINGLE MATNR, GROES, MEINS
      FROM MARA
      INTO @DATA(LS_MARA)
     WHERE MATNR EQ @I_MATNR.

    CHECK SY-SUBRC IS INITIAL.
    CHECK LS_MARA-GROES IS NOT INITIAL.

    LV_GROES_STR = LS_MARA-GROES.

    IF LS_MARA-GROES CA SY-ABCDE.
      RETURN.
    ENDIF.

    CALL FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
      EXPORTING
        I_STRING                   = LV_GROES_STR
      IMPORTING
        E_DEC                      = E_GROES_DEC
      EXCEPTIONS
        WRONG_CHARACTERS           = 1
        FIRST_CHARACTER_WRONG      = 2
        ARITHMETIC_SIGN            = 3
        MULTIPLE_DECIMAL_SEPARATOR = 4
        THOUSANDSEP_IN_DECIMAL     = 5
        THOUSAND_SEPARATOR         = 6
        NUMBER_TOO_BIG             = 7
        OTHERS                     = 8.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.
    E_MEINS = LS_MARA-MEINS.

  ENDMETHOD.


  METHOD GET_SIMULADOR.

    CHECK I_VBELN IS NOT INITIAL.

    SELECT SINGLE DOC_SIMULACAO
      FROM ZSDT0041
      INTO E_SIMULADOR
      WHERE VBELN EQ I_VBELN.

    CHECK SY-SUBRC IS NOT INITIAL.

    SELECT SINGLE DOC_SIMULACAO
      FROM ZSDT0090
      INTO E_SIMULADOR
      WHERE VBELN EQ I_VBELN.

  ENDMETHOD.


  METHOD GET_TEXT_OV.

    DATA: LT_LINES  TYPE TABLE OF TLINE,
          LS_TEXT   LIKE LINE OF T_TEXT,
          LV_ID     TYPE THEAD-TDID VALUE '0002',
          LV_OBJECT TYPE TDOBJECT VALUE 'VBBP',
          LV_NAME   TYPE THEAD-TDNAME.

    LV_NAME = I_VBELN.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = LV_ID
        LANGUAGE                = SY-LANGU
        NAME                    = LV_NAME
        OBJECT                  = LV_OBJECT
      TABLES
        LINES                   = LT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    LOOP AT LT_LINES INTO DATA(LS_LINE).
      LS_TEXT = LS_LINE-TDLINE+0(72).
      APPEND LS_TEXT TO T_TEXT.
    ENDLOOP.

    CHECK  AT_BACKGROUND IS INITIAL.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        IM_TITLE = CONV SYTITLE( TEXT-004 )
      CHANGING
        CH_TEXT  = T_TEXT.

  ENDMETHOD.


  METHOD ROWBACK_ENCERRAMENTO.

    CALL METHOD CHK_SOLICITACAO_EM_CARGA
      EXPORTING
        I_VBELN = I_VBELN
        I_POSNR = I_POSNR
      IMPORTING
        IS_OK   = DATA(IS_OK).

    CHECK IS_OK IS INITIAL.

    CALL METHOD CHANGE_SOLICITACAO_DISTRIBUIDA
      EXPORTING
        I_VBELN          = I_VBELN
        I_POSNR          = I_POSNR
        I_QTDE_ENCERRADA = I_QTDE_ENCERRADA.

    CALL METHOD CHANGE_SOLICITACAO_APROVADA
      EXPORTING
        I_VBELN          = I_VBELN
        I_POSNR          = I_POSNR
        I_QTDE_ENCERRADA = I_QTDE_ENCERRADA.

  ENDMETHOD.


  METHOD RUN.

    FREE: R_RETURN.

    CALL METHOD CLEAR_ATRIBUTOS
      EXPORTING
        SET_ALL = ABAP_TRUE.

    AT_BACKGROUND = IS_BACKGROUND.

    IF I_MANUTENCAO-CH_REFERENCIA IS NOT INITIAL AND
       I_MANUTENCAO-ITEM_DISTRIB IS NOT INITIAL.

      AT_CH_REFERENCIA = I_MANUTENCAO-CH_REFERENCIA.
      AT_ITEM_DISTRIB = I_MANUTENCAO-ITEM_DISTRIB.

      CALL METHOD GET_OV_CH_REFERENCIA
        EXPORTING
          I_BSTKD_E = |{ AT_CH_REFERENCIA }-{ AT_ITEM_DISTRIB }|
        IMPORTING
          E_VBELN   = E_VBELN
          E_POSNR   = E_POSNR.

      IF E_VBELN IS NOT INITIAL AND
         E_POSNR IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDIF.

    CALL METHOD CHECK_DADOS_BACKGROUND
      EXPORTING
        I_MANUTENCAO = I_MANUTENCAO
      IMPORTING
        R_RETURN     = R_RETURN.

    IF R_RETURN IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD GET_SIMULADOR
      EXPORTING
        I_VBELN     = I_MANUTENCAO-VBELN_OLD-VBELN
      IMPORTING
        E_SIMULADOR = DATA(LV_DOC_SIMULACAO).

    CHECK LV_DOC_SIMULACAO IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZSDT0040
      INTO @DATA(LS_ZSDT0040)
      WHERE DOC_SIMULACAO EQ @LV_DOC_SIMULACAO.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM VBAK
      INTO @DATA(LS_VBAK)
      WHERE VBELN EQ @I_MANUTENCAO-VBELN_OLD-VBELN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(LS_VBAP)
      WHERE VBELN EQ @I_MANUTENCAO-VBELN_OLD-VBELN
        AND POSNR EQ @I_MANUTENCAO-VBELN_OLD-POSNR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM VBKD
      INTO @DATA(LS_VBKD)
      WHERE VBELN EQ @I_MANUTENCAO-VBELN_OLD-VBELN.

    SELECT SINGLE MATNR, MATKL, MTART
      FROM MARA
      INTO @DATA(LS_MARA_OLD)
      WHERE MATNR EQ @LS_VBAP-MATNR.

    SELECT SINGLE MATNR, MATKL, MTART
      FROM MARA
      INTO @DATA(LS_MARA_NEW)
      WHERE MATNR EQ @I_MANUTENCAO-VBELN_NEW-MATNR.

    IF SY-UCOMM EQ 'DESMEMBRAR'.
*    IF I_MANUTENCAO-VBELN_NEW-KUNNR NE LS_VBAK-KUNNR.

      AT_MANUTENCAO-DESMEMBRAMENTO =
          VALUE #(
                    VBELN = LS_VBAP-VBELN
                    POSNR = LS_VBAP-POSNR
                    KUNNR = I_MANUTENCAO-VBELN_NEW-KUNNR
                    QTD_RECEBIDA = I_MANUTENCAO-VBELN_NEW-QUANTIDADE
                    ITINERARIO   = I_MANUTENCAO-VBELN_NEW-ITINERARIO
                 ).

      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>SET_DADOS_DESMEMBRAMENTO
        EXPORTING
          I_MANUTENCAO = AT_MANUTENCAO.

      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>DESMEMBRAMENTO_MASSA
        IMPORTING
          R_RETURN = R_RETURN.

      SELECT SINGLE VBELN, POSNR
        FROM VBAP
        INTO @DATA(LS_VBAP_NEW)
        WHERE VBELN EQ @AT_MANUTENCAO-VBELN_NEW.

      E_VBELN = LS_VBAP_NEW-VBELN.
      E_POSNR = LS_VBAP_NEW-POSNR.

      CLEAR AT_MANUTENCAO.
      CLEAR AT_BACKGROUND.

      RETURN.

*    ENDIF.

    ENDIF.

    CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
      EXPORTING
        I_DIRECAO     = 'O'
        I_VBELN       = LS_VBAP-VBELN
        I_POSNR       = LS_VBAP-POSNR
      RECEIVING
        I_COEFICIENTE = DATA(COEFICIENTE_O).

    IF COEFICIENTE_O IS NOT INITIAL.
      LS_VBAP-NETPR /= COEFICIENTE_O.
    ENDIF.

    AT_MANUTENCAO-TROCA =
    VALUE #(
              DOC_SIMULACAO = LS_ZSDT0040-DOC_SIMULACAO
              CULTURA       = LS_ZSDT0040-CULTURA
              SAFRA         = LS_ZSDT0040-SAFRA
              NETWR         = LS_VBAP-NETWR + LS_VBAP-MWSBP
              ORDEM_OLD = VALUE #(
                                    VBELN        = LS_VBAP-VBELN
                                    POSNR        = LS_VBAP-POSNR
                                    MATNR        = LS_VBAP-MATNR
                                    QTD_REMOVIDA = I_MANUTENCAO-VBELN_NEW-QUANTIDADE - LS_VBAP-ZMENG
                                    AUART        = LS_VBAK-AUART
                                    WERKS        = LS_VBAP-WERKS
                                    MATKL        = LS_VBAP-MATKL
                                    INCO1        = LS_VBKD-INCO1
                                    KUNNR        = LS_VBAK-KUNNR
                                    TPSIM        = LS_ZSDT0040-TPSIM
                                    KWMENG       = LS_VBAP-KWMENG
                                 )
              ORDEM_NEW = VALUE #(
                                    MATNR        = I_MANUTENCAO-VBELN_NEW-MATNR
                                    MATKL        = LS_MARA_NEW-MATKL
                                    INCO1        = LS_VBKD-INCO1
                                    WERKS        = I_MANUTENCAO-VBELN_NEW-WERKS
                                    LOTE         = LS_VBAP-CHARG
                                    LGORT        = I_MANUTENCAO-VBELN_NEW-LGORT
                                    QTD_RECEBIDA = I_MANUTENCAO-VBELN_NEW-QUANTIDADE
                                    MEINS        = LS_VBAP-MEINS
                                    VLR_VENDA    = LS_VBAP-NETPR
                                    SPART        = LS_ZSDT0040-SPART
                                    MTART        = LS_MARA_NEW-MTART
                                    NR_ROT_LR_81 = I_MANUTENCAO-VBELN_NEW-NR_ROT_LR
                                    NR_ROT_PC_81 = I_MANUTENCAO-VBELN_NEW-NR_ROT_PC
                                 )
              ).

    CALL METHOD SET_DADOS_TROCA
      EXPORTING
        I_MANUTENCAO = AT_MANUTENCAO.

    CALL METHOD TROCA_MATERIAIS_MASSA
      IMPORTING
        R_RETURN = R_RETURN.

    SELECT SINGLE VBELN, POSNR
      FROM VBAP
      INTO @LS_VBAP_NEW
      WHERE VBELN EQ @AT_MANUTENCAO-TROCA-ORDEM_NEW-VBELN.

    IF SY-SUBRC IS INITIAL.
      IF AT_MANUTENCAO-TROCA-ORDEM_NEW-POSNR IS INITIAL.
        E_VBELN = LS_VBAP_NEW-VBELN.
        E_POSNR = LS_VBAP_NEW-POSNR.
      ELSE.
        E_VBELN = AT_MANUTENCAO-TROCA-ORDEM_NEW-VBELN.
        E_POSNR = AT_MANUTENCAO-TROCA-ORDEM_NEW-POSNR.
      ENDIF.
    ENDIF.

    CLEAR AT_BACKGROUND.

  ENDMETHOD.


  METHOD SET_DADOS_DESMEMBRAMENTO.

    CALL METHOD CLEAR_ATRIBUTOS
      EXPORTING
        SET_ALL = ABAP_TRUE.

    AT_MANUTENCAO = I_MANUTENCAO.

    SELECT SINGLE MATNR
      FROM VBAP
      INTO AT_MANUTENCAO-DESMEMBRAMENTO-MATNR
      WHERE VBELN EQ AT_MANUTENCAO-DESMEMBRAMENTO-VBELN
        AND POSNR EQ AT_MANUTENCAO-DESMEMBRAMENTO-POSNR.

    AT_MANUTENCAO-VBELN_OLD     = AT_MANUTENCAO-DESMEMBRAMENTO-VBELN.
    AT_MANUTENCAO-MATNR_OLD     = AT_MANUTENCAO-DESMEMBRAMENTO-MATNR.
    AT_MANUTENCAO-QTD_REMOVIDA  = AT_MANUTENCAO-DESMEMBRAMENTO-QTD_RECEBIDA.

    AT_CATEGORIA = CONSTANTES-CATEGORIA-DESMEMBRAMENTO.

    CALL METHOD GET_SIMULADOR
      EXPORTING
        I_VBELN     = AT_MANUTENCAO-DESMEMBRAMENTO-VBELN
      IMPORTING
        E_SIMULADOR = AT_MANUTENCAO-DOC_SIMULACAO.

  ENDMETHOD.


  METHOD SET_MENSAGENS.
    FREE R_RETURN.
    APPEND INITIAL LINE TO R_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).
    <FS_RETURN>-TYPE = I_TYPE.
    <FS_RETURN>-MESSAGE = I_MENSAGEM.
  ENDMETHOD.


  METHOD TROCA_CENTRO_FORNECEDOR.

    FREE R_RETURN.

    DATA: LV_PR00  TYPE BAPIKBETR1,
          LV_WERKS TYPE WERKS_D.

    SELECT *
      FROM VBAP
      INTO TABLE @DATA(LT_VBAP)
      WHERE VBELN EQ @I_VBELN.

    IF LINES( LT_VBAP ) > 1.
      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>POPUP_CONFIRM
        EXPORTING
          I_MENSAGEM = TEXT-026
        IMPORTING
          IS_OK      = DATA(IS_OK).
      IF IS_OK IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    CHECK LT_VBAP IS NOT INITIAL.

    SELECT SINGLE *
      FROM VBAK
      INTO @DATA(LS_VBAK)
      WHERE VBELN EQ @I_VBELN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT *
      FROM V_KONV
    INTO TABLE @DATA(IT_KONV)
      WHERE KNUMV EQ @LS_VBAK-KNUMV
        AND KSCHL IN ( 'PR00', 'RB00' ).

    SELECT *
      FROM VBEP
      INTO TABLE @DATA(LT_VBEP)
       FOR ALL ENTRIES IN @LT_VBAP
        WHERE VBELN EQ @LT_VBAP-VBELN
          AND POSNR EQ @LT_VBAP-POSNR
          AND WMENG NE 0.

    AT_SALESDOCUMENT = I_VBELN.
    AT_ORDER_HEADER_INX-UPDATEFLAG = 'U'.

    AT_LOGIC_SWITCH =
    VALUE #(
              COND_HANDL = ABAP_TRUE
           ).

    LOOP AT LT_VBAP INTO DATA(LS_VBAP).

      FREE: AT_ORDER_ITEM_IN, AT_ORDER_ITEM_INX, AT_SCHEDULE_LINES, AT_SCHEDULE_LINESX, AT_CONDITIONS_IN, AT_CONDITIONS_INX, LV_WERKS.

      LV_WERKS = COND #( WHEN I_WERKS IS INITIAL THEN LS_VBAP-WERKS ELSE I_WERKS ).

      APPEND INITIAL LINE TO AT_ORDER_ITEM_IN ASSIGNING FIELD-SYMBOL(<FS_ORDER_ITEM_IN>).
      <FS_ORDER_ITEM_IN>-ITM_NUMBER = LS_VBAP-POSNR.
      <FS_ORDER_ITEM_IN>-PLANT      = LV_WERKS.
      <FS_ORDER_ITEM_IN>-PRICE_DATE = SY-DATUM.

      APPEND INITIAL LINE TO AT_ORDER_ITEM_INX ASSIGNING FIELD-SYMBOL(<FS_ORDER_ITEM_INX>).
      <FS_ORDER_ITEM_INX>-UPDATEFLAG = 'U'.
      <FS_ORDER_ITEM_INX>-ITM_NUMBER = LS_VBAP-POSNR.

      IF <FS_ORDER_ITEM_IN>-PLANT IS NOT INITIAL.
        <FS_ORDER_ITEM_INX>-PLANT      = ABAP_TRUE.
      ENDIF.

      <FS_ORDER_ITEM_INX>-PRICE_DATE = ABAP_TRUE.

      READ TABLE IT_KONV INTO DATA(LS_KONV_PR00) WITH KEY KPOSN = LS_VBAP-POSNR KSCHL = 'PR00'.
      IF SY-SUBRC IS INITIAL.

        CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
          EXPORTING
            I_DIRECAO     = 'O'
            I_VBELN       = LS_VBAP-VBELN
            I_POSNR       = LS_VBAP-POSNR
          RECEIVING
            I_COEFICIENTE = DATA(COEFICIENTE_O).

        CALL METHOD ZCL_SOLICITACAO_OV=>GET_IMPOSTO_V2
          EXPORTING
            I_DIRECAO     = 'D'
            I_CLIENTE     = LS_VBAK-KUNNR
            I_FORNECEDOR  = CONV #( |{ LV_WERKS ALPHA = IN }| )
            I_MATERIAL    = LS_VBAP-MATNR
            I_TIPO_ORDEM  = LS_VBAK-AUART
            I_WERKS       = LV_WERKS
          RECEIVING
            I_COEFICIENTE = DATA(COEFICIENTE_D).

        IF  COEFICIENTE_O NE COEFICIENTE_D.
          IF COEFICIENTE_D IS NOT INITIAL.
            LV_PR00 = LS_KONV_PR00-KBETR * COEFICIENTE_D.
          ENDIF.
        ENDIF.

        IF LV_PR00 IS NOT INITIAL.

          APPEND INITIAL LINE TO AT_CONDITIONS_IN ASSIGNING FIELD-SYMBOL(<FS_CONDITIONS_IN>).
          <FS_CONDITIONS_IN>-ITM_NUMBER = LS_VBAP-POSNR.
          <FS_CONDITIONS_IN>-COND_TYPE  = 'PR00'.
          <FS_CONDITIONS_IN>-COND_VALUE = LV_PR00.
*          <FS_CONDITIONS_IN>-COND_UNIT  = SWITCH #( LS_KONV_PR00-KMEIN WHEN 'TO' THEN 'KG' ELSE LS_KONV_PR00-KMEIN ).

          <FS_CONDITIONS_IN>-COND_ST_NO = LS_KONV_PR00-STUNR.
          <FS_CONDITIONS_IN>-COND_COUNT = LS_KONV_PR00-ZAEHK.
          <FS_CONDITIONS_IN>-CURRENCY   = LS_KONV_PR00-WAERS.

          APPEND INITIAL LINE TO AT_CONDITIONS_INX ASSIGNING FIELD-SYMBOL(<FS_CONDITIONS_INX>).
          <FS_CONDITIONS_INX>-ITM_NUMBER = LS_VBAP-POSNR.
          <FS_CONDITIONS_INX>-COND_TYPE  = 'PR00'.
          <FS_CONDITIONS_INX>-COND_VALUE = ABAP_TRUE.
*          <FS_CONDITIONS_INX>-COND_UNIT  = ABAP_TRUE.
          <FS_CONDITIONS_INX>-UPDATEFLAG = 'U'.

          <FS_CONDITIONS_INX>-COND_ST_NO = LS_KONV_PR00-STUNR.
          <FS_CONDITIONS_INX>-COND_COUNT = LS_KONV_PR00-ZAEHK.
          <FS_CONDITIONS_INX>-CURRENCY   = ABAP_TRUE.

          READ TABLE IT_KONV INTO DATA(LS_KONV_RB00) WITH KEY KPOSN = LS_VBAP-POSNR KSCHL = 'RB00'.
          IF SY-SUBRC IS INITIAL AND LS_KONV_RB00-KBETR NE 0.

            APPEND INITIAL LINE TO AT_CONDITIONS_IN ASSIGNING <FS_CONDITIONS_IN>.
            <FS_CONDITIONS_IN>-ITM_NUMBER = LS_VBAP-POSNR.
            <FS_CONDITIONS_IN>-COND_TYPE  = 'RB00'.
            <FS_CONDITIONS_IN>-COND_VALUE = 0.

            <FS_CONDITIONS_IN>-COND_ST_NO = LS_KONV_RB00-STUNR.
            <FS_CONDITIONS_IN>-COND_COUNT = LS_KONV_RB00-ZAEHK.

            APPEND INITIAL LINE TO AT_CONDITIONS_INX ASSIGNING <FS_CONDITIONS_INX>.
            <FS_CONDITIONS_INX>-ITM_NUMBER = LS_VBAP-POSNR.
            <FS_CONDITIONS_INX>-COND_TYPE  = 'RB00'.
            <FS_CONDITIONS_INX>-COND_VALUE = ABAP_TRUE.

            <FS_CONDITIONS_INX>-UPDATEFLAG = 'U'.

            <FS_CONDITIONS_INX>-COND_ST_NO = LS_KONV_RB00-STUNR.
            <FS_CONDITIONS_INX>-COND_COUNT = LS_KONV_RB00-ZAEHK.

          ENDIF.

        ENDIF.
      ENDIF.

      CALL METHOD CALL_ORDER_CHANGE
        RECEIVING
          R_RETURN = DATA(E_RETURN).

    ENDLOOP.

    APPEND INITIAL LINE TO R_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).
    <FS_RETURN>-TYPE    = CONSTANTES-TYPE-SUCESS.
    <FS_RETURN>-MESSAGE = |************ CHANGE CENTRO FORNECEDOR ************ |.

    APPEND LINES OF E_RETURN TO R_RETURN.

    CALL METHOD GET_SIMULADOR
      EXPORTING
        I_VBELN     = LS_VBAP-VBELN
      IMPORTING
        E_SIMULADOR = DATA(LV_DOC_SIMULACAO).

    CALL METHOD REPROCESSA_DESCONTO_ABS
      EXPORTING
        I_SIMULADOR = LV_DOC_SIMULACAO
        I_VBELN     = LS_VBAP-VBELN
        I_MATNR     = LS_VBAP-MATNR
      IMPORTING
        R_RETURN    = E_RETURN.

    APPEND LINES OF E_RETURN TO R_RETURN.

  ENDMETHOD.


  METHOD VERIFICAR_ITINERARIO_82.

    IF IS_BACKGROUND IS NOT INITIAL.
      AT_BACKGROUND = IS_BACKGROUND.
    ENDIF.

    IF AT_BACKGROUND IS INITIAL.
      IS_OK = ABAP_TRUE.
      RETURN.
    ENDIF.

    IS_OK = ABAP_FALSE.

    SELECT SINGLE LZONE
      FROM ZSDT0132
      INTO @DATA(LV_LZONE)
      WHERE NR_ROT EQ @I_ROTA_LR
        AND STATUS EQ @CONSTANTES-STATUS-ATIVO.

    SELECT SINGLE LZONE
      FROM ZSDT0132
      INTO @DATA(LV_AZONE)
      WHERE NR_ROT EQ @I_ROTA_PC
        AND STATUS EQ @CONSTANTES-STATUS-ATIVO.

    CHECK LV_AZONE IS NOT INITIAL.
    CHECK LV_LZONE IS NOT INITIAL.

    SELECT SINGLE ROUTE
      FROM TROLZ
    INTO E_ROUTE
    WHERE AZONE EQ LV_AZONE
      AND LZONE EQ LV_LZONE.

    CHECK E_ROUTE IS NOT INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD POPUP_CONFIRM.

    IS_OK = ABAP_FALSE.

    DATA: LV_ANSWER TYPE C.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION         = I_MENSAGEM
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = LV_ANSWER.

    CHECK LV_ANSWER EQ 1.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CANCEL_APROVADORES_EMBARQUE.

    CHECK I_VBELV IS NOT INITIAL.

    SELECT *
     FROM ZSDT0116
     INTO TABLE @DATA(LT_ZSDT0116)
     WHERE VBELN   EQ @I_VBELV
       AND STATUS  EQ @ABAP_FALSE
       AND STATUS_WORKFLOW IN ( ' ', 'A' ).

    LOOP AT LT_ZSDT0116 INTO DATA(LS_ZSDT0116).

      SELECT SINGLE *
        FROM ZSDT0116
        INTO @DATA(LS_ZSDT0116_REF)
        WHERE SEQ    EQ @LS_ZSDT0116-SEQ_REF
          AND VBELN  EQ @LS_ZSDT0116-VBELN_REF
          AND STATUS EQ @ABAP_FALSE
          AND STATUS_WORKFLOW IN ( ' ', 'A' ).

      IF SY-SUBRC IS INITIAL.

        LS_ZSDT0116_REF-VLR_LIBERADO_MOEDA += LS_ZSDT0116-VLR_LIBERADO_MOEDA.

        IF LS_ZSDT0116_REF-WAERK EQ 'USD'.
          LS_ZSDT0116_REF-VLR_LIBERADO = LS_ZSDT0116_REF-VLR_LIBERADO_MOEDA.
        ELSE.
          LS_ZSDT0116_REF-VLR_LIBERADO = LS_ZSDT0116_REF-VLR_LIBERADO_MOEDA / LS_ZSDT0116_REF-KURSF.
        ENDIF.

        UPDATE ZSDT0116 SET STATUS             = ABAP_FALSE
                            VLR_LIBERADO_MOEDA = LS_ZSDT0116_REF-VLR_LIBERADO_MOEDA
                            VLR_LIBERADO       = LS_ZSDT0116_REF-VLR_LIBERADO
          WHERE SEQ   EQ LS_ZSDT0116_REF-SEQ
            AND VBELN EQ LS_ZSDT0116_REF-VBELN.

      ENDIF.

      UPDATE ZSDT0116 SET STATUS             = ABAP_TRUE
                          VLR_LIBERADO_MOEDA = 0
                          VLR_LIBERADO       = 0
        WHERE SEQ EQ LS_ZSDT0116-SEQ
          AND VBELN EQ LS_ZSDT0116-VBELN.

      COMMIT WORK.

    ENDLOOP.

  ENDMETHOD.


  method chk_divergencia_ov_simulador.

    data soma type netwr.
    data vlr type dzwert.
    data it_vbap type table of vbap.

    clear: e_ordem_venda, e_simulador, e_diferenca_simulador, e_diferenca, soma, vlr.

    select single vlrtot
      from zsdt0040
      into @data(vl_total)
        where doc_simulacao eq @i_simulador.

    check sy-subrc is initial.

    select *
      from zsdt0090
      into table @data(it_90)
        where doc_simulacao eq @i_simulador
          and estorno eq @abap_false.

    select a~*
        from vbap as a
        inner join vbep as e on e~vbeln eq a~vbeln
                            and e~posnr eq a~posnr
        into table @it_vbap
          where e~lifsp ne '12'
          and a~vbeln in ( select vbeln from zsdt0041 where doc_simulacao = @i_simulador ).

    if it_90 is not initial.
      select a~*
        from vbap as a
        inner join vbep as e on e~vbeln eq a~vbeln
                            and e~posnr eq a~posnr
        appending table @it_vbap
        for all entries in @it_90
          where ( a~vbeln eq @it_90-vbelv
               or a~vbeln eq @it_90-vbeln
               or a~vbeln eq @i_vbeln     )
              and e~lifsp ne '12'.
    endif.

    check it_vbap is not initial.

    sort it_vbap by vbeln posnr.
    delete adjacent duplicates from it_vbap comparing vbeln posnr.

    select *
      from zsdt0090
      into table @data(t_90)
*      FOR ALL ENTRIES IN @IT_VBAP
        where doc_simulacao eq @i_simulador
        and categoria eq 'O'
        and flag eq @abap_false
        and estorno eq @abap_false.

    select single knumv
      from vbak
      into @data(vl_knumv)
      where vbeln eq @i_vbeln.

    select single posnr
    from vbap
    into @data(vl_posnr)
    where vbeln eq @i_vbeln
      and matnr eq @i_matnr.

    select single kbetr
      from v_konv
      into @data(vl_kbetr)
      where knumv eq @vl_knumv
        and kposn eq @vl_posnr
        and kschl eq 'RB00'.

    data(vl_netwr) =  reduce netwr( init x type netwr for w_vbap in it_vbap
                             next x = x + w_vbap-netwr
                         ).
    data(vl_mwsbp) =  reduce mwsbp( init y type mwsbp for w_vbap in it_vbap
                             next y = y + w_vbap-mwsbp
                         ).
    data(vl_desc)  =  reduce kbetr( init z type kbetr for w_90 in t_90
                             next z = z + w_90-desc_absoluto
                         ).
*   "// get nas auterações de quantidades, encerramentos e redistribuições do SIMULADOR
*   "// Auteração de Quantidade
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'A'
      importing
        e_vlr       = data(vlr_qtd).

*   "// Encerramento
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'E'
      importing
        e_vlr       = data(vlr_enc).

*   "// Redistribuição
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'R'
      importing
        e_vlr       = data(vlr_red).

**   "// Devolução
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'Y'
      importing
        e_vlr       = data(vlr_dev).

**   "// Retorno da Devolução
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'K'
      importing
        e_vlr       = data(vlr_rde).

*   "// Complemento
    call method get_vlr_aditivo
      exporting
        i_simulador = i_simulador
        i_categoria = 'W'
      importing
        e_vlr       = data(vlr_com).

    add vlr_qtd to vlr. " Alteração de Quantidade
    add vlr_enc to vlr. " Encerramento
    add vlr_red to vlr. " Redistribuição
    add vlr_dev to vlr. " Devolução
    add vlr_com to vlr. " Complemento
    add vlr_rde to vlr. " Retorno da Devolução

    add vl_netwr to soma.
    add vl_mwsbp to soma.

    subtract vlr from soma.

    add vl_desc to vl_total.

    check vl_total ne soma.

    e_diferenca = vl_total - soma.

    e_simulador = vl_total.
    e_ordem_venda = soma.
    e_diferenca_simulador = vl_total - soma.

    add vl_kbetr to e_diferenca.

    call method zcl_solicitacao_ov=>get_imposto_v2
      exporting
        i_direcao     = 'O'
        i_vbeln       = i_vbeln
        i_posnr       = vl_posnr
      receiving
        i_coeficiente = data(coeficiente_o).

    e_diferenca *= coeficiente_o.

  endmethod.


  METHOD CHK_ID_PROPRIEDADE.

    IS_OK = ABAP_TRUE.

    CHECK I_VBELN IS NOT INITIAL.

    SELECT SINGLE COUNT(*)
      FROM VBAP
      WHERE VBELN EQ I_VBELN
      AND SPART EQ '03'.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE *
      FROM ZSDT0132
      INTO @DATA(LS_0132)
      WHERE NR_ROT EQ @I_ROTA
        AND STATUS EQ @CONSTANTES-STATUS-ATIVO.

    CHECK LS_0132-UF EQ CONSTANTES-UF-MT.
    CHECK LS_0132-ID_PROPRIEDADE IS INITIAL.

* Caso encontre o Cliente como Revenda não é Obrigatorio o ID da Propriedade
    SELECT SINGLE *
      FROM ZSDT0216
        INTO @DATA(LS_0216)
       WHERE KUNNR EQ @LS_0132-KUNNR.

    IF LS_0216-REVENDA EQ 'S'.
      RETURN.
    ENDIF.

* Caso chegue ate o Final é Obrigatoio o Preenchimento do ID do Proprietario
* IS_OK igual a Branco
    IS_OK = ABAP_FALSE.

  ENDMETHOD.


  METHOD CHK_LATITUDE_LONGITUDE.

    CLEAR E_MSG.

    DATA: LV_LATITUDE  TYPE JBFBETA,
          LV_LONGITUDE TYPE JBFBETA,
          CNT          TYPE I.

    IF I_LATITUDE IS INITIAL.
      E_MSG = |Latitude não informada|.
      RETURN.
    ENDIF.

    IF I_LONGITUDE IS INITIAL.
      E_MSG = |Longitude não informada|.
      RETURN.
    ENDIF.

    TRY.
        LV_LATITUDE = I_LATITUDE.
      CATCH CX_SY_CONVERSION_NO_NUMBER INTO DATA(VL_ERROR).
        DATA(LV_MSG) = VL_ERROR->GET_TEXT( ).
        E_MSG = |{ E_MSG } Latitude { LV_MSG }|.
        RETURN.
    ENDTRY.

* Latitude deve estar entre -90 e +90 graus.
    IF NOT LV_LATITUDE BETWEEN -90 AND 90.
      E_MSG = |{ E_MSG } Latitude deve estar entre -90 e +90 graus.|.
      RETURN.
    ENDIF.

    IF LV_LATITUDE < 0.
      IF I_LATITUDE(1) NE '-'.
        E_MSG = |{ E_MSG } Na Latitude o Sinal "-" Deve ser Incluido no Inicio do Codigo.|.
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
        LV_LONGITUDE = I_LONGITUDE.
      CATCH CX_SY_CONVERSION_NO_NUMBER INTO VL_ERROR.
        LV_MSG = CONV #( VL_ERROR->GET_TEXT( ) ).
        E_MSG = |{ E_MSG } Longitude { LV_MSG }|.
        RETURN.
    ENDTRY.

* Latitude deve estar entre -90 e +90 graus.
    IF NOT LV_LONGITUDE BETWEEN -180 AND 180.
      E_MSG = |{ E_MSG } E Longitude deve estar entre -180 e +180 graus.|.
      RETURN.
    ENDIF.

    IF LV_LONGITUDE < 0.
      IF I_LONGITUDE(1) NE '-'.
        E_MSG = |{ E_MSG } Na Longitude o Sinal "-" Deve ser Incluido no Inicio do Codigo.|.
        RETURN.
      ENDIF.
    ENDIF.

    IF E_MSG IS NOT INITIAL.
      IF I_ORIGEM IS NOT INITIAL.
        E_MSG = |Origem: { E_MSG }|.
      ELSE.
        E_MSG = |Destino: { E_MSG }|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method fill_delete_ordem.

    call method zcl_manutencao_insumos=>clear_atributos.

    check i_vbeln is not initial.

    free r_return.

    select count(*)
      from vbak
      where vbeln eq i_vbeln.

    check sy-subrc is initial.

    at_salesdocument = i_vbeln.

    at_order_header_inx-updateflag = 'D'.

    call method call_order_change
      receiving
        r_return = data(e_return).

    if not line_exists( e_return[ type = 'E' ] ).
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    endif.

    append initial line to r_return assigning field-symbol(<fs_return>).
    <fs_return>-type    = constantes-type-sucess.
    <fs_return>-message = |************ ORDEM DELETADA ************|.

    append lines of e_return to r_return.

    if at_aditivos-doc_simulacao is not initial and
       at_aditivos-sequencia is not initial.

      update zsdt0090 set estorno = abap_true
      where sequencia eq at_aditivos-sequencia
        and doc_simulacao eq at_aditivos-doc_simulacao
        and vbeln eq i_vbeln.

    endif.

    call method cancel_aprovadores_embarque
      exporting
        i_vbelv = i_vbeln.

  endmethod.


  method get_quantidade_distribuida.

    data: lv_preco            type netwr_ap,
          lv_netwr            type netwr_ap,
          lv_valor_solicitado type netwr_ap,
          lv_kwmeng           type kwmeng.

    clear e_valor_distribuida.

    check i_vbeln is not initial.
    check sy-tcode eq constantes-tcode-zsdt0087.

    select *
      from zsdt0082
      into table @data(lt_zsdt0082)
    where vbeln eq @i_vbeln
      and status eq 1.

    check lt_zsdt0082 is not initial.

    select *
      from vbap
      into table @data(lt_vbap)
      for all entries in @lt_zsdt0082
      where vbeln eq @lt_zsdt0082-vbeln
        and posnr eq @lt_zsdt0082-posnr.

    check lt_vbap is not initial.

    loop at lt_zsdt0082 into data(ls_zsdt0082).

      read table lt_vbap into data(ls_vbap) with key vbeln = ls_zsdt0082-vbeln posnr = ls_zsdt0082-posnr.
      if sy-subrc is initial.

        add ls_vbap-netwr  to lv_netwr.
        add ls_vbap-mwsbp  to lv_netwr.
        add ls_vbap-kwmeng to lv_kwmeng.

        try.
            lv_preco = lv_netwr / lv_kwmeng.
          catch cx_sy_zerodivide.
            lv_preco = 0.
        endtry.

        lv_valor_solicitado = ls_zsdt0082-qte_sol * lv_preco.

        add lv_valor_solicitado to e_valor_distribuida.

      endif.

      clear: lv_netwr, lv_kwmeng, lv_preco, lv_valor_solicitado.

    endloop.

  endmethod.


  method set_desconto_abs_ov.

    data: soma         type netwr,
          vlr          type dzwert,
          vl_total     type kbetr,
          lv_diferenca type p decimals 5,
          lv_preco     type p decimals 14.

    free r_return.

    select single knumv
      from vbak
      into @data(vl_knumv)
      where vbeln eq @i_vbeln.

    select *
      from vbap
      into table @data(it_vbap)
      where vbeln eq @i_vbeln.

    if i_posnr is not initial.
      delete it_vbap where posnr ne i_posnr.
    endif.

    check it_vbap is not initial.

    read table it_vbap into data(ls_vbap) index 1.

    select *
  from zsdt0090
  into table @data(t_90)
    where vbelv eq @ls_vbap-vbeln
      and posnv eq @ls_vbap-posnr
      and categoria eq 'O'
      and flag eq @abap_false
      and estorno eq @abap_false.

    select *
      from v_konv
      into table @data(lt_konv)
      where knumv eq @vl_knumv
        and kposn eq @ls_vbap-posnr
        and kschl in ( 'RB00', 'ICMI', 'PR00' ).

    read table lt_konv into data(ls_konv_rb00) with key kschl = 'RB00'.
    read table lt_konv into data(ls_konv_icmi) with key kschl = 'ICMI'.
    read table lt_konv into data(ls_konv_pr00) with key kschl = 'PR00'.


    data(vl_netwr) =  reduce netwr( init x type netwr for w_vbap in it_vbap
                             next x = x + w_vbap-netwr
                         ).
    data(vl_mwsbp) =  reduce mwsbp( init y type mwsbp for w_vbap in it_vbap
                             next y = y + w_vbap-mwsbp
                         ).
    data(vl_desc)  =  reduce kbetr( init z type kbetr for w_90 in t_90
                             next z = z + w_90-desc_absoluto
                         ).

    call method zcl_solicitacao_ov=>get_imposto_v2
      exporting
        i_direcao     = 'O'
        i_vbeln       = ls_vbap-vbeln
        i_posnr       = ls_vbap-posnr
      receiving
        i_coeficiente = data(coeficiente_o). "// 0,960004

    add vl_netwr to soma.
    add vl_mwsbp to soma.

*    IF SY-UNAME EQ 'WBARBOSA'.
*       555,5601851658951 = 533,34             / 0,960004
    data(lv_preco_v)  = ls_konv_pr00-kbetr / coeficiente_o.

    if ls_konv_pr00-kmein ne ls_vbap-vrkme.
      if ls_konv_pr00-kmein eq 'TO'.
*       555,5601851658951   ÷   1000
        lv_preco_v         /=   1000.
      endif.
    endif.
*       111,112037033179  = 0,5555601851658951  ×   200
    vl_total          = lv_preco_v          *   ls_vbap-kwmeng.
*    LS_KONV_PR00-KBETR *= COEFICIENTE_O.
*    ELSE.
*      VL_TOTAL = LS_VBAP-KWMENG * LS_KONV_ICMI-KBETR.
*    ENDIF.

    add vl_desc to vl_total.

    check vl_total ne soma.

    lv_diferenca = vl_total - soma.

    lv_diferenca *= coeficiente_o.

    check lv_diferenca is not initial.

    call method set_desconto_abs
      exporting
        i_vbeln        = ls_vbap-vbeln
        i_posnr        = ls_vbap-posnr
        i_desconto_abs = conv #( lv_diferenca )
      importing
        r_return       = r_return.

  endmethod.


  METHOD CHK_CRIACAO_OV.

    CLEAR IS_OK.

    SELECT COUNT(*)
      FROM VBAK
      WHERE VBELN EQ I_VBELN.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHK_DESC_APLICADO_SIMULADOR.

    FREE IS_OK.

    SELECT COUNT(*)
      FROM ZSDT0090
      WHERE DOC_SIMULACAO EQ I_SIMULADOR
        AND CATEGORIA EQ CONSTANTES-CATEGORIA-DESCONTO_ABSOLUTO
        AND ESTORNO EQ ABAP_FALSE
        AND FLAG EQ ABAP_FALSE.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHK_FATURAMENTO_SIMULADOR.

    DATA: R_VBELN TYPE RANGE OF VBELN_VA.

    IS_OK = ABAP_FALSE.
    FREE R_VBELN.

    CHECK I_SIMULADOR IS NOT INITIAL.

    SELECT 'I'   AS SIGN,
           'EQ'  AS OPTION,
           VBELN AS LOW
      FROM ZSDT0041
    INTO TABLE @R_VBELN
     WHERE DOC_SIMULACAO EQ @I_SIMULADOR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT 'I'   AS SIGN,
           'EQ'  AS OPTION,
           VBELN AS LOW
      FROM ZSDT0090
    APPENDING TABLE @R_VBELN
      WHERE DOC_SIMULACAO EQ @I_SIMULADOR
        AND ESTORNO EQ @ABAP_FALSE.

    SELECT 'I'   AS SIGN,
           'EQ'  AS OPTION,
           VBELN AS LOW
      FROM ZSDT0090
    APPENDING TABLE @R_VBELN
      WHERE DOC_SIMULACAO EQ @I_SIMULADOR
        AND ESTORNO EQ @ABAP_FALSE.

    DELETE R_VBELN WHERE LOW IS INITIAL.
    DELETE R_VBELN WHERE LOW EQ ''.

    SORT R_VBELN.
    DELETE ADJACENT DUPLICATES FROM R_VBELN COMPARING ALL FIELDS.

    CHECK R_VBELN IS NOT INITIAL.

    SELECT COUNT(*)
      FROM VBFA
      WHERE VBELV   IN R_VBELN
        AND VBTYP_N EQ 'M'
        AND VBTYP_V EQ 'C'.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD CHK_OV_VALOR_MAIOR.

    SELECT SINGLE *
        FROM VBAP
     INTO @DATA(LS_VBAP_OLD)
     WHERE VBELN EQ @I_VBELN_OLD
       AND MATNR EQ @I_MATNR_OLD.

    CALL METHOD CHK_DESCONTO_ABS_FATURADO
      EXPORTING
        I_VBELN = LS_VBAP_OLD-VBELN
        I_MATNR = LS_VBAP_OLD-MATNR
      IMPORTING
        IS_FAT  = DATA(IS_FAT).

    IF IS_FAT IS NOT INITIAL.
      E_VBELN = I_VBELN_NEW.
      E_MATNR = I_MATNR_NEW.
      RETURN.
    ENDIF.

    SELECT SINGLE *
       FROM VBAP
     INTO @DATA(LS_VBAP_NEW)
     WHERE VBELN EQ @I_VBELN_NEW
       AND MATNR EQ @I_MATNR_NEW.

    DATA(LV_VALOR_NEW) = LS_VBAP_NEW-NETWR + LS_VBAP_NEW-MWSBP.
    DATA(LV_VALOR_OLD) = LS_VBAP_OLD-NETWR + LS_VBAP_OLD-MWSBP.

    IF LV_VALOR_NEW > LV_VALOR_OLD.
      E_VBELN = I_VBELN_NEW.
      E_MATNR = I_MATNR_NEW.
    ELSE.
      E_VBELN = I_VBELN_OLD.
      E_MATNR = I_MATNR_OLD.
    ENDIF.

  ENDMETHOD.


  METHOD GET_OV_CH_REFERENCIA.

    CHECK I_BSTKD_E IS NOT INITIAL.

    CLEAR: E_VBELN, E_POSNR.

    SELECT SINGLE VBELN, POSNR
      FROM VBKD
      INTO ( @E_VBELN, @E_POSNR )
      WHERE BSTKD_E EQ @I_BSTKD_E.

  ENDMETHOD.


  METHOD GET_PC_VIA_ROTEIRO.

    CLEAR AT_PONTO_COLETA.

    CHECK I_ROTA_PC IS NOT INITIAL.

    SELECT SINGLE LIFNR
      FROM ZSDT0132
    INTO E_PC
      WHERE NR_ROT EQ I_ROTA_PC
        AND STATUS EQ CONSTANTES-STATUS-ATIVO.

  ENDMETHOD.


  METHOD GET_PRECO_LIQUIDO_41.

    FREE E_PRECO_LIQUIDO.

    SELECT SINGLE ZWERT_LIQDO
      FROM ZSDT0041
    INTO @E_PRECO_LIQUIDO
    WHERE DOC_SIMULACAO EQ @I_SIMULADOR
      AND VBELN EQ @I_VBELN
      AND MATNR EQ @I_MATNR.

  ENDMETHOD.


  METHOD GET_ROTA.

    FREE E_ROTEIRO.

    IF I_PC IS INITIAL AND I_LR IS INITIAL.
      RETURN.
    ENDIF.

    IF I_PC IS NOT INITIAL.
      CALL METHOD GET_ZONA
        EXPORTING
          I_PC   = I_PC
        IMPORTING
          E_ZONA = DATA(E_ZONA).
    ENDIF.

    IF I_LR IS NOT INITIAL.
      CALL METHOD GET_ZONA
        EXPORTING
          I_LR   = I_LR
        IMPORTING
          E_ZONA = E_ZONA.
    ENDIF.

    SELECT SINGLE NR_ROT
      FROM ZSDT0132
      INTO E_ROTEIRO
      WHERE LZONE  EQ E_ZONA
        AND STATUS EQ CONSTANTES-STATUS-ATIVO.

  ENDMETHOD.


  method desmembramento_devolucao.

    at_desmembramento_devolucao = abap_true.

    at_categoria = constantes-categoria-retorno_devolucao.

    call method set_mensagens
      exporting
        i_type     = constantes-type-info
        i_mensagem = |{ text-022 }|
      importing
        r_return   = data(e_return).

    append lines of e_return to r_return.

    at_desconto = i_desc_abs.
    call method fill_dados_create
      importing
        e_vbak = data(ls_vbak)
        e_vbap = data(lt_vbap)
        e_vbkd = data(ls_vbkd)
        e_konv = data(ls_konv).

    call method fill_ordem_create
      changing
        i_vbak_new = ls_vbak
        i_vbap_new = lt_vbap
        i_vbkd_new = ls_vbkd
        i_konv_new = ls_konv
      receiving
        r_return   = e_return.

    append lines of e_return to r_return.

    if not line_exists( r_return[ type = cl_abap_aab_utilities=>category_error ] ).

      call method clear_atributos.

*      call method processa_desconto_abs
*        importing
*          r_return = e_return.

*      append lines of e_return to r_return.

      call method send_aprovadores_embarque
        exporting
          i_vbeln  = at_aditivos-vbeln
          i_posnn  = at_aditivos-posnn
          i_vbelv  = at_aditivos-vbelv
        importing
          r_return = e_return.

      append lines of e_return to r_return.

    else.

      call method zcl_manutencao_insumos=>fill_delete_ordem
        exporting
          i_vbeln  = at_manutencao-vbeln_new
        receiving
          r_return = e_return.

      append lines of e_return to r_return.

    endif.

    call method set_mensagens
      exporting
        i_type     = constantes-type-info
        i_mensagem = |{ text-023 }|
      importing
        r_return   = e_return.

    append lines of e_return to r_return.
    append initial line to r_return.

    at_desmembramento_devolucao = abap_false.

  endmethod.


  METHOD GET_TOLERANCIA_DESCONTO_ABS.

    DATA: LT_SETS TYPE TABLE OF RGSB4.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        SETNR      = 'ZSDT0087_TOLERANCIA_DIFE'
      TABLES
        SET_VALUES = LT_SETS.

    READ TABLE LT_SETS INTO DATA(LS_SETS) INDEX 1.

    CHECK SY-SUBRC IS INITIAL.

    E_TOLERANCIA = LS_SETS-TO.

  ENDMETHOD.


  method set_log_solicitacao.

    data: ls_zsdt0150  type zsdt0150,
          lv_field_new type fieldname,
          lv_field_old type fieldname,
          lv_seq       type n length 3,
          r_field      type range of fieldname.

    field-symbols: <fs_field_new> type any,
                   <fs_field_old> type any.

    unassign <fs_field_new>.
    unassign <fs_field_old>.

    select *
      from dd03l
      into table @data(lt_field)
      where tabname eq 'ZSDT0082'.

    lv_seq = 0.

    append value #( sign = 'I' option = 'EQ' low = 'STATUS' )  to r_field.
    append value #( sign = 'I' option = 'EQ' low = 'QTE_LIB' ) to r_field.
    append value #( sign = 'I' option = 'EQ' low = 'QTE_SOL' ) to r_field.

    loop at lt_field into data(ls_field)
      where fieldname in r_field.

      lv_field_new = |I_NEW-{ ls_field-fieldname }|.
      lv_field_old = |I_OLD-{ ls_field-fieldname }|.

      assign (lv_field_new) to <fs_field_new>.
      assign (lv_field_old) to <fs_field_old>.

      if <fs_field_new> is assigned.
        if <fs_field_new> ne <fs_field_old>.

          add 1 to lv_seq.
          clear ls_zsdt0150.

          ls_zsdt0150 =
             value #(
                      direcao      = i_direcao
                      nro_sol      = i_old-nro_sol
                      seq          = lv_seq
                      vbeln        = i_old-vbeln
                      posnr        = i_old-posnr
                      dt_registro  = sy-datum
                      hr_registro  = sy-uzeit
                      us_registro  = sy-uname
                      qtde_old     = i_old-qte_sol
                      qtde_new     = i_new-qte_sol
                      field        = ls_field-fieldname
                      value_old    = <fs_field_old>
                      value_new    = <fs_field_new>
             ).

          insert zsdt0150 from ls_zsdt0150.

        endif.
      endif.

    endloop.

    commit work.

  endmethod.


  method chk_cli_aprovadores_embarque.

    is_ok = abap_false.

    select single *
      from kna1
      into  @data(ls_cliente_origem)
      where kunnr eq @cliente_origem.

    check sy-subrc is initial.

    select single *
      from kna1
      into  @data(ls_cliente_destino)
      where kunnr eq @cliente_destino.

    check sy-subrc is initial.

    if ls_cliente_origem-stkzn is not initial.

      if ls_cliente_origem-stcd2 ne ls_cliente_destino-stcd2.
        return.
      endif.

    else.

      if ls_cliente_origem-stcd1(8) ne ls_cliente_destino-stcd1(8).
        return.
      endif.

    endif.

    is_ok = abap_true.

  endmethod.


  method chk_ov_distribuida.

    exist = abap_false.

    select single count(*)
      from zsdt0082
    where vbeln  eq @i_vbeln
      and posnr  eq @i_posnr
      and status in ( 2, 5 ).

    check sy-subrc is initial.

    exist = abap_true.

  endmethod.


  method chk_ov_solicitada.

    exist = abap_false.

    select count(*)
      from zsdt0082
    where vbeln eq @i_vbeln
      and posnr eq @i_posnr
      and status eq 1.

    check sy-subrc is initial.

    exist = abap_true.

  endmethod.


  method get_quantidade_devolvida.

    data: r_vbeln type range of vbeln_va.

    check i_vbeln is not initial.

    free e_valor_devolvido.

    select single vbelv
      from zsdt0090
      into @data(lv_vbelv)
      where vbelv eq @i_vbeln
      and categoria eq 'K'
      and estorno eq @abap_false.

    if sy-subrc is initial.

      select
        'EQ'  as option,
        'I'   as sign,
        vbeln as low
      into corresponding fields of table @r_vbeln
      from zsdt0090
      where vbelv eq @lv_vbelv
        and categoria eq 'Y'
        and estorno eq @abap_false.

    endif.

    select
      'EQ'  as option,
      'I'   as sign,
      vbeln as low
    into corresponding fields of table @r_vbeln
    from zsdt0090
    where vbelv eq @i_vbeln
      and categoria eq 'Y'
      and estorno eq @abap_false.

    check r_vbeln is not initial.

    select netwr, mwsbp
      from vbap
    into table @data(it_vbap)
    where vbeln in @r_vbeln.

    check it_vbap is not initial.

    loop at it_vbap into data(ls_vbap).
      add ls_vbap-netwr to e_valor_devolvido.
      add ls_vbap-mwsbp to e_valor_devolvido.
    endloop.

  endmethod.


  method set_estornar_solicitacao.

    clear e_msg.

    check 1 eq 2.

    call method chk_ov_solicitada
      exporting
        i_vbeln = i_vbeln
        i_posnr = i_posnr
      importing
        exist   = data(solicitada).

    check solicitada is not initial.

    call method chk_ov_distribuida
      exporting
        i_vbeln = i_vbeln
        i_posnr = i_posnr
      importing
        exist   = data(distribuida).

    if distribuida is not initial.
      e_msg = 'Ordem já possui Solicitação Distribuida!'.
      return.
    endif.

    select single *
      from zsdt0082
      into @data(ls_0082_old)
    where vbeln  eq @i_vbeln
      and posnr  eq @i_posnr
      and status eq 1.

    check sy-subrc is initial.

    data(ls_0082_new) = ls_0082_old.
    ls_0082_new-spart = 3.

    update zsdt0082 set status    = ls_0082_new-spart
                        dt_canc   = sy-datum
                        user_canc = sy-uname
          where nro_sol eq ls_0082_old-nro_sol
            and seq     eq ls_0082_old-seq
            and vbeln   eq ls_0082_old-vbeln
            and posnr   eq ls_0082_old-posnr.

    call method set_log_solicitacao
      exporting
        i_old     = ls_0082_old
        i_new     = ls_0082_new
        i_direcao = 'Estorno'.

  endmethod.


  method check_nova_ov_81.

    clear: e_nova_ov, e_agrupar.

    select single *
      from vbak
      into @data(ls_vbak)
      where vbeln eq @at_manutencao-troca-ordem_old-vbeln.

    check sy-subrc is initial.

    select single *
      from vbap
      into @data(ls_vbap)
      where vbeln eq @at_manutencao-troca-ordem_old-vbeln
        and matnr eq @at_manutencao-troca-ordem_old-matnr.

    select single *
      from vbkd
      into @data(ls_vbkd)
      where vbeln eq @at_manutencao-troca-ordem_old-vbeln.

    select single *
      from v_konv
      into @data(ls_konv)
      where knumv eq @ls_vbak-knumv
        and kposn eq @ls_vbap-posnr
        and kschl eq 'PR00'.

* "// Centro
    if at_manutencao-troca-ordem_new-werks ne at_manutencao-troca-ordem_old-werks.
      e_nova_ov = abap_true.
      return.
    endif.

* "// Setor atividade
    select single spart
      from mara
      into @data(lv_spart_old)
    where matnr eq @at_manutencao-troca-ordem_old-matnr.

    select single spart
      from mara
      into @data(lv_spart_new)
    where matnr eq @at_manutencao-troca-ordem_new-matnr.

    if lv_spart_old ne lv_spart_new.
      e_nova_ov = abap_true.
      return.
    endif.

* "// Depósito
    if at_manutencao-troca-ordem_new-lgort ne ls_vbap-lgort.
      e_nova_ov = abap_true.
      return.
    endif.

* "// Cliente
************************************
************************************


* "// Ponto Coleta
    if at_ponto_coleta is not initial.

      select count(*)
       from vbpa
     where vbeln eq @at_manutencao-troca-ordem_old-vbeln
       and lifnr eq @at_ponto_coleta
       and parvw eq @constantes-parvw-pc.

      if sy-subrc is not initial.
        e_nova_ov = abap_true.
        return.
      endif.

    endif.

* "// Local entrega
************************************
************************************


* "// Incoterms ( para defensivos )
    if lv_spart_old eq constantes-spart-_03.
      if at_manutencao-troca-ordem_new-inco1 ne at_manutencao-troca-ordem_old-inco1.
        e_nova_ov = abap_true.
        return.
      endif.
    endif.

* "// Material e Preço unitário
    if ls_konv-kbetr ne at_manutencao-troca-ordem_new-vlr_venda.
      e_nova_ov = abap_true.
      return.
    endif.

* "// Faturamento ( ov com imposto ) e Desconto Absoluto
    call method chk_desconto_abs_faturado
      exporting
        i_vbeln = ls_vbap-vbeln
        i_matnr = ls_vbap-matnr
      importing
        is_fat  = data(is_ok).

    if is_ok  is not initial.

      call method zcl_solicitacao_ov=>get_imposto_v2
        exporting
          i_direcao     = 'O'
          i_vbeln       = ls_vbap-vbeln
          i_posnr       = ls_vbap-posnr
        receiving
          i_coeficiente = data(coeficiente_o).

      if coeficiente_o ne 1.
        e_nova_ov = abap_true.
        return.
      endif.

    endif.

    if at_manutencao-troca-ordem_new-matkl eq at_manutencao-troca-ordem_old-matkl and
       at_manutencao-troca-ordem_new-werks eq at_manutencao-troca-ordem_old-werks and
       at_manutencao-troca-ordem_new-inco1 eq at_manutencao-troca-ordem_old-inco1.

      if at_manutencao-troca-ordem_new-spart eq constantes-spart-_02.

        case at_manutencao-troca-ordem_old-auart.
          when constantes-auart-zrfu or constantes-auart-zrem.
            return.
          when others.
            if at_manutencao-troca-ordem_new-mtart eq constantes-mtart-zhaw.
              at_nova_ov = abap_true.
              return.
            endif.

            select single count(*)
              from mara
            where matnr eq @at_manutencao-troca-ordem_old-matnr
              and mtart eq @at_manutencao-troca-ordem_new-mtart.

            if sy-subrc is initial.
              at_nova_ov = abap_true.
              return.
            endif.

        endcase.
      endif.

      select count(*)
        from vbap
      where vbeln eq at_manutencao-troca-ordem_old-vbeln
        and matnr eq at_manutencao-troca-ordem_new-matnr.

      if sy-subrc is initial.
        at_nova_ov = abap_true.
        return.
      endif.

    else.

      if ( at_manutencao-troca-ordem_new-matkl eq at_manutencao-troca-ordem_old-matkl ) and
         ( at_manutencao-troca-ordem_new-inco1 ne at_manutencao-troca-ordem_old-inco1 ) and
         ( at_manutencao-troca-ordem_new-spart eq constantes-spart-_02 ) and
         ( at_manutencao-troca-ordem_old-auart eq constantes-auart-zrfu or
           at_manutencao-troca-ordem_old-auart eq constantes-auart-zrem ).
        return.
      else.
        at_nova_ov = abap_true.
        return.
      endif.

    endif.

    if at_ponto_coleta is not initial.

      select count(*)
       from vbpa
     where vbeln eq @at_manutencao-troca-ordem_old-vbeln
       and lifnr eq @at_ponto_coleta
       and parvw eq @constantes-parvw-pc.

      if sy-subrc is not initial.
        at_nova_ov = abap_true.
        return.
      endif.

    endif.

  endmethod.
ENDCLASS.
