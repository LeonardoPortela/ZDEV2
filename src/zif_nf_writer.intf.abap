interface ZIF_NF_WRITER
  public .


  types:
*-IR147555 - 19.09.2023 - LF - inicio
    BEGIN OF ty_regula,
      operacao TYPE zfiwrt0008-operacao,
      rate     TYPE zfiwrt0010-rate.
  TYPES: END   OF ty_regula .
  types:
    ty_regula_t TYPE HASHED TABLE OF ty_regula WITH UNIQUE KEY operacao .

*-IR147555 - 19.09.2023 - LF - fim
  class-data AT_IF_NF_WRITER type ref to ZIF_NF_WRITER .
  data AT_CABECALHO type ZFIWRT0008 .
  data AT_FATURA type ZFIWRT0022 .
  data AT_ITENS type ZFIWRT0009_T .
  data AT_IMPOSTOS type ZFIWRT0010_T .
  data AT_CONTABIL type ZFIWRT0011_T .
  data AT_MOV_ESTOQUE type ZFIWRT0012_T .
  data AT_MENSAGENS type ZFIWRT0013_T .
  data AT_PARCEIROS type ZFIWRT0015_T .
  data AT_DOCS_REFERENCIADOS type ZFIWRT0020_T .
  data AT_CATEGORIA_NF type J_1BAA .
  data AT_LEIS_ESTADO type ZFIWRT0006 .
  data AT_SHIPFROM type REGIO .
  data AT_SHIPTO type REGIO .
  data AT_PARAMETRO_FISCAL type ZFIWRT0001 .
  data AT_PARAMETROS_IMPOSTOS type ZFIWRT0002_T .
  data AT_PARAMETROS_CONTABIL type ZFIWRT0003_T .
  data AT_PARAMETROS_MOV_ESTOQUE type ZFIWRT0004_T .
  data AT_PARAMETROS_MENSAGENS type ZFIWRT0005_T .
  data AT_PARAMETROS_LEIS_ESTADO type ZFIWRT0006_T .
  data AT_RATEIO type ZFIWRT0023_T .
  data AT_DADOS_TRANSP type ZFIWRT0019 .
  data AT_PARAMETROS_REGULATORIOS type TY_REGULA_T .

  class-methods GET_INSTANCE
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  class-methods CHECK_AUTH_DOC
    importing
      !I_SEQ_LCTO type ZFIWED006
      !I_CHECK_VALIDADE_DOCUMENTOS type CHAR01 default 'X'
    exporting
      value(E_ZFIWRT0008) type ZFIWRT0008
      value(E_ZFIWRT0009) type ZFIWRT0009_T
      value(E_ZFIWRT0020) type ZFIWRT0020_T
      value(E_ACTIVE) type J_1BNFE_ACTIVE
      value(E_DOC) type J_1BNFDOC
    returning
      value(R_DOC_AUTH) type CHAR01
    raising
      ZCX_NF_WRITER .
  class-methods ESTORNAR_DOCUMENTO
    importing
      !I_SEQ_LCTO type ZFIWED006
      !I_WAIT_ESTORNO type CHAR01 optional
    returning
      value(R_ESTORNADO) type CHAR01
    raising
      ZCX_NF_WRITER .
  methods NOVO_LANCAMENTO
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER .
  methods SET_CABECALHO
    importing
      !I_CABECALHO type ZFIWRT0008
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods ADD_ITEM
    importing
      !I_ITEM type ZFIWRT0009
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods VALIDAR_REGISTRO
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods GRAVAR_DOCUMENTO
    importing
      !I_NAO_VALIDA type CHAR01 optional
      !I_NAO_IMPOSTO type CHAR01 optional
      !I_NAO_PREPARA type CHAR01 optional
      !I_PROCESSAR_LCTO type CHAR01 optional
    exporting
      !E_SEQ_LCTO type ZFIWED006
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods ADD_PARCEIRO
    importing
      !I_PARCEIRO type ZFIWRT0015
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods MONTA_IMPOSTOS
    importing
      value(I_IMPOSTOS) type ZFIWRT0010_T optional
    exporting
      value(E_IMPOSTOS) type ZFIWRT0010_T
    changing
      !C_ITEM type ZFIWRT0009
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods PREPARA_LANCAMENTO
    importing
      value(I_IMPOSTOS) type ZFIWRT0010_T optional
      value(I_CONTABIL) type ZFIWRT0011_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods DETERMINA_FORA_DENTRO_ESTADO
    exporting
      !E_INDCOPER type CHAR01
      !E_TEXTO_FISCAL type CHAR50
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods MONTA_CONTABIL
    importing
      value(I_IMPOSTOS) type ZFIWRT0010_T optional
      value(I_CONTABIL) type ZFIWRT0011_T optional
    exporting
      value(E_CONTABIL) type ZFIWRT0011_T
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods ADD_DOC_REF
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods MONTA_MENSAGENS
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER .
  methods SET_RATEIO
    importing
      !I_RATEIO type ZFIWRT0023
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER .
  methods SET_DADOS_TRANSP
    importing
      !I_DADOS_TRANSP type ZFIWRT0019
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_PARAMETRO_CONTABIL
    importing
      value(I_PARAMETRO_CONTABIL) type ZFIWRT0003_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_PARAMETRO_FISCAL
    importing
      value(I_PARAMETRO_FISCAL) type ZFIWRT0002_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_MONTA_CONTABIL
    importing
      value(I_CONTABIL) type ZFIWRT0011_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_MONTA_FISCAL
    importing
      value(I_FISCAL) type ZFIWRT0010_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_MONTA_MOV_ESTOQUE
    importing
      value(I_MOV_ESTOQUE) type ZFIWRT0004_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_MONTA_MENSAGENS
    importing
      value(I_MENSAGENS) type ZFIWRT0005_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods GET_MONTA_CONTABIL
    exporting
      value(E_CONTABIL) type ZFIWRT0011_T
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER
    raising
      ZCX_NF_WRITER .
  methods SET_FATURA_ENERGIA
    importing
      !I_FATURA type ZFIWRT0022
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER .
  methods SET_PARAMETRO_REGULATORIO
    importing
      value(I_REGULA) type TY_REGULA_T optional
    returning
      value(R_IF_NF_WRITER) type ref to ZIF_NF_WRITER .
endinterface.
