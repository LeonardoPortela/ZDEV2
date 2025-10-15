interface ZIF_BOLETIM_PRODUCAO
  public .


  data AT_CABECALHO type ZSDT0246 .
  data AT_DADOS_PRODUCAO type ZSDT0247_T .
  data AT_DADOS_RENDIMENTO type ZSDT0248_T .
  class-data AT_IF_BOLETIM_PRODUCAO type ref to ZIF_BOLETIM_PRODUCAO .
  constants AT_ID_ORDEM_NF_DEVOLUCAO type ZDE_ORDEM_DOC_BOLETIM value 01 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_ENT_DEVOLUCAO type ZDE_ORDEM_DOC_BOLETIM value 02 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_IND type ZDE_ORDEM_DOC_BOLETIM value 03 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_ENT_IND type ZDE_ORDEM_DOC_BOLETIM value 04 ##NO_TEXT.
  constants AT_ID_ORDEM_DOC_PROD_01 type ZDE_ORDEM_DOC_BOLETIM value 05 ##NO_TEXT.
  constants AT_ID_ORDEM_DOC_PROD_02 type ZDE_ORDEM_DOC_BOLETIM value 06 ##NO_TEXT.
  constants AT_ID_ORDEM_DOC_PROD_03 type ZDE_ORDEM_DOC_BOLETIM value 07 ##NO_TEXT.
  constants AT_ID_ORDEM_DOC_PROD_04 type ZDE_ORDEM_DOC_BOLETIM value 08 ##NO_TEXT.
  constants AT_ID_ORDEM_DOC_PROD_05 type ZDE_ORDEM_DOC_BOLETIM value 09 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RFL_01 type ZDE_ORDEM_DOC_BOLETIM value 10 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RFL_02 type ZDE_ORDEM_DOC_BOLETIM value 11 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RFL_03 type ZDE_ORDEM_DOC_BOLETIM value 12 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RCO_01 type ZDE_ORDEM_DOC_BOLETIM value 13 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_ENT_RCO_01 type ZDE_ORDEM_DOC_BOLETIM value 14 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RCO_02 type ZDE_ORDEM_DOC_BOLETIM value 15 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_ENT_RCO_02 type ZDE_ORDEM_DOC_BOLETIM value 16 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_RCO_03 type ZDE_ORDEM_DOC_BOLETIM value 17 ##NO_TEXT.
  constants AT_ID_ORDEM_NF_ENT_RCO_03 type ZDE_ORDEM_DOC_BOLETIM value 18 ##NO_TEXT.
  class-data AT_DATE type SY-DATUM .
  class-data AT_TP_BOLETIM type ZTP_BOLETIM .
  class-data AT_DATE_DOC type SY-DATUM .
  class-data AT_DATE_LANC type SY-DATUM .
  class-data AT_OI_MAT_TERCEIRO type CHAR01 ..

  class-methods GET_INSTANCE
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods DESBLOQUEAR_REGISTROS
    importing
      !I_DOCNUM_ZSDT0251 type J_1BDOCNUM optional
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD optional
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO .
  class-methods BLOQUEAR_REGISTROS
    importing
      !I_DOCNUM_ZSDT0251 type J_1BDOCNUM optional
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD optional
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods SHOW_ERRO_GERAL
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods GET_SALDOS
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD
    exporting
      !E_SALDO_VINCULADO type MENGE_D
      !E_SALDO_VINCULAR type MENGE_D .
  class-methods GET_MATERIAL_BOLETIM
    importing
      !I_TP_PRODUTO type ZDE_TP_PRODUTO_PRODUCAO
    returning
      value(R_ZSDT0250) type ZSDT0250
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods GERA_ERRO_GERAL
    importing
      !I_MSGV1 type SYST_MSGV
      !I_MSGV2 type SYST_MSGV
      !I_MSGV3 type SYST_MSGV
      !I_MSGV4 type SYST_MSGV
      !I_MSGID type SYMSGID
      !I_MSGNO type SYMSGNO
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods APROVAR_LANCAMENTO
    importing
      !I_ID_BOLETIM_PRODUCAO type ZDE_ID_BOLETIM_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods CHECK_APROVACAO
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods GET_VERSAO
    importing
      !I_MATNR type MATNR
      !I_WERKS type WERKS_D
    returning
      value(R_VERID) type VERID
    raising
      ZCX_BOLETIM_PRODUCAO .
  class-methods CHECK_DESAPROVACAO
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD .
  methods GET_STATUS_BOLETIM
    importing
      !I_ID_BOLETIM_PRODUCAO type ZDE_ID_BOLETIM_PROD
    exporting
      !E_DS_STATUS type CHAR20
    returning
      value(R_STATUS) type ZDE_STATUS_BOL_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_STATUS_BOLETIM
    importing
      !I_ID_BOLETIM_PRODUCAO type ZDE_ID_BOLETIM_PROD
    exporting
      value(E_CHANGE_STATUS) type CHAR01
    returning
      value(R_STATUS) type ZDE_STATUS_BOL_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_DOCUMENTOS_VINCULADOS
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_DOCUMENTOS_GERADOS
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_DOC_MATERIAL_VALIDO
    importing
      !I_MBLNR type MBLNR
      !I_WITH_RAISE type CHAR01 optional
      !I_DS_DOC type CHAR30 optional
    returning
      value(R_VALIDO) type CHAR01
    raising
      ZCX_BOLETIM_PRODUCAO .
*---> 28/06/2023 - Migração S4 - JS
*     !I_ID_DOC_PROD type CHAR02 optional
*<--- 28/06/2023 - Migração S4 - JS
  methods VALIDAR_MATERIAIS_DOC_PROD
    importing
      !I_MBLNR type MBLNR optional
      !I_ZSDT0252 type ZSDT0252 optional
      !I_ID_DOC_PROD type ZCHAR02 optional
    returning
      value(R_VALIDADO) type CHAR01
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods ESTORNO_DOC_PRODUCAO
    importing
      !I_MBLNR type MBLNR
    returning
      value(R_MBLNR_ESTORNO) type MBLNR
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_PERMISSAO_MODIFICACAO
    importing
      !I_SEQ_LCTO_ZNFW type ZFIWED006 optional
      !I_MBLNR type MBLNR optional
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD optional
      !I_DOCNUM type J_1BDOCNUM optional
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GET_DOC_BOLETIM_VALIDO
    importing
      !I_KEY_DOCS type ZDE_KEY_DOCS_BOLETIM
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
      !E_MBLNR type MBLNR
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GET_PERC_RENDIMENTO
    importing
      !I_ZSDT0247 type ZSDT0247_T
    changing
      !C_ZSDT0248 type ZSDT0248_T
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GET_TP_PRODUTOS_PRODUCAO
    importing
      !E_TP_PRODUTO type ZDE_TP_PRODUTO_PRODUCAO optional
    returning
      value(R_TP_PRODUTOS) type ZSDT0250_T
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GET_TP_PRODUTOS_RENDIMENTO
    returning
      value(R_TP_PRODUTOS) type ZSDT0250_T
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods NOVO_REGISTRO
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods VALIDAR_REGISTRO
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GRAVAR_REGISTRO
    exporting
      value(E_ID_BOLETIM) type ZDE_ID_BOLETIM_PROD
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods DELETAR_REGISTRO
    importing
      !I_ID_BOLETIM_PRODUCAO type ZDE_ID_BOLETIM_PROD
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods SET_DADOS_BOLETIM
    importing
      !I_DADOS_BOLETIM type ZSDT0246
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods SET_DADOS_PRODUCAO
    importing
      !I_DADOS_PRODUCAO type ZSDT0247_T
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods SET_DADOS_RENDIMENTO
    importing
      !I_DADOS_RENDIMENTO type ZSDT0248_T
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods VINCULAR_NF
    importing
      !I_NOTAS type ZSDT0249_T
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods DESVINCULAR_NF
    importing
      !I_NOTAS type ZSDT0249_T
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods SET_REGISTRO
    importing
      !I_ID_BOLETIM type ZDE_ID_BOLETIM_PROD
      !I_COM_BLOQUEIO type CHAR01 optional
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_NF_DEVOLUCAO
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_ENT_DEVOLUCAO
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_INDUSTRIALIZACAO
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER
      ZCX_PRECO .
  methods GERAR_NF_ENT_INDUSTRIALIZACAO
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_RFL_01
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER
      ZCX_PRECO .
  methods GERAR_NF_RFL_02
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER
      ZCX_PRECO .
  methods GERAR_NF_RFL_03
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER
      ZCX_PRECO .
  methods GERAR_NF_RCO_01
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_ENT_RCO_01
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_RCO_02
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_ENT_RCO_02
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_RCO_03
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_NF_ENT_RCO_03
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_SEQ_LCTO_ZNFW type ZFIWED006
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO
      ZCX_NF_WRITER .
  methods GERAR_DOC_PRODUCAO_01
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_MBLNR type MBLNR
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_DOC_PRODUCAO_02
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_MBLNR type MBLNR
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_DOC_PRODUCAO_03
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_MBLNR type MBLNR
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_DOC_PRODUCAO_04
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_MBLNR type MBLNR
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_DOC_PRODUCAO_05
    importing
      !I_ZSDT0252 type ZSDT0252
    exporting
      !E_MBLNR type MBLNR
    returning
      value(R_IF_BOLETIM_PRODUCAO) type ref to ZIF_BOLETIM_PRODUCAO
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GERAR_RATEIO_PRODUTOS
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods GET_KEY_DOCS
    importing
      !I_ZSDT0252 type ZSDT0252
    returning
      value(R_KEY_DOCS) type ZDE_KEY_DOCS_BOLETIM
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods BLOQUEIO_DESBLOQUEIO_PRODUTOS
    importing
      !I_ITENS type BAPI2017_GM_ITEM_CREATE_T
      !I_ACAO type CHAR01
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods CHECK_ESTOQUE_PRODUTOS
    importing
      !I_ITENS type BAPI2017_GM_ITEM_CREATE_T
    raising
      ZCX_BOLETIM_PRODUCAO .
  methods SET_DATE_PROCESSAMENTO
    importing
      !I_DATE type SY-DATUM .
  methods SET_TP_BOLETIM
    importing
      !I_TP_BOLETIM type ZTP_BOLETIM .
  methods GET_UND_MATERIAL
    importing
      !I_MATNR type MATNR
    exporting
      value(E_MEINS) type MEINS .
  methods DESAPROVAR_LANCAMENTO
    importing
      !I_ID_BOLETIM_PRODUCAO type ZDE_ID_BOLETIM_PROD .
endinterface.
