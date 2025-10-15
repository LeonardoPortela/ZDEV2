interface ZIF_TRACE_COTTON
  public .


  data AT_INFO_REQUEST_HTTP type ZDE_INTEGRACAO_HTTP_CONFIG .
  class-data AT_IF_TRACE_COTTON type ref to ZIF_TRACE_COTTON .
  data AT_JSON type STRING .
  data AT_METODO type STRING .
  data AT_ID_REFERENCIA type STRING .
  data AT_TP_REFERENCIA type STRING .
  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_SET_TOKEN type STRING .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_ZSEQ_INST type ZSEQ_INST .
  data AT_OBJEK type OBJNUM .
  data AT_OBJECTTABLE type TABELLE .
  data AT_CONTRATO type TEXT50 .
  data AT_ZSDT0045 type ZSDT0045 .
  data AT_ZSDT0066 type ZSDT0066 .
  data AT_MENSAGEM_LOG type ZINTEGRACAO_LOG .
  data AT_ID_CONTRATO type VBELN_VA .
  data AT_ZSDT0143 type ZSDT0143 .
  data AT_NRO_SOL_OV type ZSDED013 .
  data AT_POSNR type POSNR_VA .
  data AT_VBELN type VBELN .
  data AT_ID_CARGA type ZID_CARGA .
  data AT_RETORNO type ZSDT0330_RETORNO .
  data AT_RET_ESTORNO type ZSDT0330_RET_ESTORNO .
  data AT_DATA_INBOUND type ZSDT0330_TRACE .
  data AT_DATA_ESTORNO type ZSDT0330_ESTORNO .
  data AT_ZSDT0330 type ZSDT0330_T .
  data AT_ZSDT0331 type ZSDT0331_T .
  data AT_ZSDT0001 type ZSDT0001 .
  data AT_SAFRA type LGORT_D .
  constants C_ZSDR0151_JOB type BTCPROG value 'ZSDR0151_JOB' ##NO_TEXT.
  constants C_ZSDR0152_JOB type BTCPROG value 'ZSDR0152_JOB' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO .
  class-methods SET_FORMATA_DATA
    importing
      !I_DATA type DATUM
    returning
      value(E_DATA) type STRING .
  class-methods SET_AGUARDAR_JOB .
  class-methods GET_QTDE_PROGRAM_EXEC
    importing
      !I_SHOW_MSG type CHAR1
    returning
      value(E_QUANTIDADE) type I .
  methods SET_VALIDAR_DADOS_ESTORNO
    exporting
      value(E_MSG_ERRO) type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods SET_RECEIVE_CARREGAMENTO
    importing
      !I_AT_INFO_REQUEST_HTTP type ZDE_INTEGRACAO_HTTP_CONFIG
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ESTORNO_CARREGAMENTO
    importing
      !I_AT_INFO_REQUEST_HTTP type ZDE_INTEGRACAO_HTTP_CONFIG
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GRAVAR_RETORNO
    importing
      !I_RETORNO type ZSDT0331
      !I_TIPO_OPERACAO type ZTIPO_OPERACAO optional
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods SET_GRAVAR_LOG
    importing
      !I_TIPO_INTEGRA type ZTIPO_INTEGRA optional
      !I_ZSEQ_INST type ZSEQ_INST optional
      !I_OBJEK type OBJNUM optional
      !I_OBJECTTABLE type TABELLE optional
      !I_ID_CONTRATO type VBELN_VA optional
      !I_NRO_SOL_OV type ZSDED013 optional
      !I_POSNR type POSNR_VA optional
      !I_ID_CARGA type ZID_CARGA optional
      !I_MATNR type MATNR optional
      !I_WERKS type WERKS_D optional
      !I_LGORT type LGORT_D optional
      !I_ACHARG type CHARG_D optional
      !I_SAFRA type CHAR4 optional
      !I_TIPO_MSG type BAPI_MTYPE optional
      !I_MENSAGEM type STRING optional
      !I_METODO_HTTP type ZDE_HTTP_METODO optional
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods SET_JSON_CRIAR_DOCTO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_JSON_EXCLUI_DOCTO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_JSON_CRIAR_CONTRATO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_JSON_EXCLUI_CONTRATO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_JSON_CRIAR_ORDEM_VENDA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_JSON_EXCLUI_ORDEM_VENDA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_MENSAGEM
    importing
      !I_COD_MSG type CHAR02
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_METODO type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods GET_METODO
    exporting
      !E_METODO type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_MENSAGEM type ZINTEGRACAO_LOG
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods SET_HEADER
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON .
  methods SET_EXEC_TRACE
    importing
      !I_METODO type STRING
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CADASTRA_INSTRUCAO
    importing
      !I_ZSEQ_INST type ZSEQ_INST
      !I_OBJEK type OBJNUM
      !I_OBJECTTABLE type TABELLE
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EXCLUI_INSTRUCAO
    importing
      !I_ZSEQ_INST type ZSEQ_INST
      !I_OBJEK type OBJNUM
      !I_OBJECTTABLE type TABELLE
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_TRATAR_MENSAGEM
    importing
      !I_JSON_RETORNO type ZINTEGRACAO_LOG
      !I_MENSAGEM type STRING
    returning
      value(E_MENSAGEM_RETORNO) type STRING .
  methods SET_CADASTRA_CONTRATOS
    importing
      !I_ID_CONTRATO type VBELN_VA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EXCLUI_CONTRATOS
    importing
      !I_ID_CONTRATO type VBELN_VA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CADASTRA_ORDEM_VENDA
    importing
      !I_NRO_SOL_OV type ZSDED013
      !I_POSNR type POSNR_VA
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EXCLUI_ORDEM_VENDA
    importing
      !I_NRO_SOL_OV type ZSDED013
      !I_POSNR type POSNR_VA
      !I_VBELN type VBELN optional
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_RETORNO_TRACE
    importing
      value(I_ID_CARGA) type ZID_CARGA optional
      !I_TIPO_OPERACAO type ZTIPO_OPERACAO optional
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ENVIA_NOTA_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ENVIA_NF_CANCELADA
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_TRACE_COTTON) type ref to ZIF_TRACE_COTTON
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods ADD_FILA_GERA_SOBRA_PERDA
    importing
      !I_ZSDT0330_T type ZSDT0330_T.
endinterface.
