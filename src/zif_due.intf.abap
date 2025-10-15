interface ZIF_DUE
  public .


  data AT_DUE type ZSDT0170 .
  data AT_ITENS type ZSDT0172_T .
  data AT_ITENS_FATURAS_REF type ZSDT0173_T .
  data AT_ITENS_PAISES_DESTINO type ZSDT0174_T .
  data AT_ITENS_LPCO type ZSDT0190_T .
  data CK_ALTEROU type CHAR01 .
  data AT_TOKEN type ref to ZCL_TOKEN_SISCOMEX .
  data AT_XMLS_EXPORTACAO type ZNFE_XML_SEFAZ_AUTH_T .

  class-methods GERAR_NR_RUC
    importing
      !I_DADOS_GERACAO_RUC type ZDE_DADOS_GERACAO_RUC
    returning
      value(R_NUMERO_RUC) type ZDE_NUMERO_RUC
    raising
      ZCX_DUE .
  class-methods GERAR_NR_RUC_WITH_SCREEN
    importing
      !I_DADOS_GERACAO_RUC type ZDE_DADOS_GERACAO_RUC
    returning
      value(R_NUMERO_RUC) type ZDE_NUMERO_RUC
    raising
      ZCX_DUE .
  class-methods GET_DUE
    importing
      !I_DUE type ZSDT0170-ID_DUE
    returning
      value(R_DUE) type ZDE_DUE .
  methods TROCA_DUE
    importing
      !I_ID_DUE type ZDE_ID_DUE
    returning
      value(R_TROCADA) type CHAR01
    raising
      ZCX_DUE .
  methods SET_ID_DUE
    importing
      !I_ID_DUE type ZDE_ID_DUE .
  methods SET_BUKRS
    importing
      !I_BUKRS type BUKRS .
  methods SET_NUMERO_DUE
    importing
      !I_NUMERO_DUE type ZDE_NUMERO_DUE .
  methods SET_NUMERO_RUC
    importing
      !I_NUMERO_RUC type ZDE_NUMERO_RUC .
  methods SET_CODIGO_URF_DESPACHO
    importing
      !I_CODIGO_URF_DESPACHO type ZDE_CODIGO_URF_DESPACHO .
  methods SET_CODIGO_RA_DESPACHO
    importing
      !I_CODIGO_RA_DESPACHO type ZDE_CODIGO_RA_DESPACHO .
  methods SET_FORMA_EXPORTACAO
    importing
      !I_FORMA_EXPORTACAO type ZDE_FORMA_EXPORTACAO .
  methods SET_CASO_ESPECIAL_TRANSPORTE
    importing
      !I_CASO_ESPECIAL_TRANSPORTE type ZDE_CASO_ESPECIAL_TRANSPORTE .
  methods SET_SITUACAO_ESPECIAL
    importing
      !I_SITUACAO_ESPECIAL type ZDE_SITUACAO_ESPECIAL .
  methods SET_OBSERVACOES_GERAIS
    importing
      !I_OBSERVACOES_GERAIS type ZDE_OBSERVACOES_GERAIS .
  methods SET_MOEDA_CAMBIO
    importing
      !I_MOEDA_CAMBIO type ZDE_MOEDA_CAMBIO .
  methods SET_MOTIVO
    importing
      !I_MOTIVO type ZDE_MOTIVO .
  methods SET_CNPJ_DECLARANTE
    importing
      !I_CNPJ_DECLARANTE type ZDE_CNPJ_DECLARANTE .
  methods SET_CODIGO_URF_EMBARQUE
    importing
      !I_CODIGO_URF_EMBARQUE type ZDE_CODIGO_URF_EMBARQUE .
  methods SET_CODIGO_RA_EMBARQUE
    importing
      !I_CODIGO_RA_EMBARQUE type ZDE_CODIGO_RA_EMBARQUE .
  methods SET_TP_DUE
    importing
      !I_TP_DUE type ZDE_TP_DUE .
  methods SET_ID_DUE_REF
    importing
      !I_ID_DUE_REF type ZDE_ID_DUE_REF .
  methods SET_STATUS
    importing
      !I_STATUS type ZDE_STATUS_DUE .
  methods SET_TP_COD_LOCAL_DESPACHO
    importing
      !I_TP_COD_LOCAL_DESPACHO type ZDE_TP_COD_LOCAL_DESPACHO .
  methods SET_CNPJ_CPF_RESP_LOC_DESP
    importing
      !I_CNPJ_CPF_RESP_LOC_DESP type ZDE_CNPJ_CPF_RESP_LOC_DESP .
  methods SET_LOCAL_DESPACHO_LONGITUDE
    importing
      !I_LOCAL_DESPACHO_LONGITUDE type ZDE_LOCAL_DESPACHO_LONGITUDE .
  methods SET_LOCAL_DESPACHO_LATITUDE
    importing
      !I_LOCAL_DESPACHO_LATITUDE type ZDE_LOCAL_DESPACHO_LATITUDE .
  methods SET_LOCAL_DESPACHO_END
    importing
      !I_LOCAL_DESPACHO_END type ZDE_LOCAL_DESPACHO_END .
  methods SET_CABECALHO
    importing
      !I_ZSDT0170 type ZSDT0170 .
  methods ADD_ITEM
    importing
      !I_ZSDT0172 type ZSDT0172 .
  methods DEL_ITEM
    importing
      !I_ZSDT0172 type ZSDT0172 .
  methods ADD_ITEM_LPCO
    importing
      !I_ZSDT0190 type ZSDT0190
    raising
      ZCX_DUE .
  methods ADD_ITEM_FATURA_REF
    importing
      !I_ZSDT0173 type ZSDT0173 .
  methods DEL_ITEM_FATURA_REF
    importing
      !I_ZSDT0173 type ZSDT0173 .
  methods ADD_ITEM_PAIS_DESTINO
    importing
      !I_ZSDT0174 type ZSDT0174 .
  methods DEL_ITEM_PAIS_DESTINO
    importing
      !I_ZSDT0174 type ZSDT0174 .
  methods GRAVAR_REGISTRO
    exporting
      !E_ID_DUE type ZDE_ID_DUE
      !E_ZSDT0170 type ZSDT0170
      !E_ZSDT0172 type ZSDT0172_T
      !E_ZSDT0173 type ZSDT0173_T
    returning
      value(R_GRAVOU) type CHAR01
    raising
      ZCX_DUE .
  methods VALIDAR_REGISTRO
    returning
      value(R_VALIDOU) type CHAR01
    raising
      ZCX_DUE .
  methods LIMPAR_REGISTRO .
  methods ENVIAR_DUE
    importing
      !I_RETRANSMISSAO type CHAR01 optional
    returning
      value(R_ENVIADA) type CHAR01
    raising
      ZCX_DUE .
  methods MONTA_XML
    returning
      value(R_XML) type STRING
    raising
      ZCX_DUE .
  methods REGISTRO_DUE
    importing
      !I_REGISTRO_DUE type ZDE_REGISTRO_DUE
    raising
      ZCX_DUE .
  methods SET_TOKEN
    importing
      !I_TOKEN type ref to ZCL_TOKEN_SISCOMEX
    exceptions
      ZCX_DUE .
  methods MODIFY_MATNR
    importing
      !I_MATNR type MATNR
      !I_ID_DUE type ZDE_ID_DUE
    returning
      value(R_ALTERADO) type CHAR01
    raising
      ZCX_DUE .
  methods MODIFY_REGION
    importing
      !I_LAND1 type LAND1
      !I_REGIO type REGIO
    returning
      value(R_ALTERADO) type CHAR01
    raising
      ZCX_DUE .
  methods MODIFY_TP_EXPORTACAO
    importing
      !I_TP_EXPORTACAO type ZDE_TP_EXPORTACAO
    returning
      value(R_ALTERADO) type CHAR01
    raising
      ZCX_DUE .
  methods BLOQ_DESBLOQ
    returning
      value(R_MODIFICADO) type CHAR01
    raising
      ZCX_DUE .
  methods CONSULTAR_DUE
    importing
      !I_ATUALIZA_REGISTRO type CHAR01 optional
    returning
      value(R_ZSDT0170) type ZSDT0170
    raising
      ZCX_DUE .
  methods LIB_LEITURA_OPUS
    raising
      ZCX_DUE .
  methods SOL_MODIFIC_OPUS
    importing
      !I_BLOQ_OPUS type CHAR01 optional
    raising
      ZCX_DUE .
  methods REGISTRAR_LOG
    raising
      ZCX_DUE .
  methods GET_XML_DUE
    importing
      !I_DOWNLOAD type CHAR01 optional
    returning
      value(E_XML) type STRING
    raising
      ZCX_DUE .
  methods ELIMINAR_REGISTRO
    raising
      ZCX_DUE .
  methods CONSULTA_DUE_COMPLETA
    importing
      !I_NUMERO_DUE type ZDE_NUMERO_DUE optional
      !I_NUMERO_RUC type ZDE_NUMERO_RUC optional
    returning
      value(R_DUE_COMPLETA) type ZDE_DUE_COMPLETA
    raising
      ZCX_DUE .
  methods SET_XMLS_EXPORTACAO
    importing
      !I_XMLS_EXPORTACAO type ZNFE_XML_SEFAZ_AUTH_T
    raising
      ZCX_DUE .
  methods VALIDA_PREENCHIMENTO_AUTO
    raising
      ZCX_DUE .
  methods RECLASSIFICACAO_EUDR
    importing
      !I_EUDR type ZEUDR
    returning
      value(R_ALTERADO) type ABAP_BOOL
    raising
      ZCX_DUE .
  methods CHECK_GENERATION_FILE_EUDR
    importing
      !I_ID_NOMEACAO_TRAN type ZID_NOMEACAO .
  methods ENVIAR_EMAIL_FILE_EUDR
    importing
      !I_FILE_GEOJSON type STRING
      !I_ID_NOMEACAO type ZID_NOM
      !I_EMAIL type ZEMAIL optional .
endinterface.
