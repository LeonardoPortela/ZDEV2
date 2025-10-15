interface ZIF_FATURAMENTO
  public .


  class-data AT_IF_FATURAMENTO type ref to ZIF_FATURAMENTO .
  constants ST_TP_PROP_VEICULO_PROPRIO type ZDE_TP_PROP_VEICULO value 'P' ##NO_TEXT.
  constants ST_TP_PROP_VEICULO_TERCEIRO type ZDE_TP_PROP_VEICULO value 'T' ##NO_TEXT.
  constants ST_TP_REME_PROPRIO type ZDE_TP_REME_MERCADORIA value 'P' ##NO_TEXT.
  constants ST_TP_REME_TERCEIRO type ZDE_TP_REME_MERCADORIA value 'T' ##NO_TEXT.
  constants ST_TP_REME_INTERCOMPANY type ZDE_TP_REME_MERCADORIA value 'I' ##NO_TEXT.
  data AT_TIPO_VEICULO type ZDE_TP_PROP_VEICULO .
  data AT_TIPO_REMETENTE type ZDE_TP_REME_MERCADORIA .
  data AT_TP_FRETE type ZDE_TP_FRETE .
  data AT_NOTA_FISCAL type CHAR01 .
  data AT_PEDAGIO type CHAR01 .
  data AT_DOC_CUSTO type CHAR01 .
  data AT_DOC_TRANS type CHAR01 .
  data AT_CONHECIMENTO type CHAR01 .
  data AT_PAG_FRETE type CHAR01 .
  data AT_MANIFESTO type CHAR01 .
  data AT_SEGURO_FRETE type CHAR01 .
  data AT_ROMANEIO type ZSDT0001 .
  data AT_UF_ORIGEM_MERCADORIA type REGIO .
  data AT_PLACA_CAVALO type ZPLACA .

  class-methods GET_INSTANCE
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO .
  class-methods GET_ROMANEIO_TROCANOTA
    importing
      !I_CH_REFERENCIA type ZCH_REF optional
      !I_VBELN type LIKP-VBELN optional
    returning
      value(E_TROCA_NOTA) type CHAR1 .
  methods GET_PROCESSO_EMISSAO_DOCS
    importing
      !I_DOCNUM type J_1BDOCNUM optional
      !I_CH_ROMANEIO type ZCH_REF optional
      !I_TKNUM type TKNUM optional
    exporting
      !E_TIPO_VEICULO type ZDE_TP_PROP_VEICULO
      !E_TIPO_REMETENTE type ZDE_TP_REME_MERCADORIA
      !E_TP_FRETE type ZDE_TP_FRETE
      !E_NOTA_FISCAL type CHAR01
      !E_PEDAGIO type CHAR01
      !E_DOC_CUSTO type CHAR01
      !E_DOC_TRANS type CHAR01
      !E_CONHECIMENTO type CHAR01
      !E_PAG_FRETE type CHAR01
      !E_MANIFESTO type CHAR01
      !E_SEGURO_FRETE type CHAR01
      !E_UF_ORIGEM_MERCADORIA type REGIO
      !E_PLACA_CAVALO type ZPLACA
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_ROMANEIO_FROM_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_ZSDT0001 type ZSDT0001
      !E_TP_FRETE type ZDE_TP_FRETE
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_AGENTE_FRETE
    importing
      !I_TIPO_AGENTE type CHAR1 default '1'
      !I_PLACA type ZPLACA
      !I_UF_ORIGEM_MERCADORIA type REGIO
      !I_BUKRS type BUKRS optional
    exporting
      !E_AGENTE_FRETE type LIFNR
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_TIPO_VEICULO
    importing
      !I_PLACA type ZPLACA
      !I_TKNUM type TKNUM optional
    exporting
      !E_TIPO type ZDE_TP_PROP_VEICULO
      !E_PROPRIETARIO type LFA1
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_CK_EMISSAO_MDFE
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_CK_AVERBA_SEGURO
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_ROMANEIO_FROM_TRANSPORTE
    importing
      !I_TKNUM type TKNUM
    exporting
      !E_ZSDT0001 type ZSDT0001
      !E_TP_FRETE type ZDE_TP_FRETE
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_TIPO_TRANSPORTE
    importing
      !I_TIPO_MOV type ZTP_MOV
      !I_VSART type VERSART
      !I_LAUFK type LAUFK optional
      !I_TIPO_OV type AUART optional
      !I_TIPO_PEDIDO type BSART optional
      !I_PARID_LR type KUNNR optional
      !I_PARID_Z1 type LIFNR optional
      !I_PARID_SP type LIFNR optional
    exporting
      !E_SHTYP type SHTYP
    returning
      value(R_IF_FATURAMENTO) type ref to ZIF_FATURAMENTO
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_DOCUMENTOS_FATURAMENTO
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_PULA_CHECK type CHAR1 optional
    returning
      value(T_PDF_FILES) type ZSDT_PDF_FILES
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_AUTH_DOCUMENT
    importing
      value(I_DOCNUM) type J_1BNFDOC-DOCNUM
    returning
      value(E_ERRO) type CHAR1
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_DATA_URL
    importing
      !I_FILENAME type STRING
      !I_PULA_CHECK type CHAR1
    exporting
      value(E_DATA) type ZDE_DATA_XSTRING
      value(E_LEN) type INT4
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_MERGE_PDF
    importing
      value(T_PDF_FILES) type ZSDT_PDF_FILES optional
    returning
      value(E_MERGED_PDF) type ZDE_DATA_XSTRING
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_DOCUMENTOS_OBRIGATORIOS
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !T_PDF_FILES type ZSDT_PDF_FILES optional
    exporting
      value(T_DOCTOS_FALTANTES) type ZSDT_DOCTOS_FALTANTES
    returning
      value(E_FALTAM_DOCUMENTOS) type CHAR1
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_VALIDA_MERGE_PDF
    importing
      !T_PDF_FILES type ZSDT_PDF_FILES optional
    returning
      value(T_PDF_FILES_NEW) type ZSDT_PDF_FILES
    raising
      ZCX_FATURAMENTO
      ZCX_ERROR .
  methods GET_STATUS_GEROU_VT_VI
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_CHECK_DOC_GERADO type CHAR01 optional
    exporting
      !E_TKNUM type TKNUM
      !E_FKNUM type FKNUM
    returning
      value(R_GEROU_VT) type CHAR01
    raising
      ZCX_ERROR .
  methods GET_GERA_VT_FROTA_PROPRIA
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_GERA_TRANSP) type CHAR01 .
  methods GET_CHECK_REM_CONTA_ORDEM
    importing
      !I_ZSDT0001 type ZSDT0001
    returning
      value(R_REM_CONTA_ORDEM) type CHAR01 .
  methods SET_TRANSFERE_BIODIESEL_FROTA
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_BIODIESEL_OK) type CHAR01
    raising
      ZCX_ERROR .
  methods SET_ESTORNAR_BIODIESEL_FROTA
    importing
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_ESTORNO_OK) type CHAR01
    raising
      ZCX_ERROR .
endinterface.
