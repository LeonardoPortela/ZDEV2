interface ZIF_CTE_SUBCONTRATACAO
  public .


  class-data AT_IF_CTE_SUBCONTRATACAO type ref to ZIF_CTE_SUBCONTRATACAO .
  data AT_CHAVE_CTE type ZDE_CHAVE_DOC_E .
  data AT_INTERCOMPANY type CHAR01 .
  data AT_VBELN_ORDER type VBELN_VA .
  data AT_VBELN_FATURA type VBELN_VF .

  class-methods GET_INSTANCE
    returning
      value(R_IF_CTE_SUBCONTRATACAO) type ref to ZIF_CTE_SUBCONTRATACAO
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  class-methods CHECK_EXIST_ORDER_GERADA
    importing
      !I_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_VBELN_ORDER type VBELN_VA
      !E_VBELN_FATURA type VBELN_VF
      !E_DOCNUM_CTE type J_1BDOCNUM
      !E_STATUS_CTE type CHAR4
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods GERAR_OV
    exporting
      !E_VBELN_ORDER type VBELN_VA
    returning
      value(R_IF_CTE_SUBCONTRATACAO) type ref to ZIF_CTE_SUBCONTRATACAO
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods GERAR_FATURA
    importing
      !I_VBELN_ORDER type VBELN_VA
    exporting
      !E_VBELN_FATURA type VBELN_VF
      !E_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_CTE_SUBCONTRATACAO) type ref to ZIF_CTE_SUBCONTRATACAO
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods SET_CHAVE_CTE
    importing
      !I_CHAVE_CTE type ZDE_CHAVE_DOC_E
    returning
      value(R_IF_CTE_SUBCONTRATACAO) type ref to ZIF_CTE_SUBCONTRATACAO
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods GERAR_OV_INTERCOMPANY
    exporting
      !E_VBELN_ORDER type VBELN_VA
    returning
      value(R_IF_CTE_SUBCONTRATACAO) type ref to ZIF_CTE_SUBCONTRATACAO
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods GET_CFOP
    importing
      !I_INDUSTRY type J_1BINDUS2
      !I_DIRECT type J_1BDIRECT
      !I_DSTCAT type J_1BDSTCAT
      !I_TP_PARC type ZTIPODOCPARCEIRO
      !I_TDLNR type TDLNR
      !I_BUKRS type BUKRS
    returning
      value(R_CFOP) type J_1BCFOP
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
  methods GET_AGENTE_FRETE
    importing
      !I_TIPO_AGENTE type CHAR1 default '1'
      !I_CNPJ_PROP_VEICULO type STCD1
      !I_UF_ORIGEM_MERCADORIA type REGIO
      !I_BUKRS type BUKRS optional
    returning
      value(R_AGENTE_FRETE) type LIFNR
    raising
      ZCX_CTE_SUBCONTRATACAO
      ZCX_ERROR .
endinterface.
