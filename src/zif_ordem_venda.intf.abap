interface ZIF_ORDEM_VENDA
  public .


  class-data AT_ORDEM_VENDA type ref to ZIF_ORDEM_VENDA .
  data AT_VBAK type VBAK .
  data AT_VBAP type VBAP .
  data AT_VBKD type VBKD .
  data AT_VBPA type TAB_VBPA .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA .
  class-methods GET_ORDEM_VENDA_SALDO
    importing
      !I_VBELN type VBELN_VA
    exporting
      !E_SALDO type KWMENG
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  class-methods GET_TEXT_CAB_1
    importing
      !I_VBELN type VBELN_VA
    returning
      value(R_TEXTO) type STRING
    raising
      ZCX_ORDEM_VENDA
      ZCX_ERROR .
  class-methods GET_TEXT_CAB_FORMULARIO
    importing
      !I_VBELN type VBELN_VA
    returning
      value(R_TEXTO) type STRING
    raising
      ZCX_ORDEM_VENDA
      ZCX_ERROR .
  class-methods OPEN
    importing
      !I_VBELN type VBELN_VA .
  methods GET_ORDEM_VENDA
    exporting
      !E_ORDEM_VENDA type VBAK
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods SET_ORDEM_VENDA
    importing
      !I_VBELN type VBELN_VA
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_ORDEM_VENDA_TRANSGENICA
    importing
      !I_EUDR type ZEUDR optional
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_ORDEM_VENDA_CONVENCIONAL
    importing
      !I_EUDR type ZEUDR optional
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_FILIAL_EMISSORA_ROMANEIO
    importing
      !I_BRANCH type ZDE_BRANCH_RECEB
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods GET_PARTINER
    importing
      !I_FUNCAO_PARTINER type PARVW
    exporting
      !E_PARTINER type VBPA
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_ORDEM_VENDA_DCO
    importing
      !I_BRANCH type WERKS_D
      !I_MATNR type MATNR
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_SAFRA
    importing
      !I_SAFRA type CHARG_D
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods CK_TIPO_FRETE
    importing
      !I_TIPO_FRETE type ZDE_TP_FRETE
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods GET_TIPO_FRETE
    exporting
      !E_TIPO_FRETE type ZDE_TP_FRETE
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods GET_CK_VALOR_FRETE
    importing
      !I_VALOR_FRETE_EMPRESA type KBETR_KOND
      !I_VALOR_FRETE_MOTORISTA type KBETR_KOND
      !I_PLACA_VEIC_TRACAO type ZPC_VEICULO
      !I_ID_ORDEM type ZDE_ID_ORDEM optional
      !I_ZLEST0181 type ZLEST0181 optional
      !I_VIAGEM_ID type ZLEST0185-VIAGEM_ID optional
      !I_PAGA_TRECHO_TERCEIRO type ZDE_CARGUERO_REQ_DATA-PAGA_TRECHO_TERCEIRO optional
    exporting
      !E_PARAMETROS_EMPRESA type STRING
      !E_PARAMETROS_MOTORISTA type STRING
      !E_VALOR_FRETE_ENTRADA type KBETR_KOND
      !E_VALOR_FRETE_MOTORISTA type KBETR_KOND
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA
      ZCX_ERROR
      ZCX_CALC_FRETE .
  methods GET_TIPO_TRANSPORTE
    importing
      !I_VSART type VERSART default '01'
    exporting
      !E_TIPO_TRANSPORTE type SHTYP
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods GET_ZONAS_TRANSPORTE
    importing
      !I_FRETE_ENTRADA type CHAR01 default ABAP_FALSE
    exporting
      !E_LZONEA type LZONEA
      !E_LZONEZ type LZONEZ
      !E_LC_COLETA type LIFNR
      !E_LC_ENTREGA type KUNNR
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
  methods GET_ITEM
    exporting
      !E_VBAP type VBAP
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA .
  methods GET_DADOS_COMERCIAIS
    exporting
      !E_VBKD type VBKD
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA .
  methods GET_PARCEIROS
    exporting
      !E_VBPA type TT_VBPA "#EC CI_USAGE_OK[2228056]
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA .
  methods GET_CENTRO
    exporting
      !E_WERKS type WERKS_EXT
      !E_PARID_WERKS type ZPARID
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA .
  methods CK_GRUPO_MERCADORIA
    importing
      !I_TIPO_FRETE type ZDE_TP_FRETE
    returning
      value(R_INSTANCIA) type ref to ZIF_ORDEM_VENDA
    raising
      ZCX_ORDEM_VENDA .
endinterface.
