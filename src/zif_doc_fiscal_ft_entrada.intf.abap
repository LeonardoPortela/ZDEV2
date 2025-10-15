interface ZIF_DOC_FISCAL_FT_ENTRADA
  public .


  data AT_AVISO_RECEBIMENTO type ZLEST0108 .
  data AT_AVISO_RECEB_ITEMS type ZLEST0109 .
  data AT_ORDEM_CARREGAMENTO type ZSDT0001OD .
  data AT_NOTA_FISCAL type ZLEST0110 .
  class-data INSTANCE type ref to ZIF_DOC_FISCAL_FT_ENTRADA .
  data AT_DACTE type XSTRING .
  data AT_DECLARACAO type XSTRING .
  data AT_CONTRATO_VIAGEM type XSTRING .
  data AT_MDFE type XSTRING .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  class-methods SET_GERAR_PEDAGIO
    importing
      !I_TKNUM type TKNUM
      !I_CD_CID_ORIGEM type ZDE_CIDADE_ORIGEM optional
      !I_CD_CID_DESTINO type ZDE_CIDADE_DESTINO optional
      !I_NR_CARTAO type ZDE_REPOM_CARTAO_PED
      !I_ID_ROTA_REPOM type ZDE_ID_ROTA_REPOM
      !I_ID_PERCURSO_REPOM type ZDE_ID_PERCURSO_REPOM
    returning
      value(E_ZLEST0123) type ZLEST0123
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  class-methods GET_ERRO_GERAL
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  class-methods SET_CRIAR_AVISO_NOTA_CARGA
    importing
      !I_AGENTE_FRETE type TDLNR
      !I_COD_LOC_ENTREGA type KUNNR
      !I_NOTA type ZDE_ZSDT0001NT_ALV
    exporting
      !E_VBELN type VBELN
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  class-methods SET_DELE_AVISO_NOTA_CARGA
    importing
      !I_NOTA type ZDE_ZSDT0001NT_ALV
    returning
      value(R_DELETOU) type CHAR01
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  class-methods GET_PRECO_FRETE_ENTRADA
    importing
      !I_AVISO type VBELN
      !I_COD_LOC_COLETA type LIFNR
      !I_COD_LOC_ENTREGA type KUNNR
      !I_EBELN type EBELN
      !I_PLACA_CAV type ZDE_PLACA_TRATOR
      !I_DT_REFERECIA type DATUM optional
      !I_AGENTE_FRETE type TDLNR
      !I_MATERIAL type MATNR
      !I_VIAGEM_ID type ZLEST0185-VIAGEM_ID optional
    exporting
      !E_TARIFA type KBETR_KOND
      !E_DATA type LDDAT
      !E_QTD_PRECOS_ENCONTRADOS type I
      !E_MESSAGE type STRING
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_NEW_ID_DOC_AGRUPADO
    exporting
      !E_ID_AGRUPA type ZDE_ID_AGRUPA_FRETE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_NEW_DOCUMENTO_AGRUPADO
    importing
      !I_AVISOS type T_VBELN
      !I_ID_AGRUPA_FRETE type ZDE_ID_AGRUPA_FRETE optional
    exporting
      !E_DOC_TRANSP type TKNUM
      !E_FKNUM type FKNUM
      !E_OV_FRETE type VBELN_VA
      !E_FATURA_FRETE type VBELN_VF
      !E_NRO_NF_FRETE type J_1BDOCNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_NEW_DOCUMENTO_ORDEM_CARREG
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
      !I_ID_LOCAL_COLETA type LIFNR
      !I_PESO_LIQ type NTGEW
      !I_NOTA_FISCAL type ZDE_INFO_NOTA
      !I_ID_ROTA_REPOM type ZDE_ID_ROTA_REPOM optional
      !I_ID_PERCURSO_REPOM type ZDE_ID_PERCURSO_REPOM optional
      !I_NR_CARTAO_REPOM type ZDE_REPOM_CARTAO_PED optional
    exporting
      !E_EBELN type EBELN
      !E_EBELP type EBELP
      !E_VBELN type VBELN
      !E_DOC_TRANSP type TKNUM
      !E_FKNUM type FKNUM
      !E_OV_FRETE type VBELN_VA
      !E_FATURA_FRETE type VBELN_VF
      !E_NRO_NF_FRETE type J_1BDOCNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_NEW_DOCUMENTO_IF_CARGA
    importing
      !I_IF_CARGA type ref to ZIF_CARGA
    exporting
      !E_EBELN type EBELN
      !E_EBELP type EBELP
      !E_VBELN type VBELN
      !E_DOC_TRANSP type TKNUM
      !E_FKNUM type FKNUM
      !E_OV_FRETE type VBELN_VA
      !E_FATURA_FRETE type VBELN_VF
      !E_NRO_NF_FRETE type J_1BDOCNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_PEDIDO_COMPRA
    importing
      !I_BUKRS type BUKRS
      !I_LIFNR type LIFNR
      !I_MATNR type MATNR
      !I_CHARG type CHARG_D
      !I_WERKS type EWERK
      !I_LGORT type LGORT_D
      !I_ID_IVA type MWSKZ
    exporting
      !E_INFO_PEDIDO type ZDE_INFO_PEDIDO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_ORDEM_CARREGAMENTO
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    exporting
      !E_ORDEM_CARREGAMENTO type ZDE_ZSDT0001OD_ALV
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_TIPO_ENTRADA
    importing
      !I_ID_ENTRADA type ZDE_ID_ENTRADA
      !I_ID_EMPRESA type BUKRS
      !I_CK_NFE type J_1BNFE
    exporting
      !E_TIPO_ENTRADA type ZSDT0001TE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_AVISO_RECEBIMENTO
    importing
      !I_NUMERO type ZDE_NR_NFE
      !I_SERIE type J_1BSERIES
      !I_EMISSOR type LIFNR
      !I_EBELN type EBELN optional
      !I_EBELP type EBELP optional
    exporting
      !E_LIKP type LIKP
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_CLEAR
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_CRIAR_AVISO_RECEBIMENTO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_DELE_AVISO_FRETE
    importing
      !I_VBELN type VBELN
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_CRIAR_DOC_TRANSPORTE
    importing
      !I_TVRO type TVRO
      !I_SHTYP type SHTYP
      !I_TVTK type TVTK
    exporting
      !E_DOC_TRANSP type TKNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_GRAVAR_REGISTRO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_ITINERARIO_RELEVANTE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_PRECO_TARIFA_FRETE
    exporting
      !E_TARIFA type KBETR_KOND
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_TIPO_TRANSPORTE
    exporting
      !E_SHTYP type SHTYP
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_INFO_VEICULO
    importing
      !I_PLACA type ZPC_VEICULO
    exporting
      !E_VEICULO type ZLEST0002
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_CONDICAO_ZSEG
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_CONDICAO_ZIOF
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_ITINERARIO
    exporting
      !E_TVRO type TVRO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_LOCAL_COLETA
    exporting
      !E_LFA1 type LFA1
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_INFO_TIPO_TRANSPORTE
    importing
      !I_SHTYP type SHTYP
    exporting
      !E_TVTK type TVTK
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_ENTRONCAMENTO_LC_ENTREGA
    exporting
      !E_TVKN type TVKN
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_LOCAL_ENTREGA_FORNECEDOR
    exporting
      !E_LIFNR type LIFNR
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_ADIANTAMENTO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_ITINERARIO_PEDAGIO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_CRIAR_DOC_CUSTO
    exporting
      !E_FKNUM type FKNUM
      !E_OV_FRETE type VBELN_VA
      !E_FATURA_FRETE type VBELN_VF
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_ATUALIZA_CONHECIMENTO
    exporting
      !E_NRO_NF_FRETE type J_1BDOCNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_AUTORIZAR_DOCUMENTO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_ERRO_GERAL_STRING
    importing
      !I_TEXTO type STRING
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_CK_FINALIZADO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods GET_DOC_FISCAL_FT_ENTRADA
    exporting
      !E_AVISO_RECEBIMENTO type ZLEST0108
      !E_AVISO_RECEB_ITEMS type ZLEST0109
      !E_ORDEM_CARREGAMENTO type ZSDT0001OD
      !E_NOTA_FISCAL type ZLEST0110
      !E_DACTE type XSTRING
      !E_DECLARACAO type XSTRING
      !E_CONTRATO_VIAGEM type XSTRING
      !E_MDFE type XSTRING
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_REGISTRO
    importing
      !I_EBELN type EBELN optional
      !I_EBELP type EBELP optional
      !I_WERKS type EWERK optional
      !I_LGORT type LGORT_D optional
      !I_CHARG type CHARG_D optional
      !I_VBELN type VBELN
      !I_NAO_CARREGAR_ARQUIVOS type CHAR01 default ' '
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_ESTORNAR_DOCUMENTOS
    importing
      !I_ESTORNAR_AVISO type CHAR01 default ABAP_TRUE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_ESTORNAR_DOC_CUSTO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_ESTORNAR_AVISO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_ESTORNAR_DOC_TRANSPORTE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_VALIDAR_AVISO
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_GERAR_DOCUMENTO_TRANSPORTE
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
  methods SET_GERAR_MDFE_AVULSA
    returning
      value(R_INSTANCIA) type ref to ZIF_DOC_FISCAL_FT_ENTRADA
    raising
      ZCX_DOC_FISCAL_FT_ENTRADA .
endinterface.
