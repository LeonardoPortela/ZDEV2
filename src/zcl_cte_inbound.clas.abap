class ZCL_CTE_INBOUND definition
  public
  inheriting from ZCL_FISCAL
  final
  create public .

public section.
  type-pools ICON .

  interfaces ZIF_CADASTRO .
  interfaces ZIF_PESQUISA .

  constants ST_DOCUMENTO_00 type ZDE_ST_NFE_DOCUMENTO value '00' ##NO_TEXT.
  constants ST_DOCUMENTO_99 type ZDE_ST_NFE_DOCUMENTO value '99' ##NO_TEXT.
  constants ST_DOCUMENTO_01 type ZDE_ST_NFE_DOCUMENTO value '01' ##NO_TEXT.
  constants ST_FISCAL_PENDENTE type ZDE_ST_NFE_FISCAL value '00' ##NO_TEXT.
  constants ST_FISCAL_DEPARTAMENTO type ZDE_ST_NFE_FISCAL value '01' ##NO_TEXT.
  constants ST_FISCAL_DEPART_APROVADO type ZDE_ST_NFE_FISCAL value '02' ##NO_TEXT.
  constants ST_FISCAL_NAO_ACEITO_FISCAL type ZDE_ST_NFE_FISCAL value '97' ##NO_TEXT.
  constants ST_FISCAL_SEM_ACEITE_FISCAL type ZDE_ST_NFE_FISCAL value '98' ##NO_TEXT.
  constants ST_FISCAL_COM_ACEITE_FISCAL type ZDE_ST_NFE_FISCAL value '99' ##NO_TEXT.
  constants ST_FISICO_ type ZDE_ST_NFE_FISICO value ' ' ##NO_TEXT.
  constants ST_FISICO_00 type ZDE_ST_NFE_FISICO value '00' ##NO_TEXT.
  constants ST_FISICO_01 type ZDE_ST_NFE_FISICO value '01' ##NO_TEXT.
  constants ST_FISICO_AVISO_GERADO type ZDE_ST_NFE_FISICO value '02' ##NO_TEXT.
  constants ST_FISICO_MIGO_GERADA type ZDE_ST_NFE_FISICO value '03' ##NO_TEXT.
  constants ST_FISICO_MIRO_GERADA type ZDE_ST_NFE_FISICO value '04' ##NO_TEXT.
  constants ST_FISICO_FISCAL_GERADO type ZDE_ST_NFE_FISICO value '05' ##NO_TEXT.
  constants ST_FISICO_98 type ZDE_ST_NFE_FISICO value '98' ##NO_TEXT.
  constants ST_FISICO_99 type ZDE_ST_NFE_FISICO value '99' ##NO_TEXT.
  constants ST_ARMAZENAGEM_00 type ZDE_ST_NFE_ARMAZEM value '00' ##NO_TEXT.
  constants ST_ARMAZENAGEM_01 type ZDE_ST_NFE_ARMAZEM value '01' ##NO_TEXT.
  constants ST_ARMAZENAGEM_02 type ZDE_ST_NFE_ARMAZEM value '02' ##NO_TEXT.
  constants ST_ARMAZENAGEM_03 type ZDE_ST_NFE_ARMAZEM value '03' ##NO_TEXT.
  constants ST_ARMAZENAGEM_04 type ZDE_ST_NFE_ARMAZEM value '04' ##NO_TEXT.
  constants ST_ARMAZENAGEM_05 type ZDE_ST_NFE_ARMAZEM value '05' ##NO_TEXT.
  constants ST_ARMAZENAGEM_97 type ZDE_ST_NFE_ARMAZEM value '97' ##NO_TEXT.
  constants ST_ARMAZENAGEM_98 type ZDE_ST_NFE_ARMAZEM value '98' ##NO_TEXT.
  constants ST_ARMAZENAGEM_99 type ZDE_ST_NFE_ARMAZEM value '99' ##NO_TEXT.
  constants TRANSACAO_DEPARTAMENTO type TCODE value 'ZMM0111' ##NO_TEXT.
  constants TRANSACAO_FORNECEDOR type TCODE value 'XK03' ##NO_TEXT.
  constants TRANSACAO_PARAN_IVA type TCODE value 'ZMM0113' ##NO_TEXT.
  constants TP_COMPRA_FUTURA_FATURA type ZDE_TP_COMPRA_FUTURA value '1' ##NO_TEXT.
  constants TP_COMPRA_FUTURA_MERCADORIA type ZDE_TP_COMPRA_FUTURA value '2' ##NO_TEXT.
  data MENSAGENS_RETORNO type BAPIRET2_T .
  data CK_SOMENTE_UMA_MIGO_PEDIDO type CHAR01 .
  data CK_ALTEROU_IVA type CHAR01 .
  data CK_ALTEROU_BLOQUEIO_PAGA type CHAR01 .
  data CK_IGNORA_DATA_SE_VENCIMENTO type CHAR01 .

  methods EXCLUIR_LOTE_ITEM
    importing
      !I_CD_LOTE_ITEM type ZDE_CD_LOTE_ITEM .
  methods SET_NR_DOCUMENTO_MATERIAL
    importing
      !I_MBLNR type MBLNR
      !I_MJAHR type MJAHR .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_FATURA_NFE_INBOUND
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_RSEG type J_1B_TT_RSEG
    returning
      value(R_RBKP) type RBKP .
  methods VALIDA_ATRIBUTO_ALTERAVEL_LOTE
    importing
      !I_CAMPO type NAME_FELD
      !I_PROD_ITEM type J_1BITMNUM optional
      !I_EBELN type EBELN optional
      !I_EBELP type EBELP optional
      !I_LOTE type CHARG_D
    returning
      value(R_PERMITIDO) type CHAR01 .
  class-methods GET_ICON_STATUS_FISICO
    importing
      !I_STATUS type ZDE_ST_NFE_FISICO
    returning
      value(R_ICON) type CHAR04 .
  class-methods GET_ICON_STATUS_FISCAL
    importing
      !I_STATUS type ZDE_ST_NFE_FISCAL
    returning
      value(R_ICON) type CHAR04 .
  class-methods GET_ICON_STATUS_DOCUMENTO
    importing
      !I_STATUS type ZDE_ST_NFE_DOCUMENTO
    returning
      value(R_ICON) type CHAR04 .
  class-methods GET_ICON_STATUS_ARMAZEM
    importing
      !I_STATUS type ZDE_ST_NFE_ARMAZEM
    returning
      value(R_ICON) type CHAR04 .
  methods CONSTRUCTOR
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E optional
      !I_SEM_BLOQUEIO type CHAR01 optional
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods GET_LOG_PROC_NFE
    importing
      !P_CHAVE type ZDE_CHAVE_DOC_E
    returning
      value(E_LOGS) type ZDE_NFE_DIST_LOG_ALV_T .
  class-methods GET_NFE_INBOUND_ALV_SAIDA
    importing
      !I_CTE_DIST type ZIB_CTE_DIST_TER_T
    returning
      value(E_ALV) type ZDE_CTE_DIST_TER_ALV_T .
  class-methods SET_ENQUEUE_NFE
    importing
      !I_CHAVE type ZDE_CHAVE_DOC_E
      !I_SEM_BLOQUEIO_REGISTRO type CHAR01 default ABAP_FALSE
    exceptions
      ZFOREIGN_LOCK
      ZSYSTEM_FAILURE
      ERRO .
  class-methods SET_DENQUEUE_NFE
    importing
      !I_CHAVE type ZDE_CHAVE_DOC_E
      !I_SEM_BLOQUEIO_REGISTRO type CHAR01 default ABAP_FALSE .
  methods SET_INFO_SAP
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods GET_INFO_NOTA
    returning
      value(R_CTE_INBOUND) type ZCTE_INBOUND .
  methods NFE_INBOUND_VISUALIZAR .
  methods NFE_INBOUND_ACEITE_FISICO
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods NFE_INBOUND_ACEITE_FATURA
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods GET_INFO_FORNECEDOR
    importing
      !I_LIFNR type LIFNR
    returning
      value(R_LFA1) type LFA1 .
  methods GET_TOMADOR
    importing
      !I_CNPJ type ZDE_DEST_CNPJ
      !I_IE type ZDE_DEST_INSC_ES
    returning
      value(R_J_1BBRANCH) type J_1BBRANCH
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_FORNECEDOR
    importing
      !I_FORNE_CNPJ type ZDE_FORN_CNPJ
      !I_FORNE_CPF type ZDE_FORN_CPF
      !I_FORNE_IE type ZDE_FORN_INSC_ES
      !I_SERIE_NOTA type J_1BSERIES optional
      !I_BUKRS type BUKRS optional
    returning
      value(R_LIFNR) type LFA1
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_MATERIAL
    importing
      !I_EMISSOR type LIFNR
      !I_PROD_CODIGO type ZDE_PROD_CODIGO
      !I_UNIDADE_ITEM type ZDE_UND_COMERCI
    returning
      value(R_001) type ZIB_NFE_DIST_001
    exceptions
      NAO_ACHOU_DEPARADA .
  methods GET_PEDIDO_COMPRA_CHAVE
    importing
      !I_NOTA type ZIB_NFE_DIST_TER
      !I_ITEM type ZIB_NFE_DIST_ITM
    exporting
      !E_SALDO_ITEM type ZDE_EKPO_HELP_SALDO
    returning
      value(R_EKPO) type EKPO
    exceptions
      ERRO .
  methods GET_INFO_DEPARTAMENTO
    importing
      !I_CD_DEPARTAMENTO type ZDE_DEPARTAMENTO
    returning
      value(R_DS_DEPARTAMENTO) type ZDE_DS_DEPARTAMENTO .
  methods SET_DEPARTAMENTO
    importing
      !I_CD_DEPARTAMENTO type ZDE_DEPARTAMENTO
    raising
      ZCX_CADASTRO
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_ARMAZEM
    importing
      !I_F_ARMAZEM type ZDE_F_ARMAZEM
    returning
      value(R_LFA1) type LFA1 .
  methods SET_TRANSPORTADORA
    importing
      !I_F_TRANSPORTE type ZDE_F_TRANSPORTE
    returning
      value(R_LFA1) type LFA1 .
  methods FREE .
  methods SET_ITEM_MATERIAL
    importing
      !I_PROD_ITEM type J_1BITMNUM
      !I_MATNR type MATNR
      !I_EBELN type EBELN
      !I_EBELP type EBELP
      !I_MENGE type J_1BNETQTY
      !I_MEINS type J_1BNETUNT
      !I_NETPR type J_1BNETPRI optional
      !I_NETWR type J_1BNETVAL optional
      !I_VBELN_VL type VBELN_VL optional
      !I_POSNR_VL type POSNR_VL optional
      !I_LGORT type LGORT_D optional
    exceptions
      ERRO .
  methods SET_ITEM_MATERIAL_PRECO
    importing
      !I_PROD_ITEM type J_1BITMNUM
      !I_PRECO_PEDIDO type ZDE_NFE_DIST_ITM_PRECO_PED
    exceptions
      ERRO .
  methods SET_ITEM_FATURA
    importing
      !I_PROD_ITEM type J_1BITMNUM
      !I_BELNR type BELNR_D
      !I_GJAHR type GJAHR
      !I_BUZEI type RBLGP .
  methods SET_CK_POSSUI_FRETE
    importing
      !I_CK_POSSUI_FRETE type ZDE_CK_POSSUI_FRETE .
  methods SET_ACEITAR_FISICO
    importing
      !I_PRECO_NOTA_FISCAL type CHAR01 default ABAP_FALSE
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods SET_ACEITAR_FATURAR
    importing
      !I_CK_SOMENTE_VALIDAR type CHAR01 optional
      !I_CK_RETORNO_SEM_AJUSTE type CHAR01 optional
      !I_US_MIRO type UNAME optional
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods SET_ACEITAR_DOCUMENTO
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods NFE_INBOUND_REJEITA_ACEITE
    importing
      !I_MOTIVO type ZDE_MOTIVO_REJEICAO_FISCAL
      !I_TP_AUTORIZACAO type ZDE_TP_AUTORIZACAO
      !I_TLINE type TLINE_T
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods NFE_INBOUND_CANCELA_FATURA
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_MIRO_EXCEPTION
      ZCX_CADASTRO .
  methods SET_REGISTRO_OUTROS
    importing
      !I_FORNE_CNPJ type ZDE_FORN_CNPJ
      !I_FORNE_CPF type ZDE_FORN_CPF
      !I_FORNE_IE type ZDE_FORN_INSC_ES
      !I_NUMERO type ZDE_NR_NFE
      !I_SERIE type J_1BSERIES
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods SET_ITEM_MATERIAL_EMISSOR .
  methods GET_REMESSA_NFE_INBOUND
    exporting
      !E_VTTK type VTTK
      !E_VFKP type VFKP
    returning
      value(R_LIKP) type LIKP .
  methods GET_IVA_PARAMETRIZADO_NFE
    returning
      value(R_MWSKZ) type MWSKZ
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_CARATERISTICAS
    importing
      !I_CLASS type KLASSE_D
      !I_CLASSTYPE type KLASSENART
      !I_OBJECT type OBJNUM
    returning
      value(R_CARACTERISTICAS) type ZDE_CLOBJDAT_T
    raising
      ZCX_CHARG_EXCEPTION .
  methods ADD_LOTE_ITEM
    importing
      !I_PROD_ITEM type J_1BITMNUM
    exporting
      !E_CARACTERISTICAS type ZIB_NFE_DIST_LCA_T
    returning
      value(R_LOTE) type ZIB_NFE_DIST_LOT
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CHARG_EXCEPTION
      ZCX_CADASTRO .
  methods ADD_LOTE_PEDIDO_ITEM
    importing
      !I_EBELN type EBELN
      !I_EBELP type EBELP
    exporting
      !E_CARACTERISTICAS type ZIB_NFE_DIST_LCA_T
    returning
      value(R_LOTE) type ZIB_NFE_DIST_LOT
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CHARG_EXCEPTION
      ZCX_CADASTRO .
  methods SET_LOTE_ITEM
    changing
      !I_LOTE_CARACT type ZIB_NFE_DIST_LCA_T
      !I_LOTE type ZIB_NFE_DIST_LOT .
  methods DESBLOQUEAR_OBJETO .
  methods ADD_ITEM
    importing
      !I_PROD_ITEM_BASE type J_1BITMNUM
      !I_PROD_QTD_COMERCI type J_1BNETQTY
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods EXCLUIR_ITEM
    importing
      !I_PROD_ITEM_BASE type J_1BITMNUM
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_OBRIGATORIO_NR_FASE
    returning
      value(R_CK_OBRIGATORIO) type CHAR01
    raising
      ZCX_CADASTRO .
  methods SET_NR_FASE
    importing
      !I_NR_FASE type ZDE_NR_FASE .
  methods SET_DT_VENCIMENTO
    importing
      !I_DT_VENCIMENTO type ZDE_DT_VENCIMENTO .
  methods SET_BANCO_PARCEIRO
    importing
      !I_BVTYP type BVTYP .
  methods GET_VALOR_NOTA_FISCAL_FATURA
    exporting
      !E_WAERS type WAERS
      !E_WKURS type WKURS
      !E_KUFIX type KUFIX
      !E_SINAL type CHAR01
      !E_VALOR_TOTAL type BAPI_RMWWR
      !E_VALOR_PRODUTOS type BAPI_WSKTO
      !E_ZTERM type DZTERM .
  methods GET_CONFIG_TIPO_PEDIDO
    returning
      value(R_ZMMT0075) type ZMMT0075
    raising
      ZCX_CADASTRO .
  methods SET_CTR_VALOR_TOTAL
    importing
      !I_CTR_VALOR_TOTAL type ZDE_VLR_TOTAL_CTR
    returning
      value(R_CTR_WKURS) type WKURS .
  methods SET_BLOQUEIO_PAGAMENTO
    importing
      !I_ZLSPR type DZLSPR .
  methods GET_POSSUI_ADIANTAMENTO
    returning
      value(R_CK_POSSUI) type CHAR01 .
  methods SET_MEIO_DE_PAGAMENTO
    importing
      !I_MEIO_PAGAMENTO type DZLSCH .
  methods SET_BANCO_EMPRESA
    importing
      !I_BANCO_EMPRESA type HBKID .
  methods SET_CK_GERAR_SOMENTE_AVISO .
  methods SET_PC_PARTINER
    importing
      !I_PC_PARTINER type LIFNR .
  methods SET_LR_PARTINER
    importing
      !I_LR_PARTINER type KUNNR .
  class-methods NFE_INBOUND_CAN_MIGO_MIRO
    importing
      !I_VBELN type VBELN_VL
    returning
      value(R_CANCELOU) type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO
      ZCX_PEDIDO_COMPRA_EXCEPTION
      ZCX_MIRO_EXCEPTION .
  methods GET_CFOP_ESCRITURACAO_ENTRADA
    importing
      !I_PROD_ITEM type J_1BITMNUM optional
      !I_GRAVAR_LOG type CHAR01 default ' '
    returning
      value(R_CFOP) type J_1BCFOP
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_COLETA_TUDO
    importing
      !I_CK_COLETA_TUDO type CHAR01 .
  methods GET_ALTEROU_PEDIDO_COMPRA
    returning
      value(R_ALTEROU) type CHAR01 .
  methods SET_VLR_DESCONTO
    importing
      !I_VLR_DESCONTO type ZDE_VLR_DESCONTO_CTR .
  methods SET_OBS_FINANCEIRA
    importing
      !I_OBS_FINANCEIRA type ZDE_OBS_FINANCEIRA_CTR .
  methods SET_BOLETO
    importing
      !I_BOLETO type ZDE_NR_BOLETO .
  class-methods GET_CK_RECEBIMENTO_BURRO
    importing
      !I_FILIAL type J_1BBRANC_
    returning
      value(R_RECEBIMENTO_BURRO) type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods MOD_PEDIDO_NOTA
    returning
      value(R_VAZIO) type CHAR1
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods ADD_PEDIDO_NOTA
    importing
      !I_PEDIDO type ZIB_NFE_DIST_PED
      !I_EXCLUIR type CHAR01 default ' '
    returning
      value(R_ALV) type ZDE_NFE_DIST_PED_ALV
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_CK_ALTEROU_IVA
    returning
      value(R_ALTEROU_IVA) type CHAR01 .
  methods GET_CABECALHO_NOTA
    returning
      value(R_NOTA) type ZIB_NFE_DIST_TER .
  class-methods DANFE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_CHAMAR_BROWSER type CHAR01 default 'X'
    exporting
      !E_URL type STRING
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  class-methods GET_VALIDA_ITEM_PEDIDO
    importing
      !I_NFE type ZIB_NFE_DIST_TER
      !I_EBELN type EBELN
      !I_EBELP type EBELP
      !I_GERAR_EXCEPTION type CHAR01 default ' '
    exporting
      !E_VALIDADO type CHAR01
      !E_MSG type STRING
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_ARM_ENVIO_ACEITE
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_CK_POSSUI_EDI_ARMAZENAGEM
    returning
      value(R_POSSUI) type CHAR01 .
  methods SET_ARM_ENVIAR_ACEITE_OPERACAO
    importing
      !I_CANCELAR type CHAR01 default ABAP_FALSE .
  methods NFE_INBOUND_SAIDA_DEVOLUCAO
    importing
      !I_TOTAL type CHAR01 default ABAP_TRUE
    exporting
      !E_MBLNR_DEV type MBLNR
      !E_MJAHR_DEV type MJAHR
      !E_BELNR_DEV type RE_BELNR
      !E_GJAHR_DEV type GJAHR
      !E_DOCNUM_DEV type J_1BDOCNUM
      !E_ZMMT0114 type ZMMT0114
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods NFE_INBOUND_SAIDA_ARMAZENAGEM
    importing
      !I_TOTAL type CHAR01 default ABAP_TRUE
    exporting
      !E_ZMMT0114 type ZMMT0114
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_CFOP_ARMAZENAGEM
    returning
      value(R_CFOP) type J_1BCFOP .
  methods NFE_ARM_ATUALIZA_CFOP_FISCAL .
  methods GET_CK_GERAR_SAIDA_ARMAZEM
    exporting
      !CK_ENVIA_ACEITE type CHAR01
    returning
      value(R_GERA_SAIDA) type CHAR01 .
  methods NFE_INBOUND_CAN_SAIDA_DEVOLU
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods NFE_INBOUND_CAN_SAIDA_ARMAZEM
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods REFRESH .
  methods GET_CK_CFOP_RETORNO_ARMAZENA
    returning
      value(R_CFOP_RETORNO) type CHAR01 .
  methods SET_DADOS_RETORNO
    importing
      !I_RETORNO type ZIB_NFE_DIST_RET_T .
  methods GET_DADOS_RETORNO
    exporting
      !E_RETORNO type ZIB_NFE_DIST_RET_T .
  methods SET_AUT_EMBARQUE
    importing
      value(I_AUT_EMBARQUE) type ZDE_AUT_EMBARQUE optional .
  methods SET_VALIDAR_AUT_EMBARQUE
    importing
      value(I_AUT_EMBARQUE) type ZDE_AUT_EMBARQUE optional
      !I_ULTIMO type CHAR1 optional
    exporting
      value(E_SEQ_CAM) type NUMC3
      value(E_NRO_SOL) type ZDE_NRO_SOL
      value(E_SEQ) type NUMC3
      value(E_FILIAL_RESP) type VKBUR
      value(E_AUT_EMBARQUE_SAIDA) type ZDE_AUT_EMBARQUE
      value(T_TAB_EMBARQUE) type ZSDS060_T
      value(T_TAB_EMBSUGST) type ZSDS060_T
    returning
      value(R_AUT_EMBARQUE) type ZDE_AUT_EMBARQUE .
  methods GRAVAR_ROMANEIO_FERTILIZANTE .
  methods SET_PLACA_CAV
    importing
      value(I_PLACA_CAV) type ZPLACA optional .
  methods SET_CK_RETORNO_SEM_AJUSTE
    importing
      !I_MESSAGE_V1 type SYST_MSGV optional
      !I_MESSAGE_V2 type SYST_MSGV optional
      !I_MESSAGE_V3 type SYST_MSGV optional
      !I_MESSAGE_V4 type SYST_MSGV optional .
  methods CHECK_OBLIGATORY_CHARG
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  PROTECTED SECTION.
private section.

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .
  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  data ST_FISCAL_ANTERIOR type ZDE_ST_NFE_FISCAL .
  data CTE type ZIB_CTE_DIST_TER .
  data NOTA type ZIB_NFE_DIST_TER .
  data ITENS type ZIB_NFE_DIST_ITM_T .
  data VOLUMES_TRANSP type ZIB_NFE_DIST_TVO_T .
  data LOTES type ZIB_NFE_DIST_LOT_T .
  data LOTES_CARACTERISTICAS type ZIB_NFE_DIST_LCA_T .
  data LOGS type ZDE_NFE_DIST_LOG_T .
  data PEDIDOS type ZDE_NFE_DIST_PED_T .
  data DEPARTAMENTO type ref to ZCL_MM_DEPARTAMENTO .
  data FRETE type ZIB_NFE_DIST_FRT .
  data DADOS_RETORNO type ZIB_NFE_DIST_RET_T .
  data LC_SEQUENCIA type ZDE_SEQ_LOG .
  data CK_ELIMINAR_LOG type CHAR01 .
  data CK_ESTORNANDO_FISICO type CHAR01 .
  data AT_CHAVES_LOCK type ZDE_CHAVE_DOC_E_T .
  data CK_GERAR_SOMENTE_AVISO type CHAR01 .
  data CK_ACEITE_FISCAL type CHAR01 .
  data CK_ACEITE_FISICO type CHAR01 .
  data CK_ACEITE_FATURAR type CHAR01 .
  data CK_SOMENTE_VALIDAR_FATURA type CHAR01 .
  data CK_ALTEROU_LOTES type CHAR01 .
  data CK_COLETA_TUDO type CHAR01 .
  data CK_ALTEROU_PEDIDO_COMPRA type CHAR01 .
  data CK_NAO_ESTORNAR_AVISO type CHAR01 .
  data CK_SEM_BLOQUEIO_REGISTRO type CHAR01 .
  data CK_PRECO_NOTA_FISCAL type CHAR01 .
  data CK_RETORNO_SEM_AJUSTE type CHAR1 .
  data CK_TAXA_APROVACAO type CHAR1 .
  data AT_SIMULAR type CHAR1 .

  methods SET_NR_PEDIDO_COMPRA_INF_XML
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_NR_FATURA
    importing
      !I_BELNR type RE_BELNR
      !I_GJAHR type GJAHR
      !I_RSEG type J_1B_TT_RSEG .
  methods SET_NR_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM .
  methods SET_NR_PEDIDO_COMPRA
    importing
      !I_EBELN type EBELN .
  methods SET_NR_PEDIDO_COMPRA_CLEAR .
  methods SET_NR_PEDIDO_COMPRA_ITENS_EX
    importing
      !I_RSEG type J_1B_TT_RSEG .
  methods SET_NR_DOCUMENTO_FATURA
    importing
      !I_BELNR type RE_BELNR
      !I_GJAHR type GJAHR .
  methods SET_NR_DOC_MATERIAL_CLEAR .
  methods NFE_INBOUND_GRAVAR_LOG .
  methods SET_ADD_LOG_NFE
    importing
      !I_TRANSACAO type TCODE optional
      !I_TYPE type BAPI_MTYPE
      !I_ID type SYMSGID default 'ZNFE_DISTRI'
      !I_NUM type SYMSGNO
      !I_MESSAGE_V1 type SYST_MSGV optional
      !I_MESSAGE_V2 type SYST_MSGV optional
      !I_MESSAGE_V3 type SYST_MSGV optional
      !I_MESSAGE_V4 type SYST_MSGV optional
      !I_ESTRATEGIA type CHAR01 optional
      !I_CD_APROVACAO type ZDE_EST_APROVACAO optional
      !I_DT_ATUALIZACAO type ERDAT default SY-DATUM
      !I_HR_ATUALIZACAO type ERZET default SY-UZEIT
    changing
      value(P_LC_SEQUENCIA) type ZDE_SEQ_LOG .
  methods SET_ADD_LOG_NFE_ERRO
    importing
      !I_ERRO type ref to ZCX_NFE_INBOUND_EXCEPTION .
  methods NFE_INBOUND_CRIA_LOTES
    raising
      ZCX_CHARG_EXCEPTION .
  methods SET_MATERIAL
    importing
      !I_EMISSOR type LIFNR
      !I_PROD_CODIGO type ZDE_PROD_CODIGO
      !I_UNIDADE_ITEM type ZDE_UND_COMERCI
      !I_MATNR type MATNR
      !I_MEINS type J_1BNETUNT
      !I_FATOR type ZDE_NFE_FATOR .
  methods GET_DEPARTAMENTO_ITENS
    importing
      !I_ITENS type ZIB_NFE_DIST_ITM_T
    returning
      value(R_CD_DEPARTAMENTO) type ZDE_DEPARTAMENTO
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods GET_SEQUENCIA_LOG
    returning
      value(R_LC_SEQUENCIA) type ZDE_SEQ_LOG .
  methods SET_PEDIDO_COMPRA
    importing
      !I_PROD_ITEM type J_1BITMNUM
      !I_EBELN type EBELN
      !I_EBELP type EBELP .
  methods SET_ST_FISCAL
    importing
      !I_ST_FISCAL type ZDE_ST_NFE_FISCAL .
  methods SET_ST_FISICO
    importing
      !I_ST_FISICO type ZDE_ST_NFE_FISICO .
  methods SET_ST_ARMAZEM
    importing
      !I_ST_ARMAZEM type ZDE_ST_NFE_ARMAZEM .
  methods NFE_INBOUND_REMESSA
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DELIVERY
      ZCX_CADASTRO .
  methods NFE_INBOUND_FATURA
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_CADASTRO
      ZCX_MIRO_EXCEPTION .
  methods SET_NR_REMESSA
    importing
      !I_REMESSA type VBELN_VL .
  methods SET_NR_REMESSA_CLEAR .
  methods SET_NR_TRANSPORTE
    importing
      !I_TKNUM type TKNUM .
  methods SET_NR_DOC_CUSTO
    importing
      !I_FKNUM type FKNUM .
  methods SET_INFORMACOES_EXISTENTES
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods SET_DOCNUM_NFE
    importing
      !I_DOCNUM type J_1BDOCNUM .
  methods SET_CK_FISCAL
    importing
      !I_CHECK type ZDE_CK_FISCAL .
  methods SET_CK_FISICO
    importing
      !I_CHECK type ZDE_CK_FISICO .
  methods SET_CK_ARMAZEM
    importing
      !I_CHECK type ZDE_CK_ARMAZEM .
  methods SET_IVA
    importing
      !I_MWSKZ type MWSKZ .
  methods NFE_INBOUND_CRIA_ROM_SAIDA
    raising
      ZCX_CADASTRO
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_CD_ROMANEIO
    importing
      !I_CD_ROMANEIO type ZCH_REF .
  methods GET_AVISO_VALIDO
    returning
      value(R_LIKP) type LIKP .
  methods GET_MIGO_VALIDA
    returning
      value(R_MIGO) type MKPF
    raising
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods GET_MIRO_VALIDA
    returning
      value(R_MIRO) type RBKP .
  methods GET_ROMANEIO
    returning
      value(R_ZSDT0001) type ZSDT0001 .
  methods SET_CTR_WKURS
    importing
      !I_CTR_WKURS type WKURS .
  methods NFE_ATUALIZA_LOG_APROVACAO
    importing
      !I_CD_APROVACAO type ZDE_EST_APROVACAO optional .
  methods SET_ST_DOCUMENTO
    importing
      !I_ST_DOCUMENTO type ZDE_ST_NFE_DOCUMENTO .
  methods SET_COMPRA_FUTURA .
  methods SET_CK_COMPRA_FUTURA
    importing
      !I_CK_COMPRA_FUTURA type ZDE_CK_COMPRA_FUTURA .
  methods SET_TP_COMPRA_FUTURA
    importing
      !I_TP_COMPRA_FUTURA type ZDE_TP_COMPRA_FUTURA .
  methods NFE_ATUALIZA_CFOP_NO_FISCAL .
  methods SET_DEPARTAMENTO_NOTA
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CADASTRO .
  methods GET_CATEGORIA_NOTA_FISCAL
    returning
      value(R_CATEGORIA) type J_1BNFTYPE .
  methods SET_ALTEROU_PEDIDO_COMPRA
    raising
      ZCX_CADASTRO .
  methods SET_CK_NAO_ESTORNAR_AVISO
    importing
      !I_CK_NAO_ESTORNAR_AVISO type CHAR01 .
  methods SET_SALVAR .
  methods SET_CALC_TOTAL_MOEDA_EMPRESA
    exporting
      !E_VALORES_PEDIDO type ZDE_NFE_INBOUND_VLR_CTR
      !E_ACHOU type CHAR01
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_CALC_TOTAL_MOEDA_EMP_ITEM
    importing
      !I_ITEM type ZIB_NFE_DIST_ITM
    exporting
      !E_VALORES_PEDIDO type ZDE_NFE_INBOUND_VLR_CTR
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods SET_REINICIA_ROMANEIO
    importing
      !I_AUT_EMBARQUE type ZIB_NFE_DIST_TER-AUT_EMBARQUE optional .
ENDCLASS.



CLASS ZCL_CTE_INBOUND IMPLEMENTATION.


  METHOD ADD_ITEM.

    DATA: NEW_ITEM  LIKE LINE OF ME->ITENS.

    READ TABLE ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>)
     WITH KEY PROD_ITEM = I_PROD_ITEM_BASE.

    CHECK SY-SUBRC IS INITIAL.

    IF I_PROD_QTD_COMERCI GE <FS_ITEM>-PROD_QTD_COMERCI.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_SEM_VOLUME_DISP-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_SEM_VOLUME_DISP-MSGNO
                            ATTR1 = CONV #( I_PROD_ITEM_BASE )
                            ATTR2 = CONV #( I_PROD_QTD_COMERCI ) )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_SEM_VOLUME_DISP-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_SEM_VOLUME_DISP-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_PROD_ITEM_BASE )
          MSGV2  = CONV #( I_PROD_QTD_COMERCI ).
    ENDIF.

    DESCRIBE TABLE ME->ITENS LINES DATA(QTD_LINHAS).
    ADD 1 TO QTD_LINHAS.

    ME->CK_ALTEROU = ABAP_TRUE.

    MOVE-CORRESPONDING <FS_ITEM> TO NEW_ITEM.
    IF <FS_ITEM>-PROD_ITEM_ORIGEM IS NOT INITIAL.
      NEW_ITEM-PROD_ITEM_ORIGEM = <FS_ITEM>-PROD_ITEM_ORIGEM.
    ELSE.
      NEW_ITEM-PROD_ITEM_ORIGEM = NEW_ITEM-PROD_ITEM.
    ENDIF.
    NEW_ITEM-PROD_ITEM        = QTD_LINHAS.
    NEW_ITEM-PROD_QTD_COMERCI = I_PROD_QTD_COMERCI.

    "Proporções
    NEW_ITEM-PROD_VLR_TOTAL_B   = <FS_ITEM>-PROD_VLR_TOTAL_B   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PROD_QTD_TRIB      = <FS_ITEM>-PROD_QTD_TRIB      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PROD_VL_FRETE      = <FS_ITEM>-PROD_VL_FRETE      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PROD_VL_SEGURO     = <FS_ITEM>-PROD_VL_SEGURO     * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PROD_VL_DESCONTO   = <FS_ITEM>-PROD_VL_DESCONTO   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PROD_VL_OUTRO      = <FS_ITEM>-PROD_VL_OUTRO      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_BASE          = <FS_ITEM>-ICMS_BASE          * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_AQT           = <FS_ITEM>-ICMS_AQT           * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_RED_BASE      = <FS_ITEM>-ICMS_RED_BASE      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_VALOR         = <FS_ITEM>-ICMS_VALOR         * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_ST_MARGEM     = <FS_ITEM>-ICMS_ST_MARGEM     * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_ST_RED_BASE   = <FS_ITEM>-ICMS_ST_RED_BASE   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_ST_AQT        = <FS_ITEM>-ICMS_ST_AQT        * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_ST_BASE       = <FS_ITEM>-ICMS_ST_BASE       * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_ST_VALOR      = <FS_ITEM>-ICMS_ST_VALOR      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-ICMS_VL_DESONERADO = <FS_ITEM>-ICMS_VL_DESONERADO * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-IPI_QTD_SELO_CON   = <FS_ITEM>-IPI_QTD_SELO_CON   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-IPI_BASE           = <FS_ITEM>-IPI_BASE           * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-IPI_QTD_TRIBUTAD   = <FS_ITEM>-IPI_QTD_TRIBUTAD   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-IPI_AQT            = <FS_ITEM>-IPI_AQT            * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-IPI_VALOR          = <FS_ITEM>-IPI_VALOR          * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_BASE           = <FS_ITEM>-PIS_BASE           * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_AQT            = <FS_ITEM>-PIS_AQT            * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_VALOR          = <FS_ITEM>-PIS_VALOR          * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_QTD_VENDIDA    = <FS_ITEM>-PIS_QTD_VENDIDA    * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_AQT_REAIS      = <FS_ITEM>-PIS_AQT_REAIS      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_ST_BASE        = <FS_ITEM>-PIS_ST_BASE        * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_ST_AQT         = <FS_ITEM>-PIS_ST_AQT         * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_ST_QTD_VENDI   = <FS_ITEM>-PIS_ST_QTD_VENDI   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_ST_AQT_REAIS   = <FS_ITEM>-PIS_ST_AQT_REAIS   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-PIS_ST_VALOR       = <FS_ITEM>-PIS_ST_VALOR       * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_BASE           = <FS_ITEM>-COF_BASE           * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_AQT            = <FS_ITEM>-COF_AQT            * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_VALOR          = <FS_ITEM>-COF_VALOR          * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_QTD_VENDIDA    = <FS_ITEM>-COF_QTD_VENDIDA    * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_AQT_REAIS      = <FS_ITEM>-COF_AQT_REAIS      * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_ST_BASE        = <FS_ITEM>-COF_ST_BASE        * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_ST_AQT         = <FS_ITEM>-COF_ST_AQT         * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_ST_QTD_VENDI   = <FS_ITEM>-COF_ST_QTD_VENDI   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_ST_AQT_REAIS   = <FS_ITEM>-COF_ST_AQT_REAIS   * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-COF_ST_VALOR       = <FS_ITEM>-COF_ST_VALOR       * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-MENGE              = <FS_ITEM>-MENGE              * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-NETPR              = <FS_ITEM>-NETPR              * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).
    NEW_ITEM-NETWR              = <FS_ITEM>-NETWR              * ( I_PROD_QTD_COMERCI / <FS_ITEM>-PROD_QTD_COMERCI ).

    "Reduz Volume do Item
    <FS_ITEM>-PROD_QTD_COMERCI   = <FS_ITEM>-PROD_QTD_COMERCI   - NEW_ITEM-PROD_QTD_COMERCI.
    <FS_ITEM>-PROD_VLR_TOTAL_B   = <FS_ITEM>-PROD_VLR_TOTAL_B   - NEW_ITEM-PROD_VLR_TOTAL_B.
    <FS_ITEM>-PROD_QTD_TRIB      = <FS_ITEM>-PROD_QTD_TRIB      - NEW_ITEM-PROD_QTD_TRIB.
    <FS_ITEM>-PROD_VL_FRETE      = <FS_ITEM>-PROD_VL_FRETE      - NEW_ITEM-PROD_VL_FRETE.
    <FS_ITEM>-PROD_VL_SEGURO     = <FS_ITEM>-PROD_VL_SEGURO     - NEW_ITEM-PROD_VL_SEGURO.
    <FS_ITEM>-PROD_VL_DESCONTO   = <FS_ITEM>-PROD_VL_DESCONTO   - NEW_ITEM-PROD_VL_DESCONTO.
    <FS_ITEM>-PROD_VL_OUTRO      = <FS_ITEM>-PROD_VL_OUTRO      - NEW_ITEM-PROD_VL_OUTRO.
    <FS_ITEM>-ICMS_BASE          = <FS_ITEM>-ICMS_BASE          - NEW_ITEM-ICMS_BASE.
    <FS_ITEM>-ICMS_AQT           = <FS_ITEM>-ICMS_AQT           - NEW_ITEM-ICMS_AQT.
    <FS_ITEM>-ICMS_RED_BASE      = <FS_ITEM>-ICMS_RED_BASE      - NEW_ITEM-ICMS_RED_BASE.
    <FS_ITEM>-ICMS_VALOR         = <FS_ITEM>-ICMS_VALOR         - NEW_ITEM-ICMS_VALOR.
    <FS_ITEM>-ICMS_ST_MARGEM     = <FS_ITEM>-ICMS_ST_MARGEM     - NEW_ITEM-ICMS_ST_MARGEM.
    <FS_ITEM>-ICMS_ST_RED_BASE   = <FS_ITEM>-ICMS_ST_RED_BASE   - NEW_ITEM-ICMS_ST_RED_BASE.
    <FS_ITEM>-ICMS_ST_AQT        = <FS_ITEM>-ICMS_ST_AQT        - NEW_ITEM-ICMS_ST_AQT.
    <FS_ITEM>-ICMS_ST_BASE       = <FS_ITEM>-ICMS_ST_BASE       - NEW_ITEM-ICMS_ST_BASE.
    <FS_ITEM>-ICMS_ST_VALOR      = <FS_ITEM>-ICMS_ST_VALOR      - NEW_ITEM-ICMS_ST_VALOR.
    <FS_ITEM>-ICMS_VL_DESONERADO = <FS_ITEM>-ICMS_VL_DESONERADO - NEW_ITEM-ICMS_VL_DESONERADO.
    <FS_ITEM>-IPI_QTD_SELO_CON   = <FS_ITEM>-IPI_QTD_SELO_CON   - NEW_ITEM-IPI_QTD_SELO_CON.
    <FS_ITEM>-IPI_BASE           = <FS_ITEM>-IPI_BASE           - NEW_ITEM-IPI_BASE.
    <FS_ITEM>-IPI_QTD_TRIBUTAD   = <FS_ITEM>-IPI_QTD_TRIBUTAD   - NEW_ITEM-IPI_QTD_TRIBUTAD.
    <FS_ITEM>-IPI_AQT            = <FS_ITEM>-IPI_AQT            - NEW_ITEM-IPI_AQT.
    <FS_ITEM>-IPI_VALOR          = <FS_ITEM>-IPI_VALOR          - NEW_ITEM-IPI_VALOR.
    <FS_ITEM>-PIS_BASE           = <FS_ITEM>-PIS_BASE           - NEW_ITEM-PIS_BASE.
    <FS_ITEM>-PIS_AQT            = <FS_ITEM>-PIS_AQT            - NEW_ITEM-PIS_AQT.
    <FS_ITEM>-PIS_VALOR          = <FS_ITEM>-PIS_VALOR          - NEW_ITEM-PIS_VALOR.
    <FS_ITEM>-PIS_QTD_VENDIDA    = <FS_ITEM>-PIS_QTD_VENDIDA    - NEW_ITEM-PIS_QTD_VENDIDA.
    <FS_ITEM>-PIS_AQT_REAIS      = <FS_ITEM>-PIS_AQT_REAIS      - NEW_ITEM-PIS_AQT_REAIS.
    <FS_ITEM>-PIS_ST_BASE        = <FS_ITEM>-PIS_ST_BASE        - NEW_ITEM-PIS_ST_BASE.
    <FS_ITEM>-PIS_ST_AQT         = <FS_ITEM>-PIS_ST_AQT         - NEW_ITEM-PIS_ST_AQT.
    <FS_ITEM>-PIS_ST_QTD_VENDI   = <FS_ITEM>-PIS_ST_QTD_VENDI   - NEW_ITEM-PIS_ST_QTD_VENDI.
    <FS_ITEM>-PIS_ST_AQT_REAIS   = <FS_ITEM>-PIS_ST_AQT_REAIS   - NEW_ITEM-PIS_ST_AQT_REAIS.
    <FS_ITEM>-PIS_ST_VALOR       = <FS_ITEM>-PIS_ST_VALOR       - NEW_ITEM-PIS_ST_VALOR.
    <FS_ITEM>-COF_BASE           = <FS_ITEM>-COF_BASE           - NEW_ITEM-COF_BASE.
    <FS_ITEM>-COF_AQT            = <FS_ITEM>-COF_AQT            - NEW_ITEM-COF_AQT.
    <FS_ITEM>-COF_VALOR          = <FS_ITEM>-COF_VALOR          - NEW_ITEM-COF_VALOR.
    <FS_ITEM>-COF_QTD_VENDIDA    = <FS_ITEM>-COF_QTD_VENDIDA    - NEW_ITEM-COF_QTD_VENDIDA.
    <FS_ITEM>-COF_AQT_REAIS      = <FS_ITEM>-COF_AQT_REAIS      - NEW_ITEM-COF_AQT_REAIS.
    <FS_ITEM>-COF_ST_BASE        = <FS_ITEM>-COF_ST_BASE        - NEW_ITEM-COF_ST_BASE.
    <FS_ITEM>-COF_ST_AQT         = <FS_ITEM>-COF_ST_AQT         - NEW_ITEM-COF_ST_AQT.
    <FS_ITEM>-COF_ST_QTD_VENDI   = <FS_ITEM>-COF_ST_QTD_VENDI   - NEW_ITEM-COF_ST_QTD_VENDI.
    <FS_ITEM>-COF_ST_AQT_REAIS   = <FS_ITEM>-COF_ST_AQT_REAIS   - NEW_ITEM-COF_ST_AQT_REAIS.
    <FS_ITEM>-COF_ST_VALOR       = <FS_ITEM>-COF_ST_VALOR       - NEW_ITEM-COF_ST_VALOR.
    <FS_ITEM>-MENGE              = <FS_ITEM>-MENGE              - NEW_ITEM-MENGE.
    <FS_ITEM>-NETPR              = <FS_ITEM>-NETPR              - NEW_ITEM-NETPR.
    <FS_ITEM>-NETWR              = <FS_ITEM>-NETWR              - NEW_ITEM-NETWR.

    "Inclui novo item
    APPEND NEW_ITEM TO ME->ITENS.

  ENDMETHOD.


  METHOD ADD_LOTE_ITEM.

    DATA: WA_CARACTERISRICA TYPE ZIB_NFE_DIST_LCA,
          LC_ITEM           TYPE ZDE_CD_LOTE_ITEM.

    CLEAR: R_LOTE, WA_CARACTERISRICA.

    "Busca Grupos do Departamento
    CREATE OBJECT DEPARTAMENTO.
    DEPARTAMENTO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
    DATA(IT_GRUPOS) = DEPARTAMENTO->GET_GRUPO_MERCADORIA( ).
    CLEAR: DEPARTAMENTO.

    "Busca Item
    READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM INTO DATA(WA_ITEM).
    CHECK SY-SUBRC IS INITIAL.

    LOOP AT ME->LOTES INTO DATA(WA_LOTE_SALDO) WHERE PROD_ITEM EQ I_PROD_ITEM.
      WA_ITEM-MENGE = WA_ITEM-MENGE - WA_LOTE_SALDO-MENGE.
    ENDLOOP.

    IF WA_ITEM-MENGE LE 0.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO-MSGID
                            ATTR1 = CONV #( I_PROD_ITEM ) )
          MSGTY  = 'E'
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO-MSGNO
          MSGV1  = CONV #( I_PROD_ITEM ).
    ENDIF.

    "Busca Material
    SELECT SINGLE * INTO @DATA(WA_MARA) FROM MARA WHERE MATNR EQ @WA_ITEM-MATNR.
    CHECK SY-SUBRC IS INITIAL.

    "Busca Grupo
    READ TABLE IT_GRUPOS WITH KEY MATKL = WA_MARA-MATKL INTO DATA(WA_GRUPO).
    CHECK SY-SUBRC IS INITIAL.

    "Busca Classe
    SELECT SINGLE * INTO @DATA(WA_KLAH) FROM KLAH WHERE CLINT EQ @WA_GRUPO-CLINT.
    CHECK SY-SUBRC IS INITIAL.

    "Busca item do Pedido de Compra
    SELECT SINGLE * INTO @DATA(WA_EKET) FROM EKET WHERE EBELN EQ @WA_ITEM-EBELN AND EBELP EQ @WA_ITEM-EBELP.

    "Busca Caracteristicas da Classe
    "Código para administração em lote obrigatória
    IF WA_MARA-XCHPF EQ ABAP_TRUE.
      DATA(R_T_OBJECTDATA) = ME->GET_CARATERISTICAS( I_CLASS = WA_KLAH-CLASS  I_CLASSTYPE = WA_KLAH-KLART I_OBJECT = CONV #( WA_ITEM-MATNR ) ).
    ENDIF.

    "Inclui um Lote na Sequencia
    CLEAR: LC_ITEM.
    LOOP AT ME->LOTES INTO DATA(WA_LOTE) WHERE CD_LOTE_ITEM(1) EQ '$'.
      CONCATENATE '0' WA_LOTE-CD_LOTE_ITEM+1(9) INTO WA_LOTE-CD_LOTE_ITEM.
      IF WA_LOTE-CD_LOTE_ITEM GE LC_ITEM.
        ADD 1 TO WA_LOTE-CD_LOTE_ITEM.
        LC_ITEM = WA_LOTE-CD_LOTE_ITEM.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_ITEM
          IMPORTING
            OUTPUT = LC_ITEM.
      ENDIF.
    ENDLOOP.
    IF LC_ITEM IS INITIAL.
      LC_ITEM = '0000000001'.
    ENDIF.

    "Dados Iniciais do Lote
    CONCATENATE '$' LC_ITEM+1(9) INTO R_LOTE-CD_LOTE_ITEM.
    R_LOTE-CHAVE_NFE = ME->NOTA-CHAVE_NFE.
    R_LOTE-PROD_ITEM = WA_ITEM-PROD_ITEM.
    R_LOTE-MATNR     = WA_ITEM-MATNR.
    R_LOTE-WERKS     = ME->NOTA-F_TOMADORA.
    R_LOTE-CLINT     = WA_KLAH-CLINT.
    R_LOTE-KLART     = WA_KLAH-KLART.
    R_LOTE-CLASS     = WA_KLAH-CLASS.
    R_LOTE-CHARG     = WA_EKET-CHARG.
    R_LOTE-MENGE     = WA_ITEM-MENGE.

    "Característicias do Lote (Vazias)
    LOOP AT R_T_OBJECTDATA INTO DATA(WA_OBJECTDATA).
      CLEAR: WA_CARACTERISRICA.
      WA_CARACTERISRICA-CD_LOTE_ITEM = R_LOTE-CD_LOTE_ITEM.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          INPUT  = WA_OBJECTDATA-ATNAM
        IMPORTING
          OUTPUT = WA_OBJECTDATA-ATIMB.

      WA_CARACTERISRICA-ATINN        = WA_OBJECTDATA-ATIMB.
      WA_CARACTERISRICA-ATNAM        = WA_OBJECTDATA-ATNAM.
      WA_CARACTERISRICA-SMBEZ        = WA_OBJECTDATA-SMBEZ.
      WA_CARACTERISRICA-ATWRT        = ''.
      APPEND WA_CARACTERISRICA TO ME->LOTES_CARACTERISTICAS.
    ENDLOOP.

    "Retorna Lote e Características
    APPEND R_LOTE TO ME->LOTES.
    MOVE ME->LOTES_CARACTERISTICAS[] TO E_CARACTERISTICAS[].
    ME->CK_ALTEROU_LOTES = ABAP_TRUE.

  ENDMETHOD.


  METHOD ADD_LOTE_PEDIDO_ITEM.

    DATA: WA_CARACTERISRICA TYPE ZIB_NFE_DIST_LCA,
          LC_ITEM           TYPE ZDE_CD_LOTE_ITEM.

    CLEAR: R_LOTE, WA_CARACTERISRICA.

    "Busca Grupos do Departamento
    CREATE OBJECT DEPARTAMENTO.
    DEPARTAMENTO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
    DATA(IT_GRUPOS) = DEPARTAMENTO->GET_GRUPO_MERCADORIA( ).
    CLEAR: DEPARTAMENTO.

    "Busca Item
    READ TABLE ME->PEDIDOS WITH KEY EBELN = I_EBELN EBELP = I_EBELP INTO DATA(WA_PEDIDO_ITEM).
    CHECK SY-SUBRC IS INITIAL.

    LOOP AT ME->LOTES INTO DATA(WA_LOTE_SALDO) WHERE EBELN = I_EBELN AND EBELP = I_EBELP.
      WA_PEDIDO_ITEM-MENGE = WA_PEDIDO_ITEM-MENGE - WA_LOTE_SALDO-MENGE.
    ENDLOOP.

    IF WA_PEDIDO_ITEM-MENGE LE 0.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO_PED-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO_PED-MSGID
                            ATTR1 = CONV #( I_EBELN )
                            ATTR2 = CONV #( I_EBELP ) )
          MSGTY  = 'E'
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO_PED-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_SALDO_PARTICAO_PED-MSGNO
          MSGV1  = CONV #( I_EBELN )
          MSGV2  = CONV #( I_EBELP ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_EKPO)
      FROM EKPO
     WHERE EBELN EQ @I_EBELN
       AND EBELP EQ @I_EBELP.

    "Busca Material
    SELECT SINGLE * INTO @DATA(WA_MARA) FROM MARA WHERE MATNR EQ @WA_PEDIDO_ITEM-MATNR.
    CHECK SY-SUBRC IS INITIAL.

    "Busca Grupo
    READ TABLE IT_GRUPOS WITH KEY MATKL = WA_MARA-MATKL INTO DATA(WA_GRUPO).
    IF WA_MARA-XCHPF EQ ABAP_TRUE AND WA_EKPO-KNTTP IS INITIAL AND SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGID
                            ATTR1 = CONV #( WA_MARA-MATKL ) )
          MSGTY  = 'E'
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGNO
          MSGV1  = CONV #( WA_MARA-MATKL ).
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    "Busca Classe
    SELECT SINGLE * INTO @DATA(WA_KLAH) FROM KLAH WHERE CLINT EQ @WA_GRUPO-CLINT.

    IF WA_MARA-XCHPF EQ ABAP_TRUE AND WA_EKPO-KNTTP IS INITIAL AND SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGID
                            ATTR1 = CONV #( WA_MARA-MATKL ) )
          MSGTY  = 'E'
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_CLASSE_ZMM0111-MSGNO
          MSGV1  = CONV #( WA_MARA-MATKL ).
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    "Busca item do Pedido de Compra
    SELECT SINGLE * INTO @DATA(WA_EKET) FROM EKET WHERE EBELN EQ @WA_PEDIDO_ITEM-EBELN AND EBELP EQ @WA_PEDIDO_ITEM-EBELP.

    "Busca Caracteristicas da Classe
    "Código para administração em lote obrigatória
    IF WA_MARA-XCHPF EQ ABAP_TRUE.
      DATA(R_T_OBJECTDATA) = ME->GET_CARATERISTICAS( I_CLASS = WA_KLAH-CLASS  I_CLASSTYPE = WA_KLAH-KLART I_OBJECT = CONV #( WA_PEDIDO_ITEM-MATNR ) ).
    ENDIF.

    "Inclui um Lote na Sequencia
    CLEAR: LC_ITEM.
    LOOP AT ME->LOTES INTO DATA(WA_LOTE) WHERE CD_LOTE_ITEM(1) EQ '$'.
      CONCATENATE '0' WA_LOTE-CD_LOTE_ITEM+1(9) INTO WA_LOTE-CD_LOTE_ITEM.
      IF WA_LOTE-CD_LOTE_ITEM GE LC_ITEM.
        ADD 1 TO WA_LOTE-CD_LOTE_ITEM.
        LC_ITEM = WA_LOTE-CD_LOTE_ITEM.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_ITEM
          IMPORTING
            OUTPUT = LC_ITEM.
      ENDIF.
    ENDLOOP.
    IF LC_ITEM IS INITIAL.
      LC_ITEM = '0000000001'.
    ENDIF.

    "Dados Iniciais do Lote
    CONCATENATE '$' LC_ITEM+1(9) INTO R_LOTE-CD_LOTE_ITEM.
    R_LOTE-CHAVE_NFE = ME->NOTA-CHAVE_NFE.
    "R_LOTE-PROD_ITEM = WA_ITEM-PROD_ITEM.
    R_LOTE-MATNR     = WA_PEDIDO_ITEM-MATNR.
    R_LOTE-WERKS     = ME->NOTA-F_TOMADORA.
    R_LOTE-CLINT     = WA_KLAH-CLINT.
    R_LOTE-KLART     = WA_KLAH-KLART.
    R_LOTE-CLASS     = WA_KLAH-CLASS.
    R_LOTE-CHARG     = WA_EKET-CHARG.
    R_LOTE-MENGE     = WA_PEDIDO_ITEM-MENGE.
    R_LOTE-EBELN     = WA_PEDIDO_ITEM-EBELN.
    R_LOTE-EBELP     = WA_PEDIDO_ITEM-EBELP.

    "Característicias do Lote (Vazias)
    LOOP AT R_T_OBJECTDATA INTO DATA(WA_OBJECTDATA).
      CLEAR: WA_CARACTERISRICA.
      WA_CARACTERISRICA-CD_LOTE_ITEM = R_LOTE-CD_LOTE_ITEM.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          INPUT  = WA_OBJECTDATA-ATNAM
        IMPORTING
          OUTPUT = WA_OBJECTDATA-ATIMB.

      WA_CARACTERISRICA-ATINN        = WA_OBJECTDATA-ATIMB.
      WA_CARACTERISRICA-ATNAM        = WA_OBJECTDATA-ATNAM.
      WA_CARACTERISRICA-SMBEZ        = WA_OBJECTDATA-SMBEZ.
      WA_CARACTERISRICA-ATWRT        = ''.
      APPEND WA_CARACTERISRICA TO ME->LOTES_CARACTERISTICAS.
    ENDLOOP.

    "Retorna Lote e Características
    APPEND R_LOTE TO ME->LOTES.
    MOVE ME->LOTES_CARACTERISTICAS[] TO E_CARACTERISTICAS[].
    ME->CK_ALTEROU_LOTES = ABAP_TRUE.

  ENDMETHOD.


  METHOD ADD_PEDIDO_NOTA.

    DATA: rg_kscha TYPE RANGE OF kscha.

    DATA: lc_kposn TYPE kposn,
          lc_kwert TYPE kwert.

    CASE i_excluir.
      WHEN abap_false.

        SELECT SINGLE * INTO @DATA(wa_ekko) "MWSKZ PEINH INTO @DATA(LC_IVA)
          FROM ekko
         WHERE ebeln EQ @i_pedido-ebeln.

        lc_kposn = i_pedido-ebelp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_kposn
          IMPORTING
            output = lc_kposn.

        rg_kscha = VALUE #( option = 'EQ' sign = 'I' ( low = 'HB01' high = 'HB01' ) ).

        SELECT FROM v_konv FIELDS * WHERE knumv EQ @wa_ekko-knumv AND kposn EQ @lc_kposn AND kschl IN @rg_kscha INTO TABLE @DATA(it_konv) .

        SELECT SINGLE * INTO @DATA(wa_ekpo) "MWSKZ PEINH INTO @DATA(LC_IVA)
          FROM ekpo
         WHERE ebeln EQ @i_pedido-ebeln
           AND ebelp EQ @i_pedido-ebelp.

        lc_kwert = 0.
        LOOP AT it_konv INTO DATA(wa_konv).
          ADD wa_konv-kwert TO lc_kwert.
        ENDLOOP.

        READ TABLE me->pedidos ASSIGNING FIELD-SYMBOL(<item>)
        WITH KEY ebeln = i_pedido-ebeln
                 ebelp = i_pedido-ebelp.

        IF sy-subrc IS INITIAL.

          IF i_pedido-menge NE <item>-menge.
            me->ck_alterou = abap_true.
            <item>-menge = i_pedido-menge.
            IF me->nota-vl_desconto IS INITIAL.
* ---> CS1057218/IR124176
*              <item>-total = ( i_pedido-menge * ( <item>-netpr / wa_ekpo-peinh  ) ) - ( i_pedido-menge * ( lc_kwert / i_pedido-menge ) ) .
              <item>-total = ( i_pedido-menge * ( wa_ekpo-netwr / wa_ekpo-menge  ) ) - ( i_pedido-menge * ( lc_kwert / i_pedido-menge ) ) .
* <--- CS1057218/IR124176
            ELSE.
* ---> CS1057218/IR124176
*              <item>-total = ( i_pedido-menge * ( <item>-netpr / wa_ekpo-peinh  ) ).
              <item>-total = ( i_pedido-menge * ( wa_ekpo-netwr / wa_ekpo-menge  ) ).
* <--- CS1057218/IR124176
            ENDIF.
            IF wa_ekpo-menge IS NOT INITIAL.
              <item>-navnw = i_pedido-menge * ( wa_ekpo-navnw / wa_ekpo-menge ).
            ENDIF.
          ENDIF.
          "CS2021000533
          IF wa_ekpo-netwr <> <item>-total AND <item>-menge EQ wa_ekpo-menge.
            <item>-total = wa_ekpo-netwr.
          ENDIF.
          "CS2021000533
          <item>-vfdat = i_pedido-vfdat.
          <item>-hsdat = i_pedido-hsdat.
          <item>-charg = i_pedido-charg.
          "
          "ALRS
          <item>-unload_pt  = i_pedido-unload_pt.
          <item>-gr_rcpt    = i_pedido-gr_rcpt.
          <item>-item_text  = i_pedido-item_text.
          "

        ELSE.

          me->ck_alterou = abap_true.
          DATA(lc_pedido) = i_pedido.
          lc_pedido-chave_nfe = me->nota-chave_nfe.
          IF me->nota-vl_desconto IS INITIAL.
            lc_pedido-total     = ( lc_pedido-menge * ( lc_pedido-netpr / wa_ekpo-peinh ) ) - ( lc_pedido-menge * ( lc_kwert / lc_pedido-menge ) ).
          ELSE.
            lc_pedido-total     = ( lc_pedido-menge * ( lc_pedido-netpr / wa_ekpo-peinh ) ).
          ENDIF.
          "CS2021000533
          IF wa_ekpo-netwr <> lc_pedido-total AND i_pedido-menge EQ wa_ekpo-menge.
            lc_pedido-total = wa_ekpo-netwr.
          ENDIF.
          "CS2021000533
          lc_pedido-navnw     = wa_ekpo-navnw.
          lc_pedido-mwskz     = wa_ekpo-mwskz.
          lc_pedido-vfdat     = i_pedido-vfdat.
          lc_pedido-hsdat     = i_pedido-hsdat.
          lc_pedido-charg     = i_pedido-charg.
          APPEND lc_pedido TO me->pedidos.

        ENDIF.

        MOVE-CORRESPONDING i_pedido TO r_alv.
        r_alv-total  = r_alv-menge * ( r_alv-netpr / wa_ekpo-peinh ) .
        IF wa_ekpo-menge IS NOT INITIAL.
          r_alv-navnw  = r_alv-menge * ( wa_ekpo-navnw / wa_ekpo-menge ).
        ENDIF.

        SELECT SINGLE maktx INTO r_alv-maktx
          FROM makt
         WHERE spras EQ sy-langu
           AND matnr EQ r_alv-matnr.

        r_alv-chave_nfe = me->nota-chave_nfe.

        IF wa_ekpo-mwskz IS NOT INITIAL.
          me->set_iva( i_mwskz = wa_ekpo-mwskz ).
        ENDIF.

        DATA(r_zmmt0075) = me->get_config_tipo_pedido( ).

        IF ( r_zmmt0075-zlspr IS NOT INITIAL AND me->nota-zlspr IS INITIAL ) OR ( me->nota-zlspr NE r_zmmt0075-zlspr ).
          me->set_bloqueio_pagamento( i_zlspr = r_zmmt0075-zlspr ).
        ENDIF.

      WHEN abap_true.

        DELETE me->pedidos WHERE ebeln EQ i_pedido-ebeln
                             AND ebelp EQ i_pedido-ebelp.

        IF me->pedidos IS INITIAL.
          CLEAR: wa_ekpo-mwskz.
          me->set_iva( i_mwskz = wa_ekpo-mwskz ).
        ENDIF.

    ENDCASE.

    IF me->nota-dt_vencimento IS INITIAL.

      READ TABLE me->pedidos INDEX 1 INTO lc_pedido.

      CHECK sy-subrc IS INITIAL.

      SELECT SINGLE * INTO @wa_ekko
        FROM ekko
       WHERE ebeln EQ @lc_pedido-ebeln.

      CHECK sy-subrc IS INITIAL.

      DATA: e_faedt TYPE rfpos-faedt.

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = me->nota-dt_emissao
          i_zbd1t = wa_ekko-zbd1t
          i_zbd2t = wa_ekko-zbd2t
          i_zbd3t = wa_ekko-zbd3t
          i_shkzg = space
          i_rebzg = space
          i_koart = space
        IMPORTING
          e_faedt = e_faedt.

      me->set_dt_vencimento( i_dt_vencimento = e_faedt ).

    ENDIF.

  ENDMETHOD.


  METHOD CHECK_OBLIGATORY_CHARG.

    DATA(lt_lotes) = me->lotes.
    SORT lt_lotes BY matnr werks charg licha.
    DELETE ADJACENT DUPLICATES FROM lt_lotes COMPARING matnr werks charg licha.

    SELECT *
      FROM zppt0016
      INTO TABLE @DATA(lt_zppt0016)
      FOR ALL ENTRIES IN @lt_lotes
      WHERE matnr = @lt_lotes-matnr
        AND werks = @lt_lotes-werks
        AND charg = @lt_lotes-charg.
    IF sy-subrc IS INITIAL.
      SORT lt_zppt0016 BY matnr werks charg.
    ENDIF.

    LOOP AT me->lotes ASSIGNING FIELD-SYMBOL(<fs_lotes>).
      IF <fs_lotes>-class EQ 'DEFENSIVOS' AND <fs_lotes>-klart EQ '023' AND <fs_lotes>-licha IS INITIAL.

        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_fab-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_fab-msgno
                              attr1 = CONV #( <fs_lotes>-class )
                              attr2 = CONV #( <fs_lotes>-klart )
                              attr3 = ''
                              attr4 = '' )
            msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_fab-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_fab-msgno
            msgv1  = CONV #( <fs_lotes>-class )
            msgv2  = CONV #( <fs_lotes>-klart )
            msgv3  = ''
            msgv4  = ''.
      ELSE.

        READ TABLE lt_zppt0016 ASSIGNING FIELD-SYMBOL(<fs_zppt0016>)
        WITH KEY matnr = <fs_lotes>-matnr
                 werks = <fs_lotes>-werks
                 charg = <fs_lotes>-charg
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF <fs_lotes>-licha NE <fs_zppt0016>-zlicha(15).

            RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
              EXPORTING
                textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_zlicha_existe-msgid
                                  msgno = zcx_nfe_inbound_exception=>zcx_zlicha_existe-msgno
                                  attr1 = CONV #( <fs_lotes>-charg )
                                  attr2 = CONV #( <fs_zppt0016>-zlicha )
                                  attr3 = ''
                                  attr4 = '' )
                msgid  = zcx_nfe_inbound_exception=>zcx_zlicha_existe-msgid
                msgno  = zcx_nfe_inbound_exception=>zcx_zlicha_existe-msgno
                msgv1  = CONV #( <fs_lotes>-charg )
                msgv2  = CONV #( <fs_zppt0016>-zlicha )
                msgv3  = ''
                msgv4  = ''.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    SUPER->CONSTRUCTOR( ).

    ME->CK_SEM_BLOQUEIO_REGISTRO = I_SEM_BLOQUEIO.

    IF I_CHAVE_NFE IS NOT INITIAL.
      ME->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = I_CHAVE_NFE ).
    ENDIF.

  ENDMETHOD.


  METHOD DANFE.
    DATA: LC_NODE_DATA TYPE BXMNODES-URL.

    SELECT SINGLE * INTO @DATA(WA_NOTA)
      FROM ZIB_NFE_DIST_TER
     WHERE CHAVE_NFE EQ @I_CHAVE_NFE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #(  MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CHAVE_NAO_ENCONTRADA-MSGNO
                             MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CHAVE_NAO_ENCONTRADA-MSGID
                             ATTR1 = CONV #( I_CHAVE_NFE ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CHAVE_NAO_ENCONTRADA-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CHAVE_NAO_ENCONTRADA-MSGID
          MSGV1  = CONV #( I_CHAVE_NFE ).
    ENDIF.

    IF WA_NOTA-URLDANFE IS NOT INITIAL.

      E_URL = WA_NOTA-URLDANFE.

      IF I_CHAMAR_BROWSER EQ ABAP_TRUE.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            NODE_DATA = LC_NODE_DATA.
      ENDIF.

    ELSEIF WA_NOTA-ID_SIMETRYA IS INITIAL.

      DATA: IT_URLLIST TYPE TIHTTPURLS2.

      CALL FUNCTION 'HTTP_GET_URL2'
        EXPORTING
          HANDLERCLASS     = 'ZCL_FMCALL_DOC_FISCAL'
        IMPORTING
          URLLIST          = IT_URLLIST
        EXCEPTIONS
          HTTP_NOT_ENABLED = 1
          OTHERS           = 2.

      CHECK SY-SUBRC IS INITIAL.

      READ TABLE IT_URLLIST INDEX 1 INTO DATA(WA_URLLIST).

      DATA(WA_DOMINIO) = WA_URLLIST-PROTOCOL && '://' && WA_URLLIST-HOST && ':' && WA_URLLIST-PORT && WA_URLLIST-URL.

      E_URL = WA_DOMINIO && '/getnfepdf?' && 'sap-client=' && SY-MANDT && '&i_chave=' && I_CHAVE_NFE.
      LC_NODE_DATA = E_URL.

      IF I_CHAMAR_BROWSER EQ ABAP_TRUE.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            NODE_DATA = LC_NODE_DATA.
      ENDIF.

*      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
*        EXPORTING
*          TEXTID = VALUE #(  MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ID_SIMETRYA-MSGNO
*                             MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ID_SIMETRYA-MSGID
*                             ATTR1 = CONV #( I_CHAVE_NFE ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ID_SIMETRYA-MSGNO
*          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ID_SIMETRYA-MSGID
*          MSGV1  = CONV #( I_CHAVE_NFE ).
    ELSE.

      DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
      CREATE OBJECT OB_WEB_SERVICE.

      TRY .
          OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'YN' ).
          OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'Y' ).
          "http://simetrya.grupomaggi.com.br:8080/nfe/monitorNfeEntradaDanfePdf?numrNfeSeqc=
          DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).

        CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #(  MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGNO
                                 MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGID )
              MSGTY  = 'E'
              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGNO
              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_WEBSERVICE_DANFE-MSGID.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_NOTA-ID_SIMETRYA
        IMPORTING
          OUTPUT = WA_NOTA-ID_SIMETRYA.

      LC_NODE_DATA = LC_URI && WA_NOTA-ID_SIMETRYA && ','.
      E_URL = LC_NODE_DATA.

      IF I_CHAMAR_BROWSER EQ ABAP_TRUE.
        CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
          EXPORTING
            NODE_DATA = LC_NODE_DATA.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD DESBLOQUEAR_OBJETO.

    LOOP AT ME->AT_CHAVES_LOCK INTO DATA(LC_CHAVE).

      CALL METHOD ZCL_NFE_INBOUND=>SET_DENQUEUE_NFE
        EXPORTING
          I_CHAVE                 = LC_CHAVE
          I_SEM_BLOQUEIO_REGISTRO = ME->CK_SOMENTE_VALIDAR_FATURA.

    ENDLOOP.

    CLEAR: ME->AT_CHAVES_LOCK.

  ENDMETHOD.


  METHOD EXCLUIR_ITEM.

    DATA: NEW_ITEM LIKE LINE OF ME->ITENS.

    READ TABLE ME->ITENS INTO DATA(WA_FAKE) WITH KEY PROD_ITEM = I_PROD_ITEM_BASE.

    CHECK SY-SUBRC IS INITIAL.

    IF WA_FAKE-PROD_ITEM_ORIGEM IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_ORIGINAL-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_ORIGINAL-MSGNO
                            ATTR1 = CONV #( I_PROD_ITEM_BASE ) )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_ORIGINAL-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ITEM_ORIGINAL-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_PROD_ITEM_BASE ).
    ENDIF.

    ME->CK_ALTEROU = ABAP_TRUE.

    READ TABLE ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM_ORIGINAL>) WITH KEY PROD_ITEM = WA_FAKE-PROD_ITEM_ORIGEM.

    "Retorna Volume do Item
    <FS_ITEM_ORIGINAL>-PROD_QTD_COMERCI   = <FS_ITEM_ORIGINAL>-PROD_QTD_COMERCI   + WA_FAKE-PROD_QTD_COMERCI.
    <FS_ITEM_ORIGINAL>-PROD_VLR_TOTAL_B   = <FS_ITEM_ORIGINAL>-PROD_VLR_TOTAL_B   + WA_FAKE-PROD_VLR_TOTAL_B.
    <FS_ITEM_ORIGINAL>-PROD_QTD_TRIB      = <FS_ITEM_ORIGINAL>-PROD_QTD_TRIB      + WA_FAKE-PROD_QTD_TRIB.
    <FS_ITEM_ORIGINAL>-PROD_VL_FRETE      = <FS_ITEM_ORIGINAL>-PROD_VL_FRETE      + WA_FAKE-PROD_VL_FRETE.
    <FS_ITEM_ORIGINAL>-PROD_VL_SEGURO     = <FS_ITEM_ORIGINAL>-PROD_VL_SEGURO     + WA_FAKE-PROD_VL_SEGURO.
    <FS_ITEM_ORIGINAL>-PROD_VL_DESCONTO   = <FS_ITEM_ORIGINAL>-PROD_VL_DESCONTO   + WA_FAKE-PROD_VL_DESCONTO.
    <FS_ITEM_ORIGINAL>-PROD_VL_OUTRO      = <FS_ITEM_ORIGINAL>-PROD_VL_OUTRO      + WA_FAKE-PROD_VL_OUTRO.
    <FS_ITEM_ORIGINAL>-ICMS_BASE          = <FS_ITEM_ORIGINAL>-ICMS_BASE          + WA_FAKE-ICMS_BASE.
    <FS_ITEM_ORIGINAL>-ICMS_AQT           = <FS_ITEM_ORIGINAL>-ICMS_AQT           + WA_FAKE-ICMS_AQT.
    <FS_ITEM_ORIGINAL>-ICMS_RED_BASE      = <FS_ITEM_ORIGINAL>-ICMS_RED_BASE      + WA_FAKE-ICMS_RED_BASE.
    <FS_ITEM_ORIGINAL>-ICMS_VALOR         = <FS_ITEM_ORIGINAL>-ICMS_VALOR         + WA_FAKE-ICMS_VALOR.
    <FS_ITEM_ORIGINAL>-ICMS_ST_MARGEM     = <FS_ITEM_ORIGINAL>-ICMS_ST_MARGEM     + WA_FAKE-ICMS_ST_MARGEM.
    <FS_ITEM_ORIGINAL>-ICMS_ST_RED_BASE   = <FS_ITEM_ORIGINAL>-ICMS_ST_RED_BASE   + WA_FAKE-ICMS_ST_RED_BASE.
    <FS_ITEM_ORIGINAL>-ICMS_ST_AQT        = <FS_ITEM_ORIGINAL>-ICMS_ST_AQT        + WA_FAKE-ICMS_ST_AQT.
    <FS_ITEM_ORIGINAL>-ICMS_ST_BASE       = <FS_ITEM_ORIGINAL>-ICMS_ST_BASE       + WA_FAKE-ICMS_ST_BASE.
    <FS_ITEM_ORIGINAL>-ICMS_ST_VALOR      = <FS_ITEM_ORIGINAL>-ICMS_ST_VALOR      + WA_FAKE-ICMS_ST_VALOR.
    <FS_ITEM_ORIGINAL>-ICMS_VL_DESONERADO = <FS_ITEM_ORIGINAL>-ICMS_VL_DESONERADO + WA_FAKE-ICMS_VL_DESONERADO.
    <FS_ITEM_ORIGINAL>-IPI_QTD_SELO_CON   = <FS_ITEM_ORIGINAL>-IPI_QTD_SELO_CON   + WA_FAKE-IPI_QTD_SELO_CON.
    <FS_ITEM_ORIGINAL>-IPI_BASE           = <FS_ITEM_ORIGINAL>-IPI_BASE           + WA_FAKE-IPI_BASE.
    <FS_ITEM_ORIGINAL>-IPI_QTD_TRIBUTAD   = <FS_ITEM_ORIGINAL>-IPI_QTD_TRIBUTAD   + WA_FAKE-IPI_QTD_TRIBUTAD.
    <FS_ITEM_ORIGINAL>-IPI_AQT            = <FS_ITEM_ORIGINAL>-IPI_AQT            + WA_FAKE-IPI_AQT.
    <FS_ITEM_ORIGINAL>-IPI_VALOR          = <FS_ITEM_ORIGINAL>-IPI_VALOR          + WA_FAKE-IPI_VALOR.
    <FS_ITEM_ORIGINAL>-PIS_BASE           = <FS_ITEM_ORIGINAL>-PIS_BASE           + WA_FAKE-PIS_BASE.
    <FS_ITEM_ORIGINAL>-PIS_AQT            = <FS_ITEM_ORIGINAL>-PIS_AQT            + WA_FAKE-PIS_AQT.
    <FS_ITEM_ORIGINAL>-PIS_VALOR          = <FS_ITEM_ORIGINAL>-PIS_VALOR          + WA_FAKE-PIS_VALOR.
    <FS_ITEM_ORIGINAL>-PIS_QTD_VENDIDA    = <FS_ITEM_ORIGINAL>-PIS_QTD_VENDIDA    + WA_FAKE-PIS_QTD_VENDIDA.
    <FS_ITEM_ORIGINAL>-PIS_AQT_REAIS      = <FS_ITEM_ORIGINAL>-PIS_AQT_REAIS      + WA_FAKE-PIS_AQT_REAIS.
    <FS_ITEM_ORIGINAL>-PIS_ST_BASE        = <FS_ITEM_ORIGINAL>-PIS_ST_BASE        + WA_FAKE-PIS_ST_BASE.
    <FS_ITEM_ORIGINAL>-PIS_ST_AQT         = <FS_ITEM_ORIGINAL>-PIS_ST_AQT         + WA_FAKE-PIS_ST_AQT.
    <FS_ITEM_ORIGINAL>-PIS_ST_QTD_VENDI   = <FS_ITEM_ORIGINAL>-PIS_ST_QTD_VENDI   + WA_FAKE-PIS_ST_QTD_VENDI.
    <FS_ITEM_ORIGINAL>-PIS_ST_AQT_REAIS   = <FS_ITEM_ORIGINAL>-PIS_ST_AQT_REAIS   + WA_FAKE-PIS_ST_AQT_REAIS.
    <FS_ITEM_ORIGINAL>-PIS_ST_VALOR       = <FS_ITEM_ORIGINAL>-PIS_ST_VALOR       + WA_FAKE-PIS_ST_VALOR.
    <FS_ITEM_ORIGINAL>-COF_BASE           = <FS_ITEM_ORIGINAL>-COF_BASE           + WA_FAKE-COF_BASE.
    <FS_ITEM_ORIGINAL>-COF_AQT            = <FS_ITEM_ORIGINAL>-COF_AQT            + WA_FAKE-COF_AQT.
    <FS_ITEM_ORIGINAL>-COF_VALOR          = <FS_ITEM_ORIGINAL>-COF_VALOR          + WA_FAKE-COF_VALOR.
    <FS_ITEM_ORIGINAL>-COF_QTD_VENDIDA    = <FS_ITEM_ORIGINAL>-COF_QTD_VENDIDA    + WA_FAKE-COF_QTD_VENDIDA.
    <FS_ITEM_ORIGINAL>-COF_AQT_REAIS      = <FS_ITEM_ORIGINAL>-COF_AQT_REAIS      + WA_FAKE-COF_AQT_REAIS.
    <FS_ITEM_ORIGINAL>-COF_ST_BASE        = <FS_ITEM_ORIGINAL>-COF_ST_BASE        + WA_FAKE-COF_ST_BASE.
    <FS_ITEM_ORIGINAL>-COF_ST_AQT         = <FS_ITEM_ORIGINAL>-COF_ST_AQT         + WA_FAKE-COF_ST_AQT.
    <FS_ITEM_ORIGINAL>-COF_ST_QTD_VENDI   = <FS_ITEM_ORIGINAL>-COF_ST_QTD_VENDI   + WA_FAKE-COF_ST_QTD_VENDI.
    <FS_ITEM_ORIGINAL>-COF_ST_AQT_REAIS   = <FS_ITEM_ORIGINAL>-COF_ST_AQT_REAIS   + WA_FAKE-COF_ST_AQT_REAIS.
    <FS_ITEM_ORIGINAL>-COF_ST_VALOR       = <FS_ITEM_ORIGINAL>-COF_ST_VALOR       + WA_FAKE-COF_ST_VALOR.
    <FS_ITEM_ORIGINAL>-MENGE              = <FS_ITEM_ORIGINAL>-MENGE              + WA_FAKE-MENGE.
    <FS_ITEM_ORIGINAL>-NETPR              = <FS_ITEM_ORIGINAL>-NETPR              + WA_FAKE-NETPR.
    <FS_ITEM_ORIGINAL>-NETWR              = <FS_ITEM_ORIGINAL>-NETWR              + WA_FAKE-NETWR.

    "Excluir Item Fake
    DELETE ME->ITENS WHERE PROD_ITEM EQ WA_FAKE-PROD_ITEM.
    "Excluir Lote Caracteristicas Fake
    LOOP AT ME->LOTES INTO DATA(WA_LOTE_FAKE) WHERE PROD_ITEM EQ WA_FAKE-PROD_ITEM.
      DELETE ME->LOTES_CARACTERISTICAS WHERE CD_LOTE_ITEM EQ WA_LOTE_FAKE-CD_LOTE_ITEM.
    ENDLOOP.
    "Excluir Lote
    DELETE ME->LOTES WHERE PROD_ITEM EQ WA_FAKE-PROD_ITEM.

    "Ajustar Itens Restantes
    CLEAR: WA_FAKE.
    LOOP AT ME->ITENS TRANSPORTING NO FIELDS WHERE PROD_ITEM_ORIGEM IS INITIAL.
      ADD 1 TO WA_FAKE-PROD_ITEM.
    ENDLOOP.

    LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_FAKE>) WHERE PROD_ITEM_ORIGEM IS NOT INITIAL.
      ADD 1 TO WA_FAKE-PROD_ITEM.
      LOOP AT ME->LOTES ASSIGNING FIELD-SYMBOL(<FS_LOTE>) WHERE PROD_ITEM EQ <FS_FAKE>-PROD_ITEM.
        <FS_LOTE>-PROD_ITEM = WA_FAKE-PROD_ITEM.
      ENDLOOP.
      <FS_FAKE>-PROD_ITEM = WA_FAKE-PROD_ITEM.
    ENDLOOP.

  ENDMETHOD.


  METHOD EXCLUIR_LOTE_ITEM.
    DELETE ME->LOTES WHERE CD_LOTE_ITEM EQ I_CD_LOTE_ITEM.
    DELETE ME->LOTES_CARACTERISTICAS WHERE CD_LOTE_ITEM EQ I_CD_LOTE_ITEM.
    ME->CK_ALTEROU_LOTES = ABAP_TRUE.

  ENDMETHOD.


  METHOD FREE.
    ZCL_CTE_INBOUND=>SET_DENQUEUE_NFE( I_CHAVE = ME->CTE-CD_CHAVE_CTE I_SEM_BLOQUEIO_REGISTRO = ME->CK_SEM_BLOQUEIO_REGISTRO ).
    CLEAR ME->AT_CHAVES_LOCK.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_ALTEROU_PEDIDO_COMPRA.
    R_ALTEROU = ME->CK_ALTEROU_PEDIDO_COMPRA.
  ENDMETHOD.


  METHOD GET_AVISO_VALIDO.

    DATA: lc_xblnr  TYPE xblnr1,
          lc_xblnr2 TYPE xblnr1,
          l_serie   TYPE c LENGTH 3.

    CLEAR: r_likp.

    CONCATENATE me->nota-numero me->nota-serie INTO lc_xblnr2 SEPARATED BY '-'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = me->nota-numero
      IMPORTING
        output = lc_xblnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = me->nota-serie
      IMPORTING
        output = l_serie.

    CONCATENATE lc_xblnr l_serie INTO lc_xblnr SEPARATED BY '-'.

    SELECT * INTO TABLE @DATA(it_likp)
      FROM likp AS m
     WHERE lifnr EQ @me->nota-p_emissor
       AND xblnr EQ @lc_xblnr.

    IF sy-subrc IS INITIAL.
      READ TABLE it_likp INDEX 1 INTO r_likp.
    ELSE.

      SELECT SINGLE lifnr
        FROM ekko
        INTO @DATA(lv_lifnr)
        WHERE ebeln = @me->nota-ebeln.
        IF sy-subrc IS INITIAL.
          SELECT * INTO TABLE it_likp
            FROM likp AS m
           WHERE lifnr EQ lv_lifnr
             AND lifex EQ lc_xblnr OR
               ( lifex EQ lc_xblnr2 ).
          IF sy-subrc IS INITIAL.
            READ TABLE it_likp INDEX 1 INTO r_likp.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDMETHOD.


  METHOD GET_CABECALHO_NOTA.
    R_NOTA = ME->NOTA.
  ENDMETHOD.


  METHOD GET_CARATERISTICAS.

    R_CARACTERISTICAS = ZCL_CHARG=>GET_CARCATERISTICAS_CLASS( I_CLASS = I_CLASS  I_CLASSTYPE = I_CLASSTYPE I_OBJECT = I_OBJECT I_TABLE = CONV #('MARA') ).

  ENDMETHOD.


  METHOD GET_CATEGORIA_NOTA_FISCAL.

    CASE ME->NOTA-TP_COMPRA_FUTURA.
      WHEN ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_FATURA.
        R_CATEGORIA  = 'X6'.
      WHEN ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA.
        R_CATEGORIA = 'ZH'.
      WHEN OTHERS.
        R_CATEGORIA = 'NE'.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_CFOP_ARMAZENAGEM.

    CASE ME->NOTA-CK_TRANS_NF_PROPRI.
      WHEN ABAP_TRUE.
        R_CFOP = '5905AA'.
      WHEN ABAP_FALSE.
        R_CFOP = '5934AA'.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_CFOP_ESCRITURACAO_ENTRADA.

    DATA: WA_KNA1    TYPE KNA1,
          WA_J_1BAON TYPE J_1BAON.

    DATA: LC_CFOP TYPE J_1BCFOP.

    CLEAR: R_CFOP.

    SELECT SINGLE * INTO @DATA(WA_LFA1)
      FROM LFA1
     WHERE LIFNR EQ @ME->NOTA-P_EMISSOR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ME->NOTA-F_TOMADORA
      IMPORTING
        OUTPUT = WA_KNA1-KUNNR.

    SELECT SINGLE * INTO WA_KNA1
      FROM KNA1
     WHERE KUNNR EQ WA_KNA1-KUNNR.

    WA_J_1BAON-DIRECT  = '1'.
    WA_J_1BAON-VERSION = '2'.
    WA_J_1BAON-INDUS3  = '00'.

    SELECT SINGLE * INTO @DATA(WA_J_1BBRANCH)
      FROM J_1BBRANCH
     WHERE BRANCH EQ @ME->NOTA-F_TOMADORA.

    WA_J_1BAON-INDUS2  = WA_J_1BBRANCH-INDUSTRY.

    IF WA_LFA1-REGIO NE WA_KNA1-REGIO.
      "Fora do Estado
      "1  Estado diferente
      WA_J_1BAON-CFOP   = '2'.
      WA_J_1BAON-DSTCAT = '1'.
    ELSE.
      "Dentro do Estado
      "0  Mesmo estado
      WA_J_1BAON-CFOP   = '1'.
      WA_J_1BAON-DSTCAT = '0'.
    ENDIF.

    IF I_PROD_ITEM IS INITIAL.
      READ TABLE ME->ITENS INDEX 1 INTO DATA(WA_ITEM).
    ELSE.
      READ TABLE ME->ITENS INTO WA_ITEM WITH KEY PROD_ITEM = I_PROD_ITEM.
    ENDIF.

    CONCATENATE WA_J_1BAON-CFOP WA_ITEM-PROD_CFOP+1(3) '%' INTO WA_J_1BAON-CFOP.

    SELECT * INTO TABLE @DATA(IT_J_1BAON)
      FROM J_1BAON
     WHERE DIRECT  EQ @WA_J_1BAON-DIRECT
       AND VERSION EQ @WA_J_1BAON-VERSION
       AND INDUS2  EQ @WA_J_1BAON-INDUS2
       AND INDUS3  EQ @WA_J_1BAON-INDUS3
       AND DSTCAT  EQ @WA_J_1BAON-DSTCAT
       AND CFOP    LIKE @WA_J_1BAON-CFOP.

    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_J_1BAON INDEX 1 INTO WA_J_1BAON.
      R_CFOP = WA_J_1BAON-CFOP.
      MESSAGE S095 WITH WA_J_1BAON-CFOP WA_J_1BAON-INDUS2 WA_J_1BAON-VERSION.

      IF I_GRAVAR_LOG EQ ABAP_TRUE.
        CALL METHOD ME->SET_ADD_LOG_NFE
          EXPORTING
            I_TYPE         = SY-MSGTY
            I_ID           = SY-MSGID
            I_NUM          = SY-MSGNO
            I_MESSAGE_V1   = SY-MSGV1
            I_MESSAGE_V2   = SY-MSGV2
            I_MESSAGE_V3   = SY-MSGV3
            I_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

    ELSE.

      MESSAGE S101 WITH WA_J_1BAON-CFOP(4) WA_J_1BAON-INDUS2 WA_J_1BAON-VERSION.

      IF I_GRAVAR_LOG EQ ABAP_TRUE.
        SY-MSGTY = 'W'.
        CALL METHOD ME->SET_ADD_LOG_NFE
          EXPORTING
            I_TYPE         = SY-MSGTY
            I_ID           = SY-MSGID
            I_NUM          = SY-MSGNO
            I_MESSAGE_V1   = SY-MSGV1
            I_MESSAGE_V2   = SY-MSGV2
            I_MESSAGE_V3   = SY-MSGV3
            I_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      SELECT * INTO TABLE IT_J_1BAON
        FROM J_1BAON
       WHERE DIRECT  EQ WA_J_1BAON-DIRECT
         AND VERSION EQ WA_J_1BAON-VERSION
         AND INDUS3  EQ WA_J_1BAON-INDUS3
         AND DSTCAT  EQ WA_J_1BAON-DSTCAT
         AND CFOP    LIKE WA_J_1BAON-CFOP.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_J_1BAON INDEX 1 INTO WA_J_1BAON.
        R_CFOP = WA_J_1BAON-CFOP.
        MESSAGE S102 WITH WA_J_1BAON-CFOP WA_J_1BAON-VERSION.

        IF I_GRAVAR_LOG EQ ABAP_TRUE.
          CALL METHOD ME->SET_ADD_LOG_NFE
            EXPORTING
              I_TYPE         = SY-MSGTY
              I_ID           = SY-MSGID
              I_NUM          = SY-MSGNO
              I_MESSAGE_V1   = SY-MSGV1
              I_MESSAGE_V2   = SY-MSGV2
              I_MESSAGE_V3   = SY-MSGV3
              I_MESSAGE_V4   = SY-MSGV4
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.

      ELSE.
        "ERRO
        "Não Encontrado CFOP &MSGV1& p/ Cat.CFOP &MSGV2& Versão &MSGV3&!
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID    = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CFOP_NAO_ENCONTRADO-MSGID
                                 MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CFOP_NAO_ENCONTRADO-MSGNO
                                 ATTR1 = CONV #( WA_J_1BAON-CFOP    )
                                 ATTR2 = CONV #( WA_J_1BAON-INDUS2  )
                                 ATTR3 = CONV #( WA_J_1BAON-VERSION ) )
            MSGTY     = 'E'
            MSGID     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CFOP_NAO_ENCONTRADO-MSGID
            MSGNO     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_CFOP_NAO_ENCONTRADO-MSGNO
            MSGV1     = CONV #( WA_J_1BAON-CFOP )
            MSGV2     = CONV #( WA_J_1BAON-INDUS2 )
            MSGV3     = CONV #( WA_J_1BAON-VERSION )
            TRANSACAO = 'J1BTAX'.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD GET_CK_ALTEROU_IVA.
    R_ALTEROU_IVA = ME->CK_ALTEROU_IVA.
  ENDMETHOD.


  METHOD GET_CK_CFOP_RETORNO_ARMAZENA.

    CLEAR: R_CFOP_RETORNO.

    READ TABLE ME->ITENS INDEX 1 INTO DATA(WA_ITENS).

    DATA(LC_CFOP) = |{ WA_ITENS-PROD_CFOP }%|.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0119)
      FROM ZMMT0119
     WHERE CFOP LIKE @LC_CFOP.

    R_CFOP_RETORNO = COND STRING( WHEN SY-SUBRC IS INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE ).

  ENDMETHOD.


  METHOD GET_CK_GERAR_SAIDA_ARMAZEM.

    DATA(CK_EDI) = ME->GET_CK_POSSUI_EDI_ARMAZENAGEM( ).

    R_GERA_SAIDA = ABAP_FALSE.

    "Se for do processo de Armazenagem
    CHECK ME->NOTA-CK_ARMAZEM EQ ABAP_TRUE.

    CASE CK_EDI.
      WHEN ABAP_FALSE.
        "Se não tem Processo de EDI Gera Saída para Armazenagem
        R_GERA_SAIDA = ABAP_TRUE.
        CK_ENVIA_ACEITE = ABAP_FALSE.

      WHEN ABAP_TRUE.

        "Se Recebeu confirmação de chegada física no Armazem OU
        "Documento deve transitar com nota fiscal própria
        IF ( ME->NOTA-ST_ARMAZEM EQ ME->ST_ARMAZENAGEM_04 ) OR
           ( ME->NOTA-CK_TRANS_NF_PROPRI EQ ABAP_TRUE AND ME->NOTA-ST_ARMAZEM EQ ME->ST_ARMAZENAGEM_00 ).
          "Gerar Saída para Armazenagem
          R_GERA_SAIDA = ABAP_TRUE.
          CK_ENVIA_ACEITE = ABAP_TRUE.
        ELSE.
          R_GERA_SAIDA = ABAP_FALSE.
          CK_ENVIA_ACEITE = ABAP_TRUE.
        ENDIF.

    ENDCASE.

    "Continua se Deve Gerar Saída
    CHECK R_GERA_SAIDA EQ ABAP_TRUE.

    IF ME->NOTA-MBLNR IS INITIAL.
      R_GERA_SAIDA = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CK_POSSUI_EDI_ARMAZENAGEM.

    R_POSSUI = ABAP_FALSE.

    CHECK ME->NOTA-CD_DEPARTAMENTO IS NOT INITIAL.

    CHECK ME->NOTA-CK_ARMAZEM IS NOT INITIAL.

    CHECK ME->NOTA-F_ARMAZEM IS NOT INITIAL.

    SELECT SINGLE *  INTO @DATA(WA_ZMMT0076)
      FROM ZMMT0076
     WHERE CD_DEPARTAMENTO EQ @ME->NOTA-CD_DEPARTAMENTO
       AND LIFNR EQ @ME->NOTA-F_ARMAZEM.

    CHECK SY-SUBRC IS INITIAL.

    R_POSSUI = ABAP_TRUE.

  ENDMETHOD.


  METHOD GET_CK_RECEBIMENTO_BURRO.

    R_RECEBIMENTO_BURRO = ABAP_FALSE.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_SETLEAF_VF)
     WHERE SETNAME EQ 'MAGGI_SEM_VINCULO_NFE'
       AND VALFROM EQ @I_FILIAL.

    IF SY-SUBRC IS INITIAL.
      R_RECEBIMENTO_BURRO = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CONFIG_TIPO_PEDIDO.

    CLEAR: r_zmmt0075.

    CHECK me->cte-cd_departamento IS NOT INITIAL.

    "Buscar Informações do Departamento
    CREATE OBJECT me->departamento.
    me->departamento->set_registro( i_id_registro = me->cte-cd_departamento ).
    DATA(it_tipos_pedido_compra) = me->departamento->get_tipo_pedido_compra( ).
    SORT it_tipos_pedido_compra BY bstyp bsart.

    IF me->pedidos IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(it_ekko)
        FROM ekko
         FOR ALL ENTRIES IN @me->pedidos
       WHERE ebeln EQ @me->pedidos-ebeln.
    ELSEIF me->itens[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @it_ekko
        FROM ekko
         FOR ALL ENTRIES IN @me->itens
       WHERE ebeln EQ @me->itens-ebeln.
    ENDIF.

    LOOP AT it_ekko INTO DATA(wa_ekko).
      READ TABLE it_tipos_pedido_compra INTO DATA(wa_zmmt0075) WITH KEY bstyp = wa_ekko-bstyp bsart = wa_ekko-bsart BINARY SEARCH.

      IF r_zmmt0075 IS INITIAL.
        r_zmmt0075 = wa_zmmt0075.
      ELSEIF r_zmmt0075-ck_obriga_bloqueio EQ abap_false AND wa_zmmt0075-ck_obriga_bloqueio EQ abap_true.
        r_zmmt0075-ck_obriga_bloqueio = abap_true.
      ENDIF.

      IF r_zmmt0075 IS NOT INITIAL AND r_zmmt0075-ck_altera_bloqueio EQ abap_true AND wa_zmmt0075-ck_altera_bloqueio EQ abap_false.
        r_zmmt0075-ck_altera_bloqueio = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_DADOS_RETORNO.
    E_RETORNO = ME->DADOS_RETORNO.
  ENDMETHOD.


  METHOD GET_DEPARTAMENTO_ITENS.

    DATA: IT_MARA         TYPE TABLE OF MARA,
          WA_MARA         TYPE MARA,
          WA_CONT         TYPE ZDE_ZMMT0073_CONTAGEM,
          IT_CONT         TYPE HASHED TABLE OF ZDE_ZMMT0073_CONTAGEM WITH UNIQUE KEY CD_DEPARTAMENTO,
          LC_DEPARTAMENTO TYPE ZDE_ZMMT0073_CONTAGEM-CD_DEPARTAMENTO,
          LC_QUANTIDADE   TYPE ZDE_ZMMT0073_CONTAGEM-QUANTIDADE.

*016  NF-e não Atribuida a um Departamento!
*017  NF-e Atribuida ao Departamento: &1.
*018  Grupo(s) de Mercadoria não parametrizado(s)

    LOOP AT I_ITENS INTO DATA(WA_ITENS) WHERE MATNR NE SPACE.
      WA_MARA-MATNR = WA_ITENS-MATNR.
      APPEND WA_MARA TO IT_MARA.
    ENDLOOP.

    IF IT_MARA IS NOT INITIAL.

      SELECT * INTO TABLE IT_MARA
        FROM MARA
         FOR ALL ENTRIES IN IT_MARA
       WHERE MATNR EQ IT_MARA-MATNR.

      IF SY-SUBRC IS INITIAL.

        SELECT * INTO TABLE @DATA(IT_ZMMT0073)
          FROM ZMMT0073
           FOR ALL ENTRIES IN @IT_MARA
         WHERE MATKL EQ @IT_MARA-MATKL.

        IF SY-SUBRC IS INITIAL.
          "Contagem de Departamento
          LOOP AT IT_ZMMT0073 INTO DATA(WA_ZMMT0073).
            CLEAR: WA_CONT.
            WA_CONT-CD_DEPARTAMENTO = WA_ZMMT0073-CD_DEPARTAMENTO.
            WA_CONT-QUANTIDADE = 1.
            COLLECT WA_CONT INTO IT_CONT.
          ENDLOOP.

          LOOP AT IT_CONT INTO WA_CONT.
            IF LC_DEPARTAMENTO IS INITIAL.
              LC_DEPARTAMENTO = WA_CONT-CD_DEPARTAMENTO.
              LC_QUANTIDADE = WA_CONT-QUANTIDADE.
            ELSEIF WA_CONT-QUANTIDADE GT LC_QUANTIDADE.
              LC_DEPARTAMENTO = WA_CONT-CD_DEPARTAMENTO.
              LC_QUANTIDADE = WA_CONT-QUANTIDADE.
            ENDIF.
          ENDLOOP.

          "017  NF-e Atribuida ao Departamento: &1.
          R_CD_DEPARTAMENTO = LC_DEPARTAMENTO.

          SELECT SINGLE * INTO @DATA(WA_72)
            FROM ZMMT0072
           WHERE CD_DEPARTAMENTO = @LC_DEPARTAMENTO.

          MESSAGE S017 WITH WA_72-DS_DEPARTAMENTO.
        ELSE.

          SELECT SINGLE * INTO @DATA(WA_ZMMT0072_DEFAULT)
            FROM ZMMT0072
           WHERE CK_SEM_RET_GRUPO  EQ @ABAP_TRUE
             AND CK_SEM_RET_PEDIDO EQ @ABAP_TRUE.

          IF SY-SUBRC IS INITIAL.
            R_CD_DEPARTAMENTO = WA_ZMMT0072_DEFAULT-CD_DEPARTAMENTO.
            MESSAGE S017 WITH WA_ZMMT0072_DEFAULT-DS_DEPARTAMENTO.
          ELSE.
            RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
              EXPORTING
                TEXTID    = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_GRUPO_MERCADORIA-MSGID
                                     MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_GRUPO_MERCADORIA-MSGNO )
                MSGID     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_GRUPO_MERCADORIA-MSGID
                MSGNO     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_GRUPO_MERCADORIA-MSGNO
                MSGTY     = 'E'
                TRANSACAO = ZCL_NFE_INBOUND=>TRANSACAO_DEPARTAMENTO.
          ENDIF.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID    = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGID
                                 MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGNO )
            MSGID     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGID
            MSGNO     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGNO
            MSGTY     = 'E'
            TRANSACAO = ZCL_NFE_INBOUND=>TRANSACAO_DEPARTAMENTO.
      ENDIF.
    ELSE.

      SELECT SINGLE * INTO WA_ZMMT0072_DEFAULT
        FROM ZMMT0072
       WHERE CK_SEM_RET_GRUPO  EQ ABAP_TRUE
         AND CK_SEM_RET_PEDIDO EQ ABAP_TRUE.

      IF SY-SUBRC IS INITIAL.
        R_CD_DEPARTAMENTO = WA_ZMMT0072_DEFAULT-CD_DEPARTAMENTO.
        MESSAGE S017 WITH WA_ZMMT0072_DEFAULT-DS_DEPARTAMENTO.
      ELSE.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID    = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGID
                                 MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGNO )
            MSGID     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGID
            MSGNO     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_DEPARTAMENTO-MSGNO
            MSGTY     = 'E'
            TRANSACAO = ZCL_NFE_INBOUND=>TRANSACAO_DEPARTAMENTO.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_FATURA_NFE_INBOUND.

    CLEAR: R_RBKP.

    SELECT * INTO TABLE @DATA(IT_LINHAS)
      FROM J_1BNFLIN
     WHERE DOCNUM EQ @I_DOCNUM
       AND REFTYP EQ 'LI'.

    CHECK SY-SUBRC IS INITIAL.

    READ TABLE IT_LINHAS INDEX 1 INTO DATA(WA_LINHAS).

    SELECT SINGLE * INTO R_RBKP
      FROM RBKP
     WHERE BELNR EQ WA_LINHAS-REFKEY(10)
       AND GJAHR EQ WA_LINHAS-REFKEY+10(4).

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE @DATA(IT_RSEG)
      FROM RSEG
     WHERE BELNR = @R_RBKP-BELNR
       AND GJAHR = @R_RBKP-GJAHR.

    MOVE IT_RSEG[] TO E_RSEG.

  ENDMETHOD.


  METHOD GET_FORNECEDOR.

    DATA: IT_LFA1_AUX TYPE TABLE OF LFA1,
          LC_QTD      TYPE I.

    IF I_FORNE_CNPJ IS NOT INITIAL OR I_FORNE_CPF IS NOT INITIAL.

      TRY .
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO_CNPJ_CPF_IE(
             EXPORTING
              I_CNPJ          = I_FORNE_CNPJ
              I_CPF           = I_FORNE_CPF
              I_INSC_ESTATUAL = I_FORNE_IE
            )->CK_ATIVO(
            )->CK_ATIVO_EMPRESA( I_EMPRESA =  I_BUKRS
            )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = R_LIFNR-LIFNR
            ).
        CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).

          EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ME->GERA_ERRO_GERAL( I_TEXTO = MTEXT ).

      ENDTRY.

    ELSEIF I_FORNE_IE IS NOT INITIAL.

      TRY .
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO_IE( EXPORTING I_INSC_ESTATUAL = I_FORNE_IE
            )->CK_ATIVO(
            )->CK_ATIVO_EMPRESA( I_EMPRESA =  I_BUKRS
            )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = R_LIFNR-LIFNR
            ).
        CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.

          EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ME->GERA_ERRO_GERAL( I_TEXTO = MTEXT ).

      ENDTRY.

    ENDIF.

*    IF I_SERIE_NOTA GE '890' AND I_SERIE_NOTA LE '899'.
*      TRY .
*          IF I_FORNE_CNPJ IS INITIAL.
*            IF I_FORNE_IE EQ 'ISENTO'.
*
*            ELSE.
*              ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*                )->SET_PARCEIRO_CNPJ_CPF_IE(
*                 EXPORTING
*                  I_INSC_ESTATUAL = I_FORNE_IE
*                )->CK_ATIVO(
*                )->CK_ATIVO_EMPRESA( I_EMPRESA = I_BUKRS
*                )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = R_LIFNR-LIFNR
*                ).
*            ENDIF.
*          ELSE.
*            ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*              )->SET_PARCEIRO_CNPJ_CPF_IE(
*               EXPORTING
*                I_CNPJ          = I_FORNE_CNPJ
*                I_INSC_ESTATUAL = I_FORNE_IE
*              )->CK_ATIVO(
*              )->CK_ATIVO_EMPRESA( I_EMPRESA = I_BUKRS
*              )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = R_LIFNR-LIFNR
*              ).
*          ENDIF.
*        CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
*          EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ME->GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
*      ENDTRY.
*    ELSE.
*      TRY .
*          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*            )->SET_PARCEIRO_CNPJ_CPF_IE(
*             EXPORTING
*              I_CNPJ          = I_FORNE_CNPJ
*              I_INSC_ESTATUAL = I_FORNE_IE
*            )->CK_ATIVO(
*            )->CK_ATIVO_EMPRESA( I_EMPRESA =  I_BUKRS
*            )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = R_LIFNR-LIFNR
*            ).
*        CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.
*          EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ME->GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
*      ENDTRY.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_ICON_STATUS_ARMAZEM.

    CASE I_STATUS.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_00.
        R_ICON = ICON_BEN_OFFER.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_01.
        R_ICON = ICON_IMPORT_TRANSPORT_REQUEST.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_02.
        R_ICON = ICON_IMPORT_TRANSPORT_REQUEST.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_03.
        R_ICON = ICON_IMPORT_TRANSPORT_REQUEST.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_04.
        R_ICON = ICON_IMPORT_TRANSPORT_REQUEST.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_05.
        R_ICON = ICON_IMPORT_TRANSPORT_REQUEST.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_97.
        R_ICON = ICON_IMPORT_ALL_REQUESTS.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_98.
        R_ICON = ICON_BEN_TERMINATION.
      WHEN ZCL_NFE_INBOUND=>ST_ARMAZENAGEM_99.
        R_ICON = ICON_COMPLETE.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_ICON_STATUS_DOCUMENTO.

    CASE I_STATUS.
      WHEN ZCL_NFE_INBOUND=>ST_DOCUMENTO_00.
        R_ICON = ICON_BEN_OFFER_DEFAULT.
      WHEN ZCL_NFE_INBOUND=>ST_DOCUMENTO_01.
        R_ICON = ICON_RELEASE.
      WHEN ZCL_NFE_INBOUND=>ST_DOCUMENTO_99.
        R_ICON = ICON_COMPLETE.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_ICON_STATUS_FISCAL.

    CASE I_STATUS.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_PENDENTE OR SPACE.
        R_ICON = ICON_BEN_OFFER.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_DEPARTAMENTO.
        R_ICON = ICON_PERSONNEL_ADMINISTRATION.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_DEPART_APROVADO.
        R_ICON = ICON_PERSONNEL_ADMINISTRATION.
        "WHEN ZCL_NFE_INBOUND=>ST_FISCAL_AVISO_GERADO.
        "  R_ICON = ICON_RELEASE.
        "WHEN ZCL_NFE_INBOUND=>ST_FISCAL_04.
        "  R_ICON = ICON_RELEASE.
        "WHEN ZCL_NFE_INBOUND=>ST_FISCAL_05.
        "  R_ICON = ICON_RELEASE.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_NAO_ACEITO_FISCAL.
        R_ICON = ICON_DEFECT.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_SEM_ACEITE_FISCAL.
        R_ICON = ICON_BEN_TERMINATION.
      WHEN ZCL_NFE_INBOUND=>ST_FISCAL_COM_ACEITE_FISCAL.
        R_ICON = ICON_COMPLETE.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_ICON_STATUS_FISICO.

*ST_FISICO_00 Constant  Public  Type  ZDE_ST_NFE_FISICO Pendente  '00'
*ST_FISICO_01 Constant  Public  Type  ZDE_ST_NFE_FISICO Aceite Físico '01'
*ST_FISICO_AVISO_GERADO Constant  Public  Type  ZDE_ST_NFE_FISICO Aviso Gerado  '02'
*ST_FISICO_MIGO_GERADA  Constant  Public  Type  ZDE_ST_NFE_FISICO MIGO Gerada '03'
*ST_FISICO_MIRO_GERADA  Constant  Public  Type  ZDE_ST_NFE_FISICO MIRO Gerada '04'
*ST_FISICO_FISCAL_GERADO  Constant  Public  Type  ZDE_ST_NFE_FISICO Fiscal Gerada '05'
*ST_FISICO_98 Constant  Public  Type  ZDE_ST_NFE_FISICO Finalizado Sem Aceite Físico  '98'
*ST_FISICO_99 Constant  Public  Type  ZDE_ST_NFE_FISICO Finalizado Com Aceite Físico  '99'

    CASE I_STATUS.

      WHEN ZCL_NFE_INBOUND=>ST_FISICO_00 OR SPACE.
        R_ICON = ICON_BEN_OFFER.

      WHEN ZCL_NFE_INBOUND=>ST_FISICO_01.
        R_ICON = ICON_RELEASE.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_AVISO_GERADO.
        R_ICON = ICON_RELEASE.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_MIGO_GERADA.
        R_ICON = ICON_CUSTOMER_WAREHOUSE.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_MIRO_GERADA.
        R_ICON = ICON_PAYMENT.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_FISCAL_GERADO.
        R_ICON = ICON_RELEASE.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_98.
        R_ICON = ICON_BEN_TERMINATION.
      WHEN ZCL_NFE_INBOUND=>ST_FISICO_99.
        R_ICON = ICON_COMPLETE.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_INFO_DEPARTAMENTO.

    SELECT SINGLE DS_DEPARTAMENTO INTO R_DS_DEPARTAMENTO
      FROM ZMMT0072
     WHERE CD_DEPARTAMENTO EQ I_CD_DEPARTAMENTO.

  ENDMETHOD.


  METHOD GET_INFO_FORNECEDOR.

    CLEAR: R_LFA1.

    CHECK I_LIFNR IS NOT INITIAL.

    SELECT SINGLE * INTO R_LFA1
      FROM LFA1
     WHERE LIFNR EQ I_LIFNR.

  ENDMETHOD.


  METHOD get_info_nota.

    DATA: wa_ped_alv TYPE zde_nfe_dist_ped_alv.

    CLEAR: r_cte_inbound.

    MOVE-CORRESPONDING me->cte TO r_cte_inbound-cte_base.

    MOVE me->itens[]                 TO r_cte_inbound-nfe_base-itens[].
    MOVE me->lotes[]                 TO r_cte_inbound-nfe_base-lotes[].
    MOVE me->lotes_caracteristicas[] TO r_cte_inbound-nfe_base-lotes_c[].
    MOVE me->pedidos[]               TO r_cte_inbound-nfe_base-pedidos[].
    MOVE me->volumes_transp[]        TO r_cte_inbound-nfe_base-volumes_transp[].
    MOVE me->dados_retorno[]         TO r_cte_inbound-nfe_base-retornos[].

    DATA: it_cte TYPE zib_cte_dist_ter_t.

    APPEND me->cte TO it_cte.

    DATA(it_alv) = me->get_nfe_inbound_alv_saida( EXPORTING i_cte_dist = it_cte ).

*    LOOP AT it_alv INTO DATA(wa_alv).
*      MOVE-CORRESPONDING wa_alv TO r_cte_inbound-nfe_alv.
*      DATA(armazem) = me->get_info_fornecedor( i_lifnr = wa_alv-f_armazem ).
*      r_cte_inbound-cte_alv-armazem_cnpj  = armazem-stcd1.
*      r_cte_inbound-nfe_alv-armazem_ie    = armazem-stcd3.
*      r_cte_inbound-nfe_alv-armazem_razao = armazem-name1.
*    ENDLOOP.

    ck_alterou_pedido_compra = abap_false.

    IF me->pedidos[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_makt)
        FROM makt
         FOR ALL ENTRIES IN @me->pedidos
       WHERE spras EQ @sy-langu
         AND matnr EQ @me->pedidos-matnr.

      SELECT * INTO TABLE @DATA(it_marc)
        FROM marc
         FOR ALL ENTRIES IN @me->pedidos
       WHERE matnr EQ @me->pedidos-matnr
         AND werks EQ @me->nota-branch.

    ENDIF.

    LOOP AT me->pedidos INTO DATA(wa_pedido).
      CLEAR: wa_ped_alv.
      MOVE-CORRESPONDING wa_pedido TO wa_ped_alv.

      READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_pedido-matnr.
      IF sy-subrc IS INITIAL.
        wa_ped_alv-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_marc INTO DATA(wa_marc) WITH KEY matnr = wa_pedido-matnr.
      IF sy-subrc IS INITIAL.
        wa_ped_alv-ncm = wa_marc-steuc.
      ENDIF.

      APPEND wa_ped_alv TO r_cte_inbound-nfe_pedidos_alv.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_IVA_PARAMETRIZADO_NFE.

    DATA: WA_NFE_IVA   TYPE ZIB_NFE_DIST_IVA,
          LC_TEXT_ICMS TYPE C LENGTH 3.

    READ TABLE ME->ITENS INDEX 1 INTO DATA(WA_ITEM).

    CLEAR: R_MWSKZ.

    IF WA_ITEM-EBELN IS INITIAL OR WA_ITEM-EBELP IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_PEDIDO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_PEDIDO-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_PEDIDO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_PEDIDO-MSGNO
          MSGV1  = CONV #( WA_ITEM-PROD_ITEM ).
    ENDIF.

*    00	Tributada integralmente
*    10	Tributada e com cobrança do ICMS por substituição tributária
*    20	Com redução de base de cálculo
*    30	Isenta ou não tributada e com cobrança do ICMS por substitui
*    40	Isento ou não sujeito a ICMS
*    41	Não tributada
*    50	Suspensão do ICMS
*    51	Diferimento
*    60	ICMS cobrado anteriormente por substituição tributária
*    70	Com redução de base de cálculo e cobrança do ICMS por Sub.T.
*    90	Outros/as

    CASE WA_ITEM-ICMS_CST.
      WHEN '00' OR '10' OR '20' OR '30' OR '70'.
        WA_NFE_IVA-ICMS = ABAP_TRUE.
        LC_TEXT_ICMS = 'Sim'.
      WHEN '40' OR '41' OR '50' OR '51' OR '60' OR '90'.
        WA_NFE_IVA-ICMS = ABAP_FALSE.
        LC_TEXT_ICMS = 'Não'.
    ENDCASE.

    WA_NFE_IVA-BUKRS = ME->NOTA-E_TOMADORA.
    WA_NFE_IVA-LIFNR = ME->NOTA-P_EMISSOR.

    SELECT SINGLE INDUSTRY
      INTO WA_NFE_IVA-INDUSTRY
      FROM J_1BBRANCH
     WHERE BUKRS  EQ ME->NOTA-E_TOMADORA
       AND BRANCH EQ ME->NOTA-F_TOMADORA.

    SELECT SINGLE BSART
      INTO WA_NFE_IVA-BSART
      FROM EKKO
     WHERE EBELN EQ WA_ITEM-EBELN.

    "Concatenar para saida de mensagem
    CONCATENATE WA_NFE_IVA-BUKRS WA_NFE_IVA-BSART WA_NFE_IVA-LIFNR WA_NFE_IVA-INDUSTRY LC_TEXT_ICMS INTO SY-MSGV1 SEPARATED BY '/'.

    "Bucar por Fornecedor (Excessão)
    SELECT SINGLE * INTO WA_NFE_IVA
      FROM ZIB_NFE_DIST_IVA
     WHERE BUKRS    EQ WA_NFE_IVA-BUKRS
       AND BSART    EQ WA_NFE_IVA-BSART
       AND LIFNR    EQ WA_NFE_IVA-LIFNR
       AND INDUSTRY EQ WA_NFE_IVA-INDUSTRY
       AND ICMS     EQ WA_NFE_IVA-ICMS.

    "Bucar sem Fornecedor
    IF SY-SUBRC IS NOT INITIAL.
      SELECT SINGLE * INTO WA_NFE_IVA
        FROM ZIB_NFE_DIST_IVA
       WHERE BUKRS    EQ WA_NFE_IVA-BUKRS
         AND BSART    EQ WA_NFE_IVA-BSART
         AND INDUSTRY EQ WA_NFE_IVA-INDUSTRY
         AND ICMS     EQ WA_NFE_IVA-ICMS.
    ENDIF.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID    = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_IVA-MSGID
                               MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_IVA-MSGNO )
          MSGID     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_IVA-MSGID
          MSGNO     = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_DETERMINAR_IVA-MSGNO
          MSGV1     = SY-MSGV1
          TRANSACAO = ZCL_NFE_INBOUND=>TRANSACAO_PARAN_IVA.
    ELSE.
      R_MWSKZ = WA_NFE_IVA-MWSKZ.
      MESSAGE S054 WITH WA_NFE_IVA-MWSKZ SY-MSGV1.
    ENDIF.

  ENDMETHOD.


  METHOD GET_LOG_PROC_NFE.

    DATA: it_log TYPE TABLE OF zib_nfe_dist_log,
          wa_log TYPE zib_nfe_dist_log,
          wa_alv TYPE zde_nfe_dist_log_alv.

    CLEAR: e_logs.

    SELECT *
      INTO TABLE it_log
      FROM zib_nfe_dist_log
      WHERE chave_nfe EQ p_chave
      AND   ( type      EQ 'E' or NUM = 123 )
      ORDER BY nr_sequencia.

    LOOP AT it_log INTO wa_log.

      "Ctg.mens.: S sucesso, E erro, W aviso, I inform., A cancel.

      CASE wa_log-type.
        WHEN 'S'.
          wa_alv-ic_message = icon_led_green.
        WHEN 'E'.
          wa_alv-ic_message = icon_led_red.
        WHEN 'W'.
          wa_alv-ic_message = icon_led_yellow.
        WHEN 'I'.
          wa_alv-ic_message = icon_message_information_small.
        WHEN 'A'.
          wa_alv-ic_message = icon_incomplete.
      ENDCASE.

      IF wa_log-ck_estrategia EQ abap_true.
        CASE wa_log-type.
          WHEN 'S'.
            wa_alv-ic_texto = icon_allow.
          WHEN 'E'.
            wa_alv-ic_texto = icon_reject.
        ENDCASE.
      ENDIF.

      MOVE-CORRESPONDING wa_log TO wa_alv.
      APPEND wa_alv TO e_logs.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_MATERIAL.

    CHECK ME->PEDIDOS IS INITIAL.

    SELECT SINGLE *
      INTO @DATA(WA_LFA1)
      FROM LFA1
      WHERE LIFNR EQ @I_EMISSOR.

    IF WA_LFA1-STCD1 IS NOT INITIAL.

      CONCATENATE WA_LFA1-STCD1(8) '%' INTO WA_LFA1-STCD1.

      SELECT * INTO TABLE @DATA(IT_FORN)
        FROM LFA1
       WHERE STCD1 LIKE @WA_LFA1-STCD1.

      IF SY-SUBRC IS INITIAL.
        SELECT  * INTO TABLE @DATA(IT_PROD)
          FROM ZIB_NFE_DIST_001
           FOR ALL ENTRIES IN @IT_FORN
         WHERE P_EMISSOR        EQ @IT_FORN-LIFNR
           AND PROD_CODIGO      EQ @I_PROD_CODIGO
           AND PROD_UND_COMERCI EQ @I_UNIDADE_ITEM.
      ENDIF.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_PROD INTO R_001 INDEX 1.
      ENDIF.

    ELSE.
      SELECT SINGLE * INTO R_001
        FROM ZIB_NFE_DIST_001
       WHERE P_EMISSOR        EQ I_EMISSOR
         AND PROD_CODIGO      EQ I_PROD_CODIGO
         AND PROD_UND_COMERCI EQ I_UNIDADE_ITEM.
    ENDIF.

    "012  Material: Código &1 Material &2 Unidade &3 Fator &4!
    "013  Material não Encontrado: Código &1 Unidade &2!

    IF SY-SUBRC IS INITIAL.
      MESSAGE S012 WITH I_PROD_CODIGO R_001-MATNR R_001-MEINS R_001-FATOR.
    ELSE.
      MESSAGE E013 WITH I_PROD_CODIGO I_UNIDADE_ITEM RAISING NAO_ACHOU_DEPARADA.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MIGO_VALIDA.

    DATA: lc_xblnr TYPE xblnr1,
          l_serie  TYPE c LENGTH 3,
          it_migo  TYPE TABLE OF mkpf.

    CLEAR: r_migo.

    READ TABLE me->itens INTO DATA(wa_item) INDEX 1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = me->nota-numero
      IMPORTING
        output = lc_xblnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = me->nota-serie
      IMPORTING
        output = l_serie.

    CONCATENATE lc_xblnr l_serie INTO lc_xblnr SEPARATED BY '-'.

    IF me->nota-mblnr IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(wa_mkpf)
        FROM mkpf AS m
       WHERE m~mblnr EQ @me->nota-mblnr
         AND m~mjahr EQ @me->nota-mjahr
         AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).

      IF sy-subrc IS INITIAL AND wa_mkpf-xblnr EQ lc_xblnr.
        SELECT COUNT(*) FROM mseg
         WHERE  smbln = wa_mkpf-mblnr
         AND    sjahr = wa_mkpf-mjahr .
        IF sy-subrc NE 0.
          r_migo = wa_mkpf.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    "Buscar por Vinculo de NF-e
    SELECT SINGLE * INTO @r_migo
      FROM mkpf AS m
     WHERE m~zchave_nfe EQ @me->nota-chave_nfe
       AND m~xblnr      EQ @lc_xblnr
       AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).

    CHECK sy-subrc IS NOT INITIAL.

    IF me->pedidos IS INITIAL AND wa_item-ebeln IS NOT INITIAL AND wa_item-ebelp IS NOT INITIAL.
      wa_item-ebeln = wa_item-ebeln.
      wa_item-ebelp = wa_item-ebelp.
    ELSEIF me->pedidos IS NOT INITIAL.
      READ TABLE me->pedidos INDEX 1 INTO DATA(wa_pedido).
      wa_item-ebeln = wa_pedido-ebeln.
      wa_item-ebelp = wa_pedido-ebelp.
    ELSE.
      "Pedido de Compra Não informado
      EXIT.
    ENDIF.

    CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
      EXPORTING
        i_ebeln = wa_item-ebeln
        i_ebelp = wa_item-ebelp
      RECEIVING
        r_bstae = DATA(r_bstae).

    IF ( r_bstae EQ '0004' ) AND ( wa_item-deliv_numb IS NOT INITIAL ).
      "Buscar MIGO pelo Aviso de Recebimento
      SELECT SINGLE * INTO r_migo
        FROM mkpf AS m
       WHERE le_vbeln EQ wa_item-deliv_numb
         AND tcode2   EQ 'VL32N'
         AND bldat    GE me->nota-dt_emissao
         AND EXISTS ( SELECT * FROM mseg AS e WHERE e~mblnr EQ m~mblnr AND e~mjahr EQ m~mjahr AND e~lifnr EQ me->nota-p_emissor AND e~smbln EQ space )
         AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).
    ENDIF.

    CHECK r_bstae NE '0004'.

    TRY.
        EXEC SQL.
          OPEN SQL_MIRO FOR
            SELECT M.MBLNR, M.MJAHR
              FROM SAPHANADB.MKPF M
             WHERE M.MANDT = :SY-MANDT
               AND M.XBLNR = :LC_XBLNR
               AND M.BLDAT >= :ME->NOTA-DT_EMISSAO
               AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LIFNR = :ME->NOTA-P_EMISSOR AND E.SMBLN = ' ' )
               AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
        DATA(error_text) = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'E'.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT SQL_MIRO INTO
        :R_MIGO-MBLNR,
        :R_MIGO-MJAHR
      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND r_migo TO it_migo.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE SQL_MIRO
    ENDEXEC.

    IF it_migo[] IS INITIAL.

      TRY.
          EXEC SQL.
            OPEN SQL_MIRO_2 FOR
              SELECT M.MBLNR, M.MJAHR
                FROM SAPHANADB.MKPF M
               WHERE M.MANDT  = :SY-MANDT
                 AND M.XBLNR  = :LC_XBLNR
                 AND M.BLDAT >= :ME->NOTA-DT_EMISSAO
                 AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LLIEF = :ME->NOTA-P_EMISSOR AND E.SMBLN = ' ' )
                 AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
          ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).
          MESSAGE error_text TYPE 'E'.
      ENDTRY.

      DO.
        EXEC SQL.
          FETCH NEXT SQL_MIRO_2 INTO
          :R_MIGO-MBLNR,
          :R_MIGO-MJAHR
        ENDEXEC.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          APPEND r_migo TO it_migo.
        ENDIF.
      ENDDO.

      EXEC SQL.
        CLOSE SQL_MIRO_2
      ENDEXEC.

      IF it_migo[] IS NOT INITIAL.
        sy-subrc = 0.
      ENDIF.

    ELSE.
      sy-subrc = 0.
    ENDIF.

*    IF IT_MIGO[] IS INITIAL.
*      DATA(R_EKKO) = ZCL_PEDIDO_COMPRA=>GET_PEDIDO( EXPORTING I_EBELN = WA_ITEM-EBELN ).
*

*      TRY.
*          EXEC SQL.
*            OPEN SQL_MIRO_3 FOR
*              SELECT MBLNR, MJAHR
*                FROM SAPHANADB.MKPF M
*               WHERE MANDT = :SY-MANDT
*                 AND XBLNR = :LC_XBLNR
*                 AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LIFNR = :R_EKKO-LIFNR AND E.SMBLN = ' ' )
*                 AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
*          ENDEXEC.
*        CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
*          ERROR_TEXT = EXC_REF->GET_TEXT( ).
*          MESSAGE ERROR_TEXT TYPE 'E'.
*      ENDTRY.
*
*      DO.
*        EXEC SQL.
*          FETCH NEXT SQL_MIRO_3 INTO
*          :R_MIGO-MBLNR,
*          :R_MIGO-MJAHR
*        ENDEXEC.
*        IF SY-SUBRC <> 0.
*          EXIT.
*        ELSE.
*          APPEND R_MIGO TO IT_MIGO.
*        ENDIF.
*      ENDDO.
*
*      EXEC SQL.
*        CLOSE SQL_MIRO_3
*      ENDEXEC.
*
*      IF IT_MIGO[] IS NOT INITIAL.
*        SY-SUBRC = 0.
*      ENDIF.
*
*    ENDIF.
    "ALRS 22/10/2023 BUG
    DATA tabix TYPE sy-tabix.
    LOOP AT it_migo INTO r_migo.
      tabix = sy-tabix.
      SELECT COUNT(*) FROM mseg
        WHERE  smbln = r_migo-mblnr
        AND    sjahr = r_migo-mjahr .
      IF sy-subrc = 0.
        r_migo-xblnr = 'DELETE'.
        MODIFY it_migo FROM r_migo INDEX tabix TRANSPORTING xblnr.
      ENDIF.
    ENDLOOP.
    DELETE it_migo WHERE xblnr = 'DELETE'.
    CLEAR r_migo.
    IF it_migo[] IS NOT INITIAL.
      READ TABLE it_migo INDEX 1 INTO r_migo.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MIRO_VALIDA.

    DATA: LC_XBLNR TYPE XBLNR1,
          L_SERIE  TYPE C LENGTH 3.

    CLEAR: R_MIRO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = ME->NOTA-NUMERO
      IMPORTING
        OUTPUT = LC_XBLNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = ME->NOTA-SERIE
      IMPORTING
        OUTPUT = L_SERIE.

    CONCATENATE LC_XBLNR L_SERIE INTO LC_XBLNR SEPARATED BY '-'.

    SELECT * INTO TABLE @DATA(IT_MIRO)
      FROM RBKP
     WHERE XBLNR  EQ @LC_XBLNR
       AND LIFNR  EQ @ME->NOTA-P_EMISSOR
       AND BUKRS  EQ @ME->NOTA-E_TOMADORA
       AND RBSTAT NE '2'
       AND STBLG  EQ @SPACE
       AND BLDAT  GE @ME->NOTA-DT_EMISSAO.

    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_MIRO INDEX 1 INTO R_MIRO.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NFE_INBOUND_ALV_SAIDA.

    DATA: it_doc_ter  TYPE TABLE OF zib_nfe_dist_ter,
          it_aux      TYPE zib_cte_dist_ter_t,
          it_lfa1     TYPE TABLE OF lfa1,
          wa_lfa1     TYPE lfa1,
          it_zmmt0072 TYPE TABLE OF zmmt0072,
          wa_zmmt0072 TYPE zmmt0072,
          it_nfe_forn TYPE TABLE OF zib_nfe_forn,
          wa_nfe_forn TYPE zib_nfe_forn.

    DATA: wa_doc_ter        TYPE zib_nfe_dist_ter.

    DATA: wa_cte_dist	TYPE zib_cte_dist_ter,
          wa_alv      TYPE zde_cte_dist_ter_alv.

    CLEAR: e_alv.

    it_aux = i_cte_dist.
    DELETE it_aux WHERE p_emissor EQ space.

    IF it_aux IS NOT INITIAL.
      SELECT * INTO TABLE it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN it_aux
       WHERE lifnr EQ it_aux-p_emissor.

      SORT it_lfa1 BY lifnr.
    ENDIF.

    it_aux = i_cte_dist.
    DELETE it_aux WHERE cd_departamento EQ space.
    IF it_aux IS NOT INITIAL.
      SELECT * INTO TABLE it_zmmt0072
        FROM zmmt0072
         FOR ALL ENTRIES IN it_aux
       WHERE cd_departamento EQ it_aux-cd_departamento.

      SORT it_zmmt0072 BY cd_departamento.
    ENDIF.

    IF i_cte_dist[] IS NOT INITIAL.
      SELECT * INTO TABLE it_nfe_forn
        FROM zib_nfe_forn
         FOR ALL ENTRIES IN i_cte_dist
       WHERE nu_chave = i_cte_dist-cd_chave_cte.
      SORT it_nfe_forn BY nu_chave.
    ENDIF.

    LOOP AT i_cte_dist INTO wa_cte_dist.

      CLEAR: wa_alv, wa_nfe_forn.

      MOVE-CORRESPONDING wa_cte_dist TO wa_alv.

      READ TABLE it_nfe_forn INTO wa_nfe_forn WITH KEY nu_chave = wa_cte_dist-cd_chave_cte
                             BINARY SEARCH.
*      IF wa_nfe_forn-st_nota = '1'.
*        wa_alv-ds_st_nota = 'Ativo'.
*      ELSE.
*        wa_alv-ds_st_nota = 'Cancelado'.
*      ENDIF.

      CLEAR: wa_alv-p_emissor_name.

      IF wa_cte_dist-p_emissor IS NOT INITIAL.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_cte_dist-p_emissor BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_alv-p_emissor_name = wa_lfa1-name1.
        ENDIF.
      ENDIF.

      IF wa_cte_dist-docnum_cte IS NOT INITIAL.
        wa_alv-ico_st_documento = icon_complete.
      ELSEIF wa_cte_dist-docnum_cte IS INITIAL.
        wa_alv-ico_st_documento = icon_initial.
      ELSE.
        CLEAR: wa_alv-ico_st_documento.
      ENDIF.

*      IF wa_alv-cd_departamento IS NOT INITIAL.
*        READ TABLE it_zmmt0072 INTO wa_zmmt0072 WITH KEY cd_departamento = wa_alv-cd_departamento BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          wa_alv-ds_departamento = wa_zmmt0072-ds_departamento.
*        ENDIF.
*      ENDIF.

      "Cores
      IF wa_cte_dist-docnum_cte IS NOT INITIAL.
        wa_alv-rowcolor = 'C500'.
      ELSEIF wa_cte_dist-cd_departamento IS NOT INITIAL.
        wa_alv-rowcolor = 'C100'.
      ELSEIF wa_cte_dist-p_emissor IS INITIAL.
        wa_alv-rowcolor = 'C601'.
      ELSEIF wa_cte_dist-e_tomadora IS INITIAL.
        wa_alv-rowcolor = 'C600'.
      ENDIF.

      IF wa_cte_dist-st_documento IS INITIAL.
        wa_cte_dist-st_documento = '00'.
      ENDIF.

      IF wa_cte_dist-st_fiscal IS INITIAL.
        wa_cte_dist-st_fiscal = '00'.
      ENDIF.

      IF wa_cte_dist-st_fisico IS INITIAL.
        wa_cte_dist-st_fisico = '00'.
      ENDIF.

      IF wa_cte_dist-st_armazem IS INITIAL.
        wa_cte_dist-st_armazem = '00'.
      ENDIF.

      wa_alv-ico_st_documento = zcl_nfe_inbound=>get_icon_status_documento( i_status = wa_cte_dist-st_documento ).
      wa_alv-ico_st_fiscal    = zcl_nfe_inbound=>get_icon_status_fiscal( i_status = wa_cte_dist-st_fiscal ).
      wa_alv-ico_st_fisico    = zcl_nfe_inbound=>get_icon_status_fisico( i_status = wa_cte_dist-st_fisico ).
      wa_alv-ico_st_armazem   = zcl_nfe_inbound=>get_icon_status_armazem( i_status = wa_cte_dist-st_armazem ).

      APPEND wa_alv TO e_alv.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_OBRIGATORIO_NR_FASE.

    DATA: IT_MATERIAIS  TYPE ZIB_NFE_DIST_ITM_T.

    R_CK_OBRIGATORIO = ABAP_FALSE.

    "Buscar Informações do Departamento
    CREATE OBJECT ME->DEPARTAMENTO.
    ME->DEPARTAMENTO->SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
    DATA(IT_GRUPOS) = ME->DEPARTAMENTO->GET_GRUPO_MERCADORIA( ).
    CLEAR: ME->DEPARTAMENTO.

    SORT IT_GRUPOS BY MATKL.

    IF ME->PEDIDOS IS INITIAL.
      "Verifica Materiais """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      MOVE ME->ITENS TO IT_MATERIAIS.
      DELETE IT_MATERIAIS WHERE MATNR EQ SPACE.
      SORT IT_MATERIAIS BY MATNR.
      DELETE ADJACENT DUPLICATES FROM IT_MATERIAIS COMPARING MATNR.

      IF IT_MATERIAIS IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_MARA)
          FROM MARA
           FOR ALL ENTRIES IN @IT_MATERIAIS
         WHERE MATNR EQ @IT_MATERIAIS-MATNR.
      ENDIF.
    ELSE.
      SELECT * INTO TABLE IT_MARA
        FROM MARA
         FOR ALL ENTRIES IN ME->PEDIDOS
       WHERE MATNR EQ ME->PEDIDOS-MATNR.
    ENDIF.

    LOOP AT IT_MARA INTO DATA(WA_MARA).
      READ TABLE IT_GRUPOS INTO DATA(WA_GRUPO) WITH KEY MATKL = WA_MARA-MATKL BINARY SEARCH.
      IF ( SY-SUBRC IS INITIAL ) AND ( WA_GRUPO-CK_NR_FASE EQ ABAP_TRUE ).
        R_CK_OBRIGATORIO = ABAP_TRUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_PEDIDO_COMPRA_CHAVE.

    DATA: OB_REPOSITORY TYPE REF TO ZCL_REPOSITORY_CLASSES,
          I_ZMMT0075    TYPE ZDE_ZMMT0075_T,
          I_LIFNR	      TYPE LIFNR,
          I_BUKRS	      TYPE BUKRS,
          I_WERKS	      TYPE EWERK,
          I_MATNR	      TYPE MATNR,
          I_LGORT	      TYPE LGORT_D,
          I_EBELN	      TYPE EBELN,
          I_EBELP	      TYPE EBELP,
          "I_BSTYP        TYPE EBSTYP,
          "I_BSART        TYPE ESART,
          I_MENGE_GE    TYPE BSTMG,
          I_MEINS	      TYPE BSTME.

*021  Não Informado Emissor &1!
*023  Não Informado Material &1 &2!
*024  Não Informado Empresa &1!
*025  Não Informado Local de Negócio &1!
*026  Não Informado Quantidade &1 &2!
*027  Não Informado Unidade de Estoque &1 &2!
*029  Não Foi Determinado um Departamento &1!

    IF I_NOTA-P_EMISSOR IS INITIAL.
      MESSAGE E021 WITH I_NOTA-NUMERO RAISING ERRO.
    ENDIF.

    IF I_NOTA-E_TOMADORA IS INITIAL.
      MESSAGE E024 WITH I_NOTA-NUMERO RAISING ERRO.
    ENDIF.

    IF I_NOTA-F_TOMADORA IS INITIAL.
      MESSAGE E025 WITH I_NOTA-NUMERO RAISING ERRO.
    ENDIF.

    IF I_ITEM-MATNR IS INITIAL.
      MESSAGE E023 WITH I_NOTA-NUMERO I_ITEM-PROD_ITEM RAISING ERRO.
    ENDIF.

    IF I_ITEM-MENGE IS INITIAL.
      MESSAGE E026 WITH I_NOTA-NUMERO I_ITEM-PROD_ITEM RAISING ERRO.
    ENDIF.

    IF I_ITEM-MEINS IS INITIAL.
      MESSAGE E027 WITH I_NOTA-NUMERO I_ITEM-PROD_ITEM RAISING ERRO.
    ENDIF.

    IF I_NOTA-CD_DEPARTAMENTO IS INITIAL.
      MESSAGE E029 WITH I_NOTA-NUMERO RAISING ERRO.
    ENDIF.

    I_LIFNR    = I_NOTA-P_EMISSOR.
    I_BUKRS    = I_NOTA-E_TOMADORA.
    I_WERKS    = I_NOTA-F_TOMADORA.
    I_MATNR    = I_ITEM-MATNR.
    I_MENGE_GE = I_ITEM-MENGE.
    I_MEINS    = I_ITEM-MEINS.
    I_EBELN    = I_ITEM-EBELN.
    I_EBELP    = I_ITEM-EBELP.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0072)
      FROM ZMMT0072
     WHERE CD_DEPARTAMENTO EQ @I_NOTA-CD_DEPARTAMENTO.

    IF WA_ZMMT0072-CK_SEM_RET_PEDIDO EQ ABAP_FALSE.
      SELECT * INTO TABLE @DATA(IT_ZMMT0075)
        FROM ZMMT0075
       WHERE CD_DEPARTAMENTO EQ @I_NOTA-CD_DEPARTAMENTO.

      MOVE IT_ZMMT0075[] TO I_ZMMT0075.
    ENDIF.

    CREATE OBJECT OB_REPOSITORY.

    OB_REPOSITORY->PEDIDO_COMPRA( ).

    CALL METHOD OB_REPOSITORY->AT_PEDIDO_COMPRA->GET_PEDIDO_COMPRA_CHAVE_E
      EXPORTING
        I_LIFNR               = I_LIFNR
        I_BUKRS               = I_BUKRS
        I_WERKS               = I_WERKS
        I_MATNR               = I_MATNR
        I_LGORT               = I_LGORT
        I_EBELN               = I_EBELN
        I_EBELP               = I_EBELP
        "I_BSTYP    = I_BSTYP
        "I_BSART    = I_BSART
        I_MENGE_GE            = I_MENGE_GE
        I_MEINS               = I_MEINS
        I_ZMMT0075            = I_ZMMT0075
      IMPORTING
        R_SALDO_ITEM          = E_SALDO_ITEM
      RECEIVING
        R_EKPO                = R_EKPO
      EXCEPTIONS
        NAO_ENCONTRADO_PEDIDO = 1
        OTHERS                = 2.

    IF SY-SUBRC IS NOT INITIAL.
      CLEAR: OB_REPOSITORY.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
    ENDIF.

    IF R_EKPO IS NOT INITIAL AND ME->PEDIDOS IS INITIAL.
      ME->SET_PEDIDO_COMPRA( I_PROD_ITEM = I_ITEM-PROD_ITEM I_EBELN = R_EKPO-EBELN I_EBELP = R_EKPO-EBELP ).
    ENDIF.

    CLEAR: OB_REPOSITORY.

  ENDMETHOD.


  METHOD GET_POSSUI_ADIANTAMENTO.

    CLEAR: R_CK_POSSUI.

    IF ME->PEDIDOS IS INITIAL.

      SELECT * INTO TABLE @DATA(IT_EKBE)
        FROM EKBE
         FOR ALL ENTRIES IN @ME->ITENS
       WHERE EBELN EQ @ME->ITENS-EBELN
         AND EBELP EQ @ME->ITENS-EBELP
         AND EBELN NE @SPACE
         AND BEWTP EQ 'A'
         AND VGABE EQ '4'.

    ELSE.

      SELECT * INTO TABLE @IT_EKBE
        FROM EKBE
         FOR ALL ENTRIES IN @ME->PEDIDOS
       WHERE EBELN EQ @ME->PEDIDOS-EBELN
         AND EBELP EQ @ME->PEDIDOS-EBELP
         AND EBELN NE @SPACE
         AND BEWTP EQ 'A'
         AND VGABE EQ '4'.

    ENDIF.

    IF SY-SUBRC IS INITIAL.
      R_CK_POSSUI = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD GET_REMESSA_NFE_INBOUND.

    DATA: REF_NUMBER TYPE XBLNR1.

    CLEAR: R_LIKP.

    CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
      EXPORTING
        SERIES     = ME->NOTA-SERIE
        NF_NUMBER9 = ME->NOTA-NUMERO
      IMPORTING
        REF_NUMBER = REF_NUMBER.

    SELECT SINGLE * FROM LIKP INTO R_LIKP
      WHERE XBLNR EQ REF_NUMBER
        AND LIFNR EQ ME->NOTA-P_EMISSOR.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE @DATA(IT_VTTP)
      FROM VTTP
     WHERE VBELN EQ @R_LIKP-VBELN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE @DATA(IT_VTTK)
      FROM VTTK
      FOR ALL ENTRIES IN @IT_VTTP
     WHERE TKNUM  EQ @IT_VTTP-TKNUM
       AND VSART  EQ '01'.

    CHECK SY-SUBRC IS INITIAL.

    READ TABLE IT_VTTK INDEX 1 INTO E_VTTK.

    SELECT SINGLE * INTO E_VFKP
      FROM VFKP
     WHERE REBEL EQ E_VTTK-TKNUM.

  ENDMETHOD.


  METHOD GET_ROMANEIO.

    DATA: I_ROMANEIO TYPE ZSDT0001.

    CLEAR: R_ZSDT0001.

    I_ROMANEIO-TP_MOVIMENTO = 'S'.
    I_ROMANEIO-BUKRS        = ME->NOTA-E_TOMADORA.
    I_ROMANEIO-BRANCH       = ME->NOTA-F_TOMADORA.
    I_ROMANEIO-ID_CLI_DEST  = ME->NOTA-F_ARMAZEM.
    I_ROMANEIO-PARID        = ME->NOTA-F_TOMADORA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_ROMANEIO-PARID
      IMPORTING
        OUTPUT = I_ROMANEIO-PARID.
    I_ROMANEIO-NFNUM        = ME->NOTA-NUMERO.
    I_ROMANEIO-SERIES       = ME->NOTA-SERIE.
    I_ROMANEIO-DOCDAT       = ME->NOTA-DT_EMISSAO.
    I_ROMANEIO-NFE          = ABAP_TRUE.
    I_ROMANEIO-ID_INTERFACE = ZCL_ROMANEIO=>INTERFACE_REM_ARM_NFE_INBOUND.

    SELECT SINGLE * INTO R_ZSDT0001
      FROM ZSDT0001
     WHERE TP_MOVIMENTO EQ I_ROMANEIO-TP_MOVIMENTO
       AND BUKRS        EQ I_ROMANEIO-BUKRS
       AND BRANCH       EQ I_ROMANEIO-BRANCH
       AND ID_CLI_DEST  EQ I_ROMANEIO-ID_CLI_DEST
       AND PARID        EQ I_ROMANEIO-PARID
       AND NFNUM        EQ I_ROMANEIO-NFNUM
       AND SERIES       EQ I_ROMANEIO-SERIES
       AND DOCDAT       EQ I_ROMANEIO-DOCDAT
       AND NFE          EQ I_ROMANEIO-NFE
       AND ID_INTERFACE EQ I_ROMANEIO-ID_INTERFACE.

  ENDMETHOD.


  METHOD GET_SEQUENCIA_LOG.

    SELECT MAX( NR_SEQUENCIA ) INTO R_LC_SEQUENCIA
      FROM ZIB_NFE_DIST_LOG
     WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

    IF R_LC_SEQUENCIA IS INITIAL.
      R_LC_SEQUENCIA = 1.
    ELSE.
      ADD 1 TO R_LC_SEQUENCIA.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TOMADOR.

    DATA: WA_LFA1 TYPE LFA1.

    MOVE: I_CNPJ TO R_J_1BBRANCH-STCD1,
          I_IE   TO R_J_1BBRANCH-STATE_INSC.

*008  Tomador: CNPJ: &1 IE: &2
*009  Tomador não Loc.: CNPJ: &1 IE: &2

    IF R_J_1BBRANCH-STATE_INSC IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = R_J_1BBRANCH-STATE_INSC
        IMPORTING
          OUTPUT = R_J_1BBRANCH-STATE_INSC.

      CONCATENATE '%' R_J_1BBRANCH-STATE_INSC INTO R_J_1BBRANCH-STATE_INSC.
    ENDIF.

    SELECT SINGLE * INTO R_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE STCD1      EQ   R_J_1BBRANCH-STCD1
       AND STATE_INSC LIKE R_J_1BBRANCH-STATE_INSC
       AND BRANCH     NE   '0001'. "Local de Negócio identificar CNPJ matriz para FI

    IF SY-SUBRC IS INITIAL.
      MESSAGE S008 WITH I_CNPJ I_IE.
    ELSE.
      WA_LFA1-STCD1 = R_J_1BBRANCH-STCD1.
      WA_LFA1-STCD3 = R_J_1BBRANCH-STATE_INSC.

      SELECT SINGLE * INTO WA_LFA1
        FROM LFA1
       WHERE STCD1 EQ   WA_LFA1-STCD1
         AND STCD3 LIKE WA_LFA1-STCD3
         AND KTOKK EQ   'ZFIC'.              "<<RIM-SKM-IR122559-10.02.23

      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_LFA1-LIFNR
          IMPORTING
            OUTPUT = WA_LFA1-LIFNR.

        R_J_1BBRANCH-BRANCH = WA_LFA1-LIFNR+6(4).

        SELECT SINGLE * INTO R_J_1BBRANCH
          FROM J_1BBRANCH
         WHERE BRANCH EQ R_J_1BBRANCH-BRANCH.

        IF SY-SUBRC IS INITIAL.
          MESSAGE S008 WITH I_CNPJ I_IE.
        ELSE.
          "MESSAGE E009 WITH I_CNPJ I_IE RAISING NAO_ACHOU_PARCEIRO.

          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGID
                                MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGNO
                                ATTR1 = I_CNPJ
                                ATTR2 = I_IE )
              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGID
              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_TOMADOR_NAO_LOCALIZADO-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( I_CNPJ )
              MSGV2  = CONV #( I_IE ).
        ENDIF.
      ELSE.
        "MESSAGE E009 WITH I_CNPJ I_IE RAISING NAO_ACHOU_PARCEIRO.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_VALIDA_ITEM_PEDIDO.

    DATA: lc_msg TYPE string.

    e_validado = abap_false.
    CLEAR: e_msg.

    DATA(lc_erro) = abap_false.

    "Validar se o material está expandido para o centro informado no pedido. Caso esteja prosseguir com as validações seguintes;
    "Validar se o material não possui NCM cadastrado;
    "Validar se o material não possui a categoria do CFOP informada;
    "Utilização do material e origem do material na aba contabilidade.

    SELECT SINGLE * INTO @DATA(wa_ekko)
      FROM ekpo
     WHERE ebeln EQ @i_ebeln
       AND ebelp EQ @i_ebelp.

    zcl_pedido_compra=>get_pedido_itens(
      EXPORTING
        i_ebeln          = i_ebeln
        i_ebelp          = i_ebelp
      IMPORTING
        e_ekko           = DATA(e_ekko)
        e_ekpo_t         = DATA(e_ekpo_t)
      EXCEPTIONS
        nao_achou_pedido = 1
        OTHERS           = 2 ).

    IF sy-subrc IS NOT INITIAL.
      IF i_gerar_exception EQ abap_true.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgty  = sy-msgty
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lc_msg.
        e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
        lc_erro = abap_true.
      ENDIF.
    ENDIF.

    READ TABLE e_ekpo_t INTO DATA(wa_ekpo) INDEX 1.

    SELECT SINGLE * INTO @DATA(wa_marc)
      FROM marc
     WHERE matnr EQ @wa_ekpo-matnr
       AND werks EQ @i_nfe-f_tomadora.

    "MTUSE  J_1BMATUSE  CHAR  1 0 Utilização de material
    "MTORG  J_1BMATORG  CHAR  1 0 Origem de material

    IF wa_ekpo-loekz IS NOT INITIAL.
      IF i_gerar_exception EQ abap_true.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgno
                              attr1 = CONV #( wa_ekpo-ebelp ) )
            msgid  = zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgno
            msgty  = 'E'
            msgv1  = CONV #( wa_ekpo-ebelp ).

      ELSE.
        MESSAGE ID zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_item_pedido_bloq-msgno
           WITH wa_ekpo-ebelp INTO lc_msg.
        e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
        lc_erro = abap_true.
      ENDIF.
    ELSEIF sy-subrc IS NOT INITIAL.
      IF i_gerar_exception EQ abap_true.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_material_centro-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_material_centro-msgno
                              attr1 = CONV #( wa_ekpo-matnr )
                              attr2 = CONV #( i_nfe-f_tomadora ) )
            msgid  = zcx_nfe_inbound_exception=>zcx_material_centro-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_material_centro-msgno
            msgty  = 'E'
            msgv1  = CONV #( wa_ekpo-matnr )
            msgv2  = CONV #( i_nfe-f_tomadora ).
      ELSE.
        MESSAGE ID zcx_nfe_inbound_exception=>zcx_material_centro-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_material_centro-msgno
           WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
        e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
        lc_erro = abap_true.
      ENDIF.
    ELSEIF sy-subrc IS INITIAL.
      IF wa_marc-steuc IS INITIAL.
        IF i_gerar_exception EQ abap_true.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgno
                                attr1 = CONV #( wa_ekpo-matnr )
                                attr2 = CONV #( i_nfe-f_tomadora ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_ekpo-matnr )
              msgv2  = CONV #( i_nfe-f_tomadora ).
        ELSE.
          MESSAGE ID zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_material_centro_ncm-msgno
             WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
          e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
          lc_erro = abap_true.
        ENDIF.
      ENDIF.
      IF wa_marc-indus IS INITIAL.
        IF i_gerar_exception EQ abap_true.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgno
                                attr1 = CONV #( wa_ekpo-matnr )
                                attr2 = CONV #( i_nfe-f_tomadora ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_ekpo-matnr )
              msgv2  = CONV #( i_nfe-f_tomadora ).
        ELSE.
          MESSAGE ID zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_sem_categoria_cfop-msgno
             WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
          e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
          lc_erro = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_mbew)
      FROM mbew
     WHERE matnr EQ @wa_ekpo-matnr
       AND bwkey EQ @i_nfe-f_tomadora.

    IF sy-subrc IS NOT INITIAL.

      IF i_gerar_exception EQ abap_true.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgno
                              attr1 = CONV #( wa_ekpo-matnr )
                              attr2 = CONV #( i_nfe-f_tomadora ) )
            msgid  = zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgno
            msgty  = 'E'
            msgv1  = CONV #( wa_ekpo-matnr )
            msgv2  = CONV #( i_nfe-f_tomadora ).
      ELSE.
        MESSAGE ID zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_sem_area_valiacao-msgno
           WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
        e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
        lc_erro = abap_true.
      ENDIF.

    ELSEIF sy-subrc IS INITIAL.

      IF wa_mbew-mtuse IS INITIAL.
        IF i_gerar_exception EQ abap_true.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgno
                                attr1 = CONV #( wa_ekpo-matnr )
                                attr2 = CONV #( i_nfe-f_tomadora ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_ekpo-matnr )
              msgv2  = CONV #( i_nfe-f_tomadora ).
        ELSE.
          MESSAGE ID zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_sem_utl_material-msgno
             WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
          e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
          lc_erro = abap_true.
        ENDIF.
      ENDIF.

      IF wa_mbew-mtorg IS INITIAL.
        IF i_gerar_exception EQ abap_true.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_org_material-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_sem_org_material-msgno
                                attr1 = CONV #( wa_ekpo-matnr )
                                attr2 = CONV #( i_nfe-f_tomadora ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_sem_org_material-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_sem_org_material-msgno
              msgty  = 'E'
              msgv1  = CONV #( wa_ekpo-matnr )
              msgv2  = CONV #( i_nfe-f_tomadora ).
        ELSE.
          MESSAGE ID zcx_nfe_inbound_exception=>zcx_sem_org_material-msgid TYPE 'E' NUMBER zcx_nfe_inbound_exception=>zcx_sem_org_material-msgno
             WITH wa_ekpo-matnr i_nfe-f_tomadora INTO lc_msg.
          e_msg = zcl_string=>concat( s1 = e_msg s2 = lc_msg sp = ' ***' ).
          lc_erro = abap_true.
        ENDIF.
      ENDIF.

    ENDIF.

    IF lc_erro NE abap_true.
      e_validado = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD GET_VALOR_NOTA_FISCAL_FATURA.

    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          i_data          TYPE gdatu_inv.

    CLEAR e_sinal.

    e_valor_produtos = me->nota-vl_produtos.

    IF me->pedidos IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(it_ekko)
        FROM ekko
         FOR ALL ENTRIES IN @me->pedidos
       WHERE ebeln EQ @me->pedidos-ebeln.
    ELSEIF me->itens[] IS NOT INITIAL.
      SELECT *
        INTO TABLE it_ekko
        FROM ekko
         FOR ALL ENTRIES IN me->itens
       WHERE ebeln EQ me->itens-ebeln.
    ENDIF.

    CHECK it_ekko[] IS NOT INITIAL.

    READ TABLE it_ekko INDEX 1 INTO DATA(wa_ekko).

    e_waers = wa_ekko-waers.
    e_kufix = wa_ekko-kufix.
    e_zterm = wa_ekko-zterm.

    DATA(r_zmmt0075) = me->get_config_tipo_pedido( ).

    IF r_zmmt0075-ck_nao_valida_venc_ped EQ abap_true.
      e_zterm = '0004'.
    ENDIF.

    IF wa_ekko-waers NE 'BRL'.

      CREATE OBJECT obj_zcl_util_sd.
      i_data = me->nota-dt_emissao.
      obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'BRL' ).
      obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_ekko-waers ).
      DATA(e_ukurs) = obj_zcl_util_sd->taxa_cambio( ).

      CASE e_kufix.
        WHEN abap_true.
          e_wkurs = abs( wa_ekko-wkurs ).
          IF e_ukurs LT 0.
            e_valor_total = me->nota-vl_total / wa_ekko-wkurs.
            e_valor_produtos = e_valor_produtos / wa_ekko-wkurs.
            e_sinal = '/'.
          ELSE.
            e_valor_total = me->nota-vl_total * wa_ekko-wkurs.
            e_valor_produtos = e_valor_produtos * wa_ekko-wkurs.
            e_sinal = '*'.
          ENDIF.
        WHEN abap_false.
          e_wkurs = abs( e_ukurs ).
          IF e_ukurs LT 0.
            e_valor_total = me->nota-vl_total / e_ukurs.
            e_valor_produtos = e_valor_produtos / e_ukurs.
            e_sinal = '/'.
          ELSE.
            e_valor_total = me->nota-vl_total * e_ukurs.
            e_valor_produtos = e_valor_produtos * e_ukurs.
            e_sinal = '*'.
          ENDIF.
      ENDCASE.
      CLEAR: obj_zcl_util_sd.
    ELSE.
      e_valor_total = me->nota-vl_total.
    ENDIF.

    e_valor_total    = abs( e_valor_total ).
    e_valor_produtos = abs( e_valor_produtos ).

  ENDMETHOD.


  METHOD GRAVAR_ROMANEIO_FERTILIZANTE.

    DATA: l_lines_emb TYPE i,
          l_check_rom TYPE char1,
          l_total     TYPE mseg-menge,
          l_docnum    TYPE j_1bnfdoc-docnum,
          l_meins     TYPE j_1bnflin-meins,
          l_menge     TYPE j_1bnflin-menge,
          l_saldo     TYPE zsdt0082-qte_lib,
          l_grava_rom TYPE char1,
          l_chave     TYPE zde_chave_sol,
          t_tab_emb   TYPE zsds060_t,
          t_tab_sug   TYPE zsds060_t,
          t_sdo_emb   TYPE zsds060_t,
          w_tab_emb   TYPE zsds060,
          w_sdo_emb   TYPE zsds060,
          w_zsdt0138  TYPE zsdt0138.

    FREE : l_total,
           t_tab_emb,
           t_tab_sug,
           w_zsdt0138.

    CHECK me->nota-aut_embarque IS NOT INITIAL.

*-----------------------------------
*---Separa aut. embarque
*-----------------------------------
    me->set_validar_aut_embarque(
        EXPORTING i_aut_embarque = me->nota-aut_embarque
        IMPORTING t_tab_embarque = t_tab_emb
                  t_tab_embsugst = t_tab_sug ).

*-----------------------------------
*---total da nota
*-----------------------------------
    SELECT * FROM mseg
             INTO TABLE @DATA(t_mseg)
            WHERE mblnr = @me->nota-mblnr
              AND mjahr = @me->nota-mjahr
              AND bwart = '101'.

    LOOP AT t_mseg INTO DATA(w_mseg).
      l_total = l_total + w_mseg-menge.
    ENDLOOP.

    DESCRIBE TABLE t_tab_emb LINES l_lines_emb.

    IF l_lines_emb = 1.
      LOOP AT t_tab_emb INTO w_tab_emb.
        MOVE-CORRESPONDING w_tab_emb    TO w_sdo_emb.
        MOVE l_total                    TO w_sdo_emb-saldo.
        APPEND w_sdo_emb                TO t_sdo_emb.
      ENDLOOP.
    ELSE.
      LOOP AT t_tab_emb INTO w_tab_emb.
        IF w_tab_emb-saldo >= l_total.
          MOVE-CORRESPONDING w_tab_emb  TO w_sdo_emb.
          MOVE l_total                  TO w_sdo_emb-saldo.
          APPEND w_sdo_emb              TO t_sdo_emb.
          EXIT.
        ELSE.
          APPEND w_tab_emb              TO t_sdo_emb.
          l_total = l_total - w_tab_emb-saldo.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF t_sdo_emb[] IS INITIAL.
      APPEND LINES OF t_tab_sug[] TO t_sdo_emb[].
    ENDIF.

*-----------------------------------
*---gravar ZSDT0138
*-----------------------------------
    LOOP AT t_sdo_emb  INTO w_sdo_emb.

      CLEAR: w_zsdt0138, l_docnum, l_meins, l_menge.

      SELECT SINGLE *
               FROM zsdt0138
               INTO w_zsdt0138
              WHERE seq_cam      = w_sdo_emb-seq_cam
                AND nro_sol      = w_sdo_emb-nro_sol
                AND seq          = w_sdo_emb-seq
                AND filial_resp  = w_sdo_emb-filial_resp.

      CHECK w_zsdt0138-nfenum IS INITIAL.

*-----nota fiscal
      SELECT docnum
        INTO l_docnum
        FROM j_1bnfe_active
          UP TO 1 ROWS
       WHERE docsta  = me->nota-docsta
         AND regio   = me->nota-regio
         AND model   = me->nota-model
         AND nfnum9  = me->nota-numero
         AND docnum9 = me->nota-docnum9.
      ENDSELECT.

      SELECT     meins    menge
        INTO ( l_meins, l_menge )
        FROM j_1bnflin
         UP TO 1  ROWS
       WHERE docnum = l_docnum.
      ENDSELECT.

      w_zsdt0138-mandt        = sy-mandt.

      IF w_sdo_emb-filial_resp EQ 'TCOR'.
        w_zsdt0138-seq_cam      = w_sdo_emb-seq_cam + 1.
      ELSE.
        w_zsdt0138-seq_cam      = w_sdo_emb-seq_cam.
      ENDIF.

      w_zsdt0138-nro_sol      = w_sdo_emb-nro_sol.
      w_zsdt0138-seq          = w_sdo_emb-seq.
      w_zsdt0138-filial_resp  = w_sdo_emb-filial_resp.
      w_zsdt0138-qtd_embarq   = l_menge. "w_sdo_emb-saldo.
      w_zsdt0138-um           = l_meins.
      w_zsdt0138-ebeln        = me->nota-ebeln.
      w_zsdt0138-nfenum       = me->nota-numero.
      w_zsdt0138-series       = me->nota-serie.
      w_zsdt0138-placa_cav    = me->nota-placa_cav.
      w_zsdt0138-status       = 1.
      w_zsdt0138-netwr        = me->nota-vl_total.
      w_zsdt0138-docdat_nf    = me->nota-dt_emissao.
      MODIFY zsdt0138      FROM w_zsdt0138.

*-----------------------------------
*---set tabela caminhoes
*-----------------------------------
      PERFORM grava_informacoes_caminhao IN PROGRAM zsdr0060    USING w_zsdt0138-nro_sol
                                                                      w_zsdt0138-seq
                                                                      w_zsdt0138-seq_cam
                                                                      w_zsdt0138-filial_resp
                                                                      w_zsdt0138-qtd_embarq
                                                                      w_zsdt0138-placa_cav
                                                                      w_zsdt0138-placa_car1
                                                                      w_zsdt0138-placa_car2
                                                                      w_zsdt0138-placa_car3
                                                                      w_zsdt0138-motorista.

*-----------------------------------
*---gravar romaneio
*-----------------------------------
      CASE w_zsdt0138-filial_resp.
        WHEN 'TCOR'.
          PERFORM salva_romaneios_5420   IN PROGRAM zsdr0060 CHANGING l_check_rom.
        WHEN 'TPGA'.
          PERFORM salva_romaneios_5520   IN PROGRAM zsdr0060 CHANGING l_check_rom.
        WHEN 'TROO'.
          PERFORM salva_romaneios_5620   IN PROGRAM zsdr0060 CHANGING l_check_rom.
        WHEN OTHERS.
          PERFORM salva_romaneios_5620   IN PROGRAM zsdr0060 CHANGING l_check_rom.
      ENDCASE.

*-----------------------------------
*---libera lock registro
*-----------------------------------
      SELECT * FROM zsdt0082
               INTO TABLE @DATA(t_zsdt0082)
              WHERE nro_sol   = @w_zsdt0138-nro_sol
                AND seq       = @w_zsdt0138-seq.

      DELETE t_zsdt0082 WHERE NOT ( dt_canc  IS     INITIAL
                              AND   dt_liber IS NOT INITIAL ).

      READ TABLE t_zsdt0082 INTO DATA(w_zsdt0082) INDEX 1.

      IF sy-subrc = 0.
        "//Dequeue register;
        CONCATENATE w_zsdt0082-nro_sol
                    w_zsdt0082-seq
                    w_zsdt0082-vbeln
                    w_zsdt0082-posnr INTO l_chave.

        CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
          EXPORTING
            chave = l_chave.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD MOD_PEDIDO_NOTA.
    DATA: zcod_item TYPE zde_prod_codigo.
    DATA xachou(1).
    LOOP AT me->pedidos INTO DATA(_ped).
      CLEAR xachou.
      DATA(_tabix) = sy-tabix.
      SELECT SINGLE * INTO @DATA(wa_ekpo)
        FROM ekpo
        WHERE ebeln EQ @_ped-ebeln
        AND ebelp   EQ @_ped-ebelp.

      SELECT SINGLE *
       FROM zpmt0034
       INTO @DATA(_zpmt0034)
       WHERE werks = @wa_ekpo-werks
       AND   matnr = @wa_ekpo-matnr.


*      SELECT SINGLE *
*        FROM marc
*        INTO @DATA(_marc)
*        WHERE werks = @wa_ekpo-werks
*        AND   matnr = @wa_ekpo-matnr.
      CLEAR: zcod_item.
      IF sy-subrc = 0.
        zcod_item = _zpmt0034-cod_material.
        zcod_item = |{ zcod_item ALPHA = IN }|.
*        DATA(v_nbm) = _zpmt0034-cod_material."_marc-steuc.
*        REPLACE ALL OCCURRENCES OF '.' IN v_nbm WITH ' '.
*        CONDENSE v_nbm NO-GAPS.
        LOOP AT me->itens INTO DATA(_item).
          _item-prod_codigo = |{ _item-prod_codigo ALPHA = IN }|.
          IF zcod_item EQ _item-prod_codigo."_item-prod_ncm.
            xachou = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF xachou IS INITIAL.
          _ped-mwskz = '99'.
          MODIFY  me->pedidos FROM _ped INDEX _tabix TRANSPORTING mwskz.
        ELSEIF _ped-menge NE _item-prod_qtd_comerci.
          _ped-menge =  _item-prod_qtd_comerci.
          _ped-total = ( _ped-menge * ( _ped-netpr / wa_ekpo-peinh  ) ).
          MODIFY  me->pedidos FROM _ped INDEX _tabix TRANSPORTING menge total.
*          MODIFY  me->pedidos FROM _ped INDEX _tabix TRANSPORTING total .
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE me->pedidos WHERE mwskz = '99'.

    IF me->pedidos[] IS INITIAL.
      r_vazio = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD NFE_ARM_ATUALIZA_CFOP_FISCAL.

    DATA: OBJ_HEADER        TYPE J_1BNFDOC,
          OBJ_PARTNER       TYPE TABLE OF J_1BNFNAD,
          OBJ_ITEM          TYPE TABLE OF J_1BNFLIN,
          OBJ_ITEM_TAX      TYPE TABLE OF J_1BNFSTX,
          OBJ_HEADER_MSG    TYPE TABLE OF J_1BNFFTX,
          OBJ_REFER_MSG     TYPE TABLE OF J_1BNFREF,
          OBJ_OT_PARTNER    TYPE TABLE OF J_1BNFCPD,
          OBJ_IMPORT_DI     TYPE TABLE OF J_1BNFIMPORT_DI,
          OBJ_IMPORT_ADI    TYPE TABLE OF J_1BNFIMPORT_ADI,
          OBJ_CTE_RES       TYPE TABLE OF J_1BCTE_D_RES,
          OBJ_CTE_DOCREF    TYPE TABLE OF J_1BCTE_D_DOCREF,
          OBJ_TRANS_VOLUMES TYPE TABLE OF J_1BNFTRANSVOL,
          OBJ_TRAILER_INFO  TYPE TABLE OF J_1BNFTRAILER,
          OBJ_TRADE_NOTES   TYPE TABLE OF J_1BNFTRADENOTES,
          OBJ_ADD_INFO      TYPE TABLE OF J_1BNFADD_INFO,
          OBJ_REF_PROC      TYPE TABLE OF J_1BNFREFPROC,
          OBJ_SUGAR_SUPPL   TYPE TABLE OF J_1BNFSUGARSUPPL,
          OBJ_SUGAR_DEDUC   TYPE TABLE OF J_1BNFSUGARDEDUC,
          OBJ_VEHICLE       TYPE TABLE OF J_1BNFVEHICLE,
          OBJ_PHARMACEUT    TYPE TABLE OF J_1BNFPHARMACEUT,
          OBJ_FUEL          TYPE TABLE OF J_1BNFFUEL,
          OBJ_EXPORT        TYPE TABLE OF J_1BNFE_EXPORT,
          OBJ_NVE           TYPE TABLE OF J_1BNFNVE.

    DATA: LC_REFKEY TYPE J_1BNFLIN-REFKEY,
          LC_NFOBJN TYPE J_1BINTERF-NFOBJN.

    CHECK ME->NOTA-MBLNR_ARM IS NOT INITIAL.

    CONCATENATE ME->NOTA-MBLNR_ARM ME->NOTA-MJAHR_ARM INTO LC_REFKEY.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFLIN)
      FROM J_1BNFLIN
     WHERE REFKEY EQ @LC_REFKEY
       AND REFTYP EQ 'MD'.

    CHECK SY-SUBRC IS INITIAL.

    ME->NOTA-DOCNUM_ARM = WA_J_1BNFLIN-DOCNUM.

    UPDATE ZIB_NFE_DIST_TER
       SET DOCNUM_ARM = ME->NOTA-DOCNUM_ARM
     WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

    COMMIT WORK AND WAIT.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @WA_J_1BNFLIN-DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    CHECK WA_J_1BNFDOC-NFENUM IS INITIAL.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        DOC_NUMBER         = WA_J_1BNFDOC-DOCNUM
      IMPORTING
        OBJ_NUMBER         = LC_NFOBJN
      EXCEPTIONS
        DOCUMENT_NOT_FOUND = 1
        DOCUM_LOCK         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_READ'
      EXPORTING
        OBJ_NUMBER        = LC_NFOBJN
      IMPORTING
        OBJ_HEADER        = OBJ_HEADER
      TABLES
        OBJ_PARTNER       = OBJ_PARTNER
        OBJ_ITEM          = OBJ_ITEM
        OBJ_ITEM_TAX      = OBJ_ITEM_TAX
        OBJ_HEADER_MSG    = OBJ_HEADER_MSG
        OBJ_REFER_MSG     = OBJ_REFER_MSG
        OBJ_OT_PARTNER    = OBJ_OT_PARTNER
        OBJ_IMPORT_DI     = OBJ_IMPORT_DI
        OBJ_IMPORT_ADI    = OBJ_IMPORT_ADI
        OBJ_CTE_RES       = OBJ_CTE_RES
        OBJ_CTE_DOCREF    = OBJ_CTE_DOCREF
        OBJ_TRANS_VOLUMES = OBJ_TRANS_VOLUMES
        OBJ_TRAILER_INFO  = OBJ_TRAILER_INFO
        OBJ_TRADE_NOTES   = OBJ_TRADE_NOTES
        OBJ_ADD_INFO      = OBJ_ADD_INFO
        OBJ_REF_PROC      = OBJ_REF_PROC
        OBJ_SUGAR_SUPPL   = OBJ_SUGAR_SUPPL
        OBJ_SUGAR_DEDUC   = OBJ_SUGAR_DEDUC
        OBJ_VEHICLE       = OBJ_VEHICLE
        OBJ_PHARMACEUT    = OBJ_PHARMACEUT
        OBJ_FUEL          = OBJ_FUEL
        OBJ_EXPORT        = OBJ_EXPORT
        OBJ_NVE           = OBJ_NVE
      EXCEPTIONS
        OBJECT_NOT_FOUND  = 1
        OTHERS            = 2.

    CHECK SY-SUBRC IS INITIAL.

    DATA(R_CFOP) = ME->GET_CFOP_ARMAZENAGEM( ).

    LOOP AT OBJ_ITEM ASSIGNING FIELD-SYMBOL(<WA_ITEM_NOTA>).
      <WA_ITEM_NOTA>-CFOP = R_CFOP.
    ENDLOOP.

    CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
      EXPORTING
        OBJ_NUMBER        = LC_NFOBJN
        OBJ_HEADER        = OBJ_HEADER
      TABLES
        OBJ_PARTNER       = OBJ_PARTNER
        OBJ_ITEM          = OBJ_ITEM
        OBJ_ITEM_TAX      = OBJ_ITEM_TAX
        OBJ_HEADER_MSG    = OBJ_HEADER_MSG
        OBJ_REFER_MSG     = OBJ_REFER_MSG
        OBJ_OT_PARTNER    = OBJ_OT_PARTNER
        OBJ_IMPORT_DI     = OBJ_IMPORT_DI
        OBJ_IMPORT_ADI    = OBJ_IMPORT_ADI
        OBJ_CTE_RES       = OBJ_CTE_RES
        OBJ_CTE_DOCREF    = OBJ_CTE_DOCREF
        OBJ_TRANS_VOLUMES = OBJ_TRANS_VOLUMES
        OBJ_TRAILER_INFO  = OBJ_TRAILER_INFO
        OBJ_TRADE_NOTES   = OBJ_TRADE_NOTES
        OBJ_ADD_INFO      = OBJ_ADD_INFO
        OBJ_REF_PROC      = OBJ_REF_PROC
        OBJ_SUGAR_SUPPL   = OBJ_SUGAR_SUPPL
        OBJ_SUGAR_DEDUC   = OBJ_SUGAR_DEDUC
        OBJ_VEHICLE       = OBJ_VEHICLE
        OBJ_PHARMACEUT    = OBJ_PHARMACEUT
        OBJ_FUEL          = OBJ_FUEL
        OBJ_EXPORT        = OBJ_EXPORT
        OBJ_NVE           = OBJ_NVE
      EXCEPTIONS
        OBJECT_NOT_FOUND  = 1
        OTHERS            = 2.

    CHECK SY-SUBRC IS INITIAL.

    CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
      EXPORTING
        OBJ_NUMBER         = LC_NFOBJN
      EXCEPTIONS
        OBJECT_NOT_FOUND   = 1
        DOCUMENT_NOT_FOUND = 2
        UPDATE_PROBLEM     = 3
        DOCUM_LOCK         = 4
        OTHERS             = 5.

    CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
      EXPORTING
        OBJ_NUMBER       = LC_NFOBJN
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

  ENDMETHOD.


  METHOD NFE_ATUALIZA_CFOP_NO_FISCAL.

    DATA: "WA_ACTTAB TYPE J_1BNFE_ACTIVE,
          lc_nfobjn TYPE j_1binterf-nfobjn.

    DATA: lc_belnr   TYPE belnr_d,
          lc_gjahr   TYPE gjahr,
          lv_alterou TYPE char01,
          lc_buzei   TYPE rblgp.

    DATA: obj_header        TYPE j_1bnfdoc,
          obj_partner       TYPE TABLE OF j_1bnfnad,
          obj_item          TYPE TABLE OF j_1bnflin,
          obj_item_tax      TYPE TABLE OF j_1bnfstx,
          obj_header_msg    TYPE TABLE OF j_1bnfftx,
          obj_refer_msg     TYPE TABLE OF j_1bnfref,
          obj_ot_partner    TYPE TABLE OF j_1bnfcpd,
          obj_import_di     TYPE TABLE OF j_1bnfimport_di,
          obj_import_adi    TYPE TABLE OF j_1bnfimport_adi,
          obj_cte_res       TYPE TABLE OF j_1bcte_d_res,
          obj_cte_docref    TYPE TABLE OF j_1bcte_d_docref,
          obj_trans_volumes TYPE TABLE OF j_1bnftransvol,
          obj_trailer_info  TYPE TABLE OF j_1bnftrailer,
          obj_trade_notes   TYPE TABLE OF j_1bnftradenotes,
          obj_add_info      TYPE TABLE OF j_1bnfadd_info,
          obj_ref_proc      TYPE TABLE OF j_1bnfrefproc,
          obj_sugar_suppl   TYPE TABLE OF j_1bnfsugarsuppl,
          obj_sugar_deduc   TYPE TABLE OF j_1bnfsugardeduc,
          obj_vehicle       TYPE TABLE OF j_1bnfvehicle,
          obj_pharmaceut    TYPE TABLE OF j_1bnfpharmaceut,
          obj_fuel          TYPE TABLE OF j_1bnffuel,
          obj_export        TYPE TABLE OF j_1bnfe_export,
          obj_nve           TYPE TABLE OF j_1bnfnve.

    DATA:
      it_nfdoc_text TYPE  j_1bnfdoc_text_tab,
      it_nflin_text TYPE  j_1bnflin_text_tab,
      wa_nfdoc_text LIKE LINE OF it_nfdoc_text,
      wa_nflin_text LIKE LINE OF it_nflin_text.




    IF me->pedidos IS NOT INITIAL.
      READ TABLE me->pedidos INDEX 1 INTO DATA(w_pedido).
      SELECT SINGLE * FROM ekko INTO @DATA(ws_ekko) WHERE ebeln EQ @w_pedido-ebeln.
      IF ws_ekko-bsart EQ 'PCSF'. "Pedido de combustivel externo frota própria
        EXIT.
      ENDIF.
      clear: w_pedido.

*      "Atualizar Informações NF-e
*      CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
*        EXPORTING
*          I_DOCNUM = ME->NOTA-DOCNUM_NFE
*        IMPORTING
*          E_ACTTAB = WA_ACTTAB
*        EXCEPTIONS
*          NO_ENTRY = 1
*          OTHERS   = 2.
*
*      IF SY-SUBRC IS INITIAL.
*        WA_ACTTAB-DOCNUM = ME->NOTA-DOCNUM_NFE.
*        WA_ACTTAB-REGIO          = ME->NOTA-CHAVE_NFE(2).
*        WA_ACTTAB-AUTHCOD        = ME->NOTA-NR_PROTOCOLO.
*        WA_ACTTAB-DOCSTA         = '1'.
*        WA_ACTTAB-REGIO          = ME->NOTA-REGIO.
*        WA_ACTTAB-CODE           = ME->NOTA-CD_MSG_SEFAZ.
*        WA_ACTTAB-DOCNUM9        = ME->NOTA-DOCNUM9.
*        WA_ACTTAB-CDV             = ME->NOTA-CDV.
*        WA_ACTTAB-MSSTAT         = 'A'.
*        WA_ACTTAB-TPEMIS         = ME->NOTA-CHAVE_NFE+34(1).
*        WA_ACTTAB-ACTION_REQU    = 'C'.
*        WA_ACTTAB-SCSSTA         = '0'.
*        UPDATE J_1BNFE_ACTIVE FROM WA_ACTTAB.
*        COMMIT WORK.
*      ENDIF.

      CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
        EXPORTING
          doc_number         = me->nota-docnum_nfe
        IMPORTING
          obj_number         = lc_nfobjn
        EXCEPTIONS
          document_not_found = 1
          docum_lock         = 2
          OTHERS             = 3.

      CALL FUNCTION 'J_1B_NF_OBJECT_READ'
        EXPORTING
          obj_number        = lc_nfobjn
        IMPORTING
          obj_header        = obj_header
        TABLES
          obj_partner       = obj_partner
          obj_item          = obj_item
          obj_item_tax      = obj_item_tax
          obj_header_msg    = obj_header_msg
          obj_refer_msg     = obj_refer_msg
          obj_ot_partner    = obj_ot_partner
          obj_import_di     = obj_import_di
          obj_import_adi    = obj_import_adi
          obj_cte_res       = obj_cte_res
          obj_cte_docref    = obj_cte_docref
          obj_trans_volumes = obj_trans_volumes
          obj_trailer_info  = obj_trailer_info
          obj_trade_notes   = obj_trade_notes
          obj_add_info      = obj_add_info
          obj_ref_proc      = obj_ref_proc
          obj_sugar_suppl   = obj_sugar_suppl
          obj_sugar_deduc   = obj_sugar_deduc
          obj_vehicle       = obj_vehicle
          obj_pharmaceut    = obj_pharmaceut
          obj_fuel          = obj_fuel
          obj_export        = obj_export
          obj_nve           = obj_nve
        EXCEPTIONS
          object_not_found  = 1
          OTHERS            = 2.

      CHECK sy-subrc IS INITIAL.

      "OBJ_HEADER-XMLVERS          = ME->NOTA-XMLVERS.
      "OBJ_HEADER-DOCSTAT          = '1'.
      "OBJ_HEADER-AUTHCOD          = ME->NOTA-NR_PROTOCOLO.
      "OBJ_HEADER-AUTHDATE         = ME->NOTA-DT_PROTOCOLO.
      "OBJ_HEADER-AUTHTIME         = ME->NOTA-HR_PROTOCOLO.
      "OBJ_HEADER-CODE             = ME->NOTA-CD_MSG_SEFAZ.
      obj_header-conting          = space.

      IF me->nota-belnr IS NOT INITIAL AND me->pedidos IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(it_rseg)
          FROM rseg
         WHERE belnr EQ @me->nota-belnr
           AND gjahr EQ @me->nota-gjahr.
      ENDIF.

      LOOP AT obj_item ASSIGNING FIELD-SYMBOL(<wa_item_nota>).

        IF me->pedidos IS NOT INITIAL.

          lc_belnr = <wa_item_nota>-refkey(10).
          lc_gjahr = <wa_item_nota>-refkey+10(04).
          lc_buzei = <wa_item_nota>-refitm.

          READ TABLE it_rseg INTO DATA(wa_rseg)
          WITH KEY belnr = lc_belnr
                   gjahr = lc_gjahr
                   buzei = lc_buzei.
          IF sy-subrc IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          READ TABLE me->pedidos INTO DATA(wa_pedido)
          WITH KEY ebeln = wa_rseg-ebeln
                   ebelp = wa_rseg-ebelp.

          IF sy-subrc IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          TRY.
              READ TABLE me->itens INDEX 1 INTO DATA(wa_item).
              DATA(r_cfop) = me->get_cfop_escrituracao_entrada( EXPORTING i_prod_item = wa_item-prod_item ).

              "Não Ajustar CFOP, Departamento Fiscal irá ajustar a J1BTAX (CFOP's MM)
              "<WA_ITEM_NOTA>-CFOP       = R_CFOP.
              IF me->nota-ctr_waers NE 'BRL'.
                "Ajusta o Valor Total dos Itens
                wa_item-prod_vlr_total_b = wa_pedido-menge * wa_pedido-netpr.

                IF wa_item-prod_vlr_total_b NE <wa_item_nota>-netwr.
                  <wa_item_nota>-netwr  = wa_item-prod_vlr_total_b.
                  <wa_item_nota>-nfnet  = wa_item-prod_vlr_total_b.
                  <wa_item_nota>-netwrt = wa_item-prod_vlr_total_b.
                  <wa_item_nota>-nfnett = wa_item-prod_vlr_total_b.


                ENDIF.

                LOOP AT obj_item_tax ASSIGNING FIELD-SYMBOL(<wa_item_tax>)
                  WHERE itmnum EQ <wa_item_nota>-itmnum.

                  IF <wa_item_tax>-base GT 0 AND <wa_item_tax>-base NE <wa_item_nota>-netwr.
                    DATA(lc_dif) = abs( <wa_item_tax>-base - <wa_item_nota>-netwr ).
                    IF lc_dif LE ( 2 / 100 ).
                      <wa_item_tax>-base = <wa_item_nota>-netwr.
                    ENDIF.
                  ENDIF.

                  IF <wa_item_tax>-othbas GT 0 AND <wa_item_tax>-othbas NE <wa_item_nota>-netwr.
                    lc_dif = abs( <wa_item_tax>-othbas - <wa_item_nota>-netwr ).
                    IF lc_dif LE ( 2 / 100 ).
                      <wa_item_tax>-othbas = <wa_item_nota>-netwr.
                    ENDIF.
                  ENDIF.

                  IF <wa_item_tax>-excbas GT 0 AND <wa_item_tax>-excbas NE <wa_item_nota>-netwr.
                    lc_dif = abs( <wa_item_tax>-excbas - <wa_item_nota>-netwr ).
                    IF lc_dif LE ( 2 / 100 ).
                      <wa_item_tax>-excbas = <wa_item_nota>-netwr.
                    ENDIF.
                  ENDIF.

                ENDLOOP.
              ENDIF.

            CATCH zcx_nfe_inbound_exception .
          ENDTRY.

        ELSE.
          CLEAR: lv_alterou.
          READ TABLE me->itens WITH KEY prod_item = sy-tabix INTO wa_item.
          IF ( sy-subrc IS INITIAL ) AND ( wa_item-matnr = <wa_item_nota>-matnr ).
            TRY.
                r_cfop = me->get_cfop_escrituracao_entrada( EXPORTING i_prod_item = wa_item-prod_item ).

                "Não Ajustar CFOP, Departamento Fiscal irá ajustar a J1BTAX (CFOP's MM)
                "<WA_ITEM_NOTA>-CFOP       = R_CFOP.
                IF me->nota-ctr_waers NE 'BRL'.

                  "Ajusta o Valor Total dos Itens
*                  IF WA_ITEM-PROD_VLR_TOTAL_B NE <WA_ITEM_NOTA>-NETWR.
*                    <WA_ITEM_NOTA>-NETWR  = WA_ITEM-PROD_VLR_TOTAL_B.
*                    <WA_ITEM_NOTA>-NFNET  = WA_ITEM-PROD_VLR_TOTAL_B.
*                    <WA_ITEM_NOTA>-NETWRT = WA_ITEM-PROD_VLR_TOTAL_B.
*                    <WA_ITEM_NOTA>-NFNETT = WA_ITEM-PROD_VLR_TOTAL_B.
*                  ENDIF.

                  IF wa_item-prod_vlr_total_b NE <wa_item_nota>-nfnet.
                    <wa_item_nota>-nfnet  = wa_item-prod_vlr_total_b.


                  ENDIF.

                  "Caso Tenha Desoneração Ajustar Bases e Valor do ICMS
                  IF wa_item-icms_mt_desonera IS NOT INITIAL.
*                    <WA_ITEM_NOTA>-MOTDESICMS = WA_ITEM-ICMS_MT_DESONERA.       -- Comentado 17/08/2020 - AOENNING - IR036442
*                    <WA_ITEM_NOTA>-VICMSDESON  = WA_ITEM-ICMS_VL_DESONERADO.
*                    <WA_ITEM_NOTA>-VICMSDESON = <WA_ITEM_NOTA>-VICMSDESON * -1.

                    LOOP AT obj_item_tax ASSIGNING <wa_item_tax>
                      WHERE itmnum EQ <wa_item_nota>-itmnum
                        AND taxgrp EQ 'ICMS'.

                      IF <wa_item_tax>-othbas GT 0 AND <wa_item_tax>-othbas NE <wa_item_nota>-netwr.
                        <wa_item_tax>-othbas = <wa_item_nota>-netwr.
                      ENDIF.

*                      IF <WA_ITEM_TAX>-TAXVAL GT 0 AND <WA_ITEM_TAX>-TAXVAL NE WA_ITEM-ICMS_VL_DESONERADO.        -- Comentado 17/08/2020 - AOENNING - IR036442
*                        <WA_ITEM_TAX>-TAXVAL = WA_ITEM-ICMS_VL_DESONERADO.
*                      ENDIF.
*
*                      IF <WA_ITEM_TAX>-TAXVAL LT 0 AND ABS( <WA_ITEM_TAX>-TAXVAL ) NE WA_ITEM-ICMS_VL_DESONERADO. -- Comentado 17/08/2020 - AOENNING - IR036442
*                        <WA_ITEM_TAX>-TAXVAL = WA_ITEM-ICMS_VL_DESONERADO.
*                      ENDIF.
                    ENDLOOP.

                  ELSE.
                    CLEAR: lv_alterou.
                    LOOP AT obj_item_tax ASSIGNING <wa_item_tax> WHERE itmnum EQ <wa_item_nota>-itmnum.
                      IF <wa_item_tax>-base GT 0 AND <wa_item_tax>-base NE <wa_item_nota>-netwr.
                        lc_dif = abs( <wa_item_tax>-base - <wa_item_nota>-netwr ).
                        IF lc_dif LE ( 2 / 100 ).
                          <wa_item_tax>-base = <wa_item_nota>-netwr.


                        ENDIF.
                      ENDIF.

                      IF <wa_item_tax>-othbas GT 0 AND <wa_item_tax>-othbas NE <wa_item_nota>-netwr.
                        lc_dif = abs( <wa_item_tax>-othbas - <wa_item_nota>-netwr ).
                        IF lc_dif LE ( 2 / 100 ).
                          <wa_item_tax>-othbas = <wa_item_nota>-netwr.


                        ENDIF.
                      ENDIF.

                      IF <wa_item_tax>-excbas GT 0 AND <wa_item_tax>-excbas NE <wa_item_nota>-netwr.
                        lc_dif = abs( <wa_item_tax>-excbas - <wa_item_nota>-netwr ).
                        IF lc_dif LE ( 2 / 100 ).
                          <wa_item_tax>-excbas = <wa_item_nota>-netwr.


                        ENDIF.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.

                ELSEIF wa_item-icms_mt_desonera IS NOT INITIAL. "BRL
                  CLEAR: lv_alterou.
*                  zcl_webservic_protheus=>get_empresa_cfop(
*                    EXPORTING
*                      i_nota = me->nota
*                    IMPORTING
*                      e_status   =  DATA(e_stats)   " Tabela de Dados de Importação de NF-e de Terceiro frota rodoviaria
*                  ).
*
*                  IF e_stats EQ abap_true.
*                    "Se for notas de combustivél da frota rodoviaria, não fazer o calculo.
*
*                  ELSE.
                  "Ajusta o Valor Total dos Itens COM A Desoneração
                  IF wa_item-prod_vlr_total_b NE <wa_item_nota>-netwr.
                    <wa_item_nota>-netwr  = wa_item-prod_vlr_total_b.
                    <wa_item_nota>-nfnet  = wa_item-prod_vlr_total_b.
                    <wa_item_nota>-netwrt = wa_item-prod_vlr_total_b.
                    <wa_item_nota>-nfnett = wa_item-prod_vlr_total_b.


                  ENDIF.
*                  ENDIF.

*                  "Atribuir Motivo/Valor Desonerado -- Comentado 17/08/2020 - AOENNING - IR036442
*                  <WA_ITEM_NOTA>-MOTDESICMS = WA_ITEM-ICMS_MT_DESONERA.
*                  <WA_ITEM_NOTA>-VICMSDESON  = WA_ITEM-ICMS_VL_DESONERADO.
*                  <WA_ITEM_NOTA>-VICMSDESON = <WA_ITEM_NOTA>-VICMSDESON * -1.

                ENDIF.

              CATCH zcx_nfe_inbound_exception .
            ENDTRY.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
        EXPORTING
          obj_number        = lc_nfobjn
          obj_header        = obj_header
        TABLES
          obj_partner       = obj_partner
          obj_item          = obj_item
          obj_item_tax      = obj_item_tax
          obj_header_msg    = obj_header_msg
          obj_refer_msg     = obj_refer_msg
          obj_ot_partner    = obj_ot_partner
          obj_import_di     = obj_import_di
          obj_import_adi    = obj_import_adi
          obj_cte_res       = obj_cte_res
          obj_cte_docref    = obj_cte_docref
          obj_trans_volumes = obj_trans_volumes
          obj_trailer_info  = obj_trailer_info
          obj_trade_notes   = obj_trade_notes
          obj_add_info      = obj_add_info
          obj_ref_proc      = obj_ref_proc
          obj_sugar_suppl   = obj_sugar_suppl
          obj_sugar_deduc   = obj_sugar_deduc
          obj_vehicle       = obj_vehicle
          obj_pharmaceut    = obj_pharmaceut
          obj_fuel          = obj_fuel
          obj_export        = obj_export
          obj_nve           = obj_nve
        EXCEPTIONS
          object_not_found  = 1
          OTHERS            = 2.

      CHECK sy-subrc IS INITIAL.

      "0001     Informação adic.(autoridades)
      TRY .
          CALL FUNCTION 'ZNFE_INBOUND_OBS'
            EXPORTING
              i_nota     = me
              i_tdid     = 'ZFIS'
              i_retornar = abap_true
            IMPORTING
              text       = wa_nfdoc_text-text.
          IF sy-subrc IS INITIAL.
            wa_nfdoc_text-docnum = obj_header-docnum.
            wa_nfdoc_text-textid = '0001'.
            APPEND wa_nfdoc_text TO it_nfdoc_text.
          ENDIF.
        CATCH zcx_nfe_inbound_exception.
        CATCH zcx_cadastro.
      ENDTRY.

      TRY .
          "0002     Informação compl.(empresa)
          CALL FUNCTION 'ZNFE_INBOUND_OBS'
            EXPORTING
              i_nota     = me
              i_tdid     = 'ZCON'
              i_retornar = abap_true
            IMPORTING
              text       = wa_nfdoc_text-text.

          IF sy-subrc IS INITIAL.
            wa_nfdoc_text-docnum = obj_header-docnum.
            wa_nfdoc_text-textid = '0002'.
            APPEND wa_nfdoc_text TO it_nfdoc_text.
          ENDIF.

        CATCH zcx_nfe_inbound_exception.
        CATCH zcx_cadastro.
      ENDTRY.

      IF it_nfdoc_text[] IS NOT INITIAL.
        CALL FUNCTION 'J_1B_NF_LONGTEXT_SET'
          EXPORTING
            iv_objnum     = lc_nfobjn
            iv_langu      = sy-langu
            iv_docnum     = obj_header-docnum
            it_nfdoc_text = it_nfdoc_text
            it_nflin_text = it_nflin_text.
      ENDIF.

      CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
        EXPORTING
          obj_number         = lc_nfobjn
        EXCEPTIONS
          object_not_found   = 1
          document_not_found = 2
          update_problem     = 3
          docum_lock         = 4
          OTHERS             = 5.

      CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
        EXPORTING
          obj_number       = lc_nfobjn
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

*      ENDIF.
    ENDIF.
*    clear: ws_pedido.

  ENDMETHOD.


  METHOD NFE_ATUALIZA_LOG_APROVACAO.

    DATA: IT_ZIB_NFE_DIST_EAP TYPE TABLE OF ZIB_NFE_DIST_EAP,
          IT_DD07V_AP         TYPE TABLE OF DD07V,
          IT_DD07V_AU         TYPE TABLE OF DD07V.

    IF LC_SEQUENCIA IS INITIAL.
      LC_SEQUENCIA = ME->GET_SEQUENCIA_LOG( ).
    ENDIF.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        DOMNAME    = 'ZDM_TIP_APROVACAO_NFE'
      TABLES
        VALUES_TAB = IT_DD07V_AP.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        DOMNAME    = 'ZDM_TP_AUTORIZACAO'
      TABLES
        VALUES_TAB = IT_DD07V_AU.

    IF I_CD_APROVACAO IS INITIAL.
      SELECT * INTO TABLE IT_ZIB_NFE_DIST_EAP
        FROM ZIB_NFE_DIST_EAP AS A
       WHERE CD_CHAVE_NFE EQ ME->NOTA-CHAVE_NFE
         AND NOT EXISTS ( SELECT * FROM ZIB_NFE_DIST_LOG AS L WHERE L~CHAVE_NFE = A~CD_CHAVE_NFE AND L~CD_APROVACAO EQ A~CD_APROVACAO )
       ORDER BY DT_AUTORIZACAO HR_AUTORIZACAO CD_APROVACAO.

    ELSE.
      SELECT * INTO TABLE IT_ZIB_NFE_DIST_EAP
        FROM ZIB_NFE_DIST_EAP AS A
       WHERE CD_APROVACAO EQ I_CD_APROVACAO.
    ENDIF.

    LOOP AT ME->LOGS INTO DATA(WA_LOGS) WHERE CD_APROVACAO IS NOT INITIAL.
      DELETE IT_ZIB_NFE_DIST_EAP WHERE CD_APROVACAO EQ WA_LOGS-CD_APROVACAO.
    ENDLOOP.

    LOOP AT IT_ZIB_NFE_DIST_EAP INTO DATA(WA_ZIB_NFE_DIST_EAP).

      READ TABLE IT_DD07V_AP INTO DATA(WA_DD07V_AP) WITH KEY DOMVALUE_L = WA_ZIB_NFE_DIST_EAP-TP_APROVACAO.

      READ TABLE IT_DD07V_AU INTO DATA(WA_DD07V_AU) WITH KEY DOMVALUE_L = WA_ZIB_NFE_DIST_EAP-TP_AUTORIZADO.

      CASE WA_ZIB_NFE_DIST_EAP-TP_AUTORIZADO.
        WHEN '01'.
          SY-MSGTY = 'S'.
        WHEN '02'.
          SY-MSGTY = 'E'.
      ENDCASE.

      SY-MSGV1 = WA_DD07V_AU-DDTEXT.
      SY-MSGV2 = WA_DD07V_AP-DDTEXT.
      SY-MSGV3 = WA_ZIB_NFE_DIST_EAP-TP_APROVACAO.
      SY-MSGV4 = WA_ZIB_NFE_DIST_EAP-CD_APROVACAO.

      CALL METHOD ME->SET_ADD_LOG_NFE
        EXPORTING
          I_TYPE           = SY-MSGTY
          I_ID             = 'ZNFE_DISTRI'
          I_NUM            = 000
          I_MESSAGE_V1     = SY-MSGV1
          I_MESSAGE_V2     = SY-MSGV2
          I_MESSAGE_V3     = SY-MSGV3
          I_MESSAGE_V4     = SY-MSGV4
          I_ESTRATEGIA     = ABAP_TRUE
          I_CD_APROVACAO   = WA_ZIB_NFE_DIST_EAP-CD_APROVACAO
          I_DT_ATUALIZACAO = WA_ZIB_NFE_DIST_EAP-DT_AUTORIZACAO
          I_HR_ATUALIZACAO = WA_ZIB_NFE_DIST_EAP-HR_AUTORIZACAO
        CHANGING
          P_LC_SEQUENCIA   = LC_SEQUENCIA.

    ENDLOOP.

    IF I_CD_APROVACAO IS NOT INITIAL.
      ME->NFE_INBOUND_GRAVAR_LOG( ).
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_ACEITE_FATURA.

    IF SY-TCODE EQ 'ZMM0110' OR SY-TCODE EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '08'
                                        ID 'ZNFETERMEP' FIELD ME->NOTA-E_TOMADORA
                                        ID 'ZNFETERFIL' FIELD ME->NOTA-F_TOMADORA
                                        ID 'ZNFETERDEP' FIELD ME->NOTA-CD_DEPARTAMENTO.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
                              ATTR1 = '08'
                              ATTR2 = CONV #( ME->NOTA-E_TOMADORA )
                              ATTR3 = CONV #( ME->NOTA-F_TOMADORA )
                              ATTR4 = CONV #( ME->NOTA-CD_DEPARTAMENTO ) )
            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
            MSGV1  = '08'
            MSGV2  = CONV #( ME->NOTA-E_TOMADORA )
            MSGV3  = CONV #( ME->NOTA-F_TOMADORA )
            MSGV4  = CONV #( ME->NOTA-CD_DEPARTAMENTO ).
      ENDIF.
    ENDIF.

    "Verificar Aceite Fiscal Concedido.
    IF ME->NOTA-ST_FISCAL NE ZCL_NFE_INBOUND=>ST_FISCAL_COM_ACEITE_FISCAL. "Finalizado com Aceite Fiscal
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->NOTA-TP_COMPRA_FUTURA EQ ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIRO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIRO-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIRO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIRO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ( ME->NOTA-TP_COMPRA_FUTURA EQ ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA OR ME->NOTA-CK_COMPRA_FUTURA EQ ABAP_FALSE ) AND
       ( ME->NOTA-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_MIGO_GERADA ) AND
       ( ME->NOTA-CK_FISICO EQ ABAP_FALSE ) .
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISICO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISICO-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISICO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISICO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    ME->NOTA-LAND1 = 'BR'.

    CALL FUNCTION 'ZNFE_INBOUND_ACEITE_FATURA'
      EXPORTING
        I_NOTA = ME.

  ENDMETHOD.


  METHOD NFE_INBOUND_ACEITE_FISICO.

    IF SY-TCODE EQ 'ZMM0110' OR SY-TCODE EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '07'
                                        ID 'ZNFETERMEP' FIELD ME->NOTA-E_TOMADORA
                                        ID 'ZNFETERFIL' FIELD ME->NOTA-F_TOMADORA
                                        ID 'ZNFETERDEP' FIELD ME->NOTA-CD_DEPARTAMENTO.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
                              ATTR1 = '07'
                              ATTR2 = CONV #( ME->NOTA-E_TOMADORA )
                              ATTR3 = CONV #( ME->NOTA-F_TOMADORA )
                              ATTR4 = CONV #( ME->NOTA-CD_DEPARTAMENTO ) )
            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
            MSGV1  = '07'
            MSGV2  = CONV #( ME->NOTA-E_TOMADORA )
            MSGV3  = CONV #( ME->NOTA-F_TOMADORA )
            MSGV4  = CONV #( ME->NOTA-CD_DEPARTAMENTO ).
      ENDIF.
    ENDIF.

    "Verificar Aceite Fiscal Concedido.
    IF ME->NOTA-ST_FISCAL NE ZCL_NFE_INBOUND=>ST_FISICO_99. "Finalizado com Aceite Fiscal
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_SEM_ACEITE_FISCAL-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->NOTA-TP_COMPRA_FUTURA EQ ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_FATURA.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIGO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIGO-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIGO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_GERA_MIGO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    CALL FUNCTION 'ZNFE_INBOUND_ACEITE_FISICO'
      EXPORTING
        I_NOTA = ME.

  ENDMETHOD.


  METHOD NFE_INBOUND_CANCELA_FATURA.

    DATA: e_invoicedocnumber_estorno TYPE re_belnr,
          e_fiscalyear_estorno       TYPE gjahr,
          e_retorno                  TYPE bapiret2_t,
          i_invoicedocnumber         TYPE re_belnr,
          i_fiscalyear               TYPE gjahr.

    DATA: wa_cabecalho TYPE zde_miro_cabecalho,
          handle       TYPE REF TO zcl_memory_nfe_inbound,
          root         TYPE REF TO zcl_memory_nfe_inbound_handle,
          oref         TYPE REF TO zcl_memory_nfe_inbound_handle.

    IF sy-tcode EQ 'ZMM0110' OR sy-tcode EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '08'
                                        ID 'ZNFETERMEP' FIELD me->nota-e_tomadora
                                        ID 'ZNFETERFIL' FIELD me->nota-f_tomadora
                                        ID 'ZNFETERDEP' FIELD me->nota-cd_departamento.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
                              attr1 = '08'
                              attr2 = CONV #( me->nota-e_tomadora )
                              attr3 = CONV #( me->nota-f_tomadora )
                              attr4 = CONV #( me->nota-cd_departamento ) )
            msgid  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
            msgv1  = '08'
            msgv2  = CONV #( me->nota-e_tomadora )
            msgv3  = CONV #( me->nota-f_tomadora )
            msgv4  = CONV #( me->nota-cd_departamento ).
      ENDIF.
    ENDIF.

    IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_99.
      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_nao_possui_miro-msgid
                            msgno = zcx_nfe_inbound_exception=>zcx_nao_possui_miro-msgno )
          msgid  = zcx_nfe_inbound_exception=>zcx_nao_possui_miro-msgid
          msgno  = zcx_nfe_inbound_exception=>zcx_nao_possui_miro-msgno
          msgty  = 'E'.
    ENDIF.

    IF me->nota-docnum_dev IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_existe_arm_dev-msgid
                            msgno = zcx_nfe_inbound_exception=>zcx_existe_arm_dev-msgno )
          msgid  = zcx_nfe_inbound_exception=>zcx_existe_arm_dev-msgid
          msgno  = zcx_nfe_inbound_exception=>zcx_existe_arm_dev-msgno
          msgty  = 'E'.
    ENDIF.

    IF me->nota-belnr IS NOT INITIAL.

      me->ck_eliminar_log = abap_false.
      lc_sequencia = me->get_sequencia_log( ).

      wa_cabecalho-series     = me->nota-serie.
      wa_cabecalho-nf_number9 = me->nota-numero.
      wa_cabecalho-lifnr      = me->nota-p_emissor.

      DATA(lc_referencia) =
      zcl_miro=>get_chave_referencia(
        i_nf_number  = wa_cabecalho-nf_number
        i_series     = wa_cabecalho-series
        i_subseries  = wa_cabecalho-subseries
        i_nf_number9 = wa_cabecalho-nf_number9 ).

      TRY.
          handle = zcl_memory_nfe_inbound=>attach_for_write( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
          CREATE OBJECT root AREA HANDLE handle.
          root->at_nfe_inbound = me->nota.
          handle->set_root( root ).
          handle->detach_commit( ).
        CATCH cx_shm_exclusive_lock_active.  "
        CATCH cx_shm_version_limit_exceeded.  "
        CATCH cx_shm_change_lock_active.  "
        CATCH cx_shm_parameter_error.  "
        CATCH cx_shm_pending_lock_removed.  "
        CATCH cx_shm_attach_error.
      ENDTRY.

      "Estornar Aviso de Recebimento
      DATA: lc_miro TYPE REF TO zcl_miro.
      CREATE OBJECT lc_miro.

      CALL METHOD lc_miro->estornar
        EXPORTING
          i_bapi_wait                = space
        IMPORTING
          e_invoicedocnumber_estorno = e_invoicedocnumber_estorno
          e_fiscalyear_estorno       = e_fiscalyear_estorno
          e_retorno                  = e_retorno
        CHANGING
          i_invoicedocnumber         = me->nota-belnr
          i_fiscalyear               = me->nota-gjahr
          i_postingdate              = sy-datum
        RECEIVING
          r_gerou                    = DATA(r_estornou_miro).

      CLEAR: lc_miro.

      IF r_estornou_miro EQ abap_true.

        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = sy-msgty
            i_id           = sy-msgid
            i_num          = sy-msgno
            i_message_v1   = sy-msgv1
            i_message_v2   = sy-msgv2
            i_message_v3   = sy-msgv3
            i_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.

        TRY.
            handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
            oref ?= handle->root.
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            handle->free_instance( ).
          CATCH cx_shm_inconsistent.  "
          CATCH cx_shm_no_active_version.  "
          CATCH cx_shm_read_lock_active.  "
          CATCH cx_shm_exclusive_lock_active.  "
          CATCH cx_shm_parameter_error.  "
          CATCH cx_shm_change_lock_active.  "
          CATCH cx_shm_attach_error.
        ENDTRY.

        WAIT UP TO 2 SECONDS.

        me->set_ck_fisico( i_check     = abap_false ).
        me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
        me->set_nr_fiscal( i_docnum = 0 ).
        me->set_nr_documento_fatura( i_belnr = space i_gjahr = 0 ).
        me->set_st_documento( i_st_documento = zcl_nfe_inbound=>st_documento_01 ).
*-IR063194 - 22.07.2021 - JT - inicio
        me->set_reinicia_romaneio( i_aut_embarque = me->nota-aut_embarque ).
*-IR063194 - 22.07.2021 - JT - fim

        LOOP AT e_retorno INTO DATA(wa_retorno_miro).
          CALL METHOD me->set_add_log_nfe
            EXPORTING
              i_type         = wa_retorno_miro-type
              i_id           = wa_retorno_miro-id
              i_num          = wa_retorno_miro-number
              i_message_v1   = wa_retorno_miro-message_v1
              i_message_v2   = wa_retorno_miro-message_v2
              i_message_v3   = wa_retorno_miro-message_v3
              i_message_v4   = wa_retorno_miro-message_v4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDLOOP.

        me->gravar_registro( ).
      ELSE.

        TRY.
            handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
            oref ?= handle->root.
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            handle->free_instance( ).
          CATCH cx_shm_inconsistent.  "
          CATCH cx_shm_no_active_version.  "
          CATCH cx_shm_read_lock_active.  "
          CATCH cx_shm_exclusive_lock_active.  "
          CATCH cx_shm_parameter_error.  "
          CATCH cx_shm_change_lock_active.  "
          CATCH cx_shm_attach_error.
        ENDTRY.

        LOOP AT e_retorno INTO DATA(wa_retorno_miro2).
          CALL METHOD me->set_add_log_nfe
            EXPORTING
              i_type         = wa_retorno_miro2-type
              i_id           = wa_retorno_miro2-id
              i_num          = wa_retorno_miro2-number
              i_message_v1   = wa_retorno_miro2-message_v1
              i_message_v2   = wa_retorno_miro2-message_v2
              i_message_v3   = wa_retorno_miro2-message_v3
              i_message_v4   = wa_retorno_miro2-message_v4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDLOOP.

        me->nfe_inbound_gravar_log( ).

        LOOP AT e_retorno INTO wa_retorno_miro2 WHERE type EQ 'E'.
          MESSAGE ID wa_retorno_miro2-id
                TYPE 'S'
              NUMBER wa_retorno_miro2-number
                INTO DATA(tx_erro)
                WITH wa_retorno_miro2-message_v1
                     wa_retorno_miro2-message_v2
                     wa_retorno_miro2-message_v3
                     wa_retorno_miro2-message_v4.

          me->gera_erro_geral( i_texto = tx_erro ).
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_CAN_MIGO_MIRO.

    DATA: NFE_INBOUND TYPE REF TO ZCL_NFE_INBOUND,
          LC_FILTRO   TYPE ZIB_NFE_DIST_TER_FILTRO,
          LC_VBELN    LIKE LINE OF LC_FILTRO-VBELN,
          E_REGISTROS TYPE ZDE_IB_NFE_DIST_TER_T.

    CREATE OBJECT NFE_INBOUND.
    R_CANCELOU = ABAP_FALSE.

    LC_VBELN-SIGN   = 'I'.
    LC_VBELN-OPTION = 'EQ'.
    LC_VBELN-LOW    = I_VBELN.
    LC_VBELN-HIGH   = I_VBELN.
    APPEND LC_VBELN TO LC_FILTRO-VBELN.

    DATA(E_PESQUISOU) = NFE_INBOUND->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS = LC_FILTRO IMPORTING E_REGISTROS = E_REGISTROS ).

    DESCRIBE TABLE E_REGISTROS LINES DATA(QTD_LINHAS).

    IF QTD_LINHAS GT 1.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISOS-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISOS-MSGID
                            ATTR1 = CONV #( I_VBELN ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISOS-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISOS-MSGID
          MSGV1  = CONV #( I_VBELN ).
    ELSEIF QTD_LINHAS LE 0.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISO-MSGNO
                            MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISO-MSGID
                            ATTR1 = CONV #( I_VBELN ) )
          MSGTY  = 'E'
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISO-MSGNO
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_RETORNO_AVISO-MSGID
          MSGV1  = CONV #( I_VBELN ).
    ENDIF.

    READ TABLE E_REGISTROS INDEX 1 INTO DATA(WA_REGISTROS).

    IF WA_REGISTROS-BELNR IS INITIAL AND WA_REGISTROS-MBLNR IS INITIAL.
      EXIT.
    ENDIF.

    NFE_INBOUND->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_REGISTROS-CHAVE_NFE ).
    NFE_INBOUND->NFE_INBOUND_CANCELA_FATURA(  ).
    NFE_INBOUND->NFE_INBOUND_CANCELA_FISICO( I_NAO_ESTORNAR_AVISO = ABAP_TRUE ).
    NFE_INBOUND->FREE( ).
    CLEAR: NFE_INBOUND.
    R_CANCELOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD NFE_INBOUND_CAN_SAIDA_ARMAZEM.

    DATA: I_DOC_YEAR_ESTORNO TYPE MJAHR,
          I_MAT_DOC_ESTORNO	 TYPE MBLNR.

    DATA: P_AUTORIZADO TYPE C LENGTH 1.
    DATA: R_ESTORNOU_MIGO TYPE C LENGTH 1.

    R_ESTORNOU_MIGO = ABAP_FALSE.

    "Buscar MIGO Valida
    SELECT * INTO TABLE @DATA(IT_MKPF)
      FROM MKPF AS M
     WHERE M~ZCHAVE_NFE EQ @ME->NOTA-CHAVE_NFE.

    LOOP AT IT_MKPF INTO DATA(WA_MKPF_L).

      DATA: RG_BWART TYPE RANGE OF BWART.

      RG_BWART = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = 'Z41' HIGH = 'Z41' ) ( LOW = 'ZQ1' HIGH = 'ZQ1' ) ).

      SELECT SINGLE * INTO @DATA(WA_MSEG)
        FROM MSEG
       WHERE MBLNR EQ @WA_MKPF_L-MBLNR
         AND MJAHR EQ @WA_MKPF_L-MJAHR
         AND SMBLN EQ @SPACE
         AND BWART IN @RG_BWART.

      "Se Achou é Saída para Armazenagem
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_MSEG_ESTORNADO)
        FROM MSEG AS E
       WHERE E~SMBLN EQ @WA_MKPF_L-MBLNR
         AND E~SJAHR EQ @WA_MKPF_L-MJAHR.

      "Se Achou está estornado
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      "Caso exista um documento somente alimenta
      DATA(WA_MKPF) = WA_MKPF_L.

    ENDLOOP.

    IF WA_MKPF IS NOT INITIAL.

      TRY .
          DATA(LC_ESTORNO) =
          ZCL_FACTORY_MAT_DESTINACAO=>ZIF_FACTORY_MAT_DESTINACAO~GET_INSTANCE(
             )->SET_FACTORY_OBJETO( EXPORTING I_TP_DESTINACAO = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR
             )->GET_FACTORY_OBJETO(
             )->SET_REGISTRO_DOC_DESTINACAO( I_MBLNR = WA_MKPF-MBLNR I_MJAHR = WA_MKPF-MJAHR
             )->SET_ESTORNAR_MOVIMENTO( IMPORTING E_ESTORNOU = R_ESTORNOU_MIGO
             )->FREE(
             ).

          CLEAR: LC_ESTORNO.

        CATCH ZCX_MATERIAL_DESTINACAO INTO DATA(EX_MATERIAL).

          IF LC_ESTORNO IS NOT INITIAL.
            LC_ESTORNO->FREE( ).
            CLEAR: LC_ESTORNO.
          ENDIF.

          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_MATERIAL->MSGID
                                MSGNO = EX_MATERIAL->MSGNO
                                ATTR1 = CONV #( EX_MATERIAL->MSGV1 )
                                ATTR2 = CONV #( EX_MATERIAL->MSGV2 )
                                ATTR3 = CONV #( EX_MATERIAL->MSGV3 )
                                ATTR4 = CONV #( EX_MATERIAL->MSGV4 ) )
              MSGID  = EX_MATERIAL->MSGID
              MSGNO  = EX_MATERIAL->MSGNO
              MSGTY  = EX_MATERIAL->MSGTY
              MSGV1  = EX_MATERIAL->MSGV1
              MSGV2  = EX_MATERIAL->MSGV2
              MSGV3  = EX_MATERIAL->MSGV3
              MSGV4  = EX_MATERIAL->MSGV4.

      ENDTRY.

    ELSE.

      R_ESTORNOU_MIGO = ABAP_TRUE.

    ENDIF.

    IF R_ESTORNOU_MIGO EQ ABAP_TRUE.

      CLEAR: ME->NOTA-DOCNUM_ARM,
             ME->NOTA-MBLNR_ARM,
             ME->NOTA-MJAHR_ARM.

      UPDATE ZIB_NFE_DIST_TER
         SET MBLNR_ARM  = SPACE
             MJAHR_ARM  = SPACE
             DOCNUM_ARM = SPACE
             ST_ARMAZEM = '00'
       WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

      COMMIT WORK AND WAIT.

      ME->SET_ARM_ENVIAR_ACEITE_OPERACAO( I_CANCELAR = ABAP_TRUE ).
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_CAN_SAIDA_DEVOLU.

    DATA: I_DOC_YEAR_ESTORNO TYPE MJAHR,
          I_MAT_DOC_ESTORNO	 TYPE MBLNR.

    DATA: P_AUTORIZADO TYPE C LENGTH 1.
    DATA: R_ESTORNOU_MIGO TYPE C LENGTH 1.

    R_ESTORNOU_MIGO = ABAP_FALSE.

    "Buscar MIGO Valida
    SELECT * INTO TABLE @DATA(IT_MKPF)
      FROM MKPF AS M
     WHERE M~ZCHAVE_NFE EQ @ME->NOTA-CHAVE_NFE.

    LOOP AT IT_MKPF INTO DATA(WA_MKPF_L).

      SELECT SINGLE * INTO @DATA(WA_MSEG)
        FROM MSEG
       WHERE MBLNR EQ @WA_MKPF_L-MBLNR
         AND MJAHR EQ @WA_MKPF_L-MJAHR
         AND SMBLN EQ @SPACE
         AND BWART EQ '122'.

      "Se Achou é Saída para Armazenagem
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_MSEG_ESTORNADO)
        FROM MSEG AS E
       WHERE E~SMBLN EQ @WA_MKPF_L-MBLNR
         AND E~SJAHR EQ @WA_MKPF_L-MJAHR.

      "Se Achou está estornado
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      "Caso exista um documento somente alimenta
      DATA(WA_MKPF) = WA_MKPF_L.

    ENDLOOP.

    IF WA_MKPF IS NOT INITIAL.

      TRY .

          DATA(LC_ESTORNO) =
          ZCL_FACTORY_MAT_DESTINACAO=>ZIF_FACTORY_MAT_DESTINACAO~GET_INSTANCE(
             )->SET_FACTORY_OBJETO( EXPORTING I_TP_DESTINACAO = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO
             )->GET_FACTORY_OBJETO(
             )->SET_REGISTRO_DOC_DESTINACAO( I_MBLNR = WA_MKPF-MBLNR I_MJAHR = WA_MKPF-MJAHR
             )->SET_ESTORNAR_MOVIMENTO( IMPORTING E_ESTORNOU = R_ESTORNOU_MIGO
             )->FREE(
             ).

          CLEAR: LC_ESTORNO.

        CATCH ZCX_MATERIAL_DESTINACAO INTO DATA(EX_MATERIAL).

          IF LC_ESTORNO IS NOT INITIAL.
            LC_ESTORNO->FREE( ).
            CLEAR: LC_ESTORNO.
          ENDIF.

          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_MATERIAL->MSGID
                                MSGNO = EX_MATERIAL->MSGNO
                                ATTR1 = CONV #( EX_MATERIAL->MSGV1 )
                                ATTR2 = CONV #( EX_MATERIAL->MSGV2 )
                                ATTR3 = CONV #( EX_MATERIAL->MSGV3 )
                                ATTR4 = CONV #( EX_MATERIAL->MSGV4 ) )
              MSGID  = EX_MATERIAL->MSGID
              MSGNO  = EX_MATERIAL->MSGNO
              MSGTY  = EX_MATERIAL->MSGTY
              MSGV1  = EX_MATERIAL->MSGV1
              MSGV2  = EX_MATERIAL->MSGV2
              MSGV3  = EX_MATERIAL->MSGV3
              MSGV4  = EX_MATERIAL->MSGV4.

      ENDTRY.

    ENDIF.

    IF R_ESTORNOU_MIGO EQ ABAP_TRUE.

      CLEAR: ME->NOTA-DOCNUM_DEV,
             ME->NOTA-MBLNR_DEV,
             ME->NOTA-MJAHR_DEV.

      UPDATE ZIB_NFE_DIST_TER
         SET MBLNR_DEV  = SPACE
             MJAHR_DEV  = SPACE
             DOCNUM_DEV = SPACE
       WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

      COMMIT WORK AND WAIT.

      "ME->SET_ARM_ENVIAR_ACEITE_OPERACAO( I_CANCELAR = ABAP_TRUE ).
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_CRIA_LOTES.

    DATA: it_lotes    TYPE TABLE OF zib_nfe_dist_lot,
          ex_charg    TYPE REF TO zcx_charg_exception,
          i_mcha      TYPE mcha,
          e_mch1      TYPE mch1,
          i_clbatch	  TYPE clbatch,
          lc_lgort    TYPE lgort_d,
          lt_zppt0016 TYPE TABLE OF zppt0016.

    MOVE me->lotes TO it_lotes[].

    SORT it_lotes BY charg.
    DELETE ADJACENT DUPLICATES FROM it_lotes COMPARING charg.


    IF me->pedidos[] IS INITIAL.
      SELECT *
        INTO TABLE @DATA(it_ekpo)
        FROM ekpo
         FOR ALL ENTRIES IN @me->itens
       WHERE ebeln EQ @me->itens-ebeln
         AND ebelp EQ @me->itens-ebelp.
    ELSE.
      SELECT *
        INTO TABLE @it_ekpo
        FROM ekpo
         FOR ALL ENTRIES IN @me->pedidos
       WHERE ebeln EQ @me->pedidos-ebeln
         AND ebelp EQ @me->pedidos-ebelp.
    ENDIF.

    SORT it_ekpo BY ebeln ebelp.

    LOOP AT it_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>).
      SELECT SINGLE *
        INTO @DATA(wa_mara)
       FROM mara
      WHERE matnr EQ @<fs_lote>-matnr.

      IF wa_mara-xchpf EQ abap_false.
        CONTINUE.
      ENDIF.

      IF me->pedidos[] IS INITIAL.
        "Busca Item
        READ TABLE me->itens INTO DATA(wa_itens) WITH KEY prod_item = <fs_lote>-prod_item.
        "Pedido de Compra - Item
        READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH KEY ebeln = wa_itens-ebeln ebelp = wa_itens-ebelp BINARY SEARCH.
      ELSE.
        "Pedido de Compra - Item
        READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = <fs_lote>-ebeln ebelp = <fs_lote>-ebelp BINARY SEARCH.
      ENDIF.

      TRY.

          IF wa_itens-lgort IS INITIAL.
            lc_lgort = wa_ekpo-lgort.
          ELSE.
            lc_lgort = wa_itens-lgort.
          ENDIF.

          CALL METHOD zcl_charg=>get_charg
            EXPORTING
              i_matnr = <fs_lote>-matnr
              i_charg = <fs_lote>-charg
            RECEIVING
              r_mch1  = DATA(wa_mch1).

          CALL METHOD zcl_charg=>get_charg_werks_deposito
            EXPORTING
              i_matnr = <fs_lote>-matnr
              i_charg = <fs_lote>-charg
              i_werks = <fs_lote>-werks
              i_lgort = lc_lgort
            RECEIVING
              r_mchb  = DATA(wa_mchb).

          "<FS_LOTE>-CUOBJ = WA_MCH1-CUOBJ_BM.

        CATCH zcx_charg_exception INTO ex_charg.

          IF ex_charg->if_t100_message~t100key-msgid = zcx_charg_exception=>zcx_nao_ex_lote_materialwd-msgid AND
            ( ex_charg->if_t100_message~t100key-msgno = zcx_charg_exception=>zcx_nao_ex_lote_material-msgno OR
              ex_charg->if_t100_message~t100key-msgno = zcx_charg_exception=>zcx_nao_ex_lote_materialw-msgno OR
              ex_charg->if_t100_message~t100key-msgno = zcx_charg_exception=>zcx_nao_ex_lote_materialwd-msgno ).

            DATA: lc_charg TYPE REF TO zcl_charg.

            CREATE OBJECT lc_charg.

            IF ( <fs_lote>-vfdat IS INITIAL ).
              <fs_lote>-vfdat = wa_mch1-vfdat.
            ENDIF.

            i_mcha-matnr  = <fs_lote>-matnr.
            i_mcha-werks  = <fs_lote>-werks.
            i_mcha-charg  = <fs_lote>-charg.
            i_mcha-vfdat  = <fs_lote>-vfdat.
            i_mcha-licha  = <fs_lote>-licha.
            i_mcha-herkl  = <fs_lote>-herkl.
            i_mcha-hsdat  = <fs_lote>-hsdat.

            "Classe
            lc_charg->set_class( i_class = <fs_lote>-class ).
            "Tipo de Classe
            lc_charg->set_klart( i_klart = <fs_lote>-klart ).
            lc_charg->set_kzcla( i_kzcla = '2' ).
            lc_charg->set_lgort( i_lgort = lc_lgort ).
            lc_charg->set_object( i_object = CONV #( <fs_lote>-matnr ) ).
            lc_charg->set_table( i_table = CONV #( 'MARA' ) ).
            lc_charg->set_mcha( i_mcha = i_mcha ).

            LOOP AT me->lotes_caracteristicas INTO DATA(wa_carac) WHERE cd_lote_item EQ <fs_lote>-cd_lote_item .
              i_clbatch-atnam = wa_carac-atnam.
              i_clbatch-atinn = wa_carac-atinn.
              i_clbatch-atwtb = wa_carac-atwrt.
              lc_charg->add_clbatch( i_clbatch = i_clbatch ).
            ENDLOOP.

            TRY.
                DATA(r_mchb) = lc_charg->criar_charg( IMPORTING e_mch1 = e_mch1 ).
              CATCH zcx_charg_exception INTO ex_charg.

                DATA(r_retorno) = lc_charg->get_retorno( ).

                LOOP AT r_retorno INTO DATA(wa_retorno).
                  CALL METHOD me->set_add_log_nfe
                    EXPORTING
                      i_type         = wa_retorno-type
                      i_id           = wa_retorno-id
                      i_num          = wa_retorno-number
                      i_message_v1   = wa_retorno-message_v1
                      i_message_v2   = wa_retorno-message_v2
                      i_message_v3   = wa_retorno-message_v3
                      i_message_v4   = wa_retorno-message_v4
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDLOOP.

                RAISE EXCEPTION TYPE zcx_charg_exception
                  EXPORTING
                    textid = ex_charg->if_t100_message~t100key
                    msgty  = ex_charg->msgty
                    msgno  = ex_charg->msgno
                    msgv1  = ex_charg->msgv1
                    msgv2  = ex_charg->msgv2
                    msgv3  = ex_charg->msgv3
                    msgv4  = ex_charg->msgv4
                    msgid  = ex_charg->msgid.
            ENDTRY.

            <fs_lote>-cuobj = e_mch1-cuobj_bm.

            CLEAR: lc_charg.
          ENDIF.
      ENDTRY.

      IF <fs_lote>-licha IS NOT INITIAL .
        APPEND INITIAL LINE TO lt_zppt0016 ASSIGNING FIELD-SYMBOL(<fs_zpp0016>).
        <fs_zpp0016>-matnr  = <fs_lote>-matnr.
        <fs_zpp0016>-werks  = <fs_lote>-werks.
        <fs_zpp0016>-lgort  = lc_lgort.
        <fs_zpp0016>-chargd = <fs_lote>-charg.
        <fs_zpp0016>-charg  = <fs_lote>-charg.
        <fs_zpp0016>-zlicha = <fs_lote>-licha.
        <fs_zpp0016>-clabs  = <fs_lote>-menge.
        <fs_zpp0016>-vfdat  = <fs_lote>-vfdat.
        <fs_zpp0016>-dtval  = <fs_lote>-vfdat.

      ENDIF.

    ENDLOOP.

    IF lt_zppt0016 IS NOT INITIAL.
      MODIFY zppt0016 FROM TABLE lt_zppt0016.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_CRIA_ROM_SAIDA.

    DATA: WA_REGISTRO TYPE ZDE_ZSDT0001_RET,
          I_ROMANEIO  TYPE ZSDT0001,
          I_ITEM      TYPE ZSDT0001_ITEM.

    DATA(R_ZSDT0001) = ME->GET_ROMANEIO( ).

    IF R_ZSDT0001 IS INITIAL.

      DATA: LC_ROMANEIO TYPE REF TO ZCL_ROMANEIO.
      CREATE OBJECT LC_ROMANEIO.

      I_ROMANEIO-TP_MOVIMENTO = 'S'.
      I_ROMANEIO-DT_MOVIMENTO = SY-DATUM.
      I_ROMANEIO-NR_SAFRA     = SY-DATUM(4). "Verificar de onde pegar a safra
      I_ROMANEIO-BUKRS        = ME->NOTA-E_TOMADORA.
      I_ROMANEIO-BRANCH       = ME->NOTA-F_TOMADORA.
      I_ROMANEIO-ID_CLI_DEST  = ME->NOTA-F_ARMAZEM.
      I_ROMANEIO-PARID        = ME->NOTA-F_TOMADORA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = I_ROMANEIO-PARID
        IMPORTING
          OUTPUT = I_ROMANEIO-PARID.

      I_ROMANEIO-TP_FRETE     = 'F'.
      I_ROMANEIO-NFNUM        = ME->NOTA-NUMERO.
      I_ROMANEIO-SERIES       = ME->NOTA-SERIE.
      I_ROMANEIO-DOCDAT       = ME->NOTA-DT_EMISSAO.
      I_ROMANEIO-NETWR        = ME->NOTA-VL_TOTAL.
      I_ROMANEIO-NFE          = ABAP_TRUE.
      I_ROMANEIO-ID_INTERFACE = ZCL_ROMANEIO=>INTERFACE_REM_ARM_NFE_INBOUND.
      I_ROMANEIO-PESO_LIQ     = 0.
      I_ROMANEIO-PESO_FISCAL  = 0.

      SELECT *
        INTO TABLE @DATA(IT_EKET)
        FROM EKET
         FOR ALL ENTRIES IN @ME->ITENS
       WHERE EBELN EQ @ME->ITENS-EBELN
         AND EBELP EQ @ME->ITENS-EBELP.

      SORT IT_EKET BY EBELN EBELP.

      LOOP AT ME->ITENS INTO DATA(WA_ITEM).
        ADD WA_ITEM-MENGE TO I_ROMANEIO-PESO_LIQ.
        ADD WA_ITEM-MENGE TO I_ROMANEIO-PESO_FISCAL.

        "Pedido de Compra - Item - Lote
        READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
        IF SY-SUBRC IS INITIAL AND WA_EKET-CHARG IS NOT INITIAL.
          I_ITEM-CHARG = WA_EKET-CHARG.
        ELSE.
          READ TABLE ME->LOTES INTO DATA(WA_LOTE) WITH KEY PROD_ITEM = WA_ITEM-PROD_ITEM.
          IF SY-SUBRC IS INITIAL.
            I_ITEM-CHARG = WA_LOTE-CHARG.
          ENDIF.
        ENDIF.

        IF I_ITEM-CHARG IS INITIAL.
          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_ENCONTRADO_LOTE-MSGID
                                MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_ENCONTRADO_LOTE-MSGNO
                                ATTR1 = CONV #( WA_ITEM-PROD_ITEM ) )
              MSGTY  = 'E'
              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_ENCONTRADO_LOTE-MSGNO
              MSGV1  = CONV #( WA_ITEM-PROD_ITEM )
              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NAO_ENCONTRADO_LOTE-MSGID.
        ENDIF.

        I_ITEM-MATNR = WA_ITEM-MATNR.

        LC_ROMANEIO->ADD_ITEM( CHANGING I_ITEM = I_ITEM ).
      ENDLOOP.

      LC_ROMANEIO->SET_CABECALHO_ROMANEIO( I_ROMANEIO = I_ROMANEIO ).

      DATA(LC_GRAVOU) = LC_ROMANEIO->GRAVAR_REGISTRO( ).

      IF LC_GRAVOU EQ ABAP_TRUE.
        LC_ROMANEIO->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_REGISTRO ).
        ME->SET_CD_ROMANEIO( I_CD_ROMANEIO = WA_REGISTRO-CH_REFERENCIA ).
        MESSAGE S068 WITH WA_REGISTRO-NR_ROMANEIO.
      ELSE.

        CLEAR: LC_ROMANEIO.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = SY-MSGID
                              MSGNO = SY-MSGNO
                              ATTR1 = CONV #( SY-MSGV1 )
                              ATTR2 = CONV #( SY-MSGV2 )
                              ATTR3 = CONV #( SY-MSGV3 )
                              ATTR4 = CONV #( SY-MSGV4 ) )
            MSGID  = SY-MSGID
            MSGNO  = SY-MSGNO
            MSGTY  = 'E'
            MSGV1  = SY-MSGV1
            MSGV2  = SY-MSGV2
            MSGV3  = SY-MSGV3
            MSGV4  = SY-MSGV4.
      ENDIF.

      CLEAR: LC_ROMANEIO.

    ELSE.
      ME->SET_CD_ROMANEIO( I_CD_ROMANEIO = R_ZSDT0001-CH_REFERENCIA ).
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_FATURA.
    DATA: wa_cabecalho        TYPE zde_miro_cabecalho,
          it_itens            TYPE zde_miro_itens_t,
          it_itens_des        TYPE zde_miro_itens_t,
          it_contas           TYPE zbapi_incinv_gl_account_t,
          it_lines_miro_texto TYPE tline_t,
          wa_itens            TYPE zde_miro_itens,
          wa_contas           TYPE bapi_incinv_create_gl_account,
          wa_lines_miro_texto TYPE tline,
          lc_invoice_doc_item	TYPE rblgp,
          wa_kna1             TYPE kna1,
          "E_RATE_ICMS          TYPE J_1BTXRATE,
          "E_RATE_PIS          TYPE J_1BTXRATE,
          "E_RATE_COFINS        TYPE J_1BTXRATE,
          lc_zvalor_icms      TYPE j_1btaxval,
          "LC_ZVALOR_ICMS_DF   TYPE J_1BTAXVAL,
          lc_zvalor_pis       TYPE j_1btaxval,
          lc_zvalor_cofins    TYPE j_1btaxval,
          ca_brtwr(16),
          wa_ib_nota          TYPE zib_nota_fiscal_sap,
          e_wkurs	            TYPE wkurs,
          e_valor_total	      TYPE bapi_rmwwr,
          e_valor_desconto    TYPE bapi_wskto.

    r_gerou = abap_false.


    REFRESH it_itens_des.
    "Procurar MIRO CRIADA.
    DATA(wa_miro) = me->get_miro_valida( ).

    IF wa_miro IS INITIAL.

      CLEAR: wa_cabecalho,
             it_itens,
             it_itens_des,
             it_contas,
             it_lines_miro_texto,
             wa_itens,
             wa_contas,
             wa_lines_miro_texto.

      DATA: lc_miro TYPE REF TO zcl_miro.

      CREATE OBJECT lc_miro.
      lc_miro->set_simular( i_simular = me->at_simular ).

      me->set_calc_total_moeda_empresa( IMPORTING e_valores_pedido = DATA(e_valores_pedido) e_achou = DATA(e_achou) ).
      IF e_achou EQ abap_true.
        me->nota-ctr_waers       = e_valores_pedido-ctr_waers.
        me->nota-ctr_wkurs       = e_valores_pedido-ctr_wkurs.
        me->nota-ctr_kufix       = e_valores_pedido-ctr_kufix.
        me->nota-ctr_sinal       = e_valores_pedido-ctr_sinal.
        me->nota-ctr_zterm       = e_valores_pedido-ctr_zterm.
        me->nota-ctr_valor_total = e_valores_pedido-ctr_valor_total.
        me->nota-ctr_valor_total_liquido = e_valores_pedido-ctr_valor_total_liquido.

        LOOP AT me->itens ASSIGNING FIELD-SYMBOL(<fs_item>).
          me->set_calc_total_moeda_emp_item( EXPORTING i_item = <fs_item> IMPORTING e_valores_pedido = e_valores_pedido ).
          <fs_item>-ctr_waers       = e_valores_pedido-ctr_waers.
          <fs_item>-ctr_wkurs       = e_valores_pedido-ctr_wkurs.
          <fs_item>-ctr_kufix       = e_valores_pedido-ctr_kufix.
          <fs_item>-ctr_sinal       = e_valores_pedido-ctr_sinal.
          <fs_item>-ctr_zterm       = e_valores_pedido-ctr_zterm.
          <fs_item>-ctr_valor_total = e_valores_pedido-ctr_valor_total.
          <fs_item>-ctr_valor_total_liquido = e_valores_pedido-ctr_valor_total_liquido.
        ENDLOOP.
      ENDIF.

      me->get_valor_nota_fiscal_fatura(
        IMPORTING
          e_waers          = me->nota-ctr_waers
          e_wkurs          = e_wkurs
          e_kufix          = me->nota-ctr_kufix
          e_sinal          = me->nota-ctr_sinal
          e_valor_total    = e_valor_total
          e_valor_produtos = e_valor_desconto
          e_zterm	         = me->nota-ctr_zterm  ).

      DATA(r_zmmt0075) = me->get_config_tipo_pedido( ).

      IF r_zmmt0075-ck_altera_valor EQ abap_false OR me->nota-ctr_valor_total IS INITIAL.
        me->nota-ctr_wkurs       = e_wkurs.
        me->nota-ctr_valor_total = e_valor_total.
      ENDIF.

      "Cabeçalho
      wa_cabecalho-invoice_ind    = abap_true.
      wa_cabecalho-calc_tax_ind   = abap_true.
      wa_cabecalho-goods_affected = abap_true.
      wa_cabecalho-doc_type       = r_zmmt0075-blart.
      wa_cabecalho-doc_date       = me->nota-dt_emissao.
      wa_cabecalho-doc_date_mov   = sy-datlo.

      "Movimento Sempre será na mesma data da entrada da mercadoria
*      IF ME->NOTA-MBLNR IS NOT INITIAL.
*        SELECT SINGLE * INTO @DATA(WA_MKPF)
*          FROM MKPF
*         WHERE MBLNR EQ @ME->NOTA-MBLNR
*           AND MJAHR EQ @ME->NOTA-MJAHR.
*
*        IF SY-SUBRC IS INITIAL.
*          WA_CABECALHO-DOC_DATE_MOV = WA_MKPF-BUDAT.
*        ENDIF.
*      ENDIF.

      wa_cabecalho-comp_code      = me->nota-e_tomadora.
      wa_cabecalho-bus_area       = me->nota-f_tomadora.
      wa_cabecalho-lifnr          = me->nota-p_emissor.
      wa_cabecalho-currency       = me->nota-ctr_waers.
      wa_cabecalho-header_txt     = 'NF-e InBound'.

      IF r_zmmt0075-ck_altera_bloqueio EQ abap_false.
        wa_cabecalho-pmnt_block   = r_zmmt0075-zlspr.
        me->set_bloqueio_pagamento( i_zlspr = r_zmmt0075-zlspr ).
      ELSE.
        wa_cabecalho-pmnt_block   = me->nota-zlspr.
      ENDIF.

      wa_cabecalho-pmnttrms       = me->nota-ctr_zterm.
      wa_cabecalho-j_1bnftype     = me->get_categoria_nota_fiscal( ).
      wa_cabecalho-del_costs_taxc = me->nota-mwskz.
      wa_cabecalho-alloc_nmbr     = me->nota-ebeln.
      wa_cabecalho-series         = me->nota-serie.
      wa_cabecalho-nf_number9     = me->nota-numero.
      wa_cabecalho-gross_amount   = me->nota-ctr_valor_total.
      wa_cabecalho-dsct_amount    = me->nota-vlr_desconto.
      wa_cabecalho-exch_rate      = me->nota-ctr_wkurs.
      wa_cabecalho-chave_nfe      = me->nota-chave_nfe.
      "WA_CABECALHO-HEADER_TXT     = ME->NOTA-OBS_FINANCEIRA.
      wa_cabecalho-item_text      = me->nota-obs_financeira.

      "Colocar na Tela. """""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Colocar na Tela. """""""""""""""""""""""""""""""""""""""""""""""""""""""
      wa_cabecalho-partner_bk = me->nota-zbvtyp.

      IF r_zmmt0075-ck_nao_valida_venc_ped EQ abap_true.
        "Condição não calcula
        wa_cabecalho-doc_date_ven   = me->nota-dt_vencimento.
        wa_cabecalho-doc_date_cal   = me->nota-dt_vencimento.
      ELSE.
        "Condição vai calcular vencimento
        wa_cabecalho-doc_date_ven   = me->nota-dt_emissao.
        wa_cabecalho-doc_date_cal   = me->nota-dt_vencimento.
      ENDIF.

      wa_cabecalho-pymt_meth      = me->nota-pymt_meth.
      wa_cabecalho-housebankid    = me->nota-housebankid.
      wa_cabecalho-boleto         = me->nota-boleto.

      IF wa_cabecalho-doc_date_ven IS INITIAL.
        zcl_miro=>get_proximo_venc_fatura( IMPORTING e_data_vencimento = wa_cabecalho-doc_date_cal ).
      ENDIF.
      "Colocar na Tela. """""""""""""""""""""""""""""""""""""""""""""""""""""""

      lc_invoice_doc_item = 1.

      SELECT SINGLE * INTO @DATA(wa_lfa1)
        FROM lfa1
       WHERE lifnr EQ @me->nota-p_emissor.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->nota-f_tomadora
        IMPORTING
          output = wa_kna1-kunnr.

      SELECT SINGLE * INTO wa_kna1
        FROM kna1
       WHERE kunnr EQ wa_kna1-kunnr.

      "Verificar Ligação Documento Material """"""""""""
      DATA(ck_atualizar_material) = abap_false.

      IF me->nota-mblnr IS NOT INITIAL AND me->nota-mjahr IS NOT INITIAL.
        IF me->pedidos IS NOT INITIAL.

          me->set_nr_documento_material( i_mblnr = me->nota-mblnr i_mjahr = me->nota-mjahr ).

          LOOP AT me->pedidos INTO DATA(wa_pedido).
            IF wa_pedido-mblnr IS INITIAL OR wa_pedido-mjahr IS INITIAL OR wa_pedido-zeile IS INITIAL.
              TRY .
                  CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                    EXPORTING
                      i_ebeln = wa_pedido-ebeln
                      i_ebelp = wa_pedido-ebelp
                    IMPORTING
                      e_webre = DATA(e_webre).
                CATCH zcx_pedido_compra_exception.
              ENDTRY.

              IF e_webre IS NOT INITIAL.
                ck_atualizar_material = abap_true.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT me->itens INTO DATA(wa_item).
            IF wa_item-mblnr IS INITIAL OR wa_item-mjahr IS INITIAL OR wa_item-zeile IS INITIAL.
              TRY .
                  CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                    EXPORTING
                      i_ebeln = wa_item-ebeln
                      i_ebelp = wa_item-ebelp
                    IMPORTING
                      e_webre = e_webre.
                CATCH zcx_pedido_compra_exception.
              ENDTRY.
              IF e_webre IS NOT INITIAL.
                ck_atualizar_material = abap_true.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF ck_atualizar_material EQ abap_true.
        me->set_nr_documento_material( i_mblnr = me->nota-mblnr i_mjahr = me->nota-mjahr ).
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""
      IF me->pedidos IS NOT INITIAL.
        LOOP AT me->pedidos INTO wa_pedido.

          "Buscar o centro do pedido.
          SELECT SINGLE * FROM ekpo INTO @DATA(w_ekpo) WHERE ebeln EQ @wa_pedido-ebeln AND ebelp EQ @wa_pedido-ebelp.

          CALL METHOD zcl_miro=>get_taxas_iva
            EXPORTING
              i_iva                   = me->nota-mwskz
              i_data                  = me->nota-dt_emissao
              i_bbranc                = me->nota-f_tomadora
              i_shipfrom              = wa_lfa1-regio
              i_shipto                = wa_kna1-regio
              i_matnr                 = wa_pedido-matnr
              i_icms_base             = me->nota-vl_icms_base
              i_valor_icms            = me->nota-vl_icms_total
              i_werks                 = w_ekpo-werks
            IMPORTING
              e_rate_icms             = DATA(e_rate_icms)
              e_base_icms             = DATA(e_base_icms)
              e_rate_pis              = DATA(e_rate_pis)
              e_rate_cofins           = DATA(e_rate_cofins)
              e_rate_icms_diferencial = DATA(e_rate_icms_df).

          CLEAR: wa_itens.
          wa_itens-invoice_doc_item = lc_invoice_doc_item.
          wa_itens-po_number        = wa_pedido-ebeln.
          wa_itens-po_item          = wa_pedido-ebelp.
          wa_itens-tax_code         = COND string( WHEN wa_pedido-mwskz IS INITIAL THEN me->nota-mwskz ELSE wa_pedido-mwskz ).
          wa_itens-quantity         = wa_pedido-menge.
          wa_itens-po_unit          = wa_pedido-meins.
          wa_itens-item_amount      = wa_pedido-total.

          IF me->nota-ctr_waers NE 'BRL'.
            CASE me->nota-ctr_sinal.
              WHEN '/'.
                wa_itens-item_amount = wa_itens-item_amount / me->nota-ctr_wkurs.
              WHEN '*'.
                wa_itens-item_amount = wa_itens-item_amount * me->nota-ctr_wkurs.
            ENDCASE.
          ENDIF.

          "Se estado origem diferente de estado de destino
          IF wa_lfa1-regio NE wa_kna1-regio.
            SELECT SINGLE * INTO @DATA(wa_endereco)
              FROM adrc
             WHERE addrnumber EQ @wa_kna1-adrnr.
            IF sy-subrc IS INITIAL AND wa_endereco-taxjurcode IS NOT INITIAL.
              wa_itens-taxjurcode = wa_endereco-taxjurcode.
            ENDIF.
          ENDIF.

          TRY.
              CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                EXPORTING
                  i_ebeln = wa_pedido-ebeln
                  i_ebelp = wa_pedido-ebelp
                IMPORTING
                  e_webre = e_webre.
            CATCH zcx_pedido_compra_exception.
          ENDTRY.

          IF e_webre IS NOT INITIAL.
            wa_itens-ref_doc      = wa_pedido-mblnr.
            wa_itens-ref_doc_year = wa_pedido-mjahr.
            wa_itens-ref_doc_it   = wa_pedido-zeile.
            APPEND wa_itens TO it_itens.
            ADD 1 TO lc_invoice_doc_item.
          ELSE.
            READ TABLE it_itens WITH KEY po_number = wa_pedido-ebeln po_item = wa_pedido-ebelp ASSIGNING FIELD-SYMBOL(<fs_itens>).
            IF sy-subrc IS INITIAL.
              ADD wa_itens-item_amount TO <fs_itens>-item_amount.
              ADD wa_itens-quantity    TO <fs_itens>-quantity.
            ELSE.
              APPEND wa_itens TO it_itens.
              ADD 1 TO lc_invoice_doc_item.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ELSE.
        DATA vl_item_des TYPE zde_miro_itens-item_amount.

        DATA vl_impostos TYPE zde_miro_itens-item_amount.
        DATA vl_impostos_t TYPE zde_miro_itens-item_amount.

        DATA vl_desonera TYPE zde_miro_itens-item_amount.
        DATA vl_desonera_t TYPE zde_miro_itens-item_amount.

        vl_impostos   = 0.
        vl_impostos_t = 0.
        vl_desonera   = 0.
        vl_desonera_t = 0.
        LOOP AT me->itens INTO wa_item.

          "Buscar o centro do pedido.
          CLEAR: w_ekpo.
          SELECT SINGLE * FROM ekpo INTO w_ekpo WHERE ebeln EQ wa_item-ebeln AND ebelp EQ wa_item-ebelp.

          CALL METHOD zcl_miro=>get_taxas_iva
            EXPORTING
              i_iva                   = me->nota-mwskz
              i_data                  = me->nota-dt_emissao
              i_bbranc                = me->nota-f_tomadora
              i_shipfrom              = wa_lfa1-regio
              i_shipto                = wa_kna1-regio
              i_matnr                 = wa_item-matnr
              i_icms_base             = me->nota-vl_icms_base
              i_valor_icms            = me->nota-vl_icms_total
              i_werks                 = w_ekpo-werks
            IMPORTING
              e_rate_icms             = e_rate_icms
              e_base_icms             = e_base_icms
              e_rate_pis              = e_rate_pis
              e_rate_cofins           = e_rate_cofins
              e_rate_icms_diferencial = e_rate_icms_df.

          CLEAR: wa_itens.
          wa_itens-invoice_doc_item = lc_invoice_doc_item.
          wa_itens-po_number        = wa_item-ebeln.
          wa_itens-po_item          = wa_item-ebelp.
          wa_itens-tax_code         = me->nota-mwskz.
          wa_itens-quantity         = wa_item-menge.
          wa_itens-po_unit          = wa_item-meins.

          "CS2020001452
          SELECT SINGLE *
            FROM zmmt0149
            INTO @DATA(_zmmt0149)
           WHERE chave_nfe = @me->nota-chave_nfe
           AND   status    = 'A'.

          IF sy-subrc = 0 AND me->nota-ctr_waers NE 'BRL'.
*            wa_itens-item_amount = ( wa_item-ctr_valor_total * wa_item-ctr_wkurs )  / me->nota-ctr_wkurs.
            wa_itens-item_amount = ( wa_item-prod_vlr_total_b )  / me->nota-ctr_wkurs.
          ELSEIF me->nota-ctr_waers NE 'BRL' AND wa_item-icms_vl_desonerado IS NOT INITIAL.

            IF wa_item-ctr_valor_total <> wa_item-ctr_valor_total_liquido.
              wa_itens-item_amount = wa_item-ctr_valor_total_liquido.
            ELSE.

              "Valor do item sempre vai ser o valor do pedidod e compra
              "Valor Total da MIRO já foi informado
              SELECT SINGLE * INTO @DATA(wa_ekpo) FROM ekpo
               WHERE ebeln EQ @wa_item-ebeln
                 AND ebelp EQ @wa_item-ebelp.

              IF sy-subrc IS INITIAL.
                IF wa_ekpo-bprme NE wa_itens-po_unit.
                  wa_itens-item_amount = ( wa_item-menge / wa_ekpo-bpumn ) *  ( wa_ekpo-netpr / wa_ekpo-peinh ).
                ELSE.
                  wa_itens-item_amount = wa_item-menge *  ( wa_ekpo-netpr / wa_ekpo-peinh ).
                ENDIF.
              ENDIF.

            ENDIF.

          ELSE.

            IF wa_item-icms_vl_desonerado IS NOT INITIAL.

              SELECT SINGLE * INTO @wa_ekpo FROM ekpo
               WHERE ebeln EQ @wa_item-ebeln
                 AND ebelp EQ @wa_item-ebelp.

              IF sy-subrc IS INITIAL.
                IF wa_ekpo-bprme NE wa_itens-po_unit.
                  wa_itens-item_amount = ( wa_item-menge / wa_ekpo-bpumn ) *  ( wa_ekpo-netpr / wa_ekpo-peinh ).
                ELSE.
                  wa_itens-item_amount = wa_item-menge *  ( wa_ekpo-netpr / wa_ekpo-peinh ).
                ENDIF.
              ENDIF.

            ELSE.

              DATA(lv_desonerado_icms) = wa_item-icms_vl_desonerado.
              DATA(lv_desconto)        = wa_item-prod_vl_desconto.

              IF e_rate_icms GT 0.
                IF e_base_icms GT 0 AND wa_item-icms_vl_desonerado GT 0.
                  lc_zvalor_icms = ( wa_item-prod_vlr_total_b - lv_desonerado_icms - lv_desconto ) * ( e_base_icms / 100 ) * ( e_rate_icms / 100 ).
                ELSE.
                  lc_zvalor_icms = ( ( wa_item-prod_vlr_total_b - lv_desconto ) * ( e_base_icms / 100 ) ) * ( e_rate_icms / 100 ).
                ENDIF.
              ELSE.
                lc_zvalor_icms = 0.
              ENDIF.

              DATA(_ativar) = 'N'.
              SELECT COUNT(*)
                  FROM tvarvc
                  WHERE name = 'ZMM_EXCLUIR_ICMS_BASE_PIS_COF'
                  AND   low  = abap_true.
              IF sy-subrc IS INITIAL.
                _ativar = 'S'.
              ENDIF.

              "CS2023000341 - Atualização ZMM0110 (NT 2023.001 v1.20)-ALRS
              IF e_rate_pis GT 0.
                IF  _ativar = 'N'.
                  lc_zvalor_pis = ( ( wa_item-prod_vlr_total_b - lv_desconto ) * ( e_base_icms / 100 ) ) * ( e_rate_pis / 100 ).
                ELSE.
                  lc_zvalor_pis = (  wa_item-prod_vlr_total_b - lc_zvalor_icms - lv_desconto   ) * ( e_rate_pis / 100 ).
                ENDIF.
              ELSE.
                lc_zvalor_pis = 0.
              ENDIF.

              IF e_rate_cofins GT 0.
                IF  _ativar = 'N'.
                  lc_zvalor_cofins = ( ( wa_item-prod_vlr_total_b - lv_desconto ) * ( e_base_icms / 100 ) ) * ( e_rate_cofins / 100 ).
                ELSE.
                  lc_zvalor_cofins =  ( wa_item-prod_vlr_total_b - lc_zvalor_icms - lv_desconto  ) * ( e_rate_cofins / 100 ).
                ENDIF.
              ELSE.
                lc_zvalor_cofins = 0.
              ENDIF.
              "CS2023000341 - Atualização ZMM0110 (NT 2023.001 v1.20)-ALRS

              "Valor do Documento - Menos Impostos
              wa_itens-item_amount  = ( wa_item-prod_vlr_total_b - lv_desconto ) -
                                      ( ( lc_zvalor_icms + lv_desonerado_icms ) + lc_zvalor_pis + lc_zvalor_cofins ) +
                                      wa_item-ipi_valor.

              vl_item_des           = ( wa_item-prod_vlr_total_b - lv_desconto ) -
                                      ( ( lc_zvalor_icms  ) + lc_zvalor_pis + lc_zvalor_cofins ) +
                                      wa_item-ipi_valor.

              vl_impostos =    (  lc_zvalor_icms  + lc_zvalor_pis + lc_zvalor_cofins ) + wa_item-ipi_valor - lv_desconto. "sem desonerado
              vl_desonera =    lv_desonerado_icms.

              IF me->nota-ctr_waers NE 'BRL'.
                CASE me->nota-ctr_sinal.
                  WHEN '/'.
                    wa_itens-item_amount = wa_itens-item_amount / me->nota-ctr_wkurs.
                    vl_item_des          = vl_item_des / me->nota-ctr_wkurs.
                    vl_impostos_t        = vl_impostos_t + ( vl_impostos /  me->nota-ctr_wkurs ).
                    vl_desonera_t        = vl_desonera_t + ( vl_desonera /  me->nota-ctr_wkurs ).
                  WHEN '*'.
                    wa_itens-item_amount = wa_itens-item_amount * me->nota-ctr_wkurs.
                    vl_item_des          = vl_item_des * me->nota-ctr_wkurs.
                    vl_impostos_t        = vl_impostos_t + ( vl_impostos *  me->nota-ctr_wkurs ).
                    vl_desonera_t        = vl_desonera_t + ( vl_desonera *  me->nota-ctr_wkurs ).
                ENDCASE.
              ELSE.
                vl_impostos_t        = vl_impostos_t + vl_impostos.
                vl_desonera_t        = vl_desonera_t + vl_desonera.
              ENDIF.

            ENDIF.

          ENDIF.

          "Se estado origem diferente de estado de destino
          IF wa_lfa1-regio NE wa_kna1-regio.
            SELECT SINGLE * INTO wa_endereco
              FROM adrc
             WHERE addrnumber EQ wa_kna1-adrnr.
            IF sy-subrc IS INITIAL AND wa_endereco-taxjurcode IS NOT INITIAL.
              wa_itens-taxjurcode = wa_endereco-taxjurcode.
            ENDIF.
          ENDIF.

          TRY .
              CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                EXPORTING
                  i_ebeln = wa_item-ebeln
                  i_ebelp = wa_item-ebelp
                IMPORTING
                  e_webre = e_webre.
            CATCH zcx_pedido_compra_exception.
          ENDTRY.

          IF e_webre IS NOT INITIAL.
            wa_itens-ref_doc      = wa_item-mblnr.
            wa_itens-ref_doc_year = wa_item-mjahr.
            wa_itens-ref_doc_it   = wa_item-zeile.
            APPEND wa_itens TO it_itens.

            wa_itens-item_amount = vl_item_des.
            APPEND wa_itens TO it_itens_des.

            ADD 1 TO lc_invoice_doc_item.
          ELSE.
            READ TABLE it_itens WITH KEY po_number = wa_item-ebeln po_item = wa_item-ebelp ASSIGNING <fs_itens>.
            IF sy-subrc IS INITIAL.
              ADD wa_itens-item_amount TO <fs_itens>-item_amount.
              ADD wa_itens-quantity    TO <fs_itens>-quantity.
              READ TABLE it_itens_des WITH KEY po_number = wa_pedido-ebeln po_item = wa_pedido-ebelp ASSIGNING FIELD-SYMBOL(<fs_itens2>).
              IF sy-subrc IS INITIAL.
                ADD vl_item_des          TO <fs_itens2>-item_amount.
                ADD wa_itens-quantity    TO <fs_itens2>-quantity.
              ENDIF.
            ELSE.
              APPEND wa_itens TO it_itens.

              wa_itens-item_amount = vl_item_des.
              APPEND wa_itens TO it_itens_des.

              ADD 1 TO lc_invoice_doc_item.
            ENDIF.
          ENDIF.

        ENDLOOP.
        "
*        DATA TABIX TYPE SY-TABIX.
*        DATA(LV_PRODUTO) =  WA_CABECALHO-GROSS_AMOUNT.
*        DATA(LV_DESONERADO) =  WA_CABECALHO-GROSS_AMOUNT.
*
*        LV_PRODUTO = 0.
*        LOOP AT IT_ITENS INTO WA_ITENS.
*          ADD  WA_ITENS-ITEM_AMOUNT TO LV_PRODUTO.
*        ENDLOOP.
*        ADD VL_IMPOSTOS_T TO LV_PRODUTO.
*
*        LV_DESONERADO = 0.
*        IF LV_PRODUTO GT WA_CABECALHO-GROSS_AMOUNT.
*          LV_DESONERADO = LV_PRODUTO - WA_CABECALHO-GROSS_AMOUNT.
*          VL_DESONERA_T =  ABS( VL_DESONERA_T - LV_DESONERADO ).
*          IF VL_DESONERA_T LE '0.1'.
*            IT_ITENS[] = IT_ITENS_DES[].
*          ENDIF.
*
*        ENDIF.


      ENDIF.

      "Texto do Documento - NOTA
      wa_lines_miro_texto-tdformat = '*'.
      wa_lines_miro_texto-tdline   = 'MONITOR NF-E INBOUND'.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      CONCATENATE 'NF-e' me->nota-chave_nfe INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      WRITE me->nota-vl_total TO ca_brtwr.
      CONDENSE ca_brtwr NO-GAPS.
      CONCATENATE 'Valor Total Nota:' ca_brtwr INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      WRITE me->nota-vl_icms_total TO ca_brtwr.
      CONDENSE ca_brtwr NO-GAPS.
      CONCATENATE 'Valor ICMS:' ca_brtwr INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      WRITE me->nota-vl_pis_total TO ca_brtwr.
      CONDENSE ca_brtwr NO-GAPS.
      CONCATENATE 'Valor PIS:' ca_brtwr INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      WRITE me->nota-vl_cof_total TO ca_brtwr.
      CONDENSE ca_brtwr NO-GAPS.
      CONCATENATE 'Valor COFINS:' ca_brtwr INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      WRITE me->nota-vl_ipi_total TO ca_brtwr.
      CONDENSE ca_brtwr NO-GAPS.
      CONCATENATE 'Valor IPI:' ca_brtwr INTO wa_lines_miro_texto-tdline SEPARATED BY space.
      APPEND wa_lines_miro_texto TO it_lines_miro_texto.

      DATA: handle TYPE REF TO zcl_memory_nfe_inbound,
            root   TYPE REF TO zcl_memory_nfe_inbound_handle,
            oref   TYPE REF TO zcl_memory_nfe_inbound_handle.

      DATA(lc_referencia) =
      zcl_miro=>get_chave_referencia(
        i_nf_number  = wa_cabecalho-nf_number
        i_series     = wa_cabecalho-series
        i_subseries  = wa_cabecalho-subseries
        i_nf_number9 = wa_cabecalho-nf_number9 ).

      TRY.
          handle = zcl_memory_nfe_inbound=>attach_for_write( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
          CREATE OBJECT root AREA HANDLE handle.
          root->at_nfe_inbound = me->nota.
          handle->set_root( root ).
          handle->detach_commit( ).
        CATCH cx_shm_exclusive_lock_active.  "
        CATCH cx_shm_version_limit_exceeded.  "
        CATCH cx_shm_change_lock_active.  "
        CATCH cx_shm_parameter_error.  "
        CATCH cx_shm_pending_lock_removed.  "
        CATCH cx_shm_attach_error.
      ENDTRY.

      CALL METHOD lc_miro->criar
        EXPORTING
          i_cabecalho        = wa_cabecalho
          i_itens            = it_itens
          i_contas           = it_contas
          i_lines_miro_texto = it_lines_miro_texto
          i_bapi_wait        = space
        IMPORTING
          e_invoicedocnumber = DATA(e_invoicedocnumber)
          e_fiscalyear       = DATA(e_fiscalyear)
          e_retorno          = me->mensagens_retorno
          e_j_1bnfdoc        = DATA(e_j_1bnfdoc)
        RECEIVING
          r_gerou            = r_gerou.

*  CS2021000618 US - 68327 - Inicio   - BG

      LOOP AT me->mensagens_retorno INTO DATA(_wa_retorno_miro).
        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = _wa_retorno_miro-type
            i_id           = _wa_retorno_miro-id
            i_num          = _wa_retorno_miro-number
            i_message_v1   = _wa_retorno_miro-message_v1
            i_message_v2   = _wa_retorno_miro-message_v2
            i_message_v3   = _wa_retorno_miro-message_v3
            i_message_v4   = _wa_retorno_miro-message_v4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDLOOP.
*  CS2021000618 US - 68327 - Fim   - BG

      TRY.
          handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
          oref ?= handle->root.
          CLEAR oref.
          handle->detach( ).
          handle->free_area( ).
          handle->free_instance( ).
        CATCH cx_shm_inconsistent.  "
        CATCH cx_shm_no_active_version.  "
        CATCH cx_shm_read_lock_active.  "
        CATCH cx_shm_exclusive_lock_active.  "
        CATCH cx_shm_parameter_error.  "
        CATCH cx_shm_change_lock_active.  "
        CATCH cx_shm_attach_error.
      ENDTRY.

      IF r_gerou EQ abap_false AND it_itens_des[] IS NOT INITIAL.
        REFRESH me->mensagens_retorno.
        CALL METHOD lc_miro->criar
          EXPORTING
            i_cabecalho        = wa_cabecalho
            i_itens            = it_itens_des
            i_contas           = it_contas
            i_lines_miro_texto = it_lines_miro_texto
            i_bapi_wait        = space
          IMPORTING
            e_invoicedocnumber = DATA(e_invoicedocnumber2)
            e_fiscalyear       = DATA(e_fiscalyear2)
            e_retorno          = me->mensagens_retorno
            e_j_1bnfdoc        = DATA(e_j_1bnfdoc2)
          RECEIVING
            r_gerou            = r_gerou.
        TRY.
            handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
            oref ?= handle->root.
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            handle->free_instance( ).
          CATCH cx_shm_inconsistent.  "
          CATCH cx_shm_no_active_version.  "
          CATCH cx_shm_read_lock_active.  "
          CATCH cx_shm_exclusive_lock_active.  "
          CATCH cx_shm_parameter_error.  "
          CATCH cx_shm_change_lock_active.  "
          CATCH cx_shm_attach_error.
        ENDTRY.
        IF r_gerou EQ abap_true.
          e_invoicedocnumber = e_invoicedocnumber2.
          e_fiscalyear       = e_fiscalyear2.
          e_j_1bnfdoc        = e_j_1bnfdoc2.
        ENDIF.
      ENDIF.

      IF r_gerou EQ abap_true.
        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = sy-msgty
            i_id           = sy-msgid
            i_num          = sy-msgno
            i_message_v1   = sy-msgv1
            i_message_v2   = sy-msgv2
            i_message_v3   = sy-msgv3
            i_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.

        me->set_nr_documento_fatura( i_belnr = e_invoicedocnumber i_gjahr = e_fiscalyear ).

        IF e_j_1bnfdoc IS NOT INITIAL.
          me->set_nr_fiscal( i_docnum = e_j_1bnfdoc-docnum ).
          me->nfe_atualiza_cfop_no_fiscal( ).
        ENDIF.

      ENDIF.

      LOOP AT me->mensagens_retorno INTO DATA(wa_retorno_miro).
        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = wa_retorno_miro-type
            i_id           = wa_retorno_miro-id
            i_num          = wa_retorno_miro-number
            i_message_v1   = wa_retorno_miro-message_v1
            i_message_v2   = wa_retorno_miro-message_v2
            i_message_v3   = wa_retorno_miro-message_v3
            i_message_v4   = wa_retorno_miro-message_v4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDLOOP.

      CLEAR: lc_miro.

    ELSE.

      me->set_nr_documento_fatura( i_belnr = wa_miro-belnr i_gjahr = wa_miro-gjahr ).
      r_gerou = abap_true.

      SELECT SINGLE * INTO e_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE belnr EQ wa_miro-belnr
         AND gjahr EQ wa_miro-gjahr.

      CHECK sy-subrc IS INITIAL.

      me->set_nr_fiscal( i_docnum = e_j_1bnfdoc-docnum ).
      me->nfe_atualiza_cfop_no_fiscal( ).

    ENDIF.
    CLEAR me->at_simular.

  ENDMETHOD.


  METHOD NFE_INBOUND_GRAVAR_LOG.

*-CS2021000662 - 19.11.2021 - JT - inicio
*-------------------------------
*---Trazer log de retorno drm ajuste
*-------------------------------
    IF me->ck_eliminar_log EQ abap_true.
      SELECT *
        APPENDING TABLE me->logs
        FROM zib_nfe_dist_log
       WHERE chave_nfe = me->nota-chave_nfe
         AND id        = 'SD'
         AND num       = '024'.

      IF sy-subrc = 0.
        SORT me->logs BY message
                         dt_atualizacao  DESCENDING
                         hr_atualizacao  DESCENDING.
        DELETE ADJACENT DUPLICATES FROM me->logs
                              COMPARING message.
        SORT me->logs BY dt_atualizacao hr_atualizacao nr_sequencia.

        lc_sequencia = 0.
        LOOP AT me->logs   INTO DATA(wa_logs_aux).
          lc_sequencia             = lc_sequencia + 1.
          wa_logs_aux-nr_sequencia = lc_sequencia.
          MODIFY me->logs  FROM wa_logs_aux INDEX sy-tabix.
        ENDLOOP.
      ENDIF.
    ENDIF.
*-CS2021000662 - 19.11.2021 - JT - fim

    IF me->ck_eliminar_log EQ abap_true.
      DELETE FROM zib_nfe_dist_log WHERE chave_nfe EQ me->nota-chave_nfe.
      me->nfe_atualiza_log_aprovacao( ).
    ENDIF.
    LOOP AT me->logs INTO DATA(wa_logs).
      wa_logs-chave_nfe = me->nota-chave_nfe.
      MODIFY zib_nfe_dist_log FROM wa_logs.
    ENDLOOP.

    IF me->pedidos IS NOT INITIAL.
      DELETE FROM zib_nfe_dist_log
       WHERE chave_nfe EQ me->nota-chave_nfe
         AND id        EQ 'ZNFE_DISTRI'
         AND num       EQ 13.
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_REJEITA_ACEITE.

    DATA: EX_CADASTRO         TYPE REF TO ZCX_CADASTRO,
          WA_ZIB_NFE_DIST_EAP TYPE ZIB_NFE_DIST_EAP,
          WL_HEADER           TYPE THEAD,
          TL_TLINES           TYPE TABLE OF TLINE,
          RTP_APROVACAO       TYPE RANGE OF ZDE_TIP_APROVACAO_NFE,
          WTP_APROVACAO       LIKE LINE OF RTP_APROVACAO.

    IF SY-TCODE EQ 'ZMM0110' OR SY-TCODE EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '10'
                                        ID 'ZNFETERMEP' FIELD ME->NOTA-E_TOMADORA
                                        ID 'ZNFETERFIL' FIELD ME->NOTA-F_TOMADORA
                                        ID 'ZNFETERDEP' FIELD ME->NOTA-CD_DEPARTAMENTO.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
                              ATTR1 = '10'
                              ATTR2 = CONV #( ME->NOTA-E_TOMADORA )
                              ATTR3 = CONV #( ME->NOTA-F_TOMADORA )
                              ATTR4 = CONV #( ME->NOTA-CD_DEPARTAMENTO ) )
            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
            MSGV1  = '10'
            MSGV2  = CONV #( ME->NOTA-E_TOMADORA )
            MSGV3  = CONV #( ME->NOTA-F_TOMADORA )
            MSGV4  = CONV #( ME->NOTA-CD_DEPARTAMENTO ).
      ENDIF.
    ENDIF.

    IF ME->NOTA-CK_FISICO IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ESTORNA_MOVIMENTO_MERC-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ESTORNA_MOVIMENTO_MERC-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ESTORNA_ROMANEIO_SAIDA-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_ESTORNA_ROMANEIO_SAIDA-MSGNO.
    ENDIF.

    IF ME->NOTA-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_00 AND ME->NOTA-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_ACEITE_FISICO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_ACEITE_FISICO-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_ACEITE_FISICO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_ACEITE_FISICO-MSGNO.
    ENDIF.

    CASE I_TP_AUTORIZACAO.
      WHEN '01'. "Aprovado Recusa do Documento
        ME->SET_ST_DOCUMENTO( I_ST_DOCUMENTO = ME->ST_DOCUMENTO_99 ).
        ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_NAO_ACEITO_FISCAL ).
        ME->SET_CK_FISCAL( I_CHECK = ABAP_TRUE ).
      WHEN '02'. "Rejeitado Recusa do Documento

        WTP_APROVACAO-SIGN   = 'I'.
        WTP_APROVACAO-OPTION = 'EQ'.
        IF I_MOTIVO EQ '210240'.
          "210240	Operação não Realizada
          WTP_APROVACAO-LOW    = '210220'.
          WTP_APROVACAO-HIGH   = '210220'.
          APPEND WTP_APROVACAO TO RTP_APROVACAO.
        ELSEIF I_MOTIVO EQ '210220'.
          "210220	Desconhecimento de Operação
          WTP_APROVACAO-LOW    = '210240'.
          WTP_APROVACAO-HIGH   = '210240'.
          APPEND WTP_APROVACAO TO RTP_APROVACAO.
        ENDIF.

        SELECT * INTO TABLE @DATA(IT_ZIB_NFE_DIST_EAP)
          FROM ZIB_NFE_DIST_EAP
         WHERE CD_CHAVE_NFE  EQ @ME->NOTA-CHAVE_NFE
           AND TP_APROVACAO  IN @RTP_APROVACAO
           AND TP_AUTORIZADO EQ '01'
           AND CK_ULTIMO     EQ @ABAP_TRUE.

        IF SY-SUBRC IS NOT INITIAL.
          ME->SET_ST_DOCUMENTO( I_ST_DOCUMENTO = ME->ST_DOCUMENTO_00 ).
          ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_PENDENTE ).
          ME->SET_CK_FISCAL( I_CHECK = ABAP_FALSE ).
        ELSE.
          ME->CK_ALTEROU = ABAP_TRUE.
        ENDIF.
    ENDCASE.

    DATA(CK_GRAVOU) = ME->ZIF_CADASTRO~GRAVAR_REGISTRO( ).

    IF CK_GRAVOU EQ ABAP_TRUE.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR = '01'
          OBJECT      = 'ZIB_NFEEAP'
        IMPORTING
          NUMBER      = WA_ZIB_NFE_DIST_EAP-CD_APROVACAO.

      WA_ZIB_NFE_DIST_EAP-TP_APROVACAO    = I_MOTIVO.
      WA_ZIB_NFE_DIST_EAP-CD_CHAVE_NFE    = ME->NOTA-CHAVE_NFE.
      WA_ZIB_NFE_DIST_EAP-DS_NAME_USUARIO = SY-UNAME.
      WA_ZIB_NFE_DIST_EAP-DT_AUTORIZACAO  = SY-DATUM.
      WA_ZIB_NFE_DIST_EAP-HR_AUTORIZACAO  = SY-UZEIT.
      WA_ZIB_NFE_DIST_EAP-TP_AUTORIZADO   = I_TP_AUTORIZACAO.
      WA_ZIB_NFE_DIST_EAP-CK_ULTIMO       = ABAP_TRUE.

      UPDATE ZIB_NFE_DIST_EAP
         SET CK_ULTIMO = ABAP_FALSE
       WHERE TP_APROVACAO EQ WA_ZIB_NFE_DIST_EAP-TP_APROVACAO
         AND CD_CHAVE_NFE EQ WA_ZIB_NFE_DIST_EAP-CD_CHAVE_NFE
         AND CK_ULTIMO    EQ WA_ZIB_NFE_DIST_EAP-CK_ULTIMO.

      MODIFY ZIB_NFE_DIST_EAP FROM WA_ZIB_NFE_DIST_EAP.

      MOVE I_TLINE[] TO TL_TLINES[].

      WL_HEADER-TDOBJECT = 'ZAPROVACAO'.
      WL_HEADER-TDID     = 'ZNFE'.
      WL_HEADER-TDSPRAS  = SY-LANGU.
      CONCATENATE SY-MANDT WA_ZIB_NFE_DIST_EAP-CD_APROVACAO INTO WL_HEADER-TDNAME.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER          = WL_HEADER
          INSERT          = ABAP_TRUE
          SAVEMODE_DIRECT = ABAP_TRUE
        TABLES
          LINES           = TL_TLINES.

      COMMIT WORK.

      ME->NFE_ATUALIZA_LOG_APROVACAO( EXPORTING I_CD_APROVACAO = WA_ZIB_NFE_DIST_EAP-CD_APROVACAO ).

    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_REMESSA.

    DATA: I_ITEM   TYPE ZDE_BAPI_REMESSA_ITEM,
          I_PARID	 TYPE J_1BPARID,
          I_XBLNR	 TYPE XBLNR_V1,
          L_SERIE  TYPE C LENGTH 3,
          LC_LOTES TYPE ZIB_NFE_DIST_LOT_T.

    CLEAR: R_GEROU.

    DATA(WA_LIKP) = ME->GET_AVISO_VALIDO( ).

    IF WA_LIKP IS INITIAL.

      "Buscar Informações do Departamento
      CREATE OBJECT ME->DEPARTAMENTO.
      ME->DEPARTAMENTO->SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
      DATA(IT_TIPOS_PEDIDO_COMPRA) = ME->DEPARTAMENTO->GET_TIPO_PEDIDO_COMPRA( ).
      SORT IT_TIPOS_PEDIDO_COMPRA BY BSTYP BSART.
      CLEAR: ME->DEPARTAMENTO.

      DATA: LC_AVISO TYPE REF TO ZCL_AVISO_RECEBIMENTO.
      CREATE OBJECT LC_AVISO.
      LC_AVISO->SET_FORNECEDOR( I_LIFNR = ME->NOTA-P_EMISSOR ).

      IF ME->PEDIDOS[] IS INITIAL.
        SELECT * INTO TABLE @DATA(IT_EKKO) FROM EKKO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN.
        SELECT * INTO TABLE @DATA(IT_EKPO) FROM EKPO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN AND EBELP EQ @ME->ITENS-EBELP.
      ELSE.
        SELECT * INTO TABLE @IT_EKKO FROM EKKO FOR ALL ENTRIES IN @ME->PEDIDOS WHERE EBELN EQ @ME->PEDIDOS-EBELN.
        SELECT * INTO TABLE @IT_EKPO FROM EKPO FOR ALL ENTRIES IN @ME->PEDIDOS WHERE EBELN EQ @ME->PEDIDOS-EBELN AND EBELP EQ @ME->PEDIDOS-EBELP.
      ENDIF.

      IF IT_EKPO[] IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_EKET) FROM EKET FOR ALL ENTRIES IN @IT_EKPO WHERE EBELN EQ @IT_EKPO-EBELN AND EBELP EQ @IT_EKPO-EBELP.
      ENDIF.

      SORT IT_EKKO BY EBELN.
      SORT IT_EKPO BY EBELN EBELP.
      SORT IT_EKET BY EBELN EBELP.

      READ TABLE IT_EKKO INDEX 1 INTO DATA(WA_EKKO).
      LC_AVISO->SET_PEDIDO_COMPRA( I_EBELN = WA_EKKO-EBELN ).
      LC_AVISO->SET_DATA_LANCAMENTO( I_BLDAT = SY-DATUM ).

      IF ME->PEDIDOS[] IS INITIAL.

        LOOP AT ME->ITENS INTO DATA(WA_ITEM).
          I_ITEM-ITEM_DO_LOTE = SY-TABIX * 10.
          "Pedido de Compra
          READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_ITEM-EBELN BINARY SEARCH.
          "Pedido de Compra - Item
          READ TABLE IT_EKPO INTO DATA(WA_EKPO) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
          "Pedido de Compra - Divisões do programa de remessas
          READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
          "Departamento - Tipo de Pedido de Compra
          READ TABLE IT_TIPOS_PEDIDO_COMPRA INTO DATA(WA_TIPOS_PEDIDO_COMPRA) WITH KEY BSTYP = WA_EKKO-BSTYP BSART = WA_EKKO-BSART BINARY SEARCH.

          "Centro A FIXAR
          I_ITEM-EBELN        = WA_ITEM-EBELN.
          I_ITEM-EBELP        = WA_ITEM-EBELP.
          I_ITEM-VGTYP        = 'V'.
          I_ITEM-QUANTIDADE   = WA_ITEM-MENGE.
          I_ITEM-UNIDADE      = WA_ITEM-MEINS.
          I_ITEM-MATERIAL     = WA_ITEM-MATNR.
          I_ITEM-TRATY        = '0001'.
          I_ITEM-TRAGR        = '0001'.
          I_ITEM-LADGR        = '0003'.
          I_ITEM-MFRGR        = '00000001'.
          I_ITEM-KZBEW        = 'B'.
          I_ITEM-PLANT        = WA_EKPO-WERKS.
          I_ITEM-STGE_LOC     = WA_EKPO-LGORT.
          I_ITEM-MOVE_TYPE = WA_TIPOS_PEDIDO_COMPRA-TIPO_MOVIMENTO.

          IF WA_EKET-CHARG IS NOT INITIAL.
            CLEAR: I_ITEM-ITEM_DO_LOTE.
            I_ITEM-BATCH = WA_EKET-CHARG.
            I_ITEM-LICHA = WA_EKET-CHARG.
            LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
          ELSE.
            LC_LOTES = ME->LOTES.
            DELETE LC_LOTES WHERE PROD_ITEM NE WA_ITEM-PROD_ITEM.
            DESCRIBE TABLE LC_LOTES LINES DATA(QTD_LOTES).

            IF QTD_LOTES GT 1.
              LOOP AT ME->LOTES INTO DATA(WA_LOTE) WHERE PROD_ITEM EQ WA_ITEM-PROD_ITEM.
                I_ITEM-BATCH      = WA_LOTE-CHARG.
                I_ITEM-LICHA      = WA_LOTE-LICHA.
                I_ITEM-QUANTIDADE = WA_LOTE-MENGE.
                LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
              ENDLOOP.
            ELSE.
              CLEAR: I_ITEM-ITEM_DO_LOTE.
              LOOP AT ME->LOTES INTO WA_LOTE WHERE PROD_ITEM EQ WA_ITEM-PROD_ITEM.
                I_ITEM-BATCH      = WA_LOTE-CHARG.
                I_ITEM-LICHA      = WA_LOTE-LICHA.
                I_ITEM-QUANTIDADE = WA_LOTE-MENGE.
                LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
              ENDLOOP.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ELSE.

        LOOP AT ME->PEDIDOS INTO DATA(WA_PEDIDO_ITEM).

          I_ITEM-ITEM_DO_LOTE = SY-TABIX * 10.

          "Pedido de Compra
          READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_PEDIDO_ITEM-EBELN BINARY SEARCH.
          "Pedido de Compra - Item
          READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_PEDIDO_ITEM-EBELN EBELP = WA_PEDIDO_ITEM-EBELP BINARY SEARCH.
          "Pedido de Compra - Divisões do programa de remessas
          READ TABLE IT_EKET INTO WA_EKET WITH KEY EBELN = WA_PEDIDO_ITEM-EBELN EBELP = WA_PEDIDO_ITEM-EBELP BINARY SEARCH.
          "Departamento - Tipo de Pedido de Compra
          READ TABLE IT_TIPOS_PEDIDO_COMPRA INTO WA_TIPOS_PEDIDO_COMPRA WITH KEY BSTYP = WA_EKKO-BSTYP BSART = WA_EKKO-BSART BINARY SEARCH.

          "Centro A FIXAR
          I_ITEM-EBELN        = WA_PEDIDO_ITEM-EBELN.
          I_ITEM-EBELP        = WA_PEDIDO_ITEM-EBELP.
          I_ITEM-VGTYP        = 'V'.
          I_ITEM-QUANTIDADE   = WA_PEDIDO_ITEM-MENGE.
          I_ITEM-UNIDADE      = WA_PEDIDO_ITEM-MEINS.
          I_ITEM-MATERIAL     = WA_PEDIDO_ITEM-MATNR.
          I_ITEM-TRATY        = '0001'.
          I_ITEM-TRAGR        = '0001'.
          I_ITEM-LADGR        = '0003'.
          I_ITEM-MFRGR        = '00000001'.
          I_ITEM-KZBEW        = 'B'.
          I_ITEM-PLANT        = WA_EKPO-WERKS.
          I_ITEM-STGE_LOC     = WA_EKPO-LGORT.
          I_ITEM-MOVE_TYPE    = WA_TIPOS_PEDIDO_COMPRA-TIPO_MOVIMENTO.

          IF WA_EKET-CHARG IS NOT INITIAL.
            CLEAR: I_ITEM-ITEM_DO_LOTE.
            I_ITEM-BATCH = WA_EKET-CHARG.
            I_ITEM-LICHA = WA_EKET-CHARG.
            LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
          ELSE.
            LC_LOTES = ME->LOTES.
            DELETE LC_LOTES WHERE EBELN NE WA_PEDIDO_ITEM-EBELN AND EBELP NE WA_PEDIDO_ITEM-EBELP.
            DESCRIBE TABLE LC_LOTES LINES QTD_LOTES.

            IF QTD_LOTES GT 1.
              LOOP AT ME->LOTES INTO WA_LOTE
                 WHERE EBELN EQ WA_PEDIDO_ITEM-EBELN
                   AND EBELP EQ WA_PEDIDO_ITEM-EBELP.
                I_ITEM-BATCH      = WA_LOTE-CHARG.
                I_ITEM-LICHA      = WA_LOTE-LICHA.
                I_ITEM-QUANTIDADE = WA_LOTE-MENGE.
                LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
              ENDLOOP.
            ELSE.
              CLEAR: I_ITEM-ITEM_DO_LOTE.
              LOOP AT ME->LOTES INTO WA_LOTE
                WHERE EBELN EQ WA_PEDIDO_ITEM-EBELN
                  AND EBELP EQ WA_PEDIDO_ITEM-EBELP.
                I_ITEM-BATCH      = WA_LOTE-CHARG.
                I_ITEM-LICHA      = WA_LOTE-LICHA.
                I_ITEM-QUANTIDADE = WA_LOTE-MENGE.
                LC_AVISO->SET_ITEM( I_ITEM = I_ITEM ).
              ENDLOOP.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.

      IF ME->NOTA-PC_PARTINER IS NOT INITIAL.
        LC_AVISO->SET_LC_COLETA_PARID( I_PARID = ME->NOTA-PC_PARTINER ).
        LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
      ELSE.
        LC_AVISO->SET_LC_COLETA_PARID( I_PARID = ME->NOTA-P_EMISSOR ).
        LC_AVISO->SET_LC_COLETA_PARTYP( I_PARTYP = 'V' ).
      ENDIF.

      LC_AVISO->SET_SP_FRETE_PARID( I_PARID = ME->NOTA-F_TRANSPORTE ).
      LC_AVISO->SET_SP_FRETE_PARTYP( I_PARTYP = 'V' ).
      LC_AVISO->SET_VFDAT( I_VFDAT = ME->NOTA-DT_VENCIMENTO ).

      LC_AVISO->SET_SHIP_POINT( I_SHIP_POINT = ME->NOTA-F_TOMADORA ).

      IF ME->NOTA-LR_PARTINER IS NOT INITIAL.
        LC_AVISO->SET_LC_ENTREGA_PARID( I_PARID = ME->NOTA-LR_PARTINER ).
        LC_AVISO->SET_LC_ENTREGA_PARTYP( I_PARTYP = 'V' ).
      ELSE.
        MOVE ME->NOTA-F_TOMADORA TO I_PARID.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = I_PARID
          IMPORTING
            OUTPUT = I_PARID.

        LC_AVISO->SET_LC_ENTREGA_PARID( I_PARID = I_PARID ).
        LC_AVISO->SET_LC_ENTREGA_PARTYP( I_PARTYP = 'C' ).
      ENDIF.

      LC_AVISO->SET_VALOR_NOTA( I_VALOR_NOTA = ME->NOTA-VL_TOTAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = ME->NOTA-NUMERO
        IMPORTING
          OUTPUT = I_XBLNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = ME->NOTA-SERIE
        IMPORTING
          OUTPUT = L_SERIE.

      CONCATENATE I_XBLNR L_SERIE INTO I_XBLNR SEPARATED BY '-'.

      LC_AVISO->SET_XBLNR( I_XBLNR = I_XBLNR ).

      LC_AVISO->SET_CHAVE_NFE( I_CHAVE_NFE = me->nota-CHAVE_NFE ). "ALRS

      LC_AVISO->SET_CK_ROUTE_VALIDAR( I_CK_ROUTE_VALIDAR = ABAP_TRUE ).


      R_GEROU = LC_AVISO->CRIAR_AVISO_RECEBIMENTO( I_PARTICAO_LOTE = ABAP_TRUE ).

      DATA(R_RETORNO) = LC_AVISO->GET_RETORNO( ).

      LOOP AT R_RETORNO INTO DATA(WA_RETORNO).
        CALL METHOD ME->SET_ADD_LOG_NFE
          EXPORTING
            I_TYPE         = WA_RETORNO-TYPE
            I_ID           = WA_RETORNO-ID
            I_NUM          = WA_RETORNO-NUMBER
            I_MESSAGE_V1   = WA_RETORNO-MESSAGE_V1
            I_MESSAGE_V2   = WA_RETORNO-MESSAGE_V2
            I_MESSAGE_V3   = WA_RETORNO-MESSAGE_V3
            I_MESSAGE_V4   = WA_RETORNO-MESSAGE_V4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDLOOP.

      IF R_GEROU EQ ABAP_TRUE.
        ME->SET_NR_REMESSA( I_REMESSA = LC_AVISO->GET_NR_REMESSA( ) ).
      ENDIF.

      IF LC_AVISO IS NOT INITIAL.
        CLEAR: LC_AVISO.
      ENDIF.
    ELSE.
      R_GEROU = ABAP_TRUE.
      ME->SET_NR_REMESSA( I_REMESSA = WA_LIKP-VBELN ).
    ENDIF.

  ENDMETHOD.


  METHOD NFE_INBOUND_SAIDA_ARMAZENAGEM.

    DATA: CK_ENVIA_ACEITE TYPE CHAR01.

    CHECK GET_CK_GERAR_SAIDA_ARMAZEM( IMPORTING CK_ENVIA_ACEITE = CK_ENVIA_ACEITE ) EQ ABAP_TRUE.

    CASE I_TOTAL.
      WHEN ABAP_TRUE.

        TRY .

            DATA(LC_DESTINAR) =
            ZCL_FACTORY_MAT_DESTINACAO=>ZIF_FACTORY_MAT_DESTINACAO~GET_INSTANCE(
               )->SET_FACTORY_OBJETO( EXPORTING I_TP_DESTINACAO = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR
               )->GET_FACTORY_OBJETO(
               )->SET_NEW(
               ).

            SELECT * INTO TABLE @DATA(IT_MSEG)
              FROM MSEG
             WHERE MBLNR EQ @ME->NOTA-MBLNR
               AND MJAHR EQ @ME->NOTA-MJAHR.

            DATA: LV_SAIDA TYPE DMBTR.

            LOOP AT IT_MSEG INTO DATA(WA_MSEG).

              LV_SAIDA = 0.

              "Buscar Valor do Item com Baso no Valor do Item da Nota
              DATA(VALOR_SOMA_ITEM) =
               REDUCE J_1BBASE( INIT I TYPE J_1BBASE
                FOR LS IN ME->ITENS WHERE ( MATNR EQ WA_MSEG-MATNR )
                  NEXT I = I + LS-PROD_VLR_TOTAL_B ).

              DATA(QUANT_SOMA_ITEM) =
               REDUCE J_1BBASE( INIT I TYPE J_1BBASE
                FOR LS IN ME->ITENS WHERE ( MATNR EQ WA_MSEG-MATNR )
                  NEXT I = I + LS-MENGE ).

              LV_SAIDA = ( WA_MSEG-MENGE * VALOR_SOMA_ITEM ) / QUANT_SOMA_ITEM.

              LC_DESTINAR->SET_ADD_DOC_MATERIAL_ORIGEM(
                EXPORTING
                  I_ORIG_MBLNR             = WA_MSEG-MBLNR      " Nº documento de material
                  I_ORIG_MJAHR             = WA_MSEG-MJAHR      " Ano do documento do material
                  I_ORIG_ZEILE             = WA_MSEG-ZEILE      " Item no documento do material
                  I_ORIG_NFE               = ME->NOTA-CHAVE_NFE " Chave NF-e
                  I_MENGE                  = WA_MSEG-MENGE      " Quantidade
                  I_MEINS                  = WA_MSEG-MEINS      " Unidade de medida básica
                  I_VALOR                  = CONV #( LV_SAIDA )     " Montante básico
                  I_FORNE                  = ME->NOTA-F_ARMAZEM " Nº conta do fornecedor
                  I_CK_TOTAL_ORIGEM        = ABAP_TRUE
               ).

            ENDLOOP.

            LC_DESTINAR->SET_ENQUEUE(
              )->SET_GRAVAR(
              )->GET_REGISTRO( IMPORTING E_ZMMT0114 = E_ZMMT0114
              )->SET_GERAR_MOVIMENTO(
                    IMPORTING
                      E_MBLNR  = ME->NOTA-MBLNR_ARM
                      E_MJAHR  = ME->NOTA-MJAHR_ARM
                      E_DOCNUM = ME->NOTA-DOCNUM_ARM
                      E_GEROU  = R_GEROU
              )->GET_REGISTRO( IMPORTING E_ZMMT0114 = E_ZMMT0114
              )->FREE(
              ).

            CHECK R_GEROU EQ ABAP_TRUE.

            UPDATE ZIB_NFE_DIST_TER
               SET MBLNR_ARM  = ME->NOTA-MBLNR_ARM
                   MJAHR_ARM  = ME->NOTA-MJAHR_ARM
                   DOCNUM_ARM = ME->NOTA-DOCNUM_ARM
                   ST_ARMAZEM = '99'
             WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

            COMMIT WORK AND WAIT.

            LC_DESTINAR->FREE( ).
            CLEAR: LC_DESTINAR.

            IF ME->NOTA-DOCNUM_ARM IS NOT INITIAL.

              TRY .
                  ZCL_NFE=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->NOTA-DOCNUM_ARM
                    )->SET_REGISTRO( EXPORTING I_DOCNUM = ME->NOTA-DOCNUM_ARM I_SEM_BLOQUEIO = ABAP_TRUE
                    )->GET_CK_AUTORIZADO_USO(
                    ).

                CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).    " .

                  RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
                    EXPORTING
                      TEXTID = VALUE #( MSGID = EX_DOC_ELETRONICO->MSGID
                                        MSGNO = EX_DOC_ELETRONICO->MSGNO
                                        ATTR1 = EX_DOC_ELETRONICO->MSGV1
                                        ATTR2 = EX_DOC_ELETRONICO->MSGV2
                                        ATTR3 = EX_DOC_ELETRONICO->MSGV3
                                        ATTR4 = EX_DOC_ELETRONICO->MSGV4 )
                      MSGID  = EX_DOC_ELETRONICO->MSGID
                      MSGNO  = EX_DOC_ELETRONICO->MSGNO
                      MSGV1  = EX_DOC_ELETRONICO->MSGV1
                      MSGV2  = EX_DOC_ELETRONICO->MSGV2
                      MSGV3  = EX_DOC_ELETRONICO->MSGV3
                      MSGV4  = EX_DOC_ELETRONICO->MSGV4
                      MSGTY  = 'E'.

              ENDTRY.

            ENDIF.

          CATCH ZCX_MATERIAL_DESTINACAO INTO DATA(EX_MATERIAL).

            IF LC_DESTINAR IS NOT INITIAL.
              LC_DESTINAR->FREE( ).
              CLEAR: LC_DESTINAR.
            ENDIF.

            RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_MATERIAL->MSGID
                                  MSGNO = EX_MATERIAL->MSGNO
                                  ATTR1 = CONV #( EX_MATERIAL->MSGV1 )
                                  ATTR2 = CONV #( EX_MATERIAL->MSGV2 )
                                  ATTR3 = CONV #( EX_MATERIAL->MSGV3 )
                                  ATTR4 = CONV #( EX_MATERIAL->MSGV4 ) )
                MSGID  = EX_MATERIAL->MSGID
                MSGNO  = EX_MATERIAL->MSGNO
                MSGTY  = EX_MATERIAL->MSGTY
                MSGV1  = EX_MATERIAL->MSGV1
                MSGV2  = EX_MATERIAL->MSGV2
                MSGV3  = EX_MATERIAL->MSGV3
                MSGV4  = EX_MATERIAL->MSGV4.

        ENDTRY.

        CLEAR: LC_DESTINAR.

      WHEN ABAP_FALSE.
    ENDCASE.

*    Buscar Informações do Departamento
*    CREATE OBJECT ME->DEPARTAMENTO.
*    ME->DEPARTAMENTO->SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
*    DATA(IT_TIPOS_PEDIDO_COMPRA) = ME->DEPARTAMENTO->GET_TIPO_PEDIDO_COMPRA( ).
*
*    DATA: I_CABECALHO TYPE ZDE_MIGO_CABECALHO,
*          WA_ITENS    TYPE ZDE_MIGO_ITENS,
*          I_ITENS     TYPE ZDE_MIGO_ITENS_T.
*
*    Buscar MIGO Valida
*    SELECT * INTO TABLE @DATA(IT_MKPF)
*      FROM MKPF AS M
*     WHERE M~ZCHAVE_NFE EQ @ME->NOTA-CHAVE_NFE.
*
*    LOOP AT IT_MKPF INTO DATA(WA_MKPF_L).
*
*      SELECT SINGLE * INTO @DATA(WA_MSEG)
*        FROM MSEG
*       WHERE MBLNR EQ @WA_MKPF_L-MBLNR
*         AND MJAHR EQ @WA_MKPF_L-MJAHR
*         AND SMBLN EQ @SPACE
*         AND BWART EQ 'Z41'.
*
*      Se Achou é Saída para Armazenagem
*      IF SY-SUBRC IS NOT INITIAL.
*        CONTINUE.
*      ENDIF.
*
*      SELECT SINGLE * INTO @DATA(WA_MSEG_ESTORNADO)
*        FROM MSEG AS E
*       WHERE E~SMBLN EQ @WA_MKPF_L-MBLNR
*         AND E~SJAHR EQ @WA_MKPF_L-MJAHR.
*
*      Se Achou está estornado
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.
*
*      Caso exista um documento somente alimenta
*      DATA(WA_MKPF) = WA_MKPF_L.
*
*    ENDLOOP.
*
*    IF WA_MKPF IS INITIAL.
*
*      SELECT * INTO TABLE @DATA(IT_EKKO) FROM EKKO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN.
*      SELECT * INTO TABLE @DATA(IT_EKPO) FROM EKPO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN AND EBELP EQ @ME->ITENS-EBELP.
*
*      IF IT_EKPO[] IS NOT INITIAL.
*        SELECT * INTO TABLE @DATA(IT_EKET) FROM EKET FOR ALL ENTRIES IN @IT_EKPO WHERE EBELN EQ @IT_EKPO-EBELN AND EBELP EQ @IT_EKPO-EBELP.
*      ENDIF.
*
*      SORT IT_EKKO BY EBELN.
*      SORT IT_EKPO BY EBELN EBELP.
*      SORT IT_EKET BY EBELN EBELP.
*
*      IF IT_EKPO IS NOT INITIAL.
*        SELECT * INTO TABLE @DATA(IT_MARA)
*          FROM MARA
*           FOR ALL ENTRIES IN @IT_EKPO
*         WHERE MATNR EQ @IT_EKPO-MATNR.
*
*        SORT IT_MARA BY MATNR.
*      ENDIF.
*
*      CABEÇALHO MOVIMENTO DE MERCADORIA
*      I_CABECALHO-DATA_DOCUMENTO  = SY-DATUM.
*      I_CABECALHO-DATA_LANCAMENTO = SY-DATUM.
*      I_CABECALHO-DESCRICAO       = 'NFe Inbound'.
*      I_CABECALHO-VALOR_TOTAL     = ME->NOTA-VL_TOTAL.
*      "1  Nota individual
*      "2  Nota indiv.c/TxtContrQld.
*      "3  Nota coletiva
*      I_CABECALHO-VER_GR_GI_SLIP = '2'.
*      I_CABECALHO-VER_GR_GI_SLIP = '2'.
*      I_CABECALHO-ZCHAVE_NFE     = ME->NOTA-CHAVE_NFE.
*      I_CABECALHO-GOODSMVT_CODE  = '04'.
*
*      ITENS DO MOVIMENTO DE MERCADORIA
*      LOOP AT ME->ITENS INTO DATA(WA_ITEM).
*
*        "Pedido de Compra
*        READ TABLE IT_EKKO INTO DATA(WA_EKKO) WITH KEY EBELN = WA_ITEM-EBELN BINARY SEARCH.
*
*        "Pedido de Compra - Item
*        READ TABLE IT_EKPO INTO DATA(WA_EKPO) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
*
*        "Pedido de Compra - Divisões do programa de remessas
*        READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
*
*        "Departamento - Tipo de Pedido de Compra
*        READ TABLE IT_TIPOS_PEDIDO_COMPRA INTO DATA(WA_TIPOS_PEDIDO_COMPRA)
*        WITH KEY BSTYP = WA_EKKO-BSTYP
*                 BSART = WA_EKKO-BSART
*        BINARY SEARCH.
*
*        CLEAR: WA_ITENS.
*        WA_ITENS-TIPO_MOVIMENTO  = 'Z41'.
*        WA_ITENS-TAX_CODE        = 'ZE'.
*        WA_ITENS-MATERIAL        = WA_ITEM-MATNR.
*        WA_ITENS-LOCAL_EXPEDICAO = ME->NOTA-F_TOMADORA.
*        WA_ITENS-FORNECEDOR      = ME->NOTA-F_ARMAZEM.
*        IF WA_ITEM-LGORT IS NOT INITIAL.
*          WA_ITENS-DEPOSITO      = WA_ITEM-LGORT.
*        ELSE.
*          WA_ITENS-DEPOSITO      = WA_EKPO-LGORT.
*        ENDIF.
*
*        ZCL_PEDIDO_COMPRA=>GET_PEDIDO_ITENS(
*          EXPORTING
*            I_EBELN  = WA_ITEM-EBELN
*            I_EBELP  = WA_ITEM-EBELP
*          IMPORTING
*            E_EKPO_T = DATA(E_EKPO_T) ).
*
*        READ TABLE E_EKPO_T INTO DATA(WA_EKPO_T) INDEX 1.
*
*        READ TABLE IT_MARA INTO DATA(WA_MARA) WITH KEY MATNR = WA_ITEM-MATNR BINARY SEARCH.
*
*        IF WA_EKET-CHARG IS NOT INITIAL.
*          WA_ITENS-LOTE           = WA_EKET-CHARG.
*          WA_ITENS-PESO           = WA_ITEM-MENGE.
*          WA_ITENS-UNIDADE        = WA_ITEM-MEINS.
*          WA_ITENS-EXT_BASE_AMONT = WA_ITEM-PROD_VLR_TOTAL_B.
*          APPEND WA_ITENS TO I_ITENS.
*        ELSEIF ME->LOTES IS NOT INITIAL.
*          LOOP AT ME->LOTES INTO DATA(WA_LOTE) WHERE PROD_ITEM EQ WA_ITEM-PROD_ITEM.
*            WA_ITENS-LOTE           = WA_LOTE-CHARG.
*            WA_ITENS-PESO           = WA_LOTE-MENGE.
*            WA_ITENS-UNIDADE        = WA_ITEM-MEINS.
*            WA_ITENS-EXT_BASE_AMONT = ( WA_LOTE-MENGE * WA_ITEM-PROD_VLR_TOTAL_B ) / WA_ITEM-MENGE.
*            APPEND WA_ITENS TO I_ITENS.
*          ENDLOOP.
*        ELSEIF WA_MARA-XCHPF EQ ABAP_FALSE OR WA_EKPO_T-KNTTP IS NOT INITIAL.
*          WA_ITENS-PESO     = WA_ITEM-MENGE.
*          WA_ITENS-UNIDADE  = WA_ITEM-MEINS.
*          WA_ITENS-EXT_BASE_AMONT = WA_ITEM-PROD_VLR_TOTAL_B.
*          APPEND WA_ITENS TO I_ITENS.
*        ENDIF.
*
*      ENDLOOP.
*
*      DATA: LC_MIGO TYPE REF TO ZCL_MIGO.
*
*      CREATE OBJECT LC_MIGO.
*
*      CALL METHOD LC_MIGO->CRIAR
*        EXPORTING
*          I_CABECALHO = I_CABECALHO
*          I_ITENS     = I_ITENS
*          I_BAPI_WAIT = ABAP_TRUE
*        IMPORTING
*          E_RETORNO   = DATA(E_RETORNO)
*          E_J_1BNFDOC = DATA(E_J_1BNFDOC)
*          MAT_DOC     = DATA(MAT_DOC)
*          DOC_YEAR    = DATA(DOC_YEAR)
*        RECEIVING
*          R_GEROU     = DATA(R_GEROU_MIGO).
*
*      IF R_GEROU_MIGO EQ ABAP_TRUE.
*        ME->NOTA-MBLNR_ARM = MAT_DOC.
*        ME->NOTA-MJAHR_ARM = DOC_YEAR.
*        R_GEROU = ABAP_TRUE.
*      ENDIF.
*
*      CLEAR: LC_MIGO.
*
*    ELSE.
*      ME->NOTA-MBLNR_ARM = WA_MKPF-MBLNR.
*      ME->NOTA-MJAHR_ARM = WA_MKPF-MJAHR.
*      R_GEROU = ABAP_TRUE.
*    ENDIF.



*    ME->NFE_ARM_ATUALIZA_CFOP_FISCAL( ).
*
*    "Continua de Deve ser Enviado Aceite de Operação ai Armazem com EDIR
*    CHECK CK_ENVIA_ACEITE EQ ABAP_TRUE.
*    ME->SET_ARM_ENVIAR_ACEITE_OPERACAO( ).

  ENDMETHOD.


  METHOD NFE_INBOUND_SAIDA_DEVOLUCAO.

    DATA: CK_ENVIA_ACEITE TYPE CHAR01.

    "CHECK GET_CK_GERAR_SAIDA_ARMAZEM( IMPORTING CK_ENVIA_ACEITE = CK_ENVIA_ACEITE ) EQ ABAP_TRUE.

    CLEAR: E_MBLNR_DEV, E_MJAHR_DEV, E_BELNR_DEV, E_GJAHR_DEV, E_DOCNUM_DEV.

    CASE I_TOTAL.
      WHEN ABAP_TRUE.

        TRY .

            DATA(LC_DESTINAR) =
            ZCL_FACTORY_MAT_DESTINACAO=>ZIF_FACTORY_MAT_DESTINACAO~GET_INSTANCE(
               )->SET_FACTORY_OBJETO( EXPORTING I_TP_DESTINACAO = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO
               )->GET_FACTORY_OBJETO(
               )->SET_NEW(
               ).

            SELECT * INTO TABLE @DATA(IT_MSEG)
              FROM MSEG
             WHERE MBLNR EQ @ME->NOTA-MBLNR
               AND MJAHR EQ @ME->NOTA-MJAHR.

            LOOP AT IT_MSEG INTO DATA(WA_MSEG).

              "Buscar Valor do Item com Baso no Valor do Item da Nota
              DATA(VALOR_SOMA_ITEM) =
               REDUCE J_1BBASE( INIT I TYPE J_1BBASE
                FOR LS IN ME->ITENS WHERE ( MATNR EQ WA_MSEG-MATNR )
                  NEXT I = I + LS-PROD_VLR_TOTAL_B ).

              DATA(QUANT_SOMA_ITEM) =
               REDUCE J_1BBASE( INIT I TYPE J_1BBASE
                FOR LS IN ME->ITENS WHERE ( MATNR EQ WA_MSEG-MATNR )
                  NEXT I = I + LS-MENGE ).

              DATA(LV_SAIDA) = ( WA_MSEG-MENGE * VALOR_SOMA_ITEM ) / QUANT_SOMA_ITEM.

              LC_DESTINAR->SET_ADD_DOC_MATERIAL_ORIGEM(
                EXPORTING
                  I_ORIG_MBLNR             = WA_MSEG-MBLNR      " Nº documento de material
                  I_ORIG_MJAHR             = WA_MSEG-MJAHR      " Ano do documento do material
                  I_ORIG_ZEILE             = WA_MSEG-ZEILE      " Item no documento do material
                  I_ORIG_NFE               = ME->NOTA-CHAVE_NFE " Chave NF-e
                  I_MENGE                  = WA_MSEG-MENGE      " Quantidade
                  I_MEINS                  = WA_MSEG-MEINS      " Unidade de medida básica
                  I_VALOR                  = CONV #( LV_SAIDA ) " Montante básico
                  I_FORNE                  = WA_MSEG-LIFNR      " Nº conta do fornecedor
                  I_CK_TOTAL_ORIGEM        = ABAP_TRUE
               ).

            ENDLOOP.

            LC_DESTINAR->SET_ENQUEUE(
              )->SET_GRAVAR(
              )->GET_REGISTRO( IMPORTING E_ZMMT0114 = E_ZMMT0114
              )->SET_GERAR_MOVIMENTO(
                    IMPORTING
                      E_MBLNR      = ME->NOTA-MBLNR_DEV
                      E_MJAHR      = ME->NOTA-MJAHR_DEV
                      E_DOCNUM     = ME->NOTA-DOCNUM_DEV
                      E_BELNR_DEV  = ME->NOTA-BELNR_DEV
                      E_GJAHR_DEV  = ME->NOTA-GJAHR_DEV
                      E_DOCNUM_DEV = ME->NOTA-DOCNUM_DEV
                      E_GEROU      = R_GEROU
              )->GET_REGISTRO( IMPORTING E_ZMMT0114 = E_ZMMT0114
              )->FREE(
              ).

            E_MBLNR_DEV   = ME->NOTA-MBLNR_DEV.
            E_MJAHR_DEV   = ME->NOTA-MJAHR_DEV.
            E_BELNR_DEV   = ME->NOTA-BELNR_DEV.
            E_GJAHR_DEV   = ME->NOTA-GJAHR_DEV.
            E_DOCNUM_DEV  = ME->NOTA-DOCNUM_DEV.

            CHECK R_GEROU EQ ABAP_TRUE.

            UPDATE ZIB_NFE_DIST_TER
               SET MBLNR_DEV  = ME->NOTA-MBLNR_DEV
                   MJAHR_DEV  = ME->NOTA-MJAHR_DEV
                   DOCNUM_DEV = ME->NOTA-DOCNUM_DEV
             WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.

            COMMIT WORK AND WAIT.

            LC_DESTINAR->FREE( ).
            CLEAR: LC_DESTINAR.

          CATCH ZCX_MATERIAL_DESTINACAO INTO DATA(EX_MATERIAL).

            IF LC_DESTINAR IS NOT INITIAL.
              LC_DESTINAR->FREE( ).
              CLEAR: LC_DESTINAR.
            ENDIF.

            RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_MATERIAL->MSGID
                                  MSGNO = EX_MATERIAL->MSGNO
                                  ATTR1 = CONV #( EX_MATERIAL->MSGV1 )
                                  ATTR2 = CONV #( EX_MATERIAL->MSGV2 )
                                  ATTR3 = CONV #( EX_MATERIAL->MSGV3 )
                                  ATTR4 = CONV #( EX_MATERIAL->MSGV4 ) )
                MSGID  = EX_MATERIAL->MSGID
                MSGNO  = EX_MATERIAL->MSGNO
                MSGTY  = EX_MATERIAL->MSGTY
                MSGV1  = EX_MATERIAL->MSGV1
                MSGV2  = EX_MATERIAL->MSGV2
                MSGV3  = EX_MATERIAL->MSGV3
                MSGV4  = EX_MATERIAL->MSGV4.

        ENDTRY.

        CLEAR: LC_DESTINAR.

      WHEN ABAP_FALSE.
    ENDCASE.

*    Buscar Informações do Departamento
*    CREATE OBJECT ME->DEPARTAMENTO.
*    ME->DEPARTAMENTO->SET_REGISTRO( I_ID_REGISTRO = ME->NOTA-CD_DEPARTAMENTO ).
*    DATA(IT_TIPOS_PEDIDO_COMPRA) = ME->DEPARTAMENTO->GET_TIPO_PEDIDO_COMPRA( ).
*
*    DATA: I_CABECALHO TYPE ZDE_MIGO_CABECALHO,
*          WA_ITENS    TYPE ZDE_MIGO_ITENS,
*          I_ITENS     TYPE ZDE_MIGO_ITENS_T.
*
*    Buscar MIGO Valida
*    SELECT * INTO TABLE @DATA(IT_MKPF)
*      FROM MKPF AS M
*     WHERE M~ZCHAVE_NFE EQ @ME->NOTA-CHAVE_NFE.
*
*    LOOP AT IT_MKPF INTO DATA(WA_MKPF_L).
*
*      SELECT SINGLE * INTO @DATA(WA_MSEG)
*        FROM MSEG
*       WHERE MBLNR EQ @WA_MKPF_L-MBLNR
*         AND MJAHR EQ @WA_MKPF_L-MJAHR
*         AND SMBLN EQ @SPACE
*         AND BWART EQ 'Z41'.
*
*      Se Achou é Saída para Armazenagem
*      IF SY-SUBRC IS NOT INITIAL.
*        CONTINUE.
*      ENDIF.
*
*      SELECT SINGLE * INTO @DATA(WA_MSEG_ESTORNADO)
*        FROM MSEG AS E
*       WHERE E~SMBLN EQ @WA_MKPF_L-MBLNR
*         AND E~SJAHR EQ @WA_MKPF_L-MJAHR.
*
*      Se Achou está estornado
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.
*
*      Caso exista um documento somente alimenta
*      DATA(WA_MKPF) = WA_MKPF_L.
*
*    ENDLOOP.
*
*    IF WA_MKPF IS INITIAL.
*
*      SELECT * INTO TABLE @DATA(IT_EKKO) FROM EKKO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN.
*      SELECT * INTO TABLE @DATA(IT_EKPO) FROM EKPO FOR ALL ENTRIES IN @ME->ITENS WHERE EBELN EQ @ME->ITENS-EBELN AND EBELP EQ @ME->ITENS-EBELP.
*
*      IF IT_EKPO[] IS NOT INITIAL.
*        SELECT * INTO TABLE @DATA(IT_EKET) FROM EKET FOR ALL ENTRIES IN @IT_EKPO WHERE EBELN EQ @IT_EKPO-EBELN AND EBELP EQ @IT_EKPO-EBELP.
*      ENDIF.
*
*      SORT IT_EKKO BY EBELN.
*      SORT IT_EKPO BY EBELN EBELP.
*      SORT IT_EKET BY EBELN EBELP.
*
*      IF IT_EKPO IS NOT INITIAL.
*        SELECT * INTO TABLE @DATA(IT_MARA)
*          FROM MARA
*           FOR ALL ENTRIES IN @IT_EKPO
*         WHERE MATNR EQ @IT_EKPO-MATNR.
*
*        SORT IT_MARA BY MATNR.
*      ENDIF.
*
*      CABEÇALHO MOVIMENTO DE MERCADORIA
*      I_CABECALHO-DATA_DOCUMENTO  = SY-DATUM.
*      I_CABECALHO-DATA_LANCAMENTO = SY-DATUM.
*      I_CABECALHO-DESCRICAO       = 'NFe Inbound'.
*      I_CABECALHO-VALOR_TOTAL     = ME->NOTA-VL_TOTAL.
*      "1  Nota individual
*      "2  Nota indiv.c/TxtContrQld.
*      "3  Nota coletiva
*      I_CABECALHO-VER_GR_GI_SLIP = '2'.
*      I_CABECALHO-VER_GR_GI_SLIP = '2'.
*      I_CABECALHO-ZCHAVE_NFE     = ME->NOTA-CHAVE_NFE.
*      I_CABECALHO-GOODSMVT_CODE  = '04'.
*
*      ITENS DO MOVIMENTO DE MERCADORIA
*      LOOP AT ME->ITENS INTO DATA(WA_ITEM).
*
*        "Pedido de Compra
*        READ TABLE IT_EKKO INTO DATA(WA_EKKO) WITH KEY EBELN = WA_ITEM-EBELN BINARY SEARCH.
*
*        "Pedido de Compra - Item
*        READ TABLE IT_EKPO INTO DATA(WA_EKPO) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
*
*        "Pedido de Compra - Divisões do programa de remessas
*        READ TABLE IT_EKET INTO DATA(WA_EKET) WITH KEY EBELN = WA_ITEM-EBELN EBELP = WA_ITEM-EBELP BINARY SEARCH.
*
*        "Departamento - Tipo de Pedido de Compra
*        READ TABLE IT_TIPOS_PEDIDO_COMPRA INTO DATA(WA_TIPOS_PEDIDO_COMPRA)
*        WITH KEY BSTYP = WA_EKKO-BSTYP
*                 BSART = WA_EKKO-BSART
*        BINARY SEARCH.
*
*        CLEAR: WA_ITENS.
*        WA_ITENS-TIPO_MOVIMENTO  = 'Z41'.
*        WA_ITENS-TAX_CODE        = 'ZE'.
*        WA_ITENS-MATERIAL        = WA_ITEM-MATNR.
*        WA_ITENS-LOCAL_EXPEDICAO = ME->NOTA-F_TOMADORA.
*        WA_ITENS-FORNECEDOR      = ME->NOTA-F_ARMAZEM.
*        IF WA_ITEM-LGORT IS NOT INITIAL.
*          WA_ITENS-DEPOSITO      = WA_ITEM-LGORT.
*        ELSE.
*          WA_ITENS-DEPOSITO      = WA_EKPO-LGORT.
*        ENDIF.
*
*        ZCL_PEDIDO_COMPRA=>GET_PEDIDO_ITENS(
*          EXPORTING
*            I_EBELN  = WA_ITEM-EBELN
*            I_EBELP  = WA_ITEM-EBELP
*          IMPORTING
*            E_EKPO_T = DATA(E_EKPO_T) ).
*
*        READ TABLE E_EKPO_T INTO DATA(WA_EKPO_T) INDEX 1.
*
*        READ TABLE IT_MARA INTO DATA(WA_MARA) WITH KEY MATNR = WA_ITEM-MATNR BINARY SEARCH.
*
*        IF WA_EKET-CHARG IS NOT INITIAL.
*          WA_ITENS-LOTE           = WA_EKET-CHARG.
*          WA_ITENS-PESO           = WA_ITEM-MENGE.
*          WA_ITENS-UNIDADE        = WA_ITEM-MEINS.
*          WA_ITENS-EXT_BASE_AMONT = WA_ITEM-PROD_VLR_TOTAL_B.
*          APPEND WA_ITENS TO I_ITENS.
*        ELSEIF ME->LOTES IS NOT INITIAL.
*          LOOP AT ME->LOTES INTO DATA(WA_LOTE) WHERE PROD_ITEM EQ WA_ITEM-PROD_ITEM.
*            WA_ITENS-LOTE           = WA_LOTE-CHARG.
*            WA_ITENS-PESO           = WA_LOTE-MENGE.
*            WA_ITENS-UNIDADE        = WA_ITEM-MEINS.
*            WA_ITENS-EXT_BASE_AMONT = ( WA_LOTE-MENGE * WA_ITEM-PROD_VLR_TOTAL_B ) / WA_ITEM-MENGE.
*            APPEND WA_ITENS TO I_ITENS.
*          ENDLOOP.
*        ELSEIF WA_MARA-XCHPF EQ ABAP_FALSE OR WA_EKPO_T-KNTTP IS NOT INITIAL.
*          WA_ITENS-PESO     = WA_ITEM-MENGE.
*          WA_ITENS-UNIDADE  = WA_ITEM-MEINS.
*          WA_ITENS-EXT_BASE_AMONT = WA_ITEM-PROD_VLR_TOTAL_B.
*          APPEND WA_ITENS TO I_ITENS.
*        ENDIF.
*
*      ENDLOOP.
*
*      DATA: LC_MIGO TYPE REF TO ZCL_MIGO.
*
*      CREATE OBJECT LC_MIGO.
*
*      CALL METHOD LC_MIGO->CRIAR
*        EXPORTING
*          I_CABECALHO = I_CABECALHO
*          I_ITENS     = I_ITENS
*          I_BAPI_WAIT = ABAP_TRUE
*        IMPORTING
*          E_RETORNO   = DATA(E_RETORNO)
*          E_J_1BNFDOC = DATA(E_J_1BNFDOC)
*          MAT_DOC     = DATA(MAT_DOC)
*          DOC_YEAR    = DATA(DOC_YEAR)
*        RECEIVING
*          R_GEROU     = DATA(R_GEROU_MIGO).
*
*      IF R_GEROU_MIGO EQ ABAP_TRUE.
*        ME->NOTA-MBLNR_ARM = MAT_DOC.
*        ME->NOTA-MJAHR_ARM = DOC_YEAR.
*        R_GEROU = ABAP_TRUE.
*      ENDIF.
*
*      CLEAR: LC_MIGO.
*
*    ELSE.
*      ME->NOTA-MBLNR_ARM = WA_MKPF-MBLNR.
*      ME->NOTA-MJAHR_ARM = WA_MKPF-MJAHR.
*      R_GEROU = ABAP_TRUE.
*    ENDIF.



*    ME->NFE_ARM_ATUALIZA_CFOP_FISCAL( ).
*
*    "Continua de Deve ser Enviado Aceite de Operação ai Armazem com EDIR
*    CHECK CK_ENVIA_ACEITE EQ ABAP_TRUE.
*    ME->SET_ARM_ENVIAR_ACEITE_OPERACAO( ).

  ENDMETHOD.


  METHOD NFE_INBOUND_VISUALIZAR.

    CALL FUNCTION 'ZNFE_INBOUND_VIEW'
      EXPORTING
        I_NOTA = ME.

  ENDMETHOD.


  METHOD refresh.

    DATA(vl_cd_chave_cte) = me->cte-cd_chave_cte.

    me->limpar_registro( ).

    SELECT SINGLE * INTO me->cte FROM zib_cte_dist_ter WHERE cd_chave_cte EQ vl_cd_chave_cte.
    me->st_fiscal_anterior = me->cte-st_fiscal.

*    SELECT * INTO TABLE @DATA(it_itens) FROM zib_nfe_dist_itm WHERE chave_nfe EQ @me->nota-chave_nfe.
*    MOVE it_itens[] TO me->itens.

    SELECT SINGLE * INTO @me->frete FROM zib_nfe_dist_frt WHERE chave_nfe EQ @me->nota-chave_nfe.

    SELECT * INTO TABLE @DATA(it_pedi) FROM zib_nfe_dist_ped WHERE chave_nfe EQ @me->nota-chave_nfe.
    MOVE it_pedi[] TO me->pedidos.

    SELECT * INTO TABLE @DATA(it_lotes) FROM zib_nfe_dist_lot WHERE chave_nfe EQ @me->nota-chave_nfe.
    MOVE it_lotes[] TO me->lotes.

    IF it_lotes IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_caract) FROM zib_nfe_dist_lca FOR ALL ENTRIES IN @it_lotes WHERE cd_lote_item EQ @it_lotes-cd_lote_item.
      MOVE it_caract[] TO me->lotes_caracteristicas.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_logs) FROM zib_nfe_dist_log WHERE chave_nfe EQ @me->nota-chave_nfe.
    MOVE it_logs[] TO me->logs.

    SELECT * INTO TABLE @DATA(it_vol_trans) FROM zib_nfe_dist_tvo WHERE chave_nfe EQ @me->nota-chave_nfe.
    MOVE it_vol_trans[] TO me->volumes_transp.

    SELECT * INTO TABLE @me->dados_retorno FROM zib_nfe_dist_ret WHERE chave_nfe EQ @me->nota-chave_nfe.

*    IF me->cte-st_documento EQ zcl_cte_inbound=>st_documento_00.
*      me->nota-land1 = 'BR'.

*      LOOP AT it_itens ASSIGNING FIELD-SYMBOL(<wa_itens>).
*
*        IF <wa_itens>-icms_cst IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <wa_itens>-icms_cst
*            IMPORTING
*              output = <wa_itens>-icms_cst.
*        ENDIF.
*
*        IF <wa_itens>-ipi_cst IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <wa_itens>-ipi_cst
*            IMPORTING
*              output = <wa_itens>-ipi_cst.
*        ENDIF.
*
*        IF <wa_itens>-pis_cst IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <wa_itens>-pis_cst
*            IMPORTING
*              output = <wa_itens>-pis_cst.
*        ENDIF.
*
*        IF <wa_itens>-cof_cst IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <wa_itens>-cof_cst
*            IMPORTING
*              output = <wa_itens>-cof_cst.
*        ENDIF.
*
*      ENDLOOP.

*    ENDIF.

  ENDMETHOD.


  METHOD SET_ACEITAR_DOCUMENTO.

    IF sy-tcode EQ 'ZMM0110' OR sy-tcode EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '06'
                                        ID 'ZNFETERMEP' FIELD me->nota-e_tomadora
                                        ID 'ZNFETERFIL' FIELD me->nota-f_tomadora
                                        ID 'ZNFETERDEP' FIELD me->nota-cd_departamento.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
                              attr1 = '06'
                              attr2 = CONV #( me->nota-e_tomadora )
                              attr3 = CONV #( me->nota-f_tomadora )
                              attr4 = CONV #( me->nota-cd_departamento ) )
            msgid  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
            msgv1  = '06'
            msgv2  = CONV #( me->nota-e_tomadora )
            msgv3  = CONV #( me->nota-f_tomadora )
            msgv4  = CONV #( me->nota-cd_departamento ).
      ENDIF.
    ENDIF.

    DATA(ck_retorno) = me->get_ck_cfop_retorno_armazena( ).

    "Validações do Aceite
    IF me->pedidos[] IS INITIAL AND ck_retorno EQ abap_false.

      "Verificar se campos obrigatórios foram preenchidos
      LOOP AT me->itens ASSIGNING FIELD-SYMBOL(<fs_item>).
        IF <fs_item>-ebeln IS INITIAL OR <fs_item>-ebelp IS INITIAL OR
           <fs_item>-matnr IS INITIAL OR <fs_item>-meins IS INITIAL OR
           <fs_item>-menge IS INITIAL OR me->nota-cd_departamento IS INITIAL.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_campos_obrig-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_campos_obrig-msgid
                                attr1 = CONV #( TEXT-001 ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_campos_obrig-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_campos_obrig-msgno
              msgv1  = CONV #( TEXT-001 ).
        ELSE.
          IF sy-tabix EQ 1.
            me->set_nr_pedido_compra( i_ebeln = <fs_item>-ebeln ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      "Verificar/Determinar o IVA para a Entrada da Mercadoria""""""""""""""""""""""""""
      "DATA(LC_IVA) = ME->GET_IVA_PARAMETRIZADO_NFE( ).
      "ME->SET_IVA( I_MWSKZ = LC_IVA ).
      "Verificar IVA do Pedido """""""""""""""""""""""""""""""""""""""""""""""""""""""""
      SELECT *
        INTO TABLE @DATA(it_ekpo)
        FROM ekpo
         FOR ALL ENTRIES IN @me->itens
       WHERE ebeln EQ @me->itens-ebeln
         AND ebelp EQ @me->itens-ebelp.

      SORT it_ekpo BY ebeln ebelp.

      LOOP AT me->itens INTO DATA(wa_item).
        READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH KEY  ebeln = wa_item-ebeln ebelp = wa_item-ebelp.
        IF ( sy-subrc IS INITIAL ) AND ( wa_ekpo-mwskz NE me->nota-mwskz ).
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_iva_errado_pedido-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_iva_errado_pedido-msgno
                                attr1 = CONV #( wa_item-ebelp )
                                attr2 = CONV #( wa_item-ebeln )
                                attr3 = CONV #( me->nota-mwskz )        )
              msgid  = zcx_nfe_inbound_exception=>zcx_iva_errado_pedido-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_iva_errado_pedido-msgno
              msgv1  = CONV #( wa_item-ebelp )
              msgv2  = CONV #( wa_item-ebeln )
              msgv3  = CONV #( me->nota-mwskz ).
        ENDIF.
      ENDLOOP.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.

    IF me->nota-ck_possui_frete = abap_true AND me->nota-f_transporte IS INITIAL.
      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_transp_obrigatorio-msgid
                            msgno = zcx_nfe_inbound_exception=>zcx_transp_obrigatorio-msgno )
          msgid  = zcx_nfe_inbound_exception=>zcx_transp_obrigatorio-msgid
          msgno  = zcx_nfe_inbound_exception=>zcx_transp_obrigatorio-msgno.
    ENDIF.

    IF me->get_obrigatorio_nr_fase( ) = abap_true AND
       me->nota-nr_fase IS INITIAL AND
       me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_fatura.
      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_fase_obrigatorio-msgid
                            msgno = zcx_nfe_inbound_exception=>zcx_fase_obrigatorio-msgno )
          msgid  = zcx_nfe_inbound_exception=>zcx_fase_obrigatorio-msgid
          msgno  = zcx_nfe_inbound_exception=>zcx_fase_obrigatorio-msgno.
    ENDIF.


    IF ( me->nota-ck_trans_nf_propri EQ abap_true ).
      IF me->nota-f_armazem IS INITIAL.
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_informar_armazem-msgid
                              msgno = zcx_nfe_inbound_exception=>zcx_informar_armazem-msgno )
            msgid  = zcx_nfe_inbound_exception=>zcx_informar_armazem-msgid
            msgno  = zcx_nfe_inbound_exception=>zcx_informar_armazem-msgno.
      ENDIF.
    ENDIF.

    IF lc_sequencia IS INITIAL.
      lc_sequencia = me->get_sequencia_log( ).
    ENDIF.

    me->ck_aceite_fiscal = abap_true.

  ENDMETHOD.


  METHOD SET_ACEITAR_FATURAR.

    IF me->nota-tp_compra_futura IS INITIAL OR me->nota-tp_compra_futura EQ zcl_nfe_inbound=>tp_compra_futura_fatura.

      IF sy-tcode EQ 'ZMM0110' OR sy-tcode EQ 'ZMM0116'.
        AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '08'
                                          ID 'ZNFETERMEP' FIELD me->nota-e_tomadora
                                          ID 'ZNFETERFIL' FIELD me->nota-f_tomadora
                                          ID 'ZNFETERDEP' FIELD me->nota-cd_departamento.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
                                msgno = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
                                attr1 = '08'
                                attr2 = CONV #( me->nota-e_tomadora )
                                attr3 = CONV #( me->nota-f_tomadora )
                                attr4 = CONV #( me->nota-cd_departamento ) )
              msgid  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgid
              msgno  = zcx_nfe_inbound_exception=>zcx_permissao_acesso-msgno
              msgv1  = '08'
              msgv2  = CONV #( me->nota-e_tomadora )
              msgv3  = CONV #( me->nota-f_tomadora )
              msgv4  = CONV #( me->nota-cd_departamento ).
        ENDIF.
      ENDIF.

      IF lc_sequencia IS INITIAL.
        lc_sequencia = me->get_sequencia_log( ).
      ENDIF.

      me->nota-land1   = 'BR'.
      me->nota-us_miro = i_us_miro.
      me->ck_aceite_faturar         = abap_true.
      me->ck_somente_validar_fatura = i_ck_somente_validar.
      me->ck_alterou                = abap_true.
      me->ck_retorno_sem_ajuste     = i_ck_retorno_sem_ajuste.

    ENDIF.

  ENDMETHOD.


  METHOD SET_ACEITAR_FISICO.

    IF ME->NOTA-TP_COMPRA_FUTURA IS INITIAL OR ME->NOTA-TP_COMPRA_FUTURA EQ ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA.

      IF SY-TCODE EQ 'ZMM0110' OR SY-TCODE EQ 'ZMM0116'.
        AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '07'
                                          ID 'ZNFETERMEP' FIELD ME->NOTA-E_TOMADORA
                                          ID 'ZNFETERFIL' FIELD ME->NOTA-F_TOMADORA
                                          ID 'ZNFETERDEP' FIELD ME->NOTA-CD_DEPARTAMENTO.

        IF SY-SUBRC IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
                                MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
                                ATTR1 = '07'
                                ATTR2 = CONV #( ME->NOTA-E_TOMADORA )
                                ATTR3 = CONV #( ME->NOTA-F_TOMADORA )
                                ATTR4 = CONV #( ME->NOTA-CD_DEPARTAMENTO ) )
              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
              MSGV1  = '07'
              MSGV2  = CONV #( ME->NOTA-E_TOMADORA )
              MSGV3  = CONV #( ME->NOTA-F_TOMADORA )
              MSGV4  = CONV #( ME->NOTA-CD_DEPARTAMENTO ).
        ENDIF.
      ENDIF.

      ME->CK_ACEITE_FISICO       = ABAP_TRUE.
      ME->CK_ALTEROU             = ABAP_TRUE.
      ME->CK_GERAR_SOMENTE_AVISO = ABAP_FALSE.
      ME->CK_PRECO_NOTA_FISCAL   = I_PRECO_NOTA_FISCAL.

      IF ME->NOTA-F_ARMAZEM IS NOT INITIAL.

        "Verifica se Armazem Tem troca de Arquivo.
        DATA(R_ARMAZENS) = DEPARTAMENTO->GET_ARMAZENS( ).
        READ TABLE R_ARMAZENS INTO DATA(WA_ARMAZEM) WITH KEY LIFNR = ME->NOTA-F_ARMAZEM.

        IF SY-SUBRC IS INITIAL.
          "EDI """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          ME->SET_ST_ARMAZEM( EXPORTING I_ST_ARMAZEM =  ME->ST_ARMAZENAGEM_01 ).
        ELSE.
          "Gerar Romaneio de Saida """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          ME->SET_ST_ARMAZEM( EXPORTING I_ST_ARMAZEM =  ME->ST_ARMAZENAGEM_05 ).
        ENDIF.
      ELSE.
        ME->SET_ST_ARMAZEM( EXPORTING I_ST_ARMAZEM =  ME->ST_ARMAZENAGEM_98 ).
      ENDIF.

      CLEAR: DEPARTAMENTO.


    ENDIF.

  ENDMETHOD.


  METHOD SET_ADD_LOG_NFE.

    DATA: WA_LOG     TYPE ZIB_NFE_DIST_LOG,
          LC_MESSAGE TYPE BAPI_MSG.

    MOVE: I_MESSAGE_V1 TO WA_LOG-MESSAGE_V1,
          I_MESSAGE_V2 TO WA_LOG-MESSAGE_V2,
          I_MESSAGE_V3 TO WA_LOG-MESSAGE_V3,
          I_MESSAGE_V4 TO WA_LOG-MESSAGE_V4.

    WA_LOG-CHAVE_NFE      = ME->NOTA-CHAVE_NFE.
    WA_LOG-DT_ATUALIZACAO = I_DT_ATUALIZACAO.
    WA_LOG-HR_ATUALIZACAO = I_HR_ATUALIZACAO.
    WA_LOG-NR_SEQUENCIA   = P_LC_SEQUENCIA.
    WA_LOG-TYPE           = I_TYPE.
    WA_LOG-ID             = I_ID.
    WA_LOG-NUM            = I_NUM.
    WA_LOG-MESSAGE_V1     = I_MESSAGE_V1.
    WA_LOG-MESSAGE_V2     = I_MESSAGE_V2.
    WA_LOG-MESSAGE_V3     = I_MESSAGE_V3.
    WA_LOG-MESSAGE_V4     = I_MESSAGE_V4.
    WA_LOG-BNAME          = SY-UNAME.
    WA_LOG-CK_ESTRATEGIA  = I_ESTRATEGIA.
    WA_LOG-TRANSACAO      = I_TRANSACAO.
    WA_LOG-CD_APROVACAO   = I_CD_APROVACAO.

    IF WA_LOG-DT_ATUALIZACAO IS INITIAL.
      WA_LOG-DT_ATUALIZACAO = SY-DATUM.
    ENDIF.
    IF WA_LOG-HR_ATUALIZACAO IS INITIAL.
      WA_LOG-HR_ATUALIZACAO = SY-UZEIT.
    ENDIF.


    IF I_ID IS NOT INITIAL AND I_TYPE IS NOT INITIAL.

      MESSAGE ID I_ID
            TYPE I_TYPE
          NUMBER I_NUM
            INTO WA_LOG-MESSAGE
            WITH WA_LOG-MESSAGE_V1
                 WA_LOG-MESSAGE_V2
                 WA_LOG-MESSAGE_V3
                 WA_LOG-MESSAGE_V4.

      APPEND WA_LOG TO ME->LOGS.

      ADD 1 TO P_LC_SEQUENCIA.
    ENDIF.

  ENDMETHOD.


  METHOD SET_ADD_LOG_NFE_ERRO.

    DATA: WA_LOG     TYPE ZIB_NFE_DIST_LOG,
          LC_MESSAGE TYPE BAPI_MSG.

    MOVE: I_ERRO->IF_T100_MESSAGE~T100KEY-ATTR1 TO WA_LOG-MESSAGE_V1,
          I_ERRO->IF_T100_MESSAGE~T100KEY-ATTR2 TO WA_LOG-MESSAGE_V2,
          I_ERRO->IF_T100_MESSAGE~T100KEY-ATTR3 TO WA_LOG-MESSAGE_V3,
          I_ERRO->IF_T100_MESSAGE~T100KEY-ATTR4 TO WA_LOG-MESSAGE_V4.

    WA_LOG-CHAVE_NFE      = ME->NOTA-CHAVE_NFE.
    WA_LOG-DT_ATUALIZACAO = SY-DATUM.
    WA_LOG-HR_ATUALIZACAO = SY-UZEIT.
    WA_LOG-NR_SEQUENCIA   = ME->LC_SEQUENCIA.
    WA_LOG-TYPE           = I_ERRO->MSGTY.  " 'E'.
    WA_LOG-ID             = I_ERRO->IF_T100_MESSAGE~T100KEY-MSGID.
    WA_LOG-NUM            = I_ERRO->IF_T100_MESSAGE~T100KEY-MSGNO.
    WA_LOG-BNAME          = SY-UNAME.
    WA_LOG-TRANSACAO      = I_ERRO->TRANSACAO.

    MESSAGE ID I_ERRO->IF_T100_MESSAGE~T100KEY-MSGID
          TYPE I_ERRO->MSGTY
        NUMBER I_ERRO->IF_T100_MESSAGE~T100KEY-MSGNO
          INTO WA_LOG-MESSAGE
          WITH WA_LOG-MESSAGE_V1
               WA_LOG-MESSAGE_V2
               WA_LOG-MESSAGE_V3
               WA_LOG-MESSAGE_V4.

    APPEND WA_LOG TO ME->LOGS.

    ADD 1 TO ME->LC_SEQUENCIA.

  ENDMETHOD.


  METHOD SET_ALTEROU_PEDIDO_COMPRA.

    CLEAR: ME->NOTA-CTR_WAERS,
           ME->NOTA-CTR_WKURS,
           ME->NOTA-CTR_KUFIX,
           ME->NOTA-CTR_SINAL,
           ME->NOTA-CTR_VALOR_TOTAL,
           ME->NOTA-CTR_ZTERM.

    DATA(R_ZMMT0075) = ME->GET_CONFIG_TIPO_PEDIDO( ).

    ME->GET_VALOR_NOTA_FISCAL_FATURA(
      IMPORTING
        E_WAERS       = ME->NOTA-CTR_WAERS
        E_WKURS       = ME->NOTA-CTR_WKURS
        E_KUFIX       = ME->NOTA-CTR_KUFIX
        E_SINAL       = ME->NOTA-CTR_SINAL
        E_VALOR_TOTAL = ME->NOTA-CTR_VALOR_TOTAL
        E_ZTERM       = ME->NOTA-CTR_ZTERM ).

    ME->SET_CTR_VALOR_TOTAL( I_CTR_VALOR_TOTAL = ME->NOTA-CTR_VALOR_TOTAL ).
    ME->SET_BLOQUEIO_PAGAMENTO( I_ZLSPR = R_ZMMT0075-ZLSPR ).

    CK_ALTEROU_PEDIDO_COMPRA = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_ARMAZEM.
    IF ME->NOTA-F_ARMAZEM NE I_F_ARMAZEM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    R_LFA1 = ME->GET_INFO_FORNECEDOR( I_LIFNR = I_F_ARMAZEM ).
    ME->NOTA-F_ARMAZEM     = R_LFA1-LIFNR.
    ME->NOTA-ARMAZEM_CNPJ  = R_LFA1-STCD1.
    ME->NOTA-ARMAZEM_IE    = R_LFA1-STCD3.
    ME->NOTA-ARMAZEM_RAZAO = R_LFA1-NAME1.
  ENDMETHOD.


  METHOD SET_ARM_ENVIAR_ACEITE_OPERACAO.

    CASE I_CANCELAR.
      WHEN ABAP_TRUE.
      WHEN ABAP_FALSE.
    ENDCASE.

  ENDMETHOD.


  METHOD SET_ARM_ENVIO_ACEITE.

    CHECK ME->cte-CK_ARMAZEM EQ ABAP_TRUE.

    IF ME->cte-ST_ARMAZEM EQ SPACE.
      ME->cte-ST_ARMAZEM = ME->ST_ARMAZENAGEM_00.
    ENDIF.

    "CHECK ME->cte-CK_TRANS_NF_PROPRI EQ ABAP_FALSE.

    DATA(CK_EDI) = ME->GET_CK_POSSUI_EDI_ARMAZENAGEM( ).

    "Enviar Informações para EDI
    IF CK_EDI NE ABAP_TRUE.
      "Status de Recebido Físico, pois não existe essa confirmação
      ME->CTE-ST_ARMAZEM = ME->ST_ARMAZENAGEM_04.
    ENDIF.

    CHECK CK_EDI EQ ABAP_TRUE.

    "Marcado para Envio
    ME->NOTA-ST_ARMAZEM = ME->ST_ARMAZENAGEM_01.

    "Se for ser enviado de forma Sincrona, colocar aqui"""""""""""""""""""""""""""""""""""""""""
    ME->SET_ARM_ENVIAR_ACEITE_OPERACAO( ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.


  METHOD SET_AUT_EMBARQUE.

    IF me->nota-aut_embarque NE i_aut_embarque.
      me->ck_alterou = abap_true.
    ENDIF.

    me->nota-aut_embarque = i_aut_embarque.

  ENDMETHOD.


  METHOD SET_BANCO_EMPRESA.

    IF ME->NOTA-HOUSEBANKID NE I_BANCO_EMPRESA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-HOUSEBANKID = I_BANCO_EMPRESA.

  ENDMETHOD.


  METHOD SET_BANCO_PARCEIRO.

    IF ME->NOTA-ZBVTYP NE I_BVTYP.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ZBVTYP = I_BVTYP.

  ENDMETHOD.


  METHOD SET_BLOQUEIO_PAGAMENTO.

    IF ME->NOTA-ZLSPR NE I_ZLSPR.
      ME->CK_ALTEROU = ABAP_TRUE.
      ME->CK_ALTEROU_BLOQUEIO_PAGA = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ZLSPR = I_ZLSPR.

  ENDMETHOD.


  METHOD SET_BOLETO.

    IF I_BOLETO NE ME->NOTA-BOLETO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-BOLETO = I_BOLETO.

  ENDMETHOD.


  METHOD SET_CALC_TOTAL_MOEDA_EMPRESA.

    DATA: wa_bases TYPE zde_nfe_dist_itm_preco_ped.

    TYPES BEGIN OF ty_base_calculo.
    TYPES: valor_moeda_outra TYPE zib_nfe_dist_ter-vl_total.
    TYPES: valor_moeda_outra_liq TYPE zib_nfe_dist_ter-vl_total.
    TYPES END OF ty_base_calculo.

    DATA: valor TYPE ty_base_calculo.
    DATA: soma TYPE ty_base_calculo.

    CLEAR: e_valores_pedido, e_achou.

    SELECT SINGLE *
      INTO @DATA(_zmmt0149)
      FROM zmmt0149
     WHERE chave_nfe EQ @me->nota-chave_nfe
     AND   status EQ 'A'.

    IF sy-subrc = 0.
      EXIT.
    ENDIF.

    DATA(it_itens) = me->itens[].
    DELETE it_itens WHERE ebelp EQ space.
    DELETE it_itens WHERE ebeln EQ space.

    CHECK it_itens[] IS NOT INITIAL.

    "Item do Pedido de Compra
    SELECT * INTO TABLE @DATA(it_ekpo)
      FROM ekpo
       FOR ALL ENTRIES IN @it_itens
     WHERE ebeln EQ @it_itens-ebeln
       AND ebelp EQ @it_itens-ebelp.

    CHECK sy-subrc IS INITIAL.

    "Pedidos de Compra
    SELECT * INTO TABLE @DATA(it_ekko)
      FROM ekko
       FOR ALL ENTRIES IN @it_ekpo
     WHERE ebeln EQ @it_ekpo-ebeln.

    CHECK sy-subrc IS INITIAL.

    DATA(lc_moeda_interna) = zcl_empresa=>get_instance( )->set_empresa( me->nota-e_tomadora )->get_moeda_interna( ).

    "Deixar Somente Pedidos Em Outra Moeda
    DELETE it_ekko WHERE waers EQ lc_moeda_interna.

    "Verificar se Ficou algum pedido
    CHECK it_ekko[] IS NOT INITIAL.

    e_valores_pedido-ctr_valor_total = 0.

    READ TABLE it_ekko INDEX 1 INTO DATA(wa_ekko).
    "CTR_WAERS  Moeda do Pedido
    e_valores_pedido-ctr_waers = wa_ekko-waers.
    "CTR_KUFIX  Código: fixação de taxa de câmbio
    e_valores_pedido-ctr_kufix = wa_ekko-kufix.
    "CTR_ZTERM  Chave de condições de pagamento
    e_valores_pedido-ctr_zterm = wa_ekko-zterm.

    "Busca Histórico de Solicitação de Pedido de Compra
    SELECT nro_sol_cp, ebeln
      INTO TABLE @DATA(it_zmmt0035)
      FROM zmmt0035
       FOR ALL ENTRIES IN @it_ekko
     WHERE ebeln EQ @it_ekko-ebeln.

    IF sy-subrc IS INITIAL.
      SELECT nro_sol_cp, ebelp, brtwr, netpr, bicms, picms INTO TABLE @DATA(it_zmmt0037)
        FROM zmmt0037
         FOR ALL ENTRIES IN @it_zmmt0035
       WHERE nro_sol_cp EQ @it_zmmt0035-nro_sol_cp.
    ENDIF.

    SORT it_zmmt0035 BY ebeln.
    SORT it_zmmt0037 BY nro_sol_cp ebelp.

    "Percorrer Pedidos de Compra em Moeda Extrangeira
    LOOP AT it_ekko INTO wa_ekko.
      LOOP AT it_ekpo INTO DATA(wa_ekpo) WHERE ebeln EQ wa_ekko-ebeln.
        LOOP AT me->itens INTO DATA(wa_item)
          WHERE ebeln EQ wa_ekpo-ebeln
            AND ebelp EQ wa_ekpo-ebelp.

          CLEAR: wa_bases.

          DATA(lc_achou) = abap_false.

          IF wa_item-brtwr IS NOT INITIAL.
            lc_achou = abap_true.
            e_achou  = abap_true.

            wa_bases-brtwr   = wa_item-brtwr.
            wa_bases-bcofins = wa_item-bcofins.
            wa_bases-bicms   = wa_item-bicms.
            wa_bases-bpis    = wa_item-bpis.
            wa_bases-pcofins = wa_item-pcofins.
            wa_bases-picms   = wa_item-picms.
            wa_bases-ppis    = wa_item-ppis.
            wa_bases-liquido = ( ( wa_bases-brtwr -
                                    (
                                      ( ( wa_bases-brtwr * ( wa_bases-bicms   / 100 ) ) * ( wa_bases-picms   / 100 ) )  +
                                      ( ( wa_bases-brtwr * ( wa_bases-bpis    / 100 ) ) * ( wa_bases-ppis    / 100 ) )  +
                                      ( ( wa_bases-brtwr * ( wa_bases-bcofins / 100 ) ) * ( wa_bases-pcofins / 100 ) )
                                    )
                                  ) / wa_ekpo-peinh ) * ( wa_item-menge / wa_ekpo-bpumn ).
          ELSE.

            "Procurar Preço
            READ TABLE it_zmmt0035 INTO DATA(wa_zmmt0035) WITH KEY ebeln = wa_item-ebeln BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              READ TABLE it_zmmt0037 INTO DATA(wa_zmmt0037) WITH KEY nro_sol_cp = wa_zmmt0035-nro_sol_cp ebelp = wa_ekpo-ebelp
              BINARY SEARCH.
              IF ( sy-subrc IS INITIAL ) AND ( wa_zmmt0037-brtwr IS NOT INITIAL ).
                lc_achou = abap_true.
                e_achou  = abap_true.

                wa_bases-brtwr   = wa_zmmt0037-brtwr.
                "WA_BASES-BCOFINS = WA_ZMMT0037-BCOFINS.
                wa_bases-bicms   = wa_zmmt0037-bicms.
                "WA_BASES-BPIS    = WA_ZMMT0037-BPIS.
                "WA_BASES-PCOFINS = WA_ZMMT0037-PCOFINS.
                wa_bases-picms   = wa_zmmt0037-picms.
                "WA_BASES-PPIS    = WA_ZMMT0037-PPIS.

              ENDIF.
            ELSE.
              wa_bases-brtwr = wa_ekpo-netpr.
            ENDIF.

          ENDIF.

          CASE lc_achou.
            WHEN abap_true.
              "Soma valores em dólar com base na quantdade informada
              soma-valor_moeda_outra = ( ( wa_bases-brtwr / wa_ekpo-peinh ) * ( wa_item-menge / wa_ekpo-bpumn ) ).
              soma-valor_moeda_outra_liq = ( ( wa_bases-brtwr -
                                              (
                                                ( ( wa_bases-brtwr * ( wa_bases-bicms   / 100 ) ) * ( wa_bases-picms   / 100 ) )  +
                                                ( ( wa_bases-brtwr * ( wa_bases-bpis    / 100 ) ) * ( wa_bases-ppis    / 100 ) )  +
                                                ( ( wa_bases-brtwr * ( wa_bases-bcofins / 100 ) ) * ( wa_bases-pcofins / 100 ) )
                                              )
                                            ) / wa_ekpo-peinh ) * ( wa_item-menge / wa_ekpo-bpumn ).
              ADD soma-valor_moeda_outra TO valor-valor_moeda_outra.
              ADD soma-valor_moeda_outra_liq TO valor-valor_moeda_outra_liq.

            WHEN abap_false.
              "Soma valores em dólar com base na quantdade informada
              soma-valor_moeda_outra = ( ( wa_ekpo-netpr / wa_ekpo-peinh ) * ( wa_item-menge / wa_ekpo-bpumn ) ).
              ADD soma-valor_moeda_outra TO valor-valor_moeda_outra.
              ADD soma-valor_moeda_outra TO valor-valor_moeda_outra_liq.
          ENDCASE.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "CTR_VALOR_TOTAL  Valor Total na Moeda do Pedido
    e_valores_pedido-ctr_valor_total = valor-valor_moeda_outra.

    "CTR_VALOR_TOTAL  Valor Total Líquido na Moeda do Pedido
    e_valores_pedido-ctr_valor_total_liquido = valor-valor_moeda_outra_liq.

    "CTR_WKURS  Taxa de câmbio
    e_valores_pedido-ctr_wkurs = ( me->nota-vl_total / valor-valor_moeda_outra ).

    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
    CREATE OBJECT obj_zcl_util_sd.
    obj_zcl_util_sd->set_data(  EXPORTING i_data  = CONV #( me->nota-dt_emissao ) ).
    obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
    obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = lc_moeda_interna ).
    obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_ekko-waers ).
    DATA(e_ukurs) = obj_zcl_util_sd->taxa_cambio( ).

    e_valores_pedido-ctr_sinal = COND #( LET taxa = e_ukurs IN WHEN taxa LT 0 THEN '/' ELSE '*' ).

    CLEAR: obj_zcl_util_sd.

  ENDMETHOD.


  METHOD SET_CALC_TOTAL_MOEDA_EMP_ITEM.

    DATA: WA_BASES TYPE ZDE_NFE_DIST_ITM_PRECO_PED.

    TYPES BEGIN OF TY_BASE_CALCULO.
    TYPES: VALOR_MOEDA_OUTRA TYPE ZIB_NFE_DIST_TER-VL_TOTAL.
    TYPES: VALOR_MOEDA_OUTRA_LIQ TYPE ZIB_NFE_DIST_TER-VL_TOTAL.
    TYPES END OF TY_BASE_CALCULO.

    DATA: VALOR TYPE TY_BASE_CALCULO.
    DATA: SOMA TYPE TY_BASE_CALCULO.

    CLEAR: E_VALORES_PEDIDO.

    "Item do Pedido de Compra
    SELECT SINGLE * INTO @DATA(WA_EKPO)
      FROM EKPO
     WHERE EBELN EQ @I_ITEM-EBELN
       AND EBELP EQ @I_ITEM-EBELP.

    CHECK SY-SUBRC IS INITIAL.

    "Pedidos de Compra
    SELECT SINGLE * INTO @DATA(WA_EKKO)
      FROM EKKO
     WHERE EBELN EQ @I_ITEM-EBELN.

    CHECK SY-SUBRC IS INITIAL.

    DATA(LC_MOEDA_INTERNA) = ZCL_EMPRESA=>GET_INSTANCE( )->SET_EMPRESA( ME->NOTA-E_TOMADORA )->GET_MOEDA_INTERNA( ).

    E_VALORES_PEDIDO-CTR_VALOR_TOTAL = 0.

    CHECK WA_EKKO-WAERS NE 'BRL'.

    "CTR_WAERS  Moeda do Pedido
    E_VALORES_PEDIDO-CTR_WAERS = WA_EKKO-WAERS.
    "CTR_KUFIX  Código: fixação de taxa de câmbio
    E_VALORES_PEDIDO-CTR_KUFIX = WA_EKKO-KUFIX.
    "CTR_ZTERM  Chave de condições de pagamento
    E_VALORES_PEDIDO-CTR_ZTERM = WA_EKKO-ZTERM.

    "Busca Histórico de Solicitação de Pedido de Compra
    SELECT NRO_SOL_CP, EBELN
      INTO TABLE @DATA(IT_ZMMT0035)
      FROM ZMMT0035
     WHERE EBELN EQ @WA_EKKO-EBELN.

    IF SY-SUBRC IS INITIAL.
      SELECT NRO_SOL_CP, EBELP, BRTWR, NETPR, BICMS, PICMS INTO TABLE @DATA(IT_ZMMT0037)
        FROM ZMMT0037
         FOR ALL ENTRIES IN @IT_ZMMT0035
       WHERE NRO_SOL_CP EQ @IT_ZMMT0035-NRO_SOL_CP.
    ENDIF.
    SORT IT_ZMMT0037 BY NRO_SOL_CP EBELP.

    CLEAR: WA_BASES.

    "Percorrer Pedidos de Compra em Moeda Extrangeira
    DATA(LC_ACHOU) = ABAP_FALSE.

    IF I_ITEM-BRTWR IS NOT INITIAL.

      WA_BASES-BRTWR   = I_ITEM-BRTWR.
      WA_BASES-BCOFINS = I_ITEM-BCOFINS.
      WA_BASES-BICMS   = I_ITEM-BICMS.
      WA_BASES-BPIS    = I_ITEM-BPIS.
      WA_BASES-PCOFINS = I_ITEM-PCOFINS.
      WA_BASES-PICMS   = I_ITEM-PICMS.
      WA_BASES-PPIS    = I_ITEM-PPIS.
      WA_BASES-LIQUIDO = ( ( WA_BASES-BRTWR -
                              (
                                ( ( WA_BASES-BRTWR * ( WA_BASES-BICMS   / 100 ) ) * ( WA_BASES-PICMS   / 100 ) )  +
                                ( ( WA_BASES-BRTWR * ( WA_BASES-BPIS    / 100 ) ) * ( WA_BASES-PPIS    / 100 ) )  +
                                ( ( WA_BASES-BRTWR * ( WA_BASES-BCOFINS / 100 ) ) * ( WA_BASES-PCOFINS / 100 ) )
                              )
                            ) / WA_EKPO-PEINH ) * ( I_ITEM-MENGE / WA_EKPO-BPUMN ).
      LC_ACHOU = ABAP_TRUE.
    ELSE.
*---> 05/07/2023 - Migração S4 - DL
    SORT IT_ZMMT0035 BY EBELN.
*<--- 05/07/2023 - Migração S4 - DL
      "Procurar Preço
      READ TABLE IT_ZMMT0035 INTO DATA(WA_ZMMT0035) WITH KEY EBELN = I_ITEM-EBELN BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_ZMMT0037 INTO DATA(WA_ZMMT0037) WITH KEY NRO_SOL_CP = WA_ZMMT0035-NRO_SOL_CP EBELP = WA_EKPO-EBELP
        BINARY SEARCH.
        IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZMMT0037-BRTWR IS NOT INITIAL ).
          LC_ACHOU = ABAP_TRUE.

          WA_BASES-BRTWR   = WA_ZMMT0037-BRTWR.
          "WA_BASES-BCOFINS = WA_ZMMT0037-BCOFINS.
          WA_BASES-BICMS   = WA_ZMMT0037-BICMS.
          "WA_BASES-BPIS    = WA_ZMMT0037-BPIS.
          "WA_BASES-PCOFINS = WA_ZMMT0037-PCOFINS.
          WA_BASES-PICMS   = WA_ZMMT0037-PICMS.
          "WA_BASES-PPIS    = WA_ZMMT0037-PPIS.

        ENDIF.
      ENDIF.

    ENDIF.

    CASE LC_ACHOU.
      WHEN ABAP_TRUE.
        "Soma valores em dólar com base na quantdade informada
        SOMA-VALOR_MOEDA_OUTRA = ( ( WA_BASES-BRTWR / WA_EKPO-PEINH ) * ( I_ITEM-MENGE / WA_EKPO-BPUMN ) ).
        SOMA-VALOR_MOEDA_OUTRA_LIQ = ( ( WA_BASES-BRTWR -
                                              (
                                                ( ( WA_BASES-BRTWR * ( WA_BASES-BICMS   / 100 ) ) * ( WA_BASES-PICMS   / 100 ) )  +
                                                ( ( WA_BASES-BRTWR * ( WA_BASES-BPIS    / 100 ) ) * ( WA_BASES-PPIS    / 100 ) )  +
                                                ( ( WA_BASES-BRTWR * ( WA_BASES-BCOFINS / 100 ) ) * ( WA_BASES-PCOFINS / 100 ) )
                                              )
                                            ) / WA_EKPO-PEINH ) * ( I_ITEM-MENGE / WA_EKPO-BPUMN ).

        ADD SOMA-VALOR_MOEDA_OUTRA TO VALOR-VALOR_MOEDA_OUTRA.
        ADD SOMA-VALOR_MOEDA_OUTRA_LIQ TO VALOR-VALOR_MOEDA_OUTRA_LIQ.

      WHEN ABAP_FALSE.
        "Soma valores em dólar com base na quantdade informada
        SOMA-VALOR_MOEDA_OUTRA = ( ( WA_EKPO-NETPR / WA_EKPO-PEINH ) * ( I_ITEM-MENGE / WA_EKPO-BPUMN ) ).
        ADD SOMA-VALOR_MOEDA_OUTRA TO VALOR-VALOR_MOEDA_OUTRA.
        ADD SOMA-VALOR_MOEDA_OUTRA TO VALOR-VALOR_MOEDA_OUTRA_LIQ.
    ENDCASE.

    "CTR_VALOR_TOTAL  Valor Total na Moeda do Pedido
    E_VALORES_PEDIDO-CTR_VALOR_TOTAL = VALOR-VALOR_MOEDA_OUTRA.

    "CTR_VALOR_TOTAL  Valor Total Líquido na Moeda do Pedido
    E_VALORES_PEDIDO-CTR_VALOR_TOTAL_LIQUIDO = VALOR-VALOR_MOEDA_OUTRA_LIQ.

    "CTR_WKURS  Taxa de câmbio
    E_VALORES_PEDIDO-CTR_WKURS = ( I_ITEM-PROD_VLR_TOTAL_B / VALOR-VALOR_MOEDA_OUTRA ).

    DATA: OBJ_ZCL_UTIL_SD TYPE REF TO ZCL_UTIL_SD.
    CREATE OBJECT OBJ_ZCL_UTIL_SD.
    OBJ_ZCL_UTIL_SD->SET_DATA(  EXPORTING I_DATA  = CONV #( ME->NOTA-DT_EMISSAO ) ).
    OBJ_ZCL_UTIL_SD->SET_KURST( EXPORTING I_KURST = 'B' ).
    OBJ_ZCL_UTIL_SD->SET_WAERK( EXPORTING I_WAERK = LC_MOEDA_INTERNA ).
    OBJ_ZCL_UTIL_SD->SET_TCURR( EXPORTING I_TCURR = WA_EKKO-WAERS ).
    DATA(E_UKURS) = OBJ_ZCL_UTIL_SD->TAXA_CAMBIO( ).

    E_VALORES_PEDIDO-CTR_SINAL = COND #( LET TAXA = E_UKURS IN WHEN TAXA LT 0 THEN '/' ELSE '*' ).

    CLEAR: OBJ_ZCL_UTIL_SD.

  ENDMETHOD.


  METHOD SET_CD_ROMANEIO.

    IF ME->NOTA-CD_ROMANEIO NE I_CD_ROMANEIO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-CD_ROMANEIO = I_CD_ROMANEIO.

  ENDMETHOD.


  METHOD SET_CK_ARMAZEM.
    IF ME->NOTA-CK_ARMAZEM NE I_CHECK.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-CK_ARMAZEM = I_CHECK.
  ENDMETHOD.


  METHOD SET_CK_COMPRA_FUTURA.

    IF ME->NOTA-CK_COMPRA_FUTURA NE I_CK_COMPRA_FUTURA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-CK_COMPRA_FUTURA = I_CK_COMPRA_FUTURA.

  ENDMETHOD.


  METHOD SET_CK_FISCAL.
    ME->NOTA-CK_FISCAL = I_CHECK.
*---> CS0979912 - JAMEDICI --->
    ME->NOTA-CK_ARMAZEM = I_CHECK.
*<--- CS0979912 - JAMEDICI <---
  ENDMETHOD.


  METHOD SET_CK_FISICO.
    IF ME->NOTA-CK_FISICO NE I_CHECK.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-CK_FISICO = I_CHECK.
  ENDMETHOD.


  METHOD SET_CK_GERAR_SOMENTE_AVISO.
    ME->CK_GERAR_SOMENTE_AVISO = ABAP_TRUE.
  ENDMETHOD.


  METHOD SET_CK_NAO_ESTORNAR_AVISO.
    ME->CK_NAO_ESTORNAR_AVISO = I_CK_NAO_ESTORNAR_AVISO.
  ENDMETHOD.


  METHOD SET_CK_POSSUI_FRETE.

    IF ME->NOTA-CK_POSSUI_FRETE NE I_CK_POSSUI_FRETE.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-CK_POSSUI_FRETE = I_CK_POSSUI_FRETE.

  ENDMETHOD.


  METHOD SET_CK_RETORNO_SEM_AJUSTE.

    DATA: l_type      TYPE bapi_mtype,
          l_id        TYPE symsgid,
          l_num       TYPE symsgno,
          l_transacao	TYPE tcode.

    l_type      = 'S'.
    l_id        = 'SD'.
    l_num       = '024'.
    l_transacao = 'Motivo Ret.s/Ajuste'.

    CALL METHOD me->set_add_log_nfe
      EXPORTING
        i_type         = l_type
        i_id           = l_id
        i_num          = l_num
        i_transacao    = l_transacao
        i_message_v1   = i_message_v1
        i_message_v2   = i_message_v2
        i_message_v3   = i_message_v3
        i_message_v4   = i_message_v4
      CHANGING
        p_lc_sequencia = lc_sequencia.

*   me->nfe_inbound_gravar_log( ).

    me->ck_aceite_fisico      = abap_true.
    me->nota-tp_compra_futura = abap_false.

  ENDMETHOD.


  METHOD SET_COLETA_TUDO.
    ME->CK_COLETA_TUDO = I_CK_COLETA_TUDO.
  ENDMETHOD.


  METHOD SET_COMPRA_FUTURA.

    DATA: R_ITMTYP TYPE RANGE OF J_1BITMTYP,
          R_CFOP   TYPE RANGE OF J_1BCFOP,
          W_ITMTYP LIKE LINE OF R_ITMTYP,
          W_CFOP   LIKE LINE OF R_CFOP.

    W_ITMTYP-SIGN   = 'I'.
    W_ITMTYP-OPTION = 'EQ'.
    W_ITMTYP-LOW    = '41'.
    W_ITMTYP-HIGH   = '41'.
    APPEND W_ITMTYP TO R_ITMTYP.
    W_ITMTYP-LOW    = '42'.
    W_ITMTYP-HIGH   = '42'.
    APPEND W_ITMTYP TO R_ITMTYP.
    W_ITMTYP-LOW    = '43'.
    W_ITMTYP-HIGH   = '44'.
    APPEND W_ITMTYP TO R_ITMTYP.

    W_CFOP-SIGN   = 'I'.
    W_CFOP-OPTION = 'CP'.
    LOOP AT ME->ITENS INTO DATA(WA_ITEM).
      CONCATENATE '*' WA_ITEM-PROD_CFOP+1(3) '*' INTO W_CFOP-LOW.
      APPEND W_CFOP TO R_CFOP.
    ENDLOOP.

    SELECT * INTO TABLE @DATA(IT_J_1BAON)
      FROM J_1BAON
     WHERE DIRECT  EQ '1'
       AND VERSION EQ '02'
       AND ITMTYP  IN @R_ITMTYP
       AND CFOP    IN @R_CFOP.

    IF SY-SUBRC IS INITIAL.

      ME->SET_CK_COMPRA_FUTURA( I_CK_COMPRA_FUTURA = ABAP_TRUE ).

      READ TABLE IT_J_1BAON INDEX 1 INTO DATA(WA_J_1BAON).

      CASE WA_J_1BAON-ITMTYP.
        WHEN '41' OR '43'.
          ME->SET_TP_COMPRA_FUTURA( I_TP_COMPRA_FUTURA = ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_FATURA ).
          "093  CFOP %&1 marcado como &2 (Faturamento)!
          SY-MSGNO = 093.
        WHEN '42'.
          ME->SET_TP_COMPRA_FUTURA( I_TP_COMPRA_FUTURA = ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA ).
          "092  CFOP %&1 marcado como &2 (Fornecimento)!
          SY-MSGNO = 092.
      ENDCASE.

      SY-MSGTY = 'S'.
      SY-MSGID = 'ZNFE_DISTRI'.
      SY-MSGV1 = CONV #( WA_J_1BAON-CFOP+1(3) ).
      SY-MSGV2 = CONV #( WA_J_1BAON-ITMTYP ).

      CALL METHOD ME->SET_ADD_LOG_NFE
        EXPORTING
          I_TYPE         = SY-MSGTY
          I_ID           = SY-MSGID
          I_NUM          = SY-MSGNO
          I_MESSAGE_V1   = SY-MSGV1
          I_MESSAGE_V2   = SY-MSGV2
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

    ELSE.
      ME->SET_CK_COMPRA_FUTURA( I_CK_COMPRA_FUTURA = ABAP_FALSE ).
      ME->SET_TP_COMPRA_FUTURA( I_TP_COMPRA_FUTURA = SPACE ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_CTR_VALOR_TOTAL.

    DATA: VALOR_INTEIRO TYPE I.

    IF ME->NOTA-CTR_VALOR_TOTAL NE I_CTR_VALOR_TOTAL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-CTR_VALOR_TOTAL = I_CTR_VALOR_TOTAL.

    ME->GET_VALOR_NOTA_FISCAL_FATURA( IMPORTING E_WAERS = DATA(E_WAERS) E_SINAL = DATA(E_SINAL) ).

    IF E_WAERS EQ 'BRL'.
      R_CTR_WKURS = 1.
      ME->SET_CTR_WKURS( I_CTR_WKURS = R_CTR_WKURS ).
    ELSE.
      CASE E_SINAL.
        WHEN '/'.
          VALOR_INTEIRO = ( ME->NOTA-VL_TOTAL / ME->NOTA-CTR_VALOR_TOTAL ) * 10000.
        WHEN '*'.
          VALOR_INTEIRO = ( ME->NOTA-CTR_VALOR_TOTAL / ME->NOTA-VL_TOTAL ) * 10000.
      ENDCASE.
      R_CTR_WKURS = VALOR_INTEIRO / 10000.
      ME->SET_CTR_WKURS( I_CTR_WKURS = R_CTR_WKURS ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_CTR_WKURS.

    IF ME->NOTA-CTR_WKURS NE I_CTR_WKURS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-CTR_WKURS = I_CTR_WKURS.

  ENDMETHOD.


  METHOD SET_DADOS_RETORNO.
    ME->DADOS_RETORNO = I_RETORNO.
    ME->CK_ALTEROU = ABAP_TRUE.
  ENDMETHOD.


  METHOD SET_DENQUEUE_NFE.

    CHECK I_SEM_BLOQUEIO_REGISTRO IS INITIAL.

    CALL FUNCTION 'ZDENQUEUE_NFE_INBOUND'
      EXPORTING
        CHAVE = I_CHAVE.

  ENDMETHOD.


  METHOD SET_DEPARTAMENTO.

    DATA: CK_ALTEROU_DEPARTAMENTO TYPE CHAR01.

    IF SY-TCODE EQ 'ZMM0110' OR SY-TCODE EQ 'ZMM0116'.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '05'
                                        ID 'ZNFETERMEP' FIELD ME->NOTA-E_TOMADORA
                                        ID 'ZNFETERFIL' FIELD ME->NOTA-F_TOMADORA
                                        ID 'ZNFETERDEP' FIELD I_CD_DEPARTAMENTO.

      IF SY-SUBRC IS NOT INITIAL.

        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_OBJETO_AUTORIZACAO-MSGID
                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_OBJETO_AUTORIZACAO-MSGNO
                              ATTR1 = '05'
                              ATTR2 = CONV #( ME->NOTA-E_TOMADORA )
                              ATTR3 = CONV #( ME->NOTA-F_TOMADORA )
                              ATTR4 = CONV #( ME->NOTA-CD_DEPARTAMENTO ) )
            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_OBJETO_AUTORIZACAO-MSGID
            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_OBJETO_AUTORIZACAO-MSGNO
            MSGTY  = 'E'
            MSGV1  = '05'
            MSGV2  = CONV #( ME->NOTA-E_TOMADORA )
            MSGV3  = CONV #( ME->NOTA-F_TOMADORA )
            MSGV4  = CONV #( ME->NOTA-CD_DEPARTAMENTO ).
        "MESSAGE E045 WITH '05' ME->NOTA-E_TOMADORA ME->NOTA-F_TOMADORA I_CD_DEPARTAMENTO RAISING ERRO_PERMISSAO.
      ENDIF.
    ENDIF.

    IF ME->NOTA-CD_DEPARTAMENTO NE I_CD_DEPARTAMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
      CK_ALTEROU_DEPARTAMENTO = ABAP_TRUE.
    ENDIF.

    "Atribuido Departamento
    ME->NOTA-CD_DEPARTAMENTO = I_CD_DEPARTAMENTO.
    ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_DEPARTAMENTO ).

    IF CK_ALTEROU_DEPARTAMENTO EQ ABAP_TRUE.
      ME->SET_ALTEROU_PEDIDO_COMPRA( ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_DEPARTAMENTO_NOTA.

    DATA: CX_NFE_INBOUND TYPE REF TO ZCX_NFE_INBOUND_EXCEPTION.

    IF ME->NOTA-CD_DEPARTAMENTO IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(WA_ZMMT0072)
        FROM ZMMT0072
       WHERE CD_DEPARTAMENTO EQ @ME->NOTA-CD_DEPARTAMENTO.

      IF WA_ZMMT0072-CK_SEM_RET_GRUPO EQ ABAP_TRUE.
        ME->SET_DEPARTAMENTO( ME->NOTA-CD_DEPARTAMENTO ).
        ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_DEPART_APROVADO ).
      ENDIF.
    ENDIF.

    CHECK WA_ZMMT0072-CK_SEM_RET_GRUPO EQ ABAP_FALSE.

    TRY.

        DATA(R_DEPARTAMENTO) = ME->GET_DEPARTAMENTO_ITENS( I_ITENS = ME->ITENS ).

        CALL METHOD ME->SET_ADD_LOG_NFE
          EXPORTING
            I_TYPE         = SY-MSGTY
            I_ID           = SY-MSGID
            I_NUM          = SY-MSGNO
            I_TRANSACAO    = ZCL_NFE_INBOUND=>TRANSACAO_DEPARTAMENTO
            I_MESSAGE_V1   = SY-MSGV1
            I_MESSAGE_V2   = SY-MSGV2
            I_MESSAGE_V3   = SY-MSGV3
            I_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        ME->SET_DEPARTAMENTO( R_DEPARTAMENTO ).

        "Futuro Talvez implementar Aprovação de Departamento
        "Aprovado Departamento
        ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_DEPART_APROVADO ).

      CATCH ZCX_NFE_INBOUND_EXCEPTION INTO CX_NFE_INBOUND.
        ME->SET_ADD_LOG_NFE_ERRO( I_ERRO = CX_NFE_INBOUND ).
    ENDTRY.

  ENDMETHOD.


  METHOD SET_DOCNUM_NFE.
    IF ME->NOTA-DOCNUM_NFE NE I_DOCNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->NOTA-BELNR IS NOT INITIAL AND ME->NOTA-TP_COMPRA_FUTURA NE ZCL_NFE_INBOUND=>TP_COMPRA_FUTURA_MERCADORIA.
      ME->SET_ST_FISICO( I_ST_FISICO = ME->ST_FISICO_99 ).
      ME->SET_CK_FISICO( I_CHECK = ABAP_TRUE ).
    ENDIF.

    ME->NOTA-DOCNUM_NFE = I_DOCNUM.

  ENDMETHOD.


  METHOD SET_DT_VENCIMENTO.

    IF ME->NOTA-DT_VENCIMENTO NE I_DT_VENCIMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-DT_VENCIMENTO = I_DT_VENCIMENTO.

  ENDMETHOD.


  METHOD SET_ENQUEUE_NFE.

    CHECK I_SEM_BLOQUEIO_REGISTRO IS INITIAL.

    CALL FUNCTION 'ZENQUEUE_NFE_INBOUND'
      EXPORTING
        CHAVE          = I_CHAVE
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ZFOREIGN_LOCK.
      WHEN 2.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ZSYSTEM_FAILURE.
      WHEN 3.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
    ENDCASE.

  ENDMETHOD.


  METHOD set_informacoes_existentes.

    "CK_FISCAL  1 Tipo  ZDE_CK_FISCAL CHAR    1 0 Processo Fiscal Iniciado NF-e Inbound
    "CK_FISICO  1 Tipo  ZDE_CK_FISICO CHAR    1 0 Processo Físico Iniciado NF-e Inbound
    "CK_ARMAZEM 1 Tipo  ZDE_CK_ARMAZEM  CHAR  1 0 Processo Armazenagem Iniciado NF-e Inbound

    DATA: wa_aviso TYPE likp,
          wa_vttk  TYPE vttk,
          wa_vfkp  TYPE vfkp,
          wa_rbkp  TYPE rbkp,
          it_rseg	 TYPE j_1b_tt_rseg,
          lc_xblnr TYPE xblnr1,
          l_serie  TYPE c LENGTH 3.

*   Pendente
*00	Pendente
*01	Atribuido Departamento
*02	Aprovado Departamento
*03	Aviso Recebimento Gerado
*04	Documento de Transporte (VT) Gerado
*05	Documento de Custo (VI) Gerado
*97	Não Aceito Fiscal
*98	Finalizado Sem Aceite Fiscal
*99	Finalizado Com Aceite Fiscal

    CALL METHOD zcl_fiscal=>get_documento_chave
      EXPORTING
        i_chave   = me->cte-cd_chave_cte
        i_propria = abap_false
      RECEIVING
        r_docnum  = DATA(docnum_cte)
      EXCEPTIONS
        erro      = 1
        OTHERS    = 2.

    IF docnum_cte IS INITIAL AND me->cte-mblnr IS NOT INITIAL.

      DATA: lc_ref TYPE j_1brefkey.

      lc_ref = me->cte-mblnr && me->cte-mjahr.

      SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
        FROM j_1bnflin
       WHERE reftyp EQ 'MD'
         AND refkey EQ @lc_ref.

      IF sy-subrc IS INITIAL.
        docnum_cte = wa_j_1bnflin-docnum.
      ENDIF.

    ENDIF.

    IF me->cte-docnum_cte IS NOT INITIAL.
      CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
        EXPORTING
          i_docnum = me->cte-docnum_cte.

      CALL METHOD zcl_fiscal=>get_documento_chave
        EXPORTING
          i_chave   = me->cte-cd_chave_cte
          i_propria = abap_false
        RECEIVING
          r_docnum  = docnum_cte
        EXCEPTIONS
          erro      = 1
          OTHERS    = 2.
    ENDIF.

*    IF me->cte-ck_armazem EQ abap_true.
*
*      IF me->nota-mblnr_arm IS NOT INITIAL.
*
*        SELECT SINGLE * INTO @DATA(wa_mkpf)
*          FROM mkpf AS m
*         WHERE m~mblnr EQ @me->nota-mblnr_arm
*           AND m~mjahr EQ @me->nota-mjahr_arm
*           AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).
*
*        IF sy-subrc IS INITIAL.
*          "99  Finalizado Com Armazenagem
*          me->set_st_armazem( i_st_armazem = me->st_armazenagem_99 ).
*        ELSE.
*          CLEAR: me->nota-mblnr_arm, me->nota-mjahr_arm.
*          "00  Pendente
*          me->set_st_armazem( i_st_armazem = me->st_armazenagem_00 ).
*        ENDIF.
*      ELSE.
*        "00  Pendente
*        me->set_st_armazem( i_st_armazem = me->st_armazenagem_00 ).
*      ENDIF.
*
*    ELSE.
*      "98  Finalizado Sem Armazenagem
*      me->set_st_armazem( i_st_armazem = me->st_armazenagem_98 ).
*    ENDIF.

    IF docnum_cte IS NOT INITIAL AND me->cte-docnum_cte IS INITIAL.

      CALL METHOD me->set_add_log_nfe
        EXPORTING
          i_type         = sy-msgty
          i_id           = sy-msgid
          i_num          = sy-msgno
          i_message_v1   = sy-msgv1
          i_message_v2   = sy-msgv2
          i_message_v3   = sy-msgv3
          i_message_v4   = sy-msgv4
        CHANGING
          p_lc_sequencia = lc_sequencia.

      CASE me->cte-ck_fisico.
        WHEN abap_true.
          me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
        WHEN abap_false.
          me->set_st_fisico( i_st_fisico = me->st_fisico_98 ).
      ENDCASE.

      CASE me->cte-ck_fiscal.
        WHEN abap_true.
          me->set_st_fiscal( i_st_fiscal = me->st_fiscal_com_aceite_fiscal ).
        WHEN abap_false.
          me->set_st_fiscal( i_st_fiscal = me->st_fiscal_sem_aceite_fiscal ).
      ENDCASE.

      CASE me->cte-ck_armazem.
        WHEN abap_true.
          me->set_st_armazem( i_st_armazem = me->st_armazenagem_99 ).
        WHEN abap_false.
          me->set_st_armazem( i_st_armazem = me->st_armazenagem_98 ).
      ENDCASE.

      me->set_st_documento( i_st_documento = me->st_documento_99 ).

*      IF ME->NOTA-ST_FISCAL EQ ME->ST_FISCAL_PENDENTE.
*        IF ME->NOTA-CK_FISCAL EQ ABAP_TRUE.
*          ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_COM_ACEITE_FISCAL ).
*          ME->SET_ST_DOCUMENTO( I_ST_DOCUMENTO = ME->ST_DOCUMENTO_99 ).
*        ELSE.
*          ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_SEM_ACEITE_FISCAL ).
*          ME->SET_ST_FISICO( I_ST_FISICO = ME->ST_FISICO_98 ).
*          ME->SET_ST_ARMAZEM( I_ST_ARMAZEM = ME->ST_ARMAZENAGEM_98 ).
*          ME->SET_ST_DOCUMENTO( I_ST_DOCUMENTO = ME->ST_DOCUMENTO_99 ).
*          ME->SET_CK_FISCAL( I_CHECK = ABAP_FALSE ).
*        ENDIF.
*      ELSE.
*        ME->SET_ST_FISCAL( I_ST_FISCAL = ME->ST_FISCAL_SEM_ACEITE_FISCAL ).
*        ME->SET_ST_FISICO( I_ST_FISICO = ME->ST_FISICO_98 ).
*        ME->SET_ST_ARMAZEM( I_ST_ARMAZEM = ME->ST_ARMAZENAGEM_98 ).
*        ME->SET_ST_DOCUMENTO( I_ST_DOCUMENTO = ME->ST_DOCUMENTO_99 ).
*      ENDIF.

      "Recuperar Fatura
      wa_rbkp = me->get_fatura_nfe_inbound( EXPORTING i_docnum = docnum_cte IMPORTING e_rseg = it_rseg ).

      "Recuperar Pedidos
      me->set_nr_fatura( i_belnr = wa_rbkp-belnr i_gjahr = wa_rbkp-gjahr i_rseg = it_rseg ).
      me->set_nr_pedido_compra_itens_ex( i_rseg = it_rseg ).

    ELSEIF me->cte-docnum_cte IS NOT INITIAL AND docnum_cte IS INITIAL.

      """"""""""""""""""""""""""""
      "Retorna Aceite Físico um passo
      me->set_docnum_nfe( i_docnum = 0 ).
      me->set_st_documento( i_st_documento = me->st_documento_01 ).
      me->set_ck_fisico( i_check = abap_false ).

*      IF me->cte-tp_compra_futura EQ zcl_cte_inbound=>tp_compra_futura_fatura.
*        me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
*      ELSE.
*        me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
*      ENDIF.

    ELSEIF ( docnum_cte IS NOT INITIAL )
       AND ( me->cte-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_sem_aceite_fiscal OR
             me->cte-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_pendente OR
             me->cte-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_departamento OR
             me->cte-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_depart_aprovado ).

      "Recuperar Fatura
      wa_rbkp = me->get_fatura_nfe_inbound( EXPORTING i_docnum = docnum_cte IMPORTING e_rseg = it_rseg ).

      "Recuperar Pedidos
      me->set_nr_fatura( i_belnr = wa_rbkp-belnr i_gjahr = wa_rbkp-gjahr i_rseg = it_rseg ).
      me->set_nr_pedido_compra_itens_ex( i_rseg = it_rseg ).

      "Determinar Departamento """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      TRY.
          DATA(r_departamento) = me->get_departamento_itens( i_itens = me->itens ).
          me->set_departamento( r_departamento ).
        CATCH zcx_nfe_inbound_exception.
        CATCH zcx_cadastro.
      ENDTRY.

      me->set_st_fiscal( i_st_fiscal = me->st_fiscal_sem_aceite_fiscal ).
      me->set_st_fisico( i_st_fisico = me->st_fisico_98 ).
      me->set_st_armazem( i_st_armazem = me->st_armazenagem_98 ).
      me->set_st_documento( i_st_documento = me->st_documento_99 ).

    ENDIF.

    "FÍSICO
*   Pendente
*00	Pendente
*01	Aceite Físico
*02	MIGO Gerada
*03	MIRO Gerada
*04	Fiscal Gerada
*99	Finalizado



*    IF me->cte-tp_compra_futura NE zcl_cte_inbound=>tp_compra_futura_fatura.
*
*      "Aviso de Recebimento
*      DATA(wa_likp) = me->get_aviso_valido( ).
*      IF me->cte-vbeln IS INITIAL.
*        IF ( me->cte-st_fiscal EQ zcl_cte_inbound=>st_fiscal_com_aceite_fiscal ) AND ( wa_likp IS NOT INITIAL ).
*          me->set_nr_remessa( i_remessa = wa_likp-vbeln ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_aviso_gerado ).
*        ENDIF.
*      ELSEIF me->cte-vbeln IS NOT INITIAL.
*        IF wa_likp IS INITIAL.
*          me->set_nr_remessa_clear( ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
*          me->set_ck_fisico( i_check = abap_false ).
*        ELSEIF me->cte-vbeln IS NOT INITIAL AND ( me->cte-st_fisico EQ me->st_fisico_00 OR me->cte-st_fisico EQ me->st_fisico_01 ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_aviso_gerado ).
*        ELSEIF wa_likp IS NOT INITIAL AND me->cte-vbeln IS INITIAL.
*          me->set_nr_remessa( i_remessa = wa_likp-vbeln ).
*          IF me->cte-st_fisico EQ me->st_fisico_00 OR me->cte-st_fisico EQ me->st_fisico_01.
*            me->set_st_fisico( i_st_fisico = me->st_fisico_aviso_gerado ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      "Buscar MIGO
*      IF me->nota-mblnr IS INITIAL AND me->nota-p_emissor IS NOT INITIAL.
*
*        DATA(wa_migo) = me->get_migo_valida( ).
*        IF ( me->nota-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_com_aceite_fiscal ) AND ( wa_migo IS NOT INITIAL ).
*          me->set_nr_documento_material( i_mblnr = wa_migo-mblnr i_mjahr = wa_migo-mjahr ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
*          me->set_ck_fisico( i_check = abap_false ).
*        ENDIF.
*
*      ELSEIF me->nota-mblnr IS NOT INITIAL.
*
*        SELECT * INTO TABLE @DATA(it_migo)
*          FROM mkpf AS m
*         WHERE mblnr EQ @me->nota-mblnr
*           AND mjahr EQ @me->nota-mjahr
*           "AND EXISTS ( SELECT * FROM MSEG AS E WHERE E~MBLNR EQ M~MBLNR AND E~MJAHR EQ M~MJAHR AND E~LIFNR EQ @ME->NOTA-P_EMISSOR AND E~SMBLN EQ @SPACE )
*           AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).
*
*        IF sy-subrc IS NOT INITIAL.
*          SELECT * INTO TABLE @it_migo
*            FROM mkpf AS m
*           WHERE mblnr EQ @me->nota-mblnr
*             AND mjahr EQ @me->nota-mjahr
*             AND EXISTS ( SELECT * FROM mseg AS e WHERE e~mblnr EQ m~mblnr AND e~mjahr EQ m~mjahr AND e~llief EQ @me->nota-p_emissor AND e~smbln EQ @space )
*             AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).
*        ENDIF.
*
*        "Migo Estornada
*        IF sy-subrc IS NOT INITIAL.
*          CLEAR: wa_migo.
*          me->set_nr_documento_material( i_mblnr = wa_migo-mblnr i_mjahr = wa_migo-mjahr ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
*          me->set_ck_fisico( i_check = abap_false ).
*        ELSEIF sy-subrc IS INITIAL.
*
*          READ TABLE it_migo INTO wa_migo INDEX 1.
*
*          IF me->nota-mblnr NE wa_migo-mblnr OR me->nota-mblnr IS INITIAL.
*            me->set_nr_documento_material( i_mblnr = wa_migo-mblnr i_mjahr = wa_migo-mjahr ).
*            me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
*
*            IF me->nota-docnum_nfe IS NOT INITIAL.
*              me->set_ck_fisico( i_check = abap_true ).
*              me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
*              me->set_st_documento( i_st_documento = me->st_documento_99 ).
*            ENDIF.
*          ELSEIF me->nota-mblnr IS NOT INITIAL AND me->nota-st_fisico NE me->st_fisico_99.
*            me->set_ck_fisico( i_check = abap_true ).
*            me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
*            me->set_st_documento( i_st_documento = me->st_documento_99 ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.



*    IF me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_mercadoria.
*      "Busca MIRO
*      DATA(wa_miro) = me->get_miro_valida( ).
*      IF me->nota-belnr IS INITIAL.
*        IF ( me->nota-st_fiscal EQ zcl_nfe_inbound=>st_fiscal_com_aceite_fiscal ) AND ( wa_miro IS NOT INITIAL ).
*          me->set_nr_documento_fatura( i_belnr = wa_miro-belnr i_gjahr = wa_miro-gjahr ).
*          me->set_st_fisico( i_st_fisico = me->st_fisico_miro_gerada ).
*          me->set_ck_fisico( i_check = abap_false ).
*
*          SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
*            FROM j_1bnfdoc
*           WHERE belnr EQ @wa_miro-belnr
*             AND gjahr EQ @wa_miro-gjahr.
*
*          IF sy-subrc IS INITIAL.
*            me->set_nr_fiscal( i_docnum = wa_j_1bnfdoc-docnum ).
*            me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
*            me->set_ck_fisico( i_check = abap_true ).
*          ENDIF.
*        ENDIF.
*
*      ELSEIF me->nota-belnr IS NOT INITIAL AND wa_miro IS INITIAL AND me->nota-mblnr IS NOT INITIAL.
*        me->set_nr_documento_fatura( i_belnr = wa_miro-belnr i_gjahr = wa_miro-gjahr ).
*        me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
*        me->set_ck_fisico( i_check = abap_false ).
*      ELSEIF me->nota-belnr IS NOT INITIAL AND wa_miro IS INITIAL AND me->nota-mblnr IS INITIAL AND me->nota-vbeln IS NOT INITIAL.
*        me->set_nr_documento_fatura( i_belnr = wa_miro-belnr i_gjahr = wa_miro-gjahr ).
*        me->set_st_fisico( i_st_fisico = me->st_fisico_aviso_gerado ).
*        me->set_ck_fisico( i_check = abap_false ).
*      ELSEIF me->nota-belnr IS NOT INITIAL AND wa_miro IS INITIAL AND me->nota-mblnr IS INITIAL AND me->nota-vbeln IS INITIAL.
*        me->set_nr_documento_fatura( i_belnr = wa_miro-belnr i_gjahr = wa_miro-gjahr ).
*        me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
*        me->set_ck_fisico( i_check = abap_false ).
*      ENDIF.
*    ENDIF.


    "ME->SET_ST_FISICO( I_ST_FISICO = ME->ST_FISICO_99 ).
    "ME->SET_ST_ARMAZEM( I_ST_ARMAZEM = ME->ST_ARMAZENAGEM_98 ).
    "ME->SET_ST_FISICO( I_ST_FISICO = ME->ST_FISICO_00 ).

*    IF me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_fatura.
*      "Busca Romaneio
*      DATA(r_zsdt0001) = me->get_romaneio( ).
*      IF me->nota-cd_romaneio IS INITIAL.
*        IF ( me->nota-st_armazem EQ zcl_nfe_inbound=>st_armazenagem_99 ) AND ( r_zsdt0001 IS NOT INITIAL ).
*          me->set_cd_romaneio( i_cd_romaneio = r_zsdt0001-ch_referencia ).
*          me->set_st_armazem( i_st_armazem = me->st_armazenagem_99 ).
*          me->set_ck_armazem( i_check = abap_true ).
*        ENDIF.
*      ELSEIF me->nota-cd_romaneio IS NOT INITIAL.
*        IF r_zsdt0001 IS INITIAL.
*          me->set_cd_romaneio( i_cd_romaneio = space ).
*          me->set_st_armazem( i_st_armazem = me->st_armazenagem_00 ).
*          me->set_ck_armazem( i_check = abap_false ).
*        ENDIF.
*      ENDIF.
*    ENDIF.
    "ME->SET_ST_ARMAZEM( I_ST_ARMAZEM = ME->ST_ARMAZENAGEM_00 ).

    IF me->cte-st_fisico = me->st_fisico_aviso_gerado.
      DATA(r_likp) = me->get_aviso_valido( ).
      IF r_likp IS INITIAL.
        me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
      ENDIF.
    ENDIF.

    me->set_docnum_nfe( i_docnum = docnum_cte ).

    " BUG REVISAO com Status 16/02/2022
    IF me->cte-ck_revisao = 'X' AND
       me->cte-st_documento = '99' AND
       me->cte-ck_fiscal IS INITIAL.
      me->ck_alterou = 'X'.
      me->cte-st_documento = ''.
      me->cte-ck_fiscal = 'X'.

    ENDIF.
    IF sy-tcode = 'ZMM0110'.
      IF (  me->cte-ebeln IS NOT INITIAL AND me->cte-belnr IS INITIAL AND me->cte-mblnr IS INITIAL AND me->cte-st_fisico NE '00' ).
        me->cte-st_fiscal    = '99'.
        me->cte-st_fisico    = '00'.
        me->cte-st_documento = ''.
        me->ck_alterou = 'X'.
      ENDIF.

      IF ( me->cte-belnr IS INITIAL AND me->cte-mblnr IS NOT INITIAL AND me->cte-st_fisico NE '03' ). "Gerou a MIGO mas não gerou a MIRO
        me->cte-st_fiscal    = '99'.
        me->cte-st_fisico    = '03'.
        me->cte-st_documento = '01'.
        me->ck_alterou = 'X'.
      ENDIF.

      IF ( me->cte-ebeln IS INITIAL AND me->cte-belnr IS INITIAL AND me->cte-mblnr IS INITIAL AND me->cte-st_fiscal IS NOT INITIAL ). "Sem documentos
        me->cte-st_fiscal    = ''.
        me->cte-st_fisico    = ''.
        me->cte-st_documento = ''.
        me->ck_alterou = 'X'.
      ENDIF.

      IF ( me->cte-ebeln IS NOT INITIAL AND me->cte-belnr IS NOT INITIAL AND me->cte-mblnr IS NOT INITIAL ). "todos gerados
        me->cte-st_fiscal    = '99'.
        me->cte-st_fisico    = '99'.
        me->cte-st_documento = '99'.
        me->ck_alterou = 'X'.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD set_info_sap.

    DATA: cx_nfe_inbound TYPE REF TO zcx_nfe_inbound_exception.


    ""CHECK  ME->NOTA-DOCNUM_NFE IS INITIAL.

*    LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITENS>).
*      CLEAR: <FS_ITENS>-MATNR,
*             <FS_ITENS>-MEINS,
*             <FS_ITENS>-MENGE,
*             <FS_ITENS>-NETPR,
*             <FS_ITENS>-NETWR.
*    ENDLOOP.

    "me->nota-land1 = 'BR'.

    IF me->st_fiscal_anterior IS INITIAL OR
       me->st_fiscal_anterior EQ me->st_fiscal_pendente OR
       me->st_fiscal_anterior EQ me->st_fiscal_departamento OR
       me->st_fiscal_anterior EQ me->st_fiscal_sem_aceite_fiscal OR
       me->pedidos[] IS INITIAL OR
       me->cte-p_emissor IS INITIAL.

      CLEAR: me->logs.
      me->ck_eliminar_log = abap_true.
      lc_sequencia = 1.

      CLEAR:
*      me->cte-bukrs,
*      me->cte-branch,
      me->cte-e_tomadora,
      me->cte-f_tomadora.

      IF me->cte-st_fiscal IS INITIAL.
        me->set_st_documento( i_st_documento = me->st_documento_00 ).
        me->set_st_fiscal( i_st_fiscal = me->st_fiscal_pendente ).
        me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).
        me->set_st_armazem( i_st_armazem = me->st_armazenagem_00 ).
      ENDIF.

      me->set_compra_futura( ).

      "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
      " Tomador """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      TRY .
          DATA(wa_j_1bbranch) = me->get_tomador( i_cnpj = me->cte-dest_cnpj i_ie = me->cte-dest_ie ).

          CALL METHOD me->set_add_log_nfe
            EXPORTING
              i_type         = sy-msgty
              i_id           = sy-msgid
              i_num          = sy-msgno
              i_message_v1   = sy-msgv1
              i_message_v2   = sy-msgv2
              i_message_v3   = sy-msgv3
              i_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.

        CATCH zcx_nfe_inbound_exception INTO cx_nfe_inbound.
          me->set_add_log_nfe_erro( i_erro = cx_nfe_inbound ).
      ENDTRY.

*      me->nota-bukrs      = wa_j_1bbranch-bukrs.
*      me->nota-branch     = wa_j_1bbranch-branch.
      me->cte-e_tomadora = wa_j_1bbranch-bukrs.
      me->cte-f_tomadora = wa_j_1bbranch-branch.

      "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
      " Emissor """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF me->cte-e_tomadora IS NOT INITIAL.

        TRY .
            DATA(wa_lifnr) = me->get_fornecedor( i_forne_cnpj = me->cte-emit_cnpj
                                                 i_forne_cpf  = me->cte-emit_cpf
                                                 i_forne_ie   = me->cte-emit_ie
                                                 i_serie_nota = me->cte-numr_serie
                                                 i_bukrs      = me->cte-e_tomadora ).
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

          CATCH zcx_nfe_inbound_exception INTO cx_nfe_inbound.
            me->set_add_log_nfe_erro( i_erro = cx_nfe_inbound ).
        ENDTRY.

        me->cte-p_emissor = wa_lifnr-lifnr.
      ENDIF.

      "Atribuir Prioridade de Pagamento do Fornecedor
      IF me->cte-zbvtyp IS INITIAL AND me->cte-p_emissor IS NOT INITIAL.
        TRY .
            me->cte-zbvtyp = zcl_miro=>get_prioridade_banco_forne( i_lifnr = me->cte-p_emissor ).

            zcl_miro=>get_formapag_banco_empresa(
              EXPORTING
                i_bukrs           = me->cte-e_emissor    " Empresa
                i_lifnr           = me->cte-p_emissor    " Nº conta do fornecedor
                i_bvtyp           = me->cte-zbvtyp       " Tipo de banco do parceiro
              IMPORTING
                e_forma_pagamento = me->cte-pymt_meth    " Forma de pagamento
                e_banco_empresa   = me->cte-housebankid  " Chave breve de um banco da empresa
            ).

          CATCH zcx_miro_exception INTO DATA(ex_miro).  "
            ex_miro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).

            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = 'W'
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
        ENDTRY.
      ENDIF.

      me->set_item_material_emissor( ).
*      me->set_departamento_nota( ). "comentado 13/03/2024 ( alterando indevidamente o fiscal)

      "Pedido Informado Pelo Fornecedor """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      TRY.
          me->set_nr_pedido_compra_inf_xml( ).
        CATCH zcx_nfe_inbound_exception INTO cx_nfe_inbound.
          me->set_add_log_nfe_erro( i_erro = cx_nfe_inbound ).
      ENDTRY.

      TRY.
          "Verificar se Existe o CFOP
          me->get_cfop_escrituracao_entrada( i_gravar_log = abap_true ).
        CATCH zcx_nfe_inbound_exception INTO cx_nfe_inbound.
          me->set_add_log_nfe_erro( i_erro = cx_nfe_inbound ).
      ENDTRY.

    ELSE.
      me->ck_eliminar_log = abap_false.
      lc_sequencia = me->get_sequencia_log( ).

      "Verifica se ainda não tem prioridade de pagemento e define
      IF me->cte-zbvtyp IS INITIAL AND me->cte-p_emissor IS NOT INITIAL.
        TRY .
            me->cte-zbvtyp = zcl_miro=>get_prioridade_banco_forne( i_lifnr = me->cte-p_emissor ).

            zcl_miro=>get_formapag_banco_empresa(
              EXPORTING
                i_bukrs           = me->cte-e_emissor    " Empresa
                i_lifnr           = me->cte-p_emissor    " Nº conta do fornecedor
                i_bvtyp           = me->cte-zbvtyp       " Tipo de banco do parceiro
              IMPORTING
                e_forma_pagamento = me->cte-pymt_meth    " Forma de pagamento
                e_banco_empresa   = me->cte-housebankid  " Chave breve de um banco da empresa
            ).

            me->ck_alterou = abap_true.

          CATCH zcx_miro_exception INTO ex_miro.  "
            ex_miro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).

            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = 'W'
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
        ENDTRY.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
      EXPORTING
        i_date   = sy-datum
        i_time   = sy-uzeit
      IMPORTING
        e_tstamp = me->cte-timestamp.

    IF me->cte-st_documento EQ me->st_documento_00.
      me->set_departamento_nota( ).
    ENDIF.

    me->set_informacoes_existentes( ).

    IF me->ck_alterou EQ abap_true.
      me->gravar_registro( ).
    ELSE.
      me->nfe_inbound_gravar_log( ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_ITEM_FATURA.

    READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM ASSIGNING FIELD-SYMBOL(<FS_ITEM>).

    IF <FS_ITEM>-BELNR_FT NE I_BELNR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    IF <FS_ITEM>-GJAHR_FT NE I_GJAHR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    IF <FS_ITEM>-BUZEI_FT NE I_BUZEI.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    <FS_ITEM>-BELNR_FT = I_BELNR.
    <FS_ITEM>-GJAHR_FT = I_GJAHR.
    <FS_ITEM>-BUZEI_FT = I_BUZEI.

  ENDMETHOD.


  METHOD SET_ITEM_MATERIAL.

    DATA: CK_ALTEROU_PEDIDO.

    READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM ASSIGNING FIELD-SYMBOL(<FS_ITEM>).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E019 WITH I_PROD_ITEM ME->NOTA-CHAVE_NFE RAISING ERRO.
    ENDIF.

    IF <FS_ITEM>-MATNR NE I_MATNR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-EBELN NE I_EBELN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-EBELP NE I_EBELP.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-MENGE NE I_MENGE.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-MEINS NE I_MEINS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-NETPR NE I_NETPR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-NETWR NE I_NETWR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-EBELN NE I_EBELN OR <FS_ITEM>-EBELP NE I_EBELP.
      CK_ALTEROU_PEDIDO = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-LGORT NE I_LGORT.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    <FS_ITEM>-MATNR = I_MATNR.
    <FS_ITEM>-EBELN = I_EBELN.
    <FS_ITEM>-EBELP = I_EBELP.
    <FS_ITEM>-MENGE = I_MENGE.
    <FS_ITEM>-MEINS = I_MEINS.
    <FS_ITEM>-NETPR = I_NETPR.
    <FS_ITEM>-NETWR = I_NETWR.
    <FS_ITEM>-LGORT = I_LGORT.
    ME->NOTA-EBELN  = I_EBELN.

    IF I_VBELN_VL IS NOT INITIAL AND <FS_ITEM>-DELIV_NUMB NE I_VBELN_VL.
      ME->CK_ALTEROU = ABAP_TRUE.
      <FS_ITEM>-DELIV_NUMB = I_VBELN_VL.
    ENDIF.

    IF I_POSNR_VL IS NOT INITIAL AND <FS_ITEM>-DELIV_ITEM NE I_POSNR_VL.
      ME->CK_ALTEROU = ABAP_TRUE.
      <FS_ITEM>-DELIV_ITEM = I_POSNR_VL.
    ENDIF.

    IF <FS_ITEM>-EBELN IS NOT INITIAL AND <FS_ITEM>-EBELP IS NOT INITIAL.
      SELECT SINGLE MWSKZ INTO @DATA(LC_IVA)
        FROM EKPO
       WHERE EBELN EQ @<FS_ITEM>-EBELN
         AND EBELP EQ @<FS_ITEM>-EBELP.

      IF LC_IVA IS NOT INITIAL.
        ME->SET_IVA( I_MWSKZ = LC_IVA ).
      ELSE.
        ME->SET_IVA( I_MWSKZ = SPACE ).
      ENDIF.
    ELSE.
      ME->SET_IVA( I_MWSKZ = SPACE ).
    ENDIF.

    IF CK_ALTEROU_PEDIDO EQ ABAP_TRUE.
      TRY .
          ME->SET_ALTEROU_PEDIDO_COMPRA( ).
        CATCH ZCX_CADASTRO INTO DATA(EX_CADASTRO).
          EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
      ENDTRY.
    ENDIF.

    IF ME->NOTA-DT_VENCIMENTO IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_EKKO)
        FROM EKKO
       WHERE EBELN EQ @<FS_ITEM>-EBELN.

      CHECK SY-SUBRC IS INITIAL.

      DATA: E_FAEDT TYPE RFPOS-FAEDT.

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          I_ZFBDT = ME->NOTA-DT_EMISSAO
          I_ZBD1T = WA_EKKO-ZBD1T
          I_ZBD2T = WA_EKKO-ZBD2T
          I_ZBD3T = WA_EKKO-ZBD3T
          I_SHKZG = SPACE
          I_REBZG = SPACE
          I_KOART = SPACE
        IMPORTING
          E_FAEDT = E_FAEDT.

      ME->SET_DT_VENCIMENTO( I_DT_VENCIMENTO = E_FAEDT ).

    ENDIF.

  ENDMETHOD.


  METHOD SET_ITEM_MATERIAL_EMISSOR.

    DATA: QT_ITEM   TYPE I,
          QT_ITEM_P TYPE P,
          I_EBELN	  TYPE EBELN.

    CHECK ME->PEDIDOS IS INITIAL.

    IF ME->NOTA-P_EMISSOR IS NOT INITIAL.

      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>).

        "Busca Material"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        CALL METHOD ME->GET_MATERIAL
          EXPORTING
            I_EMISSOR          = ME->NOTA-P_EMISSOR
            I_PROD_CODIGO      = <FS_ITEM>-PROD_CODIGO
            I_UNIDADE_ITEM     = <FS_ITEM>-PROD_UND_COMERCI
          RECEIVING
            R_001              = DATA(WA_001)
          EXCEPTIONS
            NAO_ACHOU_DEPARADA = 1
            OTHERS             = 2.

        IF SY-SUBRC IS NOT INITIAL.
          CALL METHOD ME->SET_ADD_LOG_NFE
            EXPORTING
              I_TYPE         = SY-MSGTY
              I_ID           = SY-MSGID
              I_NUM          = SY-MSGNO
              I_MESSAGE_V1   = SY-MSGV1
              I_MESSAGE_V2   = SY-MSGV2
              I_MESSAGE_V3   = SY-MSGV3
              I_MESSAGE_V4   = SY-MSGV4
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
          CONTINUE.
        ELSE.
          CALL METHOD ME->SET_ADD_LOG_NFE
            EXPORTING
              I_TYPE         = SY-MSGTY
              I_ID           = SY-MSGID
              I_NUM          = SY-MSGNO
              I_MESSAGE_V1   = SY-MSGV1
              I_MESSAGE_V2   = SY-MSGV2
              I_MESSAGE_V3   = SY-MSGV3
              I_MESSAGE_V4   = SY-MSGV4
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
        <FS_ITEM>-MATNR = WA_001-MATNR.
        <FS_ITEM>-MEINS = WA_001-MEINS.
        TRY .
            QT_ITEM = ( <FS_ITEM>-PROD_QTD_COMERCI * WA_001-FATOR ) * 1000.
            <FS_ITEM>-MENGE = QT_ITEM / 1000.
          CATCH CX_SY_ARITHMETIC_OVERFLOW.
            QT_ITEM_P = ( <FS_ITEM>-PROD_QTD_COMERCI * WA_001-FATOR ) * 1000.
            <FS_ITEM>-MENGE = QT_ITEM_P / 1000.
        ENDTRY.
        <FS_ITEM>-NETPR = <FS_ITEM>-PROD_VLR_TOTAL_B.
        <FS_ITEM>-NETWR = <FS_ITEM>-PROD_VLR_TOTAL_B.

        "Verifica Pedido de Compra Informado
        IF <FS_ITEM>-PROD_PEDIDO_COMP IS NOT INITIAL AND <FS_ITEM>-EBELN IS INITIAL AND <FS_ITEM>-EBELP IS INITIAL.

          I_EBELN = <FS_ITEM>-PROD_PEDIDO_COMP.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = I_EBELN
            IMPORTING
              OUTPUT = I_EBELN.

          ZCL_PEDIDO_COMPRA=>GET_PEDIDO_ITENS(
            EXPORTING
              I_EBELN          = I_EBELN
            IMPORTING
              E_EKKO           = DATA(E_EKKO)
              E_EKPO_T         = DATA(E_EKPO_T)
            EXCEPTIONS
              NAO_ACHOU_PEDIDO = 1
              OTHERS           = 2 ).

          IF SY-SUBRC IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          DELETE E_EKPO_T WHERE MATNR EQ <FS_ITEM>-MATNR.
          DESCRIBE TABLE E_EKPO_T LINES DATA(LC_LINHA).

          IF LC_LINHA EQ 1.
            READ TABLE E_EKPO_T INDEX 1 INTO DATA(WA_EKPO_T).
            <FS_ITEM>-EBELN = WA_EKPO_T-EBELN.
            <FS_ITEM>-EBELP = WA_EKPO_T-EBELP.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD SET_ITEM_MATERIAL_PRECO.

    DATA: CK_ALTEROU_PEDIDO.

    READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM ASSIGNING FIELD-SYMBOL(<FS_ITEM>).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E019 WITH I_PROD_ITEM ME->NOTA-CHAVE_NFE RAISING ERRO.
    ENDIF.

    IF <FS_ITEM>-BRTWR NE I_PRECO_PEDIDO-BRTWR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-BICMS NE I_PRECO_PEDIDO-BICMS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-PICMS NE I_PRECO_PEDIDO-PICMS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-BPIS NE I_PRECO_PEDIDO-BPIS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-PPIS NE I_PRECO_PEDIDO-PPIS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-BCOFINS NE I_PRECO_PEDIDO-BCOFINS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-PCOFINS NE I_PRECO_PEDIDO-PCOFINS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF <FS_ITEM>-LIQUIDO NE I_PRECO_PEDIDO-LIQUIDO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    <FS_ITEM>-BRTWR   = I_PRECO_PEDIDO-BRTWR  .
    <FS_ITEM>-BICMS   = I_PRECO_PEDIDO-BICMS  .
    <FS_ITEM>-PICMS   = I_PRECO_PEDIDO-PICMS  .
    <FS_ITEM>-BPIS    = I_PRECO_PEDIDO-BPIS   .
    <FS_ITEM>-PPIS    = I_PRECO_PEDIDO-PPIS   .
    <FS_ITEM>-BCOFINS = I_PRECO_PEDIDO-BCOFINS.
    <FS_ITEM>-PCOFINS = I_PRECO_PEDIDO-PCOFINS.
    <FS_ITEM>-LIQUIDO = I_PRECO_PEDIDO-LIQUIDO.

  ENDMETHOD.


  METHOD SET_IVA.
    IF ME->NOTA-MWSKZ NE I_MWSKZ.
      ME->CK_ALTEROU = ABAP_TRUE.
      ME->CK_ALTEROU_IVA = ABAP_TRUE.
    ENDIF.
    ME->NOTA-MWSKZ = I_MWSKZ.
  ENDMETHOD.


  METHOD SET_LOTE_ITEM.

    READ TABLE ME->LOTES ASSIGNING FIELD-SYMBOL(<FS_LOTE>) WITH KEY CD_LOTE_ITEM = I_LOTE-CD_LOTE_ITEM.

    MOVE-CORRESPONDING I_LOTE TO <FS_LOTE>.

    "Verificar se Lote Existe
    IF I_LOTE-CHARG IS NOT INITIAL.

      TRY.
          CALL METHOD ZCL_CHARG=>GET_CHARG
            EXPORTING
              I_MATNR = I_LOTE-MATNR
              I_CHARG = I_LOTE-CHARG
            RECEIVING
              R_MCH1  = DATA(R_MCH1).

          CALL METHOD ZCL_CHARG=>GET_CHARG_DETALHE
            EXPORTING
              I_MATNR         = I_LOTE-MATNR
              I_CHARG         = I_LOTE-CHARG
              I_WERKS         = I_LOTE-WERKS
            IMPORTING
              E_YMCHA         = DATA(E_YMCHA)
              E_CLASSNAME     = DATA(E_CLASSNAME)
            RECEIVING
              R_CHAR_OF_BATCH = DATA(R_CHAR_OF_BATCH).

          <FS_LOTE>-CUOBJ = E_YMCHA-CUOBJ_BM.
          <FS_LOTE>-HERKL = E_YMCHA-HERKL.
          <FS_LOTE>-HSDAT = E_YMCHA-HSDAT.
          <FS_LOTE>-LICHA = E_YMCHA-LICHA.
          <FS_LOTE>-VFDAT = E_YMCHA-VFDAT.
          <FS_LOTE>-CLASS = E_CLASSNAME.

          SELECT SINGLE * INTO @DATA(WA_KLAH)
            FROM KLAH
           WHERE CLASS EQ @E_CLASSNAME.

          IF SY-SUBRC IS INITIAL.
            <FS_LOTE>-CLINT = WA_KLAH-CLINT.
            <FS_LOTE>-KLART = WA_KLAH-KLART.
          ENDIF.

          MOVE-CORRESPONDING <FS_LOTE> TO I_LOTE.

          LOOP AT I_LOTE_CARACT ASSIGNING FIELD-SYMBOL(<FS_LOTEC>) WHERE CD_LOTE_ITEM EQ I_LOTE-CD_LOTE_ITEM.

*            READ TABLE R_CHAR_OF_BATCH WITH KEY ATNAM = <FS_LOTEC>-ATNAM INTO DATA(BP_CARAC).
*            IF SY-SUBRC IS INITIAL.
*              <FS_LOTEC>-ATWRT = BP_CARAC-ATWTB.
*            ENDIF.

            READ TABLE ME->LOTES_CARACTERISTICAS ASSIGNING FIELD-SYMBOL(<FS_CARAC>)
              WITH KEY CD_LOTE_ITEM = <FS_LOTEC>-CD_LOTE_ITEM
                       ATINN        = <FS_LOTEC>-ATINN.

            IF SY-SUBRC IS INITIAL.
              MOVE-CORRESPONDING <FS_LOTEC> TO <FS_CARAC>.
            ENDIF.

          ENDLOOP.

          ME->CK_ALTEROU = ABAP_TRUE.
          ME->CK_ALTEROU_LOTES = ABAP_TRUE.
          EXIT.

        CATCH ZCX_CHARG_EXCEPTION .
      ENDTRY.
    ENDIF.

    LOOP AT I_LOTE_CARACT INTO DATA(WA_CARAC) WHERE CD_LOTE_ITEM EQ I_LOTE-CD_LOTE_ITEM.
      READ TABLE ME->LOTES_CARACTERISTICAS ASSIGNING FIELD-SYMBOL(<FS_CARAC2>)
       WITH KEY CD_LOTE_ITEM = WA_CARAC-CD_LOTE_ITEM
                ATINN        = WA_CARAC-ATINN.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING WA_CARAC TO <FS_CARAC2>.
      ENDIF.
    ENDLOOP.

    ME->CK_ALTEROU = ABAP_TRUE.
    ME->CK_ALTEROU_LOTES = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_LR_PARTINER.

    IF ME->NOTA-LR_PARTINER NE I_LR_PARTINER.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-LR_PARTINER = I_LR_PARTINER.

  ENDMETHOD.


  METHOD SET_MATERIAL.

    DATA: WA_ZIB_NFE_DIST_001 TYPE ZIB_NFE_DIST_001.

    CHECK I_EMISSOR IS NOT INITIAL.
    CHECK I_PROD_CODIGO IS NOT INITIAL.
    CHECK I_UNIDADE_ITEM IS NOT INITIAL.
    CHECK I_MATNR IS NOT INITIAL.
    CHECK I_MEINS IS NOT INITIAL.
    CHECK I_FATOR IS NOT INITIAL.

    WA_ZIB_NFE_DIST_001-P_EMISSOR        = I_EMISSOR.
    WA_ZIB_NFE_DIST_001-PROD_CODIGO      = I_PROD_CODIGO.
    WA_ZIB_NFE_DIST_001-PROD_UND_COMERCI = I_UNIDADE_ITEM.
    WA_ZIB_NFE_DIST_001-MATNR            = I_MATNR.
    WA_ZIB_NFE_DIST_001-MEINS            = I_MEINS.
    WA_ZIB_NFE_DIST_001-FATOR            = I_FATOR.
    MODIFY ZIB_NFE_DIST_001 FROM WA_ZIB_NFE_DIST_001.

  ENDMETHOD.


  METHOD SET_MEIO_DE_PAGAMENTO.

    IF ME->NOTA-PYMT_METH NE I_MEIO_PAGAMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-PYMT_METH = I_MEIO_PAGAMENTO.

    IF ME->NOTA-PYMT_METH NE ZCL_MIRO=>ST_FORMA_PAGAMENTO_BOLETO AND ME->NOTA-BOLETO IS NOT INITIAL.
      CLEAR: ME->NOTA-BOLETO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_DOCUMENTO_FATURA.

    IF ( ME->NOTA-BELNR NE I_BELNR ) OR ( ME->NOTA-GJAHR NE I_GJAHR ).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-BELNR = I_BELNR.
    ME->NOTA-GJAHR = I_GJAHR.

    IF ME->NOTA-BELNR IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_RSEG)
        FROM RSEG
       WHERE BELNR EQ @ME->NOTA-BELNR
         AND GJAHR EQ @ME->NOTA-GJAHR.
    ENDIF.

    IF ME->PEDIDOS IS NOT INITIAL.
      LOOP AT ME->PEDIDOS ASSIGNING FIELD-SYMBOL(<FS_PEDIDO>).
        IF ME->NOTA-BELNR IS NOT INITIAL.
          READ TABLE IT_RSEG INTO DATA(WA_RSEG) WITH KEY EBELN = <FS_PEDIDO>-EBELN EBELP = <FS_PEDIDO>-EBELP.
          IF SY-SUBRC IS INITIAL.
            <FS_PEDIDO>-BELNR_FT = WA_RSEG-BELNR.
            <FS_PEDIDO>-GJAHR_FT = WA_RSEG-GJAHR.
            <FS_PEDIDO>-BUZEI_FT = WA_RSEG-BUZEI.
          ENDIF.
        ELSE.
          CLEAR: <FS_PEDIDO>-BELNR_FT, <FS_PEDIDO>-GJAHR_FT, <FS_PEDIDO>-BUZEI_FT.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
        IF ME->NOTA-BELNR IS NOT INITIAL.
          READ TABLE IT_RSEG INDEX SY-TABIX INTO WA_RSEG.
          IF SY-SUBRC IS INITIAL.
            <FS_ITEM>-BELNR_FT = WA_RSEG-BELNR.
            <FS_ITEM>-GJAHR_FT = WA_RSEG-GJAHR.
            <FS_ITEM>-BUZEI_FT = WA_RSEG-BUZEI.
          ENDIF.
        ELSE.
          CLEAR: <FS_ITEM>-BELNR_FT, <FS_ITEM>-GJAHR_FT, <FS_ITEM>-BUZEI_FT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_DOCUMENTO_MATERIAL.

    IF ( ME->NOTA-MBLNR NE I_MBLNR ) OR ( ME->NOTA-MJAHR NE I_MJAHR ).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-MBLNR = I_MBLNR.
    ME->NOTA-MJAHR = I_MJAHR.

    IF ME->NOTA-MBLNR IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_MSEG)
        FROM MSEG
       WHERE MBLNR EQ @ME->NOTA-MBLNR
         AND MJAHR EQ @ME->NOTA-MJAHR.
    ENDIF.

    IF ME->PEDIDOS IS NOT INITIAL.
      "Atualiza Pagamento via Pedido
      LOOP AT ME->PEDIDOS ASSIGNING FIELD-SYMBOL(<FS_PEDIDO>).
        IF ME->NOTA-MBLNR IS NOT INITIAL.
          READ TABLE IT_MSEG INTO DATA(WA_MSEG) WITH KEY EBELN = <FS_PEDIDO>-EBELN
                                                         EBELP = <FS_PEDIDO>-EBELP.
          IF SY-SUBRC IS INITIAL.
            <FS_PEDIDO>-MBLNR = WA_MSEG-MBLNR.
            <FS_PEDIDO>-MJAHR = WA_MSEG-MJAHR.
            <FS_PEDIDO>-ZEILE = WA_MSEG-ZEILE.
          ENDIF.

        ELSE.
          CLEAR: <FS_PEDIDO>-MBLNR, <FS_PEDIDO>-MJAHR, <FS_PEDIDO>-ZEILE.
        ENDIF.
      ENDLOOP.
    ELSE.

      DATA: ITEM_MIGO TYPE I.
      DATA: LOTE_MIGO TYPE I.

      ITEM_MIGO = 1.

      "Atualiza Pagamento via Itens de Nota Fiscal
      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>).

        DATA(LC_LOTES) = ME->LOTES[].
        DELETE LC_LOTES WHERE PROD_ITEM NE <FS_ITEM>-PROD_ITEM.
        DESCRIBE TABLE LC_LOTES LINES DATA(QT_LOTES).

        IF QT_LOTES GT 1.
          LOTE_MIGO = 1.
          LOOP AT ME->LOTES ASSIGNING FIELD-SYMBOL(<FS_LOTE>) WHERE PROD_ITEM EQ <FS_ITEM>-PROD_ITEM.

            READ TABLE IT_MSEG INTO WA_MSEG WITH KEY ZEILE = ITEM_MIGO.

            IF LOTE_MIGO EQ 1 AND SY-SUBRC IS INITIAL.
              <FS_ITEM>-MBLNR = WA_MSEG-MBLNR.
              <FS_ITEM>-MJAHR = WA_MSEG-MJAHR.
              <FS_ITEM>-ZEILE = WA_MSEG-ZEILE.
            ENDIF.

            IF SY-SUBRC IS INITIAL.
              <FS_LOTE>-MBLNR = WA_MSEG-MBLNR.
              <FS_LOTE>-MJAHR = WA_MSEG-MJAHR.
              <FS_LOTE>-ZEILE = WA_MSEG-ZEILE.
              ADD 1 TO ITEM_MIGO.
            ELSE.
              CLEAR: <FS_LOTE>-MBLNR, <FS_LOTE>-MJAHR, <FS_LOTE>-ZEILE.
            ENDIF.

            ADD 1 TO LOTE_MIGO.

          ENDLOOP.
        ELSE.
          READ TABLE IT_MSEG INTO WA_MSEG WITH KEY ZEILE = ITEM_MIGO.
          IF SY-SUBRC IS INITIAL.
            <FS_ITEM>-MBLNR = WA_MSEG-MBLNR.
            <FS_ITEM>-MJAHR = WA_MSEG-MJAHR.
            <FS_ITEM>-ZEILE = WA_MSEG-ZEILE.
            ADD 1 TO ITEM_MIGO.
          ELSE.
            CLEAR: <FS_ITEM>-MBLNR, <FS_ITEM>-MJAHR, <FS_ITEM>-ZEILE.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_DOC_CUSTO.
    IF ME->NOTA-FKNUM NE I_FKNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-FKNUM = I_FKNUM.
  ENDMETHOD.


  METHOD SET_NR_DOC_MATERIAL_CLEAR.

    IF ( ME->NOTA-MBLNR IS NOT INITIAL ) OR ( ME->NOTA-MJAHR IS NOT INITIAL ).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    CLEAR:
    ME->NOTA-MBLNR,
    ME->NOTA-MJAHR.

  ENDMETHOD.


  METHOD SET_NR_FASE.

    IF ME->NOTA-NR_FASE NE I_NR_FASE.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-NR_FASE = I_NR_FASE.

  ENDMETHOD.


  METHOD SET_NR_FATURA.

    IF ME->NOTA-BELNR NE I_BELNR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->NOTA-GJAHR NE I_GJAHR.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-BELNR = I_BELNR.
    ME->NOTA-GJAHR = I_GJAHR.

    IF ME->NOTA-BELNR IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_RSEG)
        FROM RSEG
       WHERE BELNR EQ @ME->NOTA-BELNR
         AND GJAHR EQ @ME->NOTA-GJAHR.
    ENDIF.

    LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
      IF ME->NOTA-BELNR IS NOT INITIAL.
        READ TABLE IT_RSEG INDEX SY-TABIX INTO DATA(WA_RSEG).
        IF SY-SUBRC IS INITIAL.
          <FS_ITEM>-BELNR_FT = WA_RSEG-BELNR.
          <FS_ITEM>-GJAHR_FT = WA_RSEG-GJAHR.
          <FS_ITEM>-BUZEI_FT = WA_RSEG-BUZEI.
        ENDIF.
      ELSE.
        CLEAR: <FS_ITEM>-BELNR_FT, <FS_ITEM>-GJAHR_FT, <FS_ITEM>-BUZEI_FT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_NR_FISCAL.

    DATA: LC_REFKEY	TYPE J_1BREFKEY,
          LC_REFITM	TYPE J_1BREFITM.

    IF ME->NOTA-DOCNUM_NFE NE I_DOCNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-DOCNUM_NFE = I_DOCNUM.

    IF ME->NOTA-MBLNR IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_J_1BNFLIN)
        FROM J_1BNFLIN
       WHERE DOCNUM EQ @ME->NOTA-DOCNUM_NFE.
    ENDIF.

    IF ME->PEDIDOS IS NOT INITIAL.

      IF ME->NOTA-BELNR IS NOT INITIAL.
        SELECT * INTO TABLE @DATA(IT_RSEG)
          FROM RSEG
         WHERE BELNR EQ @ME->NOTA-BELNR
           AND GJAHR EQ @ME->NOTA-GJAHR.
      ENDIF.

      LOOP AT ME->PEDIDOS ASSIGNING FIELD-SYMBOL(<FS_PEDIDO>).

        IF ME->NOTA-DOCNUM_NFE IS NOT INITIAL.
          READ TABLE IT_RSEG INTO DATA(WA_RSEG)
          WITH KEY EBELN = <FS_PEDIDO>-EBELN
                   EBELP = <FS_PEDIDO>-EBELP.

          IF SY-SUBRC IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          LC_REFKEY = WA_RSEG-BELNR && WA_RSEG-GJAHR.
          LC_REFITM = WA_RSEG-BUZEI.

          READ TABLE IT_J_1BNFLIN INTO DATA(WA_J_1BNFLIN)
          WITH KEY REFKEY = LC_REFKEY
                   REFITM = LC_REFITM.

          IF SY-SUBRC IS INITIAL.
            <FS_PEDIDO>-DOCNUM_NFE = WA_J_1BNFLIN-DOCNUM.
            <FS_PEDIDO>-ITMNUM_NFE = WA_J_1BNFLIN-ITMNUM.
          ENDIF.

        ELSE.
          CLEAR: <FS_PEDIDO>-DOCNUM_NFE, <FS_PEDIDO>-ITMNUM_NFE.
        ENDIF.

      ENDLOOP.

    ELSE.

      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
        IF ME->NOTA-DOCNUM_NFE IS NOT INITIAL.
          READ TABLE IT_J_1BNFLIN INDEX SY-TABIX INTO WA_J_1BNFLIN.
          IF SY-SUBRC IS INITIAL.
            <FS_ITEM>-DOCNUM_NFE = WA_J_1BNFLIN-DOCNUM.
            <FS_ITEM>-ITMNUM_NFE = WA_J_1BNFLIN-ITMNUM.
          ENDIF.
        ELSE.
          CLEAR: <FS_ITEM>-DOCNUM_NFE, <FS_ITEM>-ITMNUM_NFE.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_PEDIDO_COMPRA.
    IF ME->NOTA-EBELN NE I_EBELN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-EBELN = I_EBELN.
  ENDMETHOD.


  METHOD SET_NR_PEDIDO_COMPRA_CLEAR.
    IF ME->NOTA-EBELN IS NOT INITIAL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    CLEAR:
    ME->NOTA-EBELN.
  ENDMETHOD.


  METHOD SET_NR_PEDIDO_COMPRA_INF_XML.

    DATA: IT_EKKO  TYPE TABLE OF EKKO,
          LC_MENGE TYPE J_1BNETQTY.

    FIELD-SYMBOLS: <FS_ITEM_PEDIDO> TYPE EKPO.

    "Encontra Pedidos de Compra
    LOOP AT ME->ITENS INTO DATA(WA_ITEM).
      IF WA_ITEM-PROD_PEDIDO_COMP IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_ITEM-PROD_PEDIDO_COMP
          IMPORTING
            OUTPUT = WA_ITEM-PROD_PEDIDO_COMP.

        WA_ITEM-EBELN = WA_ITEM-PROD_PEDIDO_COMP.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_ITEM-EBELN
          IMPORTING
            OUTPUT = WA_ITEM-EBELN.

        SELECT SINGLE * INTO @DATA(WA_EKKO)
          FROM EKKO
         WHERE EBELN EQ @WA_ITEM-EBELN.

        IF SY-SUBRC IS INITIAL.
          APPEND WA_EKKO TO IT_EKKO.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT IT_EKKO BY EBELN.
    DELETE ADJACENT DUPLICATES FROM IT_EKKO COMPARING EBELN.

    "Verifica Pedidos Encontrados
    IF IT_EKKO IS NOT INITIAL.

      READ TABLE IT_EKKO INTO WA_EKKO INDEX 1.
      "ME->SET_NR_PEDIDO_COMPRA( I_EBELN = WA_EKKO-EBELN ).

      "049  Pedido de Compra &1 atribuido ao documento!
      MESSAGE S049 WITH WA_EKKO-EBELN.

      CALL METHOD ME->SET_ADD_LOG_NFE
        EXPORTING
          I_TYPE         = SY-MSGTY
          I_ID           = SY-MSGID
          I_NUM          = SY-MSGNO
          I_MESSAGE_V1   = SY-MSGV1
          I_MESSAGE_V2   = SY-MSGV2
          I_MESSAGE_V3   = SY-MSGV3
          I_MESSAGE_V4   = SY-MSGV4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

      SELECT * INTO TABLE @DATA(IT_EKPO)
        FROM EKPO
         FOR ALL ENTRIES IN @IT_EKKO
       WHERE EBELN EQ @IT_EKKO-EBELN.

      LOOP AT ME->ITENS INTO WA_ITEM.

        CALL METHOD ME->GET_MATERIAL
          EXPORTING
            I_EMISSOR          = ME->NOTA-P_EMISSOR
            I_PROD_CODIGO      = WA_ITEM-PROD_CODIGO
            I_UNIDADE_ITEM     = WA_ITEM-PROD_UND_COMERCI
          RECEIVING
            R_001              = DATA(WA_001)
          EXCEPTIONS
            NAO_ACHOU_DEPARADA = 1
            OTHERS             = 2.

        IF SY-SUBRC IS INITIAL.

          LC_MENGE = WA_ITEM-PROD_QTD_COMERCI * WA_001-FATOR.

          "Localiza Pedido Item Quantidade
          LOOP AT IT_EKPO INTO DATA(WA_EKPO)
            WHERE MATNR EQ WA_001-MATNR
              AND MEINS EQ WA_001-MEINS
              AND MENGE GE LC_MENGE.
            IF <FS_ITEM_PEDIDO> IS NOT ASSIGNED.
              READ TABLE IT_EKPO INDEX SY-TABIX ASSIGNING <FS_ITEM_PEDIDO>.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDLOOP.

          IF <FS_ITEM_PEDIDO> IS ASSIGNED.

            ME->SET_ITEM_MATERIAL(
                I_PROD_ITEM = WA_ITEM-PROD_ITEM
                I_MATNR     = <FS_ITEM_PEDIDO>-MATNR
                I_EBELN     = <FS_ITEM_PEDIDO>-EBELN
                I_EBELP     = <FS_ITEM_PEDIDO>-EBELP
                I_MENGE     = LC_MENGE
                I_MEINS     = <FS_ITEM_PEDIDO>-MEINS
                I_NETPR     = WA_ITEM-NETPR
                I_NETWR     = WA_ITEM-NETWR ).

            "050  Item &! Atribuido Pedido &2 item &3!
            MESSAGE S050 WITH WA_ITEM-PROD_ITEM <FS_ITEM_PEDIDO>-EBELN <FS_ITEM_PEDIDO>-EBELP.

            CALL METHOD ME->SET_ADD_LOG_NFE
              EXPORTING
                I_TYPE         = SY-MSGTY
                I_ID           = SY-MSGID
                I_NUM          = SY-MSGNO
                I_MESSAGE_V1   = SY-MSGV1
                I_MESSAGE_V2   = SY-MSGV2
                I_MESSAGE_V3   = SY-MSGV3
                I_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.

            <FS_ITEM_PEDIDO>-MENGE = <FS_ITEM_PEDIDO>-MENGE - LC_MENGE .
            UNASSIGN <FS_ITEM_PEDIDO>.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PEDIDO_NAO_INFO_XML-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PEDIDO_NAO_INFO_XML-MSGNO )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PEDIDO_NAO_INFO_XML-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PEDIDO_NAO_INFO_XML-MSGNO
          MSGTY  = 'W'.
    ENDIF.


  ENDMETHOD.


  METHOD SET_NR_PEDIDO_COMPRA_ITENS_EX.

    DATA: IT_RSEG_AUX	TYPE J_1B_TT_RSEG.

    READ TABLE I_RSEG INDEX 1 INTO DATA(WA_RSEG).

    CHECK SY-SUBRC IS INITIAL.
    ME->SET_NR_PEDIDO_COMPRA( I_EBELN = WA_RSEG-EBELN ).

    IT_RSEG_AUX = I_RSEG.
    SORT IT_RSEG_AUX BY EBELN EBELP.
    DELETE ADJACENT DUPLICATES FROM IT_RSEG_AUX COMPARING EBELN EBELP.
    DESCRIBE TABLE IT_RSEG_AUX LINES DATA(LN_RSEG).
    IT_RSEG_AUX = I_RSEG.

    IF LN_RSEG EQ 1.
      "Se somente um pedido/item
      LOOP AT ME->ITENS INTO  DATA(WA_ITEM).

        CALL METHOD ME->GET_MATERIAL
          EXPORTING
            I_EMISSOR          = ME->NOTA-P_EMISSOR
            I_PROD_CODIGO      = WA_ITEM-PROD_CODIGO
            I_UNIDADE_ITEM     = WA_ITEM-PROD_UND_COMERCI
          RECEIVING
            R_001              = DATA(WA_001)
          EXCEPTIONS
            NAO_ACHOU_DEPARADA = 1
            OTHERS             = 2.

        IF SY-SUBRC IS INITIAL.

          WA_RSEG-MENGE = WA_ITEM-PROD_QTD_COMERCI * WA_001-FATOR.

          READ TABLE IT_RSEG_AUX INTO WA_RSEG
          WITH KEY MATNR = WA_001-MATNR
                   MEINS = WA_001-MEINS
                   MENGE = WA_RSEG-MENGE.
          IF SY-SUBRC IS INITIAL.

            DELETE IT_RSEG_AUX INDEX SY-TABIX.

            ME->SET_ITEM_MATERIAL(
                I_PROD_ITEM = WA_ITEM-PROD_ITEM
                I_MATNR     = WA_RSEG-MATNR
                I_EBELN     = WA_RSEG-EBELN
                I_EBELP     = WA_RSEG-EBELP
                I_MENGE     = WA_RSEG-MENGE
                I_MEINS     = WA_RSEG-MEINS
                I_NETPR     = WA_ITEM-NETPR
                I_NETWR     = WA_ITEM-NETWR ).

            ME->SET_ITEM_FATURA(
                I_PROD_ITEM = WA_ITEM-PROD_ITEM
                I_BELNR     = WA_RSEG-BELNR
                I_GJAHR     = WA_RSEG-GJAHR
                I_BUZEI     = WA_RSEG-BUZEI ).
          ENDIF.

        ENDIF.

      ENDLOOP.
    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_REMESSA.

    IF ME->NOTA-VBELN NE I_REMESSA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-VBELN = I_REMESSA.

    "Atribui Itens da Remessa aos itens da Nota"
    IF ME->NOTA-VBELN IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(IT_LIPS)
        FROM LIPS
       WHERE VBELN EQ @I_REMESSA
         AND POSNR LT 900000.

      "Achar Exatos
      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM_T>).

        DATA(LC_CK_ACHOU)  = ABAP_FALSE.

        LOOP AT IT_LIPS ASSIGNING FIELD-SYMBOL(<FS_LIPS_T>)
          WHERE VGBEL EQ <FS_ITEM_T>-EBELN
            AND VGPOS EQ <FS_ITEM_T>-EBELP
            AND MATNR EQ <FS_ITEM_T>-MATNR
            AND LFIMG EQ <FS_ITEM_T>-MENGE
            AND MEINS EQ <FS_ITEM_T>-MEINS.

          IF LC_CK_ACHOU EQ ABAP_TRUE.
            CONTINUE.
          ENDIF.

          <FS_ITEM_T>-EBELN      = <FS_LIPS_T>-VGBEL.
          <FS_ITEM_T>-EBELP      = <FS_LIPS_T>-VGPOS.
          <FS_ITEM_T>-DELIV_NUMB = <FS_LIPS_T>-VBELN.
          <FS_ITEM_T>-DELIV_ITEM = <FS_LIPS_T>-POSNR.
          LC_CK_ACHOU          = ABAP_TRUE.
          <FS_LIPS_T>-LFIMG      = 0.
          CONTINUE.
        ENDLOOP.

        IF LC_CK_ACHOU EQ ABAP_FALSE.

          LOOP AT IT_LIPS ASSIGNING FIELD-SYMBOL(<FS_LIPS_T2>)
            WHERE VGBEL  EQ <FS_ITEM_T>-EBELN
              AND VGPOS  EQ <FS_ITEM_T>-EBELP
              AND MATNR  EQ <FS_ITEM_T>-MATNR
              AND KCMENG EQ <FS_ITEM_T>-MENGE
              AND MEINS  EQ <FS_ITEM_T>-MEINS.

            IF LC_CK_ACHOU EQ ABAP_TRUE.
              CONTINUE.
            ENDIF.

            <FS_ITEM_T>-EBELN      = <FS_LIPS_T2>-VGBEL.
            <FS_ITEM_T>-EBELP      = <FS_LIPS_T2>-VGPOS.
            <FS_ITEM_T>-DELIV_NUMB = <FS_LIPS_T2>-VBELN.
            <FS_ITEM_T>-DELIV_ITEM = <FS_LIPS_T2>-POSNR.
            LC_CK_ACHOU            = ABAP_TRUE.
            <FS_LIPS_T2>-LFIMG     = 0.
            CONTINUE.
          ENDLOOP.
        ENDIF.

        IF LC_CK_ACHOU EQ ABAP_TRUE.
          CALL METHOD ME->SET_ITEM_MATERIAL
            EXPORTING
              I_PROD_ITEM = <FS_ITEM_T>-PROD_ITEM
              I_MATNR     = <FS_ITEM_T>-MATNR
              I_EBELN     = <FS_ITEM_T>-EBELN
              I_EBELP     = <FS_ITEM_T>-EBELP
              I_MENGE     = <FS_ITEM_T>-MENGE
              I_MEINS     = <FS_ITEM_T>-MEINS
              I_NETPR     = <FS_ITEM_T>-NETPR
              I_NETWR     = <FS_ITEM_T>-NETWR
              I_VBELN_VL  = <FS_ITEM_T>-DELIV_NUMB
              I_POSNR_VL  = <FS_ITEM_T>-DELIV_ITEM.
        ELSE.
          CLEAR:
          <FS_ITEM_T>-DELIV_NUMB,
          <FS_ITEM_T>-DELIV_ITEM.
        ENDIF.
      ENDLOOP.

      "Achar Parciais
      LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM>) WHERE DELIV_NUMB IS INITIAL.

        LC_CK_ACHOU = ABAP_FALSE.

        LOOP AT IT_LIPS ASSIGNING FIELD-SYMBOL(<FS_LIPS>)
          WHERE VGBEL EQ <FS_ITEM>-EBELN
            AND MATNR EQ <FS_ITEM>-MATNR
            AND LFIMG GE <FS_ITEM>-MENGE
            AND VRKME EQ <FS_ITEM>-MEINS.

          <FS_ITEM>-EBELN      = <FS_LIPS>-VGBEL.
          <FS_ITEM>-EBELP      = <FS_LIPS>-VGPOS.
          <FS_ITEM>-DELIV_NUMB = <FS_LIPS>-VBELN.
          <FS_ITEM>-DELIV_ITEM = <FS_LIPS>-POSNR.
          LC_CK_ACHOU          = ABAP_TRUE.
          <FS_LIPS>-LFIMG      = <FS_LIPS>-LFIMG - <FS_ITEM>-MENGE.
          CONTINUE.
        ENDLOOP.

        IF LC_CK_ACHOU IS NOT INITIAL.
          CALL METHOD ME->SET_ITEM_MATERIAL
            EXPORTING
              I_PROD_ITEM = <FS_ITEM>-PROD_ITEM
              I_MATNR     = <FS_ITEM>-MATNR
              I_EBELN     = <FS_ITEM>-EBELN
              I_EBELP     = <FS_ITEM>-EBELP
              I_MENGE     = <FS_ITEM>-MENGE
              I_MEINS     = <FS_ITEM>-MEINS
              I_NETPR     = <FS_ITEM>-NETPR
              I_NETWR     = <FS_ITEM>-NETWR
              I_VBELN_VL  = <FS_ITEM>-DELIV_NUMB
              I_POSNR_VL  = <FS_ITEM>-DELIV_ITEM.
        ELSE.
          CLEAR:
          <FS_ITEM>-DELIV_NUMB,
          <FS_ITEM>-DELIV_ITEM.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD SET_NR_REMESSA_CLEAR.

    IF ME->NOTA-VBELN IS NOT INITIAL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    CLEAR:
    ME->NOTA-VBELN.

    LOOP AT ME->ITENS ASSIGNING FIELD-SYMBOL(<FS_ITEM_R>).
      CLEAR:
      <FS_ITEM_R>-DELIV_NUMB,
      <FS_ITEM_R>-DELIV_ITEM.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_NR_TRANSPORTE.
    IF ME->NOTA-TKNUM NE I_TKNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->NOTA-TKNUM = I_TKNUM.
  ENDMETHOD.


  METHOD SET_OBS_FINANCEIRA.

    IF ME->NOTA-OBS_FINANCEIRA NE I_OBS_FINANCEIRA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-OBS_FINANCEIRA = I_OBS_FINANCEIRA.

  ENDMETHOD.


  METHOD SET_PC_PARTINER.

    IF ME->NOTA-PC_PARTINER NE I_PC_PARTINER.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-PC_PARTINER = I_PC_PARTINER.

  ENDMETHOD.


  METHOD SET_PEDIDO_COMPRA.

    READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM ASSIGNING FIELD-SYMBOL(<FS_ITEM>).
    IF <FS_ITEM>-EBELN NE I_EBELN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    IF <FS_ITEM>-EBELN = I_EBELP.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    <FS_ITEM>-EBELN = I_EBELN.
    <FS_ITEM>-EBELP = I_EBELP.

  ENDMETHOD.


  METHOD SET_PLACA_CAV.

    IF me->nota-placa_cav NE i_placa_cav.
      me->ck_alterou = abap_true.
    ENDIF.

    me->nota-placa_cav = i_placa_cav.

  ENDMETHOD.


  METHOD SET_REGISTRO_OUTROS.

    SELECT SINGLE * INTO @DATA(WA_NFE_INBOUND)
      FROM ZIB_NFE_DIST_TER
     WHERE FORNE_CNPJ EQ @I_FORNE_CNPJ
       AND FORNE_CPF  EQ @I_FORNE_CPF
       AND FORNE_IE   EQ @I_FORNE_IE
       AND NUMERO     EQ @I_NUMERO
       AND SERIE      EQ @I_SERIE.

    IF SY-SUBRC IS NOT INITIAL.
      "042  Não Encontrado NF-e com: Forn: &1-&2 Número: &3-&4!
      MESSAGE S042 WITH I_FORNE_CNPJ I_FORNE_IE I_NUMERO I_SERIE.

      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGTY  = SY-MSGTY
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.

    ENDIF.

    ME->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO   = WA_NFE_INBOUND-CHAVE_NFE ).

  ENDMETHOD.


  METHOD SET_REINICIA_ROMANEIO.

    DATA: l_seqcam      TYPE numc3,
*          l_nrosol      TYPE numc5, " RIM CS1029457 ANB 30.09.2022
          l_nrosol      TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
          l_seq         TYPE numc3,
          l_filial_resp TYPE vkbur,
          l_docdat_nf   TYPE j_1bdocdat,
          l_netwr       TYPE znetwr.

    CLEAR: l_docdat_nf,
           l_netwr.

*------------------------------------
* desmenbra autor ermbarque
*------------------------------------
    me->set_validar_aut_embarque(
        EXPORTING i_aut_embarque = i_aut_embarque
        IMPORTING e_seq_cam      = l_seqcam
                  e_nro_sol      = l_nrosol
                  e_seq          = l_seq
                  e_filial_resp  = l_filial_resp ).

    CHECK l_nrosol IS NOT INITIAL.

*------------------------------------
* procura romaneio
*------------------------------------
    SELECT ch_referencia
      INTO @DATA(l_ch_referencia)
      FROM zsdt0138
        UP TO 1 ROWS
     WHERE seq_cam     = @l_seqcam
       AND nro_sol     = @l_nrosol
       AND seq         = @l_seq
       AND filial_resp = @l_filial_resp.
    ENDSELECT.

    CHECK sy-subrc = 0.

*------------------------------------
* verifica status romaneio
*------------------------------------
    SELECT st_proc
      INTO @DATA(l_st_proc)
      FROM zsdt0001
        UP TO 1 ROWS
     WHERE ch_referencia = @l_ch_referencia.
    ENDSELECT.

    CHECK sy-subrc = 0.
    CHECK l_st_proc = abap_off.

*------------------------------------
* desvincula zsdt0001 x zsdt0138
*------------------------------------
    DELETE FROM zsdt0001 WHERE ch_referencia = l_ch_referencia.

    UPDATE      zsdt0138   SET ch_referencia = abap_off
                               nfenum        = abap_off
                               series        = abap_off
                               netwr         = l_netwr
                               docdat_nf     = l_docdat_nf
                         WHERE seq_cam       = l_seqcam
                           AND nro_sol       = l_nrosol
                           AND seq           = l_seq
                           AND filial_resp   = l_filial_resp.

  ENDMETHOD.


  METHOD SET_SALVAR.

    DELETE FROM zib_nfe_dist_lca
     WHERE cd_lote_item IN ( SELECT l~cd_lote_item FROM zib_nfe_dist_lot AS l WHERE l~chave_nfe EQ me->nota-chave_nfe ).
    DELETE FROM zib_nfe_dist_lot WHERE chave_nfe EQ me->nota-chave_nfe.

    DELETE FROM zib_nfe_dist_ret WHERE chave_nfe EQ me->nota-chave_nfe.
    DELETE FROM zib_nfe_dist_lot WHERE chave_nfe EQ me->nota-chave_nfe.
    DELETE FROM zib_nfe_dist_ped WHERE chave_nfe EQ me->nota-chave_nfe.
    DELETE FROM zib_nfe_dist_frt WHERE chave_nfe EQ me->nota-chave_nfe.
    DELETE FROM zib_nfe_dist_itm WHERE chave_nfe EQ me->nota-chave_nfe.
    "
    COMMIT WORK.
    "
    LOOP AT me->pedidos ASSIGNING FIELD-SYMBOL(<pedido>).
      <pedido>-chave_nfe = me->nota-chave_nfe.
    ENDLOOP.

    LOOP AT me->dados_retorno ASSIGNING FIELD-SYMBOL(<retorno>).
      <retorno>-chave_nfe = me->nota-chave_nfe.
    ENDLOOP.

    LOOP AT me->lotes ASSIGNING FIELD-SYMBOL(<lotes>).
      <lotes>-chave_nfe = me->nota-chave_nfe.
    ENDLOOP.

    "Tabela de Dados de Importação de NF-e de Terceiro
*---CS2019001896 - 05.01.2021 - inicio
    me->set_validar_aut_embarque(
        EXPORTING i_aut_embarque       = me->nota-aut_embarque i_ultimo = abap_true
        IMPORTING e_aut_embarque_saida = me->nota-aut_embarque ).
*---CS2019001896 - 05.01.2021 - fim

    MODIFY zib_cte_dist_ter FROM me->cte.

    MODIFY zib_nfe_dist_ter FROM me->nota.

    me->frete-chave_nfe = me->nota-chave_nfe.
    "Tabela de Dados de Importação de NF-e de Terceiro - Frete
    MODIFY zib_nfe_dist_frt FROM me->frete.

    "Itens da Nota Fiscal Eletrônica
    MODIFY zib_nfe_dist_itm FROM TABLE me->itens.

    "Tabela de Dados de Importação de NF-e de Terceiro - Pedidos
    MODIFY zib_nfe_dist_ped FROM TABLE me->pedidos.

    "Lotes
    MODIFY zib_nfe_dist_lot FROM TABLE me->lotes.

    "Características do Lote
    MODIFY zib_nfe_dist_lca FROM TABLE me->lotes_caracteristicas.

    "Informações de Vinculo de Saída de Armazenagem
    MODIFY zib_nfe_dist_ret FROM TABLE me->dados_retorno.

    COMMIT WORK.

    me->ck_alterou_lotes = abap_false.
    me->ck_alterou = abap_false.

  ENDMETHOD.


  METHOD SET_ST_ARMAZEM.
    IF ME->NOTA-ST_ARMAZEM NE I_ST_ARMAZEM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ST_ARMAZEM = I_ST_ARMAZEM.
  ENDMETHOD.


  METHOD SET_ST_DOCUMENTO.

    IF ME->NOTA-ST_DOCUMENTO NE I_ST_DOCUMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ST_DOCUMENTO = I_ST_DOCUMENTO.

  ENDMETHOD.


  METHOD SET_ST_FISCAL.

    IF ME->NOTA-ST_FISCAL NE I_ST_FISCAL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ST_FISCAL = I_ST_FISCAL.

  ENDMETHOD.


  METHOD SET_ST_FISICO.
    IF ME->NOTA-ST_FISICO NE I_ST_FISICO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-ST_FISICO = I_ST_FISICO.
  ENDMETHOD.


  METHOD SET_TP_COMPRA_FUTURA.

    IF ME->NOTA-TP_COMPRA_FUTURA NE I_TP_COMPRA_FUTURA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-TP_COMPRA_FUTURA = I_TP_COMPRA_FUTURA.

  ENDMETHOD.


  METHOD SET_TRANSPORTADORA.
    IF ME->NOTA-F_TRANSPORTE NE I_F_TRANSPORTE.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    IF I_F_TRANSPORTE IS NOT INITIAL.
      R_LFA1 = ME->GET_INFO_FORNECEDOR( I_LIFNR = I_F_TRANSPORTE ).
      ME->NOTA-F_TRANSPORTE = R_LFA1-LIFNR.
    ELSE.
      CLEAR: R_LFA1, ME->NOTA-F_TRANSPORTE.
    ENDIF.

  ENDMETHOD.


  METHOD SET_VALIDAR_AUT_EMBARQUE.

    DATA: l_char10       TYPE char10, " RIM CS1029457 ANB 30.09.2022
          l_char3        TYPE char3,
          l_char3b       TYPE char3,
*          l_nrosol       TYPE numc5, " RIM CS1029457 ANB 30.09.2022
          l_nrosol       TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
          l_seqcam       TYPE numc3,
          l_others       TYPE zde_aut_embarque,
          l_others_aux   TYPE zde_aut_embarque,
          l_seq          TYPE numc3,
          l_filial_resp  TYPE vkbur,
          l_vkbur        TYPE vkbur,
          l_flg_138      TYPE c,
          l_total        TYPE zsdt0138-qtd_embarq,
          l_saldo        TYPE zsdt0082-qte_lib,
          w_tab_embarque TYPE zsds060,
          w_tab_embsugst TYPE zsds060,
          t_zsdt0082     TYPE TABLE OF zsdt0082,
          t_zsdt0138     TYPE TABLE OF zsdt0138,
          t_zsdt0138_aux TYPE TABLE OF zsdt0138,
          w_zsdt0082     TYPE zsdt0082,
          w_zsdt0138     TYPE zsdt0138.

    FREE: e_nro_sol,
          e_seq,
          e_seq_cam,
          e_filial_resp,
          r_aut_embarque,
          l_vkbur,
          l_flg_138,
          l_total,
          l_saldo,
          e_aut_embarque_saida,
          w_tab_embarque,
          w_tab_embsugst,
          t_tab_embarque,
          t_tab_embsugst.

    CHECK i_aut_embarque IS NOT INITIAL.

    CONDENSE i_aut_embarque.

    SPLIT i_aut_embarque  AT '/' INTO l_char10 l_others.
    SPLIT l_others        AT '-' INTO l_char3  l_others.
    SPLIT l_others        AT ' ' INTO l_char3b l_filial_resp.

    l_nrosol      = l_char10.
    l_seq         = l_char3.
    l_seqcam      = l_char3b.

    IF l_nrosol      IS INITIAL OR
       l_seq         IS INITIAL OR
       l_seqcam      IS INITIAL OR
       l_filial_resp IS INITIAL.
*---------------------------------
*---- CHAVE INCOMPLETA
*---------------------------------
      SPLIT i_aut_embarque  AT ' ' INTO l_char10 l_filial_resp.

      l_nrosol      = l_char10.

      IF l_filial_resp IS INITIAL.
        l_filial_resp = 'TCOR'.
      ENDIF.

      SELECT SINGLE vkbur
               INTO l_vkbur
               FROM tvbur
              WHERE vkbur = l_filial_resp.

      CHECK sy-subrc = 0.

      SELECT *
        FROM zsdt0082
        INTO TABLE t_zsdt0082
       WHERE nro_sol = l_nrosol.

      DELETE t_zsdt0082 WHERE dt_canc  IS NOT INITIAL.
      DELETE t_zsdt0082 WHERE dt_liber IS     INITIAL.

      IF t_zsdt0082[] IS NOT INITIAL.
        SELECT *
          FROM zsdt0138
          INTO TABLE t_zsdt0138
           FOR ALL ENTRIES IN t_zsdt0082
         WHERE nro_sol     = t_zsdt0082-nro_sol
           AND seq         = t_zsdt0082-seq
           AND filial_resp = l_filial_resp.

        t_zsdt0138_aux[] = t_zsdt0138[].

        DELETE t_zsdt0138 WHERE dt_canc  IS NOT INITIAL.
      ENDIF.

      LOOP AT t_zsdt0082 INTO w_zsdt0082.
        FREE: l_total.

        LOOP AT t_zsdt0138 INTO w_zsdt0138 WHERE nro_sol = w_zsdt0082-nro_sol
                                             AND seq     = w_zsdt0082-seq.
          l_total = l_total + w_zsdt0138-qtd_embarq.
        ENDLOOP.
        l_saldo = w_zsdt0082-qte_lib - l_total.

        IF l_saldo > 0.
          LOOP AT t_zsdt0138 INTO w_zsdt0138 WHERE nro_sol = w_zsdt0082-nro_sol
                                               AND seq     = w_zsdt0082-seq.
            MOVE-CORRESPONDING w_zsdt0138 TO w_tab_embarque.
            MOVE l_saldo                  TO w_tab_embarque-saldo.
            APPEND w_tab_embarque         TO t_tab_embarque.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      SORT t_tab_embarque BY seq     DESCENDING
                             seq_cam DESCENDING.

      READ TABLE t_tab_embarque INTO w_tab_embarque INDEX 1.

      IF sy-subrc <> 0.
        SORT t_zsdt0082     BY nro_sol seq     DESCENDING.
        SORT t_zsdt0138     BY nro_sol seq     DESCENDING
                                       seq_cam DESCENDING.
        SORT t_zsdt0138_aux BY nro_sol seq     DESCENDING
                                       seq_cam DESCENDING.

        READ TABLE t_zsdt0082     INTO w_zsdt0082 INDEX 1.
        READ TABLE t_zsdt0138     INTO w_zsdt0138 INDEX 1.
        IF sy-subrc = 0.
          l_flg_138 = abap_true.
        ELSE.
          READ TABLE t_zsdt0138_aux INTO w_zsdt0138 INDEX 1.
        ENDIF.

        w_tab_embsugst-nro_sol       = w_zsdt0082-nro_sol.
        w_tab_embsugst-seq           = w_zsdt0082-seq.
        IF l_flg_138 = abap_true.
          w_tab_embsugst-seq_cam     = w_zsdt0138-seq_cam.
        ELSE.
          w_tab_embsugst-seq_cam     = w_zsdt0138-seq_cam + 1.
        ENDIF.
        w_tab_embsugst-filial_resp   = l_filial_resp.
        w_tab_embsugst-saldo         = w_zsdt0082-qte_lib.
        w_tab_embarque               = w_tab_embsugst.
        APPEND w_tab_embsugst       TO t_tab_embsugst.
      ENDIF.

*---------------------------------
*---- atribuicao saida
*---------------------------------

*      ADD 1 TO w_tab_embarque-seq_cam.

      SELECT MAX( seq_cam )
        FROM zsdt0138
        INTO e_seq_cam
       WHERE nro_sol     = w_tab_embarque-nro_sol
         AND seq         = w_tab_embarque-seq
         AND filial_resp = w_tab_embarque-filial_resp.

      IF i_ultimo IS INITIAL.
        ADD 1 TO e_seq_cam.
      ENDIF.

*      e_seq_cam     = w_tab_embarque-seq_cam.
      e_nro_sol     = w_tab_embarque-nro_sol.
      e_seq         = w_tab_embarque-seq.
      e_filial_resp = w_tab_embarque-filial_resp.

      CONCATENATE e_seq_cam e_filial_resp
             INTO l_others_aux
        SEPARATED BY space.

      CONCATENATE e_nro_sol '/' e_seq '-' l_others_aux
             INTO e_aut_embarque_saida.

      CONCATENATE i_aut_embarque l_filial_resp
             INTO r_aut_embarque
        SEPARATED BY space.

    ELSE.

*---------------------------------
*---- CHAVE COMPLETA - valida escritorio de vendas
*---------------------------------
      SELECT SINGLE vkbur
               INTO l_vkbur
               FROM tvbur
              WHERE vkbur = l_filial_resp.

      CHECK sy-subrc = 0.

*---------------------------------
*---- atribuicao saida
*---------------------------------
      e_seq_cam     = l_seqcam.
      e_nro_sol     = l_nrosol.
      e_seq         = l_seq.
      e_filial_resp = l_filial_resp.

      CONCATENATE l_seqcam l_filial_resp
             INTO l_others_aux
        SEPARATED BY space.

      CONCATENATE l_nrosol '/' l_seq '-' l_others_aux
             INTO r_aut_embarque.

      e_aut_embarque_saida       = r_aut_embarque.

      w_tab_embarque-nro_sol     = e_nro_sol.
      w_tab_embarque-seq         = e_seq.
      w_tab_embarque-seq_cam     = e_seq_cam.
      w_tab_embarque-filial_resp = e_filial_resp.
      APPEND w_tab_embarque     TO t_tab_embarque.
    ENDIF.

    IF t_tab_embarque[] IS INITIAL.
      e_nro_sol = l_nrosol.
    ENDIF.

  ENDMETHOD.


  METHOD SET_VLR_DESCONTO.

    IF ME->NOTA-VLR_DESCONTO NE I_VLR_DESCONTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->NOTA-VLR_DESCONTO = I_VLR_DESCONTO.

  ENDMETHOD.


  METHOD VALIDA_ATRIBUTO_ALTERAVEL_LOTE.

    DATA: LC_LOTE_EXISTE TYPE C LENGTH 1.

    R_PERMITIDO = ABAP_FALSE.

*    IF "( ME->CK_COLETA_TUDO EQ ABAP_TRUE ) AND
*       ( ME->NOTA-ST_DOCUMENTO NE ZCL_NFE_INBOUND=>ST_DOCUMENTO_00 ).
*      EXIT.
*    ENDIF.
*
*    IF ME->NOTA-ST_FISICO NE ZCL_NFE_INBOUND=>ST_FISICO_00.
*      EXIT.
*    ENDIF.

    IF ME->NOTA-ST_FISICO	NE ZCL_NFE_INBOUND=>ST_FISICO_00.                  " 00
      EXIT.
    ENDIF.

    CHECK ( I_PROD_ITEM IS NOT INITIAL ) OR ( I_EBELN IS NOT INITIAL AND I_EBELP IS NOT INITIAL ).

    CHECK I_CAMPO NE 'CUOBJ'.
    CHECK I_CAMPO NE 'CLASS'.
    CHECK I_CAMPO NE 'KLART'.
    CHECK I_CAMPO NE 'CLINT'.

    IF NOT
       ( ( I_CAMPO EQ 'CHARG' ) OR
         ( I_CAMPO EQ 'VFDAT' ) OR
         ( I_CAMPO EQ 'LICHA' ) OR
         ( I_CAMPO EQ 'HERKL' ) OR
         ( I_CAMPO EQ 'HSDAT' ) OR
         ( I_CAMPO EQ 'ENDIF' ) OR
         ( I_CAMPO EQ 'MENGE' ) ).
      EXIT.
    ENDIF.

    IF I_CAMPO EQ 'MENGE' .
      R_PERMITIDO = ABAP_TRUE.
      EXIT.
    ENDIF.

    IF I_EBELN IS INITIAL OR I_EBELP IS INITIAL.
      READ TABLE ME->ITENS WITH KEY PROD_ITEM = I_PROD_ITEM INTO DATA(WA_ITEM).
    ELSE.
      WA_ITEM-EBELN = I_EBELN.
      WA_ITEM-EBELP = I_EBELP.
    ENDIF.
    "Busca item do Pedido de Compra
    SELECT SINGLE * INTO @DATA(WA_EKET) FROM EKET WHERE EBELN EQ @WA_ITEM-EBELN AND EBELP EQ @WA_ITEM-EBELP.

    IF WA_EKET-CHARG IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF I_EBELN IS INITIAL OR I_EBELP IS INITIAL.
      READ TABLE ME->LOTES WITH KEY PROD_ITEM = I_PROD_ITEM CHARG = I_LOTE TRANSPORTING NO FIELDS.
    ELSE.
      READ TABLE ME->LOTES WITH KEY EBELN = I_EBELN EBELP = I_EBELP CHARG = I_LOTE TRANSPORTING NO FIELDS.
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    IF I_LOTE IS INITIAL.
      IF I_CAMPO EQ 'CHARG'.
        R_PERMITIDO = ABAP_TRUE.
        EXIT.
      ELSE.
        R_PERMITIDO = ABAP_FALSE.
        EXIT.
      ENDIF.
    ELSE.

      TRY.
          IF I_LOTE IS NOT INITIAL.
            CALL METHOD ZCL_CHARG=>GET_CHARG
              EXPORTING
                I_MATNR = WA_ITEM-MATNR
                I_CHARG = I_LOTE
              RECEIVING
                R_MCH1  = DATA(R_MCH1).
*            LC_LOTE_EXISTE = ABAP_TRUE.
            LC_LOTE_EXISTE = ABAP_FALSE.
          ELSE.
            LC_LOTE_EXISTE = ABAP_FALSE.
          ENDIF.

        CATCH ZCX_CHARG_EXCEPTION .
          LC_LOTE_EXISTE = ABAP_FALSE.
      ENDTRY.

      IF LC_LOTE_EXISTE EQ ABAP_FALSE.
        R_PERMITIDO = ABAP_TRUE.
        EXIT.
      ELSE.
        IF I_CAMPO EQ 'CHARG'.
          R_PERMITIDO = ABAP_TRUE.
          EXIT.
        ELSE.
          R_PERMITIDO = ABAP_FALSE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~CHECK_ENV_APROV_TAXA.

    DATA: lva_date       TYPE sy-datum,
          lva_date_30    TYPE sy-datum,
          lva_gdatu_base TYPE char10,
          lva_gdatu      TYPE tcurr-gdatu,
          lva_tax        TYPE tcurr-ukurs,
          lva_media_tax  TYPE tcurr-ukurs,
          lva_nome_tvarv TYPE tvarvc-name,
          lva_tolera     TYPE tcurr-ukurs,
          lva_result     TYPE tvarvc-low,
          lva_desvio     TYPE zmmt0149-desvio,
          lva_count      TYPE i,
          lva_msg_exibir TYPE string,
          lva_answer     TYPE c,
          lwa_zmmt0149   TYPE zmmt0149,
          it_zmmt0149    TYPE TABLE OF zmmt0149.

    me->ck_taxa_aprovacao  = abap_false.

    CHECK sy-tcode = 'ZMM0110'.

    e_validou = abap_false.

    IF me->ck_estornando_fisico = abap_false.

      SELECT * INTO TABLE it_zmmt0149
        FROM zmmt0149
       WHERE chave_nfe EQ me->nota-chave_nfe
       AND   status NE 'X'.

      CLEAR lwa_zmmt0149.
      READ TABLE it_zmmt0149 INTO lwa_zmmt0149 INDEX 1.


      IF lwa_zmmt0149-status = 'L' .
        me->ck_taxa_aprovacao = abap_true.
        MESSAGE 'Taxa do USD em aprovação' TYPE 'I'.
      ELSE.

        IF lwa_zmmt0149-status = 'A' AND
           lwa_zmmt0149-info_wkurs = me->nota-ctr_wkurs.
          me->ck_taxa_aprovacao = abap_false.
          EXIT.
        ELSE.
          IF  lwa_zmmt0149-status = 'R' .
            me->ck_taxa_aprovacao = abap_true.
            MESSAGE 'Taxa do USD Aprovação foi Rejeitada' TYPE 'I'.
          ENDIF.

* busca dados tvarv
          lva_nome_tvarv = 'Z_MAGGI_ZMM0110_' && me->nota-bukrs.

          CLEAR lva_result.
          SELECT SINGLE low INTO lva_result
                 FROM tvarvc
                  WHERE name = lva_nome_tvarv.

          if sy-subrc eq 0.
            REPLACE ALL OCCURRENCES OF ',' IN lva_result WITH '.'.
            CONDENSE lva_result NO-GAPS.
            MOVE lva_result TO lva_tolera.
          else.
            lva_tolera = 9999.
          endif.

* Busca Taxa Dólar média 30 dias
          lva_date = me->nota-dt_emissao.
          lva_date_30 =  me->nota-dt_emissao - 30.

          WHILE lva_date >=  lva_date_30.

            lva_gdatu_base = |{ lva_date+6(2) }.{ lva_date+4(2) }.{ lva_date+0(4) }|.

            CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
              EXPORTING
                input  = lva_gdatu_base    " Entered date (screen)
              IMPORTING
                output = lva_gdatu.  " Technical date in invert.

            SELECT SINGLE * FROM tcurr
              INTO @DATA(lwa_tcurr)
            WHERE fcurr EQ 'USD'
              AND tcurr EQ 'BRL'
              AND kurst EQ 'B'.

            IF ( sy-subrc = 0 ).
              lva_tax = lva_tax + lwa_tcurr-ukurs.
              lva_count = lva_count + 1.
            ENDIF.

            lva_date = ( lva_date - 1 ).

          ENDWHILE.

* Média ultimos 30 dias
          lva_media_tax = lva_tax / lva_count.

* Percentual Desvio
          lva_desvio =  ( ( lva_media_tax - me->nota-ctr_wkurs ) /  lva_media_tax ) * 100 .

          IF abs( lva_desvio ) >  lva_tolera.

            IF lwa_zmmt0149-info_wkurs > 0 AND
              lwa_zmmt0149-info_wkurs NE me->nota-ctr_wkurs AND
              lwa_zmmt0149-status = 'A'.
              CONCATENATE 'Taxa de Cambio com desvio maior que TOLERÂNCIA e diferente da aprovada'
                            'Confirma envio para aprovação?'
                              INTO lva_msg_exibir SEPARATED BY space.
            ELSE.
              CONCATENATE 'Taxa de Cambio com desvio maior que TOLERÂNCIA,'
                           'Confirma envio para aprovação?'
                             INTO lva_msg_exibir SEPARATED BY space.
            ENDIF.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Confirmação'
                text_question         = lva_msg_exibir
                text_button_1         = 'Sim'
                text_button_2         = 'Não'
                default_button        = '1'
                display_cancel_button = ''
              IMPORTING
                answer                = lva_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF  lva_answer = '1'.

              me->ck_taxa_aprovacao  = abap_true.

              e_validou = abap_true.
              lwa_zmmt0149-bukrs       =  me->nota-bukrs.
              lwa_zmmt0149-chave_nfe   =  me->nota-chave_nfe.
              lwa_zmmt0149-status      = 'L'.
              lwa_zmmt0149-info_wkurs  =  me->nota-ctr_wkurs.
              lwa_zmmt0149-calc_wkurs  =  lva_media_tax .
              lwa_zmmt0149-desvio      =  lva_desvio.
              lwa_zmmt0149-data_atual  =  sy-datum.
              lwa_zmmt0149-hora_atual  =  sy-uzeit.
              lwa_zmmt0149-usuario     =  sy-uname.

              MODIFY zmmt0149 FROM lwa_zmmt0149.
              COMMIT WORK.
              "
              UPDATE zib_nfe_dist_ter SET ctr_valor_total = me->nota-ctr_valor_total
                                          ctr_wkurs       = me->nota-ctr_wkurs
              WHERE  chave_nfe   =  me->nota-chave_nfe.
              COMMIT WORK.

            ELSE.
              me->ck_taxa_aprovacao  = abap_true.
            ENDIF.
          ELSE.
*            DELETE FROM zmmt0149 WHERE
*            chave_nfe   =  me->nota-chave_nfe.
            UPDATE zmmt0149 SET status    = 'X'
            WHERE  chave_nfe   =  me->nota-chave_nfe.
            COMMIT WORK.
          ENDIF.
        ENDIF.
        "ENDIF.
      ENDIF.
    ELSE.
*      DELETE FROM zmmt0149 WHERE
*       chave_nfe   =  me->nota-chave_nfe.
      UPDATE zmmt0149 SET status    = 'X'
      WHERE  chave_nfe   =  me->nota-chave_nfe.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_CADASTRO~EXCLUIR_REGISTRO.
    I_EXCLUIU = ABAP_FALSE.

    IF ME->VALIDAR_EXCLUSAO( ) EQ ABAP_TRUE.

      DELETE FROM ZIB_NFE_DIST_LCA
       WHERE CD_LOTE_ITEM IN ( SELECT L~CD_LOTE_ITEM FROM ZIB_NFE_DIST_LOT AS L WHERE L~CHAVE_NFE EQ ME->NOTA-CHAVE_NFE ).

      DELETE FROM ZIB_NFE_DIST_LOT WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.
      DELETE FROM ZIB_NFE_DIST_ITM WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.
      DELETE FROM ZIB_NFE_DIST_LOG WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.
      DELETE FROM ZIB_NFE_DIST_TER WHERE CHAVE_NFE EQ ME->NOTA-CHAVE_NFE.
      COMMIT WORK.

      I_EXCLUIU = ABAP_TRUE.
      MESSAGE S031.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_CADASTRO~GET_REGISTRO.

    DATA: WA_REGISTRO TYPE ZIB_NFE_DIST_TER.
    WA_REGISTRO = ME->NOTA.
    MOVE-CORRESPONDING WA_REGISTRO TO E_REGISTRO.

  ENDMETHOD.


  METHOD zif_cadastro~gravar_registro.

    DATA: i_fator	        TYPE zde_nfe_fator,
          cx_erro         TYPE REF TO zcx_nfe_inbound_exception,
          cx_erro_pedi    TYPE REF TO zcx_pedido_compra_exception,
          cx_erro_migo    TYPE REF TO zcx_migo_exception,
          cx_erro_charg   TYPE REF TO zcx_charg_exception,
          cx_erro_miro    TYPE REF TO zcx_miro_exception,
          cx_erro_aviso   TYPE REF TO zcx_delivery,
          lc_cd_lote_item TYPE zde_cd_lote_item,
          lc_validou      TYPE char1.

    i_gravou = abap_false.

    IF me->ck_alterou EQ abap_true.

*-CS2021000662 - 19.11.2021 - JT - inicio
      IF me->ck_retorno_sem_ajuste = abap_false.
        lc_validou = me->validar_registro( ).
      ELSE.
        lc_validou = abap_true.
      ENDIF.

*     IF me->validar_registro( ) EQ abap_true.
      IF lc_validou EQ abap_true.
*-CS2021000662 - 19.11.2021 - JT - fim

        IF me->ck_aceite_fiscal EQ abap_true.

          "Grava de para de produto sap produto fornecedor
*          LOOP AT me->itens INTO DATA(wa_item).
*            IF wa_item-prod_qtd_comerci IS NOT INITIAL.
*              i_fator = wa_item-menge / wa_item-prod_qtd_comerci.
*            ELSE.
*              i_fator = 1.
*            ENDIF.
*            CALL METHOD me->set_material
*              EXPORTING
*                i_emissor      = me->nota-p_emissor
*                i_prod_codigo  = wa_item-prod_codigo
*                i_unidade_item = wa_item-prod_und_comerci
*                i_matnr        = wa_item-matnr
*                i_meins        = wa_item-meins
*                i_fator        = i_fator.
*          ENDLOOP.

          "Neste caso não existe MIGO, solicita MIRO no Aceite da Fiscal
*          IF me->nota-tp_compra_futura EQ zcl_nfe_inbound=>tp_compra_futura_fatura.
*            TRY .
*                "me->nfe_sol_miro_softexpert( ).
*                "me->set_nfe_revisada( i_usuario_revisor = sy-uname ).
*              CATCH zcx_nfe_inbound_exception INTO cx_erro.
*                CALL METHOD me->set_add_log_nfe
*                  EXPORTING
*                    i_type         = sy-msgty
*                    i_id           = cx_erro->if_t100_message~t100key-msgid
*                    i_num          = cx_erro->if_t100_message~t100key-msgno
*                    i_message_v1   = cx_erro->msgv1
*                    i_message_v2   = cx_erro->msgv2
*                    i_message_v3   = cx_erro->msgv3
*                    i_message_v4   = cx_erro->msgv4
*                  CHANGING
*                    p_lc_sequencia = lc_sequencia.
*                me->nfe_inbound_gravar_log( ).
*                COMMIT WORK.
*                RAISE EXCEPTION TYPE zcx_cadastro
*                  EXPORTING
*                    textid = cx_erro->if_t100_message~t100key
*                    msgty  = 'E'
*                    msgv1  = CONV #( cx_erro->if_t100_message~t100key-attr1 )
*                    msgv2  = CONV #( cx_erro->if_t100_message~t100key-attr2 )
*                    msgv3  = CONV #( cx_erro->if_t100_message~t100key-attr3 )
*                    msgv4  = CONV #( cx_erro->if_t100_message~t100key-attr4 ).
*            ENDTRY.
*          ENDIF.

          me->set_st_documento( i_st_documento = me->st_documento_01 ).
          me->set_st_fiscal( i_st_fiscal = me->st_fiscal_com_aceite_fiscal ).
          me->set_ck_fiscal( i_check = abap_true ).

*          CASE me->nota-f_armazem.
*            WHEN space.
*              me->set_ck_armazem( i_check = abap_false ).
*            WHEN OTHERS.
*              me->set_ck_armazem( i_check = abap_true ).
*          ENDCASE.

          me->set_arm_envio_aceite( ).

          me->set_calc_total_moeda_empresa( IMPORTING e_valores_pedido = DATA(e_valores_pedido) ).
          me->nota-ctr_waers       = e_valores_pedido-ctr_waers.
          me->nota-ctr_wkurs       = e_valores_pedido-ctr_wkurs.
          me->nota-ctr_kufix       = e_valores_pedido-ctr_kufix.
          me->nota-ctr_sinal       = e_valores_pedido-ctr_sinal.
          me->nota-ctr_zterm       = e_valores_pedido-ctr_zterm.
          me->nota-ctr_valor_total = e_valores_pedido-ctr_valor_total.
          me->nota-ctr_valor_total_liquido = e_valores_pedido-ctr_valor_total_liquido.

*          LOOP AT me->itens ASSIGNING FIELD-SYMBOL(<fs_itens>).
*            me->set_calc_total_moeda_emp_item( EXPORTING i_item = <fs_itens> IMPORTING e_valores_pedido = e_valores_pedido ).
*            <fs_itens>-ctr_waers       = e_valores_pedido-ctr_waers.
*            <fs_itens>-ctr_wkurs       = e_valores_pedido-ctr_wkurs.
*            <fs_itens>-ctr_kufix       = e_valores_pedido-ctr_kufix.
*            <fs_itens>-ctr_sinal       = e_valores_pedido-ctr_sinal.
*            <fs_itens>-ctr_zterm       = e_valores_pedido-ctr_zterm.
*            <fs_itens>-ctr_valor_total = e_valores_pedido-ctr_valor_total.
*            <fs_itens>-ctr_valor_total_liquido = e_valores_pedido-ctr_valor_total_liquido.
*          ENDLOOP.

          DELETE FROM zib_cte_dist_ter WHERE cd_chave_cte EQ me->cte-cd_chave_cte.
          DELETE FROM zib_nfe_dist_itm WHERE chave_nfe EQ me->nota-chave_nfe.
          DELETE FROM zib_nfe_dist_ped WHERE chave_nfe EQ me->nota-chave_nfe.
          MODIFY zib_cte_dist_ter FROM me->cte.
          MODIFY zib_nfe_dist_ped FROM TABLE me->pedidos.
          "MODIFY zib_nfe_dist_itm FROM TABLE me->itens.
          MODIFY zib_nfe_dist_ter FROM me->nota.
          me->nfe_inbound_gravar_log( ).
          COMMIT WORK.

        ENDIF.

*        IF me->ck_aceite_fisico EQ abap_true.
*
*          IF me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_fatura.
*            "Grava Informações de Lote Digitado
*            IF me->ck_alterou_lotes EQ abap_true.
*              LOOP AT me->lotes ASSIGNING FIELD-SYMBOL(<fs_lote>).
*                IF <fs_lote>-cd_lote_item(1) EQ '$'.
*                  CALL FUNCTION 'NUMBER_GET_NEXT'
*                    EXPORTING
*                      nr_range_nr             = '01'
*                      object                  = 'ZNFEIN'
*                    IMPORTING
*                      number                  = lc_cd_lote_item
*                    EXCEPTIONS
*                      interval_not_found      = 1
*                      number_range_not_intern = 2
*                      object_not_found        = 3
*                      quantity_is_0           = 4
*                      quantity_is_not_1       = 5
*                      interval_overflow       = 6
*                      buffer_overflow         = 7
*                      OTHERS                  = 8.
*
*                  IF sy-subrc IS NOT INITIAL.
*                    RAISE EXCEPTION TYPE zcx_cadastro
*                      EXPORTING
*                        textid = VALUE #( msgid = sy-msgid
*                                          msgno = sy-msgno
*                                          attr1 = CONV #( sy-msgv1 )
*                                          attr2 = CONV #( sy-msgv2 )
*                                          attr3 = CONV #( sy-msgv3 )
*                                          attr4 = CONV #( sy-msgv4 ) )
*                        msgid  = sy-msgid
*                        msgno  = sy-msgno
*                        msgty  = 'E'
*                        msgv1  = sy-msgv1
*                        msgv2  = sy-msgv2
*                        msgv3  = sy-msgv3
*                        msgv4  = sy-msgv4.
*                  ENDIF.
*                ELSE.
*                  lc_cd_lote_item = <fs_lote>-cd_lote_item.
*                ENDIF.
*
*                LOOP AT me->lotes_caracteristicas ASSIGNING FIELD-SYMBOL(<fs_carac>) WHERE cd_lote_item EQ <fs_lote>-cd_lote_item.
*                  <fs_carac>-cd_lote_item = lc_cd_lote_item.
*                ENDLOOP.
*
*                <fs_lote>-cd_lote_item = lc_cd_lote_item.
*              ENDLOOP.
*
*              me->set_salvar( ).
*            ENDIF. " Se Alterou Lotes
*
*            "Caso seja armazenagem não é necessário criar o lote, pois o retorno é no mesmo lote de onde saiu.
*            IF me->get_ck_cfop_retorno_armazena( ) EQ abap_false.
*              TRY.
*                  "Criar Lote
*                  me->nfe_inbound_cria_lotes( ).
*                CATCH zcx_charg_exception INTO cx_erro_charg.
*
*                  me->nfe_inbound_gravar_log( ).
*
*                  RAISE EXCEPTION TYPE zcx_cadastro
*                    EXPORTING
*                      textid = cx_erro_charg->if_t100_message~t100key
*                      msgid  = cx_erro_charg->if_t100_message~t100key-msgid
*                      msgno  = cx_erro_charg->if_t100_message~t100key-msgno
*                      msgty  = 'E'
*                      msgv1  = cx_erro_charg->msgv1
*                      msgv2  = cx_erro_charg->msgv2
*                      msgv3  = cx_erro_charg->msgv3
*                      msgv4  = cx_erro_charg->msgv4.
*              ENDTRY.
*            ENDIF.
*
*            "Gerar Remessa
*            IF me->nota-ck_possui_frete EQ abap_true OR me->ck_gerar_somente_aviso EQ abap_true.
*              TRY.
*                  DATA(r_gerou_remessa) = me->nfe_inbound_remessa( ).
*
*                  IF r_gerou_remessa EQ abap_true.
*                    CALL METHOD me->set_add_log_nfe
*                      EXPORTING
*                        i_type         = sy-msgty
*                        i_id           = sy-msgid
*                        i_num          = sy-msgno
*                        i_message_v1   = sy-msgv1
*                        i_message_v2   = sy-msgv2
*                        i_message_v3   = sy-msgv3
*                        i_message_v4   = sy-msgv4
*                      CHANGING
*                        p_lc_sequencia = lc_sequencia.
*
*                    IF me->nota-st_fisico EQ me->st_fisico_00.
*                      me->set_st_fisico( i_st_fisico = me->st_fisico_aviso_gerado ).
*                    ENDIF.
*                    me->set_salvar( ).
*                    me->nfe_inbound_gravar_log( ).
*                    COMMIT WORK.
*                  ELSE.
*
*                    MESSAGE s051 DISPLAY LIKE 'E'.
*                    sy-msgty = 'E'.
*
*                    CALL METHOD me->set_add_log_nfe
*                      EXPORTING
*                        i_type         = sy-msgty
*                        i_id           = sy-msgid
*                        i_num          = sy-msgno
*                        i_message_v1   = sy-msgv1
*                        i_message_v2   = sy-msgv2
*                        i_message_v3   = sy-msgv3
*                        i_message_v4   = sy-msgv4
*                      CHANGING
*                        p_lc_sequencia = lc_sequencia.
*
*                    me->nfe_inbound_gravar_log( ).
*                    COMMIT WORK.
*                    EXIT.
*
*                  ENDIF.
*
*                CATCH zcx_delivery INTO cx_erro_aviso.
*
*                  sy-msgty = 'E'.
*
*                  CALL METHOD me->set_add_log_nfe
*                    EXPORTING
*                      i_type         = sy-msgty
*                      i_id           = cx_erro_aviso->if_t100_message~t100key-msgid
*                      i_num          = cx_erro_aviso->if_t100_message~t100key-msgno
*                      i_message_v1   = cx_erro_aviso->msgv1
*                      i_message_v2   = cx_erro_aviso->msgv2
*                      i_message_v3   = cx_erro_aviso->msgv3
*                      i_message_v4   = cx_erro_aviso->msgv4
*                    CHANGING
*                      p_lc_sequencia = lc_sequencia.
*
*                  me->nfe_inbound_gravar_log( ).
*                  COMMIT WORK.
*
*                  RAISE EXCEPTION TYPE zcx_cadastro
*                    EXPORTING
*                      textid = cx_erro_aviso->if_t100_message~t100key
*                      msgid  = cx_erro_aviso->if_t100_message~t100key-msgid
*                      msgno  = cx_erro_aviso->if_t100_message~t100key-msgno
*                      msgty  = 'E'
*                      msgv1  = cx_erro_aviso->msgv1
*                      msgv2  = cx_erro_aviso->msgv2
*                      msgv3  = cx_erro_aviso->msgv3
*                      msgv4  = cx_erro_aviso->msgv4.
*              ENDTRY.
*            ENDIF. "Se Possui Frete
*
**-CS2021000662 - 19.11.2021 - JT - inicio
*            IF me->ck_gerar_somente_aviso NE abap_true OR me->ck_retorno_sem_ajuste = abap_true.
**-CS2021000662 - 19.11.2021 - JT - fim
*
**- PBI - 64541 - CSB - 30.11.2021. - Inicio
**              IF me->ck_taxa_aprovacao NE abap_true.
**- PBI - 64541 - CSB - 30.11.2021. - Fim
*              TRY.
*                  DATA r_gerou_migo TYPE abap_boolean.
*                  "DATA(r_gerou_migo) = me->nfe_inbound_doc_material( IMPORTING e_retorno = me->mensagens_retorno ).
*
*                  IF r_gerou_migo EQ abap_true.
*
*                    CALL METHOD me->set_add_log_nfe
*                      EXPORTING
*                        i_type         = sy-msgty
*                        i_id           = sy-msgid
*                        i_num          = sy-msgno
*                        i_message_v1   = sy-msgv1
*                        i_message_v2   = sy-msgv2
*                        i_message_v3   = sy-msgv3
*                        i_message_v4   = sy-msgv4
*                      CHANGING
*                        p_lc_sequencia = lc_sequencia.
*
*                    me->set_st_fisico( i_st_fisico = me->st_fisico_migo_gerada ).
*
*                    IF me->nota-docnum_nfe IS NOT INITIAL.
*                      me->set_ck_fisico( i_check = abap_true ).
*                      me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
*                      me->set_st_documento( i_st_documento = me->st_documento_99 ).
*                    ENDIF.
*
*                    me->set_salvar( ).
*                    me->nfe_inbound_gravar_log( ).
*                    COMMIT WORK.
*
*                  ELSE.
*
*                    me->set_salvar( ).
*                    MESSAGE s057 DISPLAY LIKE 'E'.
*
*                    sy-msgty = 'E'.
*
*                    CALL METHOD me->set_add_log_nfe
*                      EXPORTING
*                        i_type         = sy-msgty
*                        i_id           = sy-msgid
*                        i_num          = sy-msgno
*                        i_message_v1   = sy-msgv1
*                        i_message_v2   = sy-msgv2
*                        i_message_v3   = sy-msgv3
*                        i_message_v4   = sy-msgv4
*                      CHANGING
*                        p_lc_sequencia = lc_sequencia.
*
*                    me->nfe_inbound_gravar_log( ).
*                    COMMIT WORK.
*                    EXIT.
*                  ENDIF.
*
*                CATCH zcx_nfe_inbound_exception INTO cx_erro.
*
*                  CALL METHOD me->set_add_log_nfe
*                    EXPORTING
*                      i_type         = sy-msgty
*                      i_id           = cx_erro->if_t100_message~t100key-msgid
*                      i_num          = cx_erro->if_t100_message~t100key-msgno
*                      i_message_v1   = cx_erro->msgv1
*                      i_message_v2   = cx_erro->msgv2
*                      i_message_v3   = cx_erro->msgv3
*                      i_message_v4   = cx_erro->msgv4
*                    CHANGING
*                      p_lc_sequencia = lc_sequencia.
*
*                  me->nfe_inbound_gravar_log( ).
*                  COMMIT WORK.
*
*                  RAISE EXCEPTION TYPE zcx_cadastro
*                    EXPORTING
*                      textid = cx_erro->if_t100_message~t100key
*                      msgty  = 'E'
*                      msgv1  = CONV #( cx_erro->if_t100_message~t100key-attr1 )
*                      msgv2  = CONV #( cx_erro->if_t100_message~t100key-attr2 )
*                      msgv3  = CONV #( cx_erro->if_t100_message~t100key-attr3 )
*                      msgv4  = CONV #( cx_erro->if_t100_message~t100key-attr4 ).
*
*                CATCH zcx_pedido_compra_exception INTO cx_erro_pedi.
*
*                  CALL METHOD me->set_add_log_nfe
*                    EXPORTING
*                      i_type         = sy-msgty
*                      i_id           = cx_erro_pedi->if_t100_message~t100key-msgid
*                      i_num          = cx_erro_pedi->if_t100_message~t100key-msgno
*                      i_message_v1   = cx_erro_pedi->msgv1
*                      i_message_v2   = cx_erro_pedi->msgv2
*                      i_message_v3   = cx_erro_pedi->msgv3
*                      i_message_v4   = cx_erro_pedi->msgv4
*                    CHANGING
*                      p_lc_sequencia = lc_sequencia.
*
*                  me->nfe_inbound_gravar_log( ).
*                  COMMIT WORK.
*
*                  RAISE EXCEPTION TYPE zcx_cadastro
*                    EXPORTING
*                      textid = cx_erro_pedi->if_t100_message~t100key
*                      msgv1  = CONV #( cx_erro_pedi->if_t100_message~t100key-attr1 )
*                      msgv2  = CONV #( cx_erro_pedi->if_t100_message~t100key-attr2 )
*                      msgv3  = CONV #( cx_erro_pedi->if_t100_message~t100key-attr3 )
*                      msgv4  = CONV #( cx_erro_pedi->if_t100_message~t100key-attr4 ).
*
*                CATCH zcx_migo_exception INTO cx_erro_migo.
*
*                  CALL METHOD me->set_add_log_nfe
*                    EXPORTING
*                      i_type         = sy-msgty
*                      i_id           = cx_erro_migo->if_t100_message~t100key-msgid
*                      i_num          = cx_erro_migo->if_t100_message~t100key-msgno
*                      i_message_v1   = cx_erro_migo->msgv1
*                      i_message_v2   = cx_erro_migo->msgv2
*                      i_message_v3   = cx_erro_migo->msgv3
*                      i_message_v4   = cx_erro_migo->msgv4
*                    CHANGING
*                      p_lc_sequencia = lc_sequencia.
*
*                  me->nfe_inbound_gravar_log( ).
*                  COMMIT WORK.
*
*                  RAISE EXCEPTION TYPE zcx_cadastro
*                    EXPORTING
*                      textid = cx_erro_migo->if_t100_message~t100key
*                      msgv1  = CONV #( cx_erro_migo->if_t100_message~t100key-attr1 )
*                      msgv2  = CONV #( cx_erro_migo->if_t100_message~t100key-attr2 )
*                      msgv3  = CONV #( cx_erro_migo->if_t100_message~t100key-attr3 )
*                      msgv4  = CONV #( cx_erro_migo->if_t100_message~t100key-attr4 ).
*              ENDTRY.
*
*              "ME->NFE_INBOUND_SAIDA_ARMAZENAGEM( ).
*
**              ENDIF. "Validação ZMMT0149
*            ENDIF. "Se Somente Aviso
*          ELSE. "Não Gera Fisico
*            me->set_st_fisico( i_st_fisico = zcl_nfe_inbound=>st_fisico_01 ).
*          ENDIF.
*        ENDIF.



        IF sy-tcode NE 'ZMM0110'.
          me->ck_taxa_aprovacao  = abap_false.
        ENDIF.

        "Gerar MIRO
*        IF me->ck_aceite_faturar EQ abap_true AND
*           me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_mercadoria AND
*           me->ck_somente_validar_fatura EQ abap_false AND
*
*           me->ck_taxa_aprovacao EQ abap_false AND
*
*           "Retorno de Armazenagem não gera MIRO
*           me->get_ck_cfop_retorno_armazena( ) EQ abap_false.
*
*          TRY.
*              CLEAR me->at_simular.
*              DATA(r_gerou_miro) = me->nfe_inbound_fatura( ).
*
*              IF r_gerou_miro EQ abap_true.
*
*                me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
*                me->set_ck_fisico( i_check = abap_true ).
*                IF me->nota-docnum_nfe IS NOT INITIAL.
*
*                  me->set_st_documento( i_st_documento = me->st_documento_99 ).
*                  me->set_st_fiscal( i_st_fiscal = me->st_fiscal_com_aceite_fiscal ).
*
*                  CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
*                    EXPORTING
*                      i_docnum = me->nota-docnum_nfe.
*
*                ENDIF.
*
**-----CS2019001896 - 05.01.2021 - inicio
*                me->gravar_romaneio_fertilizante( ).
**-----CS2019001896 - 05.01.2021 - fim
*
*                me->set_salvar( ).
*                me->nfe_inbound_gravar_log( ).
*                COMMIT WORK.
*
*                TRY .
*                    "nfe_sol_anexa_miro_softexpert( ).
*                  CATCH zcx_nfe_inbound_exception INTO cx_erro.
*                    CALL METHOD me->set_add_log_nfe
*                      EXPORTING
*                        i_type         = sy-msgty
*                        i_id           = cx_erro->if_t100_message~t100key-msgid
*                        i_num          = cx_erro->if_t100_message~t100key-msgno
*                        i_message_v1   = cx_erro->msgv1
*                        i_message_v2   = cx_erro->msgv2
*                        i_message_v3   = cx_erro->msgv3
*                        i_message_v4   = cx_erro->msgv4
*                      CHANGING
*                        p_lc_sequencia = lc_sequencia.
*                    me->nfe_inbound_gravar_log( ).
*                    COMMIT WORK.
*                    RAISE EXCEPTION TYPE zcx_cadastro
*                      EXPORTING
*                        textid = cx_erro->if_t100_message~t100key
*                        msgty  = 'E'
*                        msgv1  = CONV #( cx_erro->if_t100_message~t100key-attr1 )
*                        msgv2  = CONV #( cx_erro->if_t100_message~t100key-attr2 )
*                        msgv3  = CONV #( cx_erro->if_t100_message~t100key-attr3 )
*                        msgv4  = CONV #( cx_erro->if_t100_message~t100key-attr4 ).
*                ENDTRY.
*
*              ELSE.
*
*                me->set_salvar( ).
*                MESSAGE s075 DISPLAY LIKE 'E'.
*
*                sy-msgty = 'E'.
*
*                CALL METHOD me->set_add_log_nfe
*                  EXPORTING
*                    i_type         = sy-msgty
*                    i_id           = sy-msgid
*                    i_num          = sy-msgno
*                    i_message_v1   = sy-msgv1
*                    i_message_v2   = sy-msgv2
*                    i_message_v3   = sy-msgv3
*                    i_message_v4   = sy-msgv4
*                  CHANGING
*                    p_lc_sequencia = lc_sequencia.
*
*                me->nfe_inbound_gravar_log( ).
*                EXIT.
*              ENDIF.
*
*            CATCH zcx_miro_exception INTO cx_erro_miro.
*
*              CALL METHOD me->set_add_log_nfe
*                EXPORTING
*                  i_type         = sy-msgty
*                  i_id           = cx_erro_miro->if_t100_message~t100key-msgid
*                  i_num          = cx_erro_miro->if_t100_message~t100key-msgno
*                  i_message_v1   = cx_erro_miro->msgv1
*                  i_message_v2   = cx_erro_miro->msgv2
*                  i_message_v3   = cx_erro_miro->msgv3
*                  i_message_v4   = cx_erro_miro->msgv4
*                CHANGING
*                  p_lc_sequencia = lc_sequencia.
*
*              me->nfe_inbound_gravar_log( ).
*              COMMIT WORK.
*
*              RAISE EXCEPTION TYPE zcx_cadastro
*                EXPORTING
*                  textid = cx_erro_miro->if_t100_message~t100key
*                  msgid  = cx_erro_miro->if_t100_message~t100key-msgid
*                  msgno  = cx_erro_miro->if_t100_message~t100key-msgno
*                  msgty  = 'E'
*                  msgv1  = cx_erro_miro->msgv1
*                  msgv2  = cx_erro_miro->msgv2
*                  msgv3  = cx_erro_miro->msgv3
*                  msgv4  = cx_erro_miro->msgv4.
*          ENDTRY.
*        ENDIF. "Gerar MIRO



        IF me->ck_aceite_faturar EQ abap_false AND
           me->ck_aceite_fiscal  EQ abap_false AND
           me->ck_aceite_fisico  EQ abap_false .

          me->set_salvar( ).
          me->nfe_inbound_gravar_log( ).
          COMMIT WORK.
        ENDIF.

        me->ck_alterou = abap_false.
        i_gravou = abap_true.
        MESSAGE s032.

      ENDIF. "Se Validou

    ENDIF. " Se foi alterado

    me->ck_aceite_fiscal          = abap_false.
    me->ck_aceite_fisico          = abap_false.
    me->ck_aceite_faturar         = abap_false.
    me->ck_somente_validar_fatura = abap_false.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: me->cte,
           me->nota,
           me->itens,
           me->pedidos,
           me->lotes,
           me->volumes_transp,
           me->logs,
           me->lotes_caracteristicas,
           me->ck_estornando_fisico,
           me->ck_alterou_iva,
           me->ck_alterou,
           me->ck_alterou_pedido_compra,
           me->frete,
           me->dados_retorno.

    CLEAR: me->ck_gerar_somente_aviso,
           me->ck_aceite_fiscal,
           me->ck_aceite_fisico,
           me->ck_aceite_faturar,
           me->ck_somente_validar_fatura,
           me->ck_alterou_lotes,
           me->ck_coleta_tudo,
           me->ck_alterou_pedido_compra,
           me->ck_nao_estornar_aviso,
           me->mensagens_retorno,
           me->ck_somente_uma_migo_pedido,
           me->ck_alterou_iva,
           me->ck_alterou_bloqueio_paga,
           me->ck_ignora_data_se_vencimento,
           me->st_fiscal_anterior,
           me->ck_preco_nota_fiscal,
           me->ck_retorno_sem_ajuste.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  METHOD zif_cadastro~set_registro.

    DATA: wa_cte TYPE zib_cte_dist_ter.

    me->limpar_registro( ).

    SELECT SINGLE * INTO wa_cte FROM zib_cte_dist_ter WHERE cd_chave_cte EQ i_id_registro.

    IF sy-subrc IS INITIAL.

      CALL METHOD zcl_cte_inbound=>set_enqueue_nfe
        EXPORTING
          i_chave                 = wa_cte-cd_chave_cte
          i_sem_bloqueio_registro = me->ck_sem_bloqueio_registro
        EXCEPTIONS
          zforeign_lock           = 1
          zsystem_failure         = 2
          erro                    = 3
          OTHERS                  = 4.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_cadastro
          EXPORTING
            textid = VALUE #( msgid = zcx_cadastro=>zcx_bloqueado-msgid
                              msgno = zcx_cadastro=>zcx_bloqueado-msgno
                              attr1 = CONV #( sy-msgv1 ) )
            msgty  = 'E'
            msgv1  = sy-msgv1
            msgid  = zcx_cadastro=>zcx_bloqueado-msgid
            msgno  = zcx_cadastro=>zcx_bloqueado-msgno.
      ELSE.
        APPEND wa_cte-cd_chave_cte TO me->at_chaves_lock.
      ENDIF.

      me->cte       = wa_cte.

      me->refresh( ).

      me->ck_alterou = abap_false.

    ELSE.

      SELECT SINGLE * INTO @DATA(wa_zib_nfe_erro)
        FROM zib_dfe_erro
       WHERE chave EQ @i_id_registro.

      IF sy-subrc IS INITIAL.
        MESSAGE s398(00)
        WITH 'XML recebido com erro!' wa_zib_nfe_erro-ds_erro+000(050) wa_zib_nfe_erro-ds_erro+050(050) wa_zib_nfe_erro-ds_erro+100(150).
      ELSE.
        MESSAGE s103 WITH i_id_registro.
      ENDIF.

      RAISE EXCEPTION TYPE zcx_cadastro
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

    MESSAGE S030 DISPLAY LIKE 'E'.
    EXIT.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_REGISTRO.

    TRY .

        DATA: it_total_item	TYPE TABLE OF zde_ekpo_help_saldo,
              wa_total_item	TYPE zde_ekpo_help_saldo,
              it_materiais  TYPE zib_nfe_dist_itm_t,
              wa_material   TYPE zib_nfe_dist_itm,
              lc_total_lote TYPE j_1bnetqty.

        DATA: zcx_nfe_inbound   TYPE REF TO zcx_nfe_inbound_exception,
              zcx_pedido_compra TYPE REF TO zcx_pedido_compra_exception,
              zcx_cad           TYPE REF TO zcx_cadastro,
              zcx_miro          TYPE REF TO zcx_miro_exception,
              e_wkurs	          TYPE wkurs,
              e_valor_total	    TYPE bapi_rmwwr,
              e_valor_desconto  TYPE bapi_wskto,
              i_wrbtr	          TYPE zde_vlr_total,
              i_desconto        TYPE zde_vlr_total,
              i_kursf	          TYPE kursf,
              t_tab_emb         TYPE zsds060_t,
              t_sdo_emb         TYPE zsds060_t,
              w_tab_emb         TYPE zsds060,
              w_sdo_emb         TYPE zsds060,
              l_lines_emb       TYPE i,
              l_seqcam          TYPE numc3,
*              l_nrosol          TYPE numc5, " RIM CS1029457 ANB 30.09.2022
              l_nrosol          TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
              l_seq             TYPE numc3,
              l_filial_resp     TYPE vkbur,
              l_total_0138      TYPE zsdt0138-qtd_embarq,
              l_saldo_0138      TYPE zsdt0082-qte_lib,
              l_total_mseg      TYPE mseg-menge,
              l_erro_dtcanc     TYPE c,
              l_erro_nfenum     TYPE c,
              l_erro_chref      TYPE c,
              l_erro_transp     TYPE c.


        CLEAR: sy-msgty.

        e_validou = abap_false.

*-------CS2019001896 - 05.01.2021 - inicio
        IF me->nota-aut_embarque IS NOT INITIAL.
          FREE : l_seqcam,      l_nrosol,
                 l_seq,         l_filial_resp,
                 l_erro_dtcanc, l_erro_nfenum,
                 l_erro_chref,  l_erro_transp,
                 l_total_0138,  l_saldo_0138,
                 l_total_mseg,  l_lines_emb,
                 t_tab_emb,     t_sdo_emb.

          me->set_validar_aut_embarque(
              EXPORTING i_aut_embarque = me->nota-aut_embarque
              IMPORTING e_seq_cam      = l_seqcam
                        e_nro_sol      = l_nrosol
                        e_seq          = l_seq
                        e_filial_resp  = l_filial_resp
                        t_tab_embarque = t_tab_emb ).

          SELECT * FROM mseg
                   INTO TABLE @DATA(t_mseg)
                  WHERE mblnr = @me->nota-mblnr
                    AND mjahr = @me->nota-mjahr
                    AND bwart = '101'.

          LOOP AT t_mseg INTO DATA(w_mseg).
            l_total_mseg = l_total_mseg + w_mseg-menge.
          ENDLOOP.

          DESCRIBE TABLE t_tab_emb LINES l_lines_emb.

          IF l_lines_emb = 1.
            SELECT * FROM zsdt0082
                     INTO TABLE @DATA(t_zsdt0082)
                    WHERE nro_sol   = @l_nrosol
                      AND seq       = @l_seq.

            DELETE t_zsdt0082 WHERE NOT ( dt_canc  IS     INITIAL
                                    AND   dt_liber IS NOT INITIAL ).

            IF t_zsdt0082[] IS NOT INITIAL.
              SELECT * FROM zsdt0138
                       INTO TABLE @DATA(t_zsdt0138)
                        FOR ALL ENTRIES IN @t_zsdt0082
                      WHERE nro_sol     = @t_zsdt0082-nro_sol
                        AND seq         = @t_zsdt0082-seq
                        AND filial_resp = @l_filial_resp.

              DELETE t_zsdt0138 WHERE NOT ( dt_canc IS INITIAL ).
            ENDIF.

            LOOP AT t_zsdt0082 INTO DATA(w_zsdt0082).

              CLEAR l_total_0138.
              LOOP AT t_zsdt0138 INTO DATA(w_zsdt0138)
                                WHERE nro_sol = w_zsdt0082-nro_sol
                                  AND seq     = w_zsdt0082-seq
                                  AND seq_cam = l_seqcam
                                  AND filial_resp  = l_filial_resp.

                IF w_zsdt0138-dt_canc IS NOT INITIAL.
                  l_erro_dtcanc = abap_true.
                  EXIT.
                ENDIF.
                IF w_zsdt0138-nfenum IS NOT INITIAL AND
                   w_zsdt0138-nfenum <> me->nota-numero.
                  l_erro_nfenum = abap_true.
                  EXIT.
                ENDIF.
                IF w_zsdt0138-ch_referencia IS NOT INITIAL.
                  SELECT ch_referencia
                    INTO @DATA(l_refer)
                     UP TO 1 ROWS
                    FROM zsdt0001
                   WHERE ch_referencia = @w_zsdt0138-ch_referencia.
                  ENDSELECT.
                  IF sy-subrc <> 0.
                    l_erro_chref  = abap_true.
                    EXIT.
                  ENDIF.
                ENDIF.
                IF w_zsdt0138-cod_transportadora IS INITIAL OR
                   w_zsdt0138-preco_frete        IS INITIAL OR
                   w_zsdt0138-placa_cav          IS INITIAL OR
                   w_zsdt0138-motorista          IS INITIAL.
                  l_erro_transp = abap_true.
                  EXIT.
                ENDIF.
                l_total_0138 = l_total_0138 + w_zsdt0138-qtd_embarq.
              ENDLOOP.

              IF l_erro_dtcanc = abap_true OR l_erro_nfenum = abap_true OR
                 l_erro_chref  = abap_true OR l_erro_transp = abap_true.
                EXIT.
              ENDIF.

              l_saldo_0138 = l_saldo_0138 + ( w_zsdt0082-qte_lib - l_total_0138 ).
            ENDLOOP.

            IF l_erro_dtcanc = abap_true.
              MESSAGE s153 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.

            IF l_erro_nfenum = abap_true.
              MESSAGE s154 WITH w_zsdt0138-nfenum DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.

            IF l_erro_chref = abap_true.
              MESSAGE s155 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.

            IF l_erro_transp = abap_true.
              MESSAGE s156 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.

            IF l_saldo_0138 < l_total_mseg.
              MESSAGE s152 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.

          ELSE.

            LOOP AT t_tab_emb INTO w_tab_emb.
              IF w_tab_emb-saldo >= l_total_mseg.
                MOVE-CORRESPONDING w_tab_emb  TO w_sdo_emb.
                MOVE l_total_mseg             TO w_sdo_emb-saldo.
                APPEND w_sdo_emb              TO t_sdo_emb.
                EXIT.
              ELSE.
                APPEND w_tab_emb              TO t_sdo_emb.
                l_total_mseg = l_total_mseg - w_tab_emb-saldo.
              ENDIF.
            ENDLOOP.

            LOOP AT t_sdo_emb INTO w_sdo_emb.
              l_saldo_0138 = l_saldo_0138 + w_sdo_emb-saldo.
            ENDLOOP.

            IF l_saldo_0138 IS INITIAL AND t_tab_emb[] IS NOT INITIAL.
              MESSAGE s158 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
*-------CS2019001896 - 05.01.2021 - fim

        IF me->ck_aceite_fiscal IS NOT INITIAL.

          IF me->nota-cancel EQ abap_true.

            MESSAGE s105 WITH me->nota-numero me->nota-serie DISPLAY LIKE 'E'.

            sy-msgty = 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

            EXIT.

          ENDIF.

          "Verifica Materiais """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          IF me->pedidos IS INITIAL.
            MOVE me->itens TO it_materiais.
            IF it_materiais IS NOT INITIAL.
              SELECT * INTO TABLE @DATA(it_marc)
                FROM marc
                 FOR ALL ENTRIES IN @it_materiais
               WHERE matnr EQ @it_materiais-matnr
                 AND werks EQ @me->nota-f_tomadora.
            ENDIF.
          ELSE.
            IF me->pedidos IS NOT INITIAL.
              SELECT * INTO TABLE it_marc
                FROM marc
                 FOR ALL ENTRIES IN me->pedidos
               WHERE matnr EQ me->pedidos-matnr
                 AND werks EQ me->nota-f_tomadora.
            ENDIF.

            LOOP AT me->pedidos INTO DATA(wa_pedidos).
              CLEAR: wa_material.
              wa_material-matnr = wa_pedidos-matnr.
              APPEND wa_material TO it_materiais.
            ENDLOOP.

          ENDIF.

          SORT it_materiais BY matnr.
          DELETE ADJACENT DUPLICATES FROM it_materiais COMPARING matnr.

          SORT it_marc BY matnr.

          DATA(ck_validou) = abap_true.
          LOOP AT it_materiais INTO DATA(wa_materiais).

            READ TABLE it_marc WITH KEY matnr = wa_materiais-matnr INTO DATA(wa_marc) BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              "040  Material &1 Existe p/ centro &2!
              MESSAGE s040 WITH wa_materiais-matnr me->nota-f_tomadora.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              IF me->pedidos IS INITIAL.
                REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_marc-steuc WITH ''.
                REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_materiais-prod_ncm WITH ''.
                IF wa_marc-steuc NE wa_materiais-prod_ncm .
                  "041  Material &1 NCM &2 na NF e no Material SAP NCM &2!
                  MESSAGE s041 WITH wa_marc-matnr wa_materiais-prod_ncm wa_marc-steuc DISPLAY LIKE 'E'.
                  sy-msgty = 'W'.
                  CALL METHOD me->set_add_log_nfe
                    EXPORTING
                      i_type         = sy-msgty
                      i_id           = sy-msgid
                      i_num          = sy-msgno
                      i_message_v1   = sy-msgv1
                      i_message_v2   = sy-msgv2
                      i_message_v3   = sy-msgv3
                      i_message_v4   = sy-msgv4
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDIF.
              ENDIF.

              IF wa_marc-steuc IS INITIAL.
                "114  Material &1 Centro &2 não possui NCM cadastrado!
                MESSAGE s114 WITH wa_marc-matnr wa_marc-werks DISPLAY LIKE 'E'.

                sy-msgty = 'E'.
                CALL METHOD me->set_add_log_nfe
                  EXPORTING
                    i_type         = sy-msgty
                    i_id           = sy-msgid
                    i_num          = sy-msgno
                    i_message_v1   = sy-msgv1
                    i_message_v2   = sy-msgv2
                    i_message_v3   = sy-msgv3
                    i_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                ck_validou = abap_false.
                EXIT.

              ELSEIF wa_marc-indus IS INITIAL.
                "115  Material &1 Centro &2 não possui Catg.CFOP cadastrado!
                MESSAGE s115 WITH wa_marc-matnr wa_marc-werks DISPLAY LIKE 'E'.

                sy-msgty = 'E'.
                CALL METHOD me->set_add_log_nfe
                  EXPORTING
                    i_type         = sy-msgty
                    i_id           = sy-msgid
                    i_num          = sy-msgno
                    i_message_v1   = sy-msgv1
                    i_message_v2   = sy-msgv2
                    i_message_v3   = sy-msgv3
                    i_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                ck_validou = abap_false.
                EXIT.

              ENDIF.

            ELSE.
              "039  Material &1 não Existe p/ centro &2!
              MESSAGE s039 WITH wa_materiais-matnr me->nota-f_tomadora DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              ck_validou = abap_false.
              EXIT.

            ENDIF.
          ENDLOOP.

          CHECK ck_validou = abap_true.

          "Verifica Saldo de Pedido""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "Verifica Saldo de Pedido (1)
          IF me->pedidos IS NOT INITIAL.
            LOOP AT me->pedidos INTO wa_pedidos.
              IF wa_pedidos-ebeln IS NOT INITIAL.
                READ TABLE it_total_item WITH KEY ebeln = wa_pedidos-ebeln ebelp = wa_pedidos-ebelp ASSIGNING FIELD-SYMBOL(<fs_item>).
                IF sy-subrc IS INITIAL.
                  ADD wa_pedidos-menge TO <fs_item>-menge_ekbe.
                ELSE.
                  wa_total_item-ebeln      = wa_pedidos-ebeln.
                  wa_total_item-ebelp      = wa_pedidos-ebelp.
                  wa_total_item-menge_ekbe = wa_pedidos-menge.
                  APPEND wa_total_item TO it_total_item.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ELSE.
            LOOP AT me->itens INTO DATA(wa_itens).
              IF wa_itens-ebeln IS NOT INITIAL.
                READ TABLE it_total_item WITH KEY ebeln = wa_itens-ebeln ebelp = wa_itens-ebelp ASSIGNING <fs_item>.
                IF sy-subrc IS INITIAL.
                  ADD wa_itens-menge TO <fs_item>-menge_ekbe.
                ELSE.
                  wa_total_item-ebeln      = wa_itens-ebeln.
                  wa_total_item-ebelp      = wa_itens-ebelp.
                  wa_total_item-menge_ekbe = wa_itens-menge.
                  APPEND wa_total_item TO it_total_item.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          "Verifica Saldo de Pedido (2)
          "Verifica Fornecedor      (3)
          DATA(wa_migo) = me->get_migo_valida( ).

          IF wa_migo IS INITIAL.

            ck_validou = abap_true.

            LOOP AT it_total_item INTO wa_total_item.

              IF me->pedidos IS NOT INITIAL.
                READ TABLE me->pedidos WITH KEY ebeln = wa_total_item-ebeln ebelp = wa_total_item-ebelp INTO wa_pedidos.
                wa_itens-matnr = wa_pedidos-matnr.
                wa_itens-menge = wa_pedidos-menge.
                wa_itens-meins = wa_pedidos-meins.
                wa_itens-ebeln = wa_pedidos-ebeln.
                wa_itens-ebelp = wa_pedidos-ebelp.
              ELSE.
                READ TABLE me->itens WITH KEY ebeln = wa_total_item-ebeln ebelp = wa_total_item-ebelp INTO wa_itens.
              ENDIF.

              CALL METHOD me->get_pedido_compra_chave
                EXPORTING
                  i_nota       = me->nota
                  i_item       = wa_itens
                IMPORTING
                  e_saldo_item = DATA(e_saldo_item)
                RECEIVING
                  r_ekpo       = DATA(r_expo)
                EXCEPTIONS
                  erro         = 1
                  OTHERS       = 2.

*034  Pedido &1 Item &2 com Saldo de &3!
*035  Pedido &1 Item &2 Sem Saldo de &3!

              IF wa_total_item-menge_ekbe LT e_saldo_item-menge_saldo.
                MESSAGE s035 WITH wa_total_item-ebeln wa_total_item-ebelp wa_total_item-menge_ekbe DISPLAY LIKE 'E'.
                sy-msgty = 'E'.
                CALL METHOD me->set_add_log_nfe
                  EXPORTING
                    i_type         = sy-msgty
                    i_id           = sy-msgid
                    i_num          = sy-msgno
                    i_message_v1   = sy-msgv1
                    i_message_v2   = sy-msgv2
                    i_message_v3   = sy-msgv3
                    i_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
                EXIT.
              ELSE.
                MESSAGE s034 WITH wa_total_item-ebeln wa_total_item-ebelp wa_total_item-menge_ekbe.
                CALL METHOD me->set_add_log_nfe
                  EXPORTING
                    i_type         = sy-msgty
                    i_id           = sy-msgid
                    i_num          = sy-msgno
                    i_message_v1   = sy-msgv1
                    i_message_v2   = sy-msgv2
                    i_message_v3   = sy-msgv3
                    i_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.

              SELECT SINGLE * INTO @DATA(wa_ekko)
                FROM ekko
               WHERE ebeln EQ @wa_total_item-ebeln.

              IF wa_ekko-lifre IS INITIAL.
                wa_ekko-lifre = wa_ekko-lifnr.
              ENDIF.

              IF wa_ekko-lifre NE me->nota-p_emissor.

                "Raiz do CNPJ
                SELECT SINGLE *
                  INTO @DATA(wa_lfa1)
                  FROM lfa1
                 WHERE lifnr EQ @me->nota-p_emissor.

                IF wa_lfa1-stcd1 IS NOT INITIAL.
                  CONCATENATE wa_lfa1-stcd1(8) '%' INTO wa_lfa1-stcd1.
                  "Pedido dentro do Raiz
                  SELECT * INTO TABLE @DATA(it_forn)
                    FROM lfa1
                   WHERE stcd1 LIKE @wa_lfa1-stcd1
                     AND lifnr EQ @wa_ekko-lifre.

                  IF sy-subrc IS INITIAL.
                    "038  Emissor da fatura distinto &1 com mesmo Raiz CNPJ!
                    MESSAGE w038 WITH me->nota-p_emissor wa_ekko-lifre.
                    CALL METHOD me->set_add_log_nfe
                      EXPORTING
                        i_type         = sy-msgty
                        i_id           = sy-msgid
                        i_num          = sy-msgno
                        i_message_v1   = sy-msgv1
                        i_message_v2   = sy-msgv2
                        i_message_v3   = sy-msgv3
                        i_message_v4   = sy-msgv4
                      CHANGING
                        p_lc_sequencia = lc_sequencia.

                  ELSE.
                    "036  Emissor Nota Fiscal &1 não é igual ao do Pedido &2.
                    MESSAGE s036 WITH me->nota-p_emissor wa_ekko-lifre DISPLAY LIKE 'E'.
                    sy-msgty = 'W'.
                    CALL METHOD me->set_add_log_nfe
                      EXPORTING
                        i_type         = sy-msgty
                        i_id           = sy-msgid
                        i_num          = sy-msgno
                        i_message_v1   = sy-msgv1
                        i_message_v2   = sy-msgv2
                        i_message_v3   = sy-msgv3
                        i_message_v4   = sy-msgv4
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                    EXIT.
                  ENDIF.
                ELSE.
                  "036  Emissor Nota Fiscal &1 não é igual ao do Pedido &2.
                  MESSAGE s036 WITH me->nota-p_emissor wa_ekko-lifre DISPLAY LIKE 'E'.
                  sy-msgty = 'W'.
                  CALL METHOD me->set_add_log_nfe
                    EXPORTING
                      i_type         = sy-msgty
                      i_id           = sy-msgid
                      i_num          = sy-msgno
                      i_message_v1   = sy-msgv1
                      i_message_v2   = sy-msgv2
                      i_message_v3   = sy-msgv3
                      i_message_v4   = sy-msgv4
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                  EXIT.
                ENDIF.
              ELSE.
                "037  Emissor Nota Fiscal &1 igual ao do Pedido &2.
                MESSAGE s037 WITH me->nota-p_emissor wa_ekko-lifre.
                CALL METHOD me->set_add_log_nfe
                  EXPORTING
                    i_type         = sy-msgty
                    i_id           = sy-msgid
                    i_num          = sy-msgno
                    i_message_v1   = sy-msgv1
                    i_message_v2   = sy-msgv2
                    i_message_v3   = sy-msgv3
                    i_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.

            ENDLOOP.

            CHECK ck_validou = abap_true.

          ENDIF.
          "Verifica Saldo de Pedido""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          "Verifica Transportadora"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          IF ( me->nota-ck_possui_frete EQ abap_true ) AND ( me->nota-f_transporte IS INITIAL ).

            MESSAGE s063 DISPLAY LIKE 'E'.
            sy-msgty = 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
            EXIT.

          ENDIF.
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          "Verificar se IVA tem Desoneração de ICMS """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          """ Comentado por CS2018001927
*          IF ME->NOTA-VL_ICMS_DESONERADO IS NOT INITIAL.
*            SELECT SINGLE * INTO @DATA(WA_A003)
*              FROM A003
*             WHERE MWSKZ EQ @ME->NOTA-MWSKZ
*               AND KAPPL EQ 'TX'
*               AND KSCHL EQ 'ICZF'
*               AND ALAND EQ 'BR'.
*
*            IF SY-SUBRC IS NOT INITIAL.
*              MESSAGE S106 DISPLAY LIKE 'E'.
*              SY-MSGTY = 'E'.
*              CALL METHOD ME->SET_ADD_LOG_NFE
*                EXPORTING
*                  I_TYPE         = SY-MSGTY
*                  I_ID           = SY-MSGID
*                  I_NUM          = SY-MSGNO
*                  I_MESSAGE_V1   = SY-MSGV1
*                  I_MESSAGE_V2   = SY-MSGV2
*                  I_MESSAGE_V3   = SY-MSGV3
*                  I_MESSAGE_V4   = SY-MSGV4
*                CHANGING
*                  P_LC_SEQUENCIA = LC_SEQUENCIA.
*              EXIT.
*            ENDIF.
*          ENDIF.
          """ Comentado por CS2018001927
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          "Verificar Armazenagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          IF ( me->nota-ck_trans_nf_propri EQ abap_true ).

            IF me->nota-f_armazem IS INITIAL.

              MESSAGE s148 DISPLAY LIKE 'E'.
              sy-msgty = 'E'.
              CALL METHOD me->set_add_log_nfe
                EXPORTING
                  i_type         = sy-msgty
                  i_id           = sy-msgid
                  i_num          = sy-msgno
                  i_message_v1   = sy-msgv1
                  i_message_v2   = sy-msgv2
                  i_message_v3   = sy-msgv3
                  i_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
              EXIT.

            ENDIF.

          ENDIF.
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        ENDIF.

        IF me->ck_aceite_fisico EQ abap_true.

          IF me->nota-cancel EQ abap_true.
            MESSAGE s105 WITH me->nota-numero me->nota-serie DISPLAY LIKE 'E'.
            sy-msgty = 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
            EXIT.
          ENDIF.

          "Verificar se Tem Sequencia
          CALL FUNCTION 'NUMBER_GET_INFO'
            EXPORTING
              nr_range_nr        = '01'
              object             = 'ZNFEIN'
            EXCEPTIONS
              interval_not_found = 1
              object_not_found   = 2
              OTHERS             = 3.

          IF sy-subrc IS NOT INITIAL.

            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
             DISPLAY LIKE 'E'.

            sy-msgty = 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
            EXIT.

          ENDIF.

          TRY .
              "Verificar Aceite Fiscal Concedido.
              IF me->nota-st_fiscal NE zcl_nfe_inbound=>st_fisico_99 AND me->ck_aceite_fiscal EQ abap_false. "Finalizado com Aceite Fiscal
                RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                  EXPORTING
                    textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_aceite_fiscal-msgid
                                      msgno = zcx_nfe_inbound_exception=>zcx_sem_aceite_fiscal-msgno )
                    msgid  = zcx_nfe_inbound_exception=>zcx_sem_aceite_fiscal-msgid
                    msgno  = zcx_nfe_inbound_exception=>zcx_sem_aceite_fiscal-msgno
                    msgty  = 'E'.
              ENDIF.

              CREATE OBJECT departamento.
              departamento->zif_cadastro~set_registro( i_id_registro = me->nota-cd_departamento ).

              DATA(r_tipo_pedidos) = departamento->get_tipo_pedido_compra( ).
              SORT r_tipo_pedidos BY bstyp bsart.

              IF me->pedidos IS NOT INITIAL.
                LOOP AT me->pedidos INTO wa_pedidos.

                  SELECT SINGLE * INTO @DATA(wa_ekpo)
                      FROM ekpo
                     WHERE ebeln EQ @wa_total_item-ebeln
                     AND   ebelp EQ @wa_total_item-ebelp.

                  SELECT SINGLE * INTO @DATA(wa_mara)
                    FROM mara
                   WHERE matnr EQ @wa_pedidos-matnr.

                  "Verificar se Material é Administrado por Lote mais Não tem Classe Definida """""""""""""""""""""""""""
                  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                  DATA(lc_ck_caracteristica) = abap_false.
                  IF wa_mara-xchpf EQ abap_true.
                    FIND REGEX 'C' IN wa_mara-pstat.
                    IF sy-subrc IS INITIAL.
                      lc_ck_caracteristica = abap_true.
                    ENDIF.
                  ENDIF.
                  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

                  "Código para administração em lote obrigatória com classificação
                  IF wa_mara-xchpf EQ abap_true AND wa_ekpo-knttp IS INITIAL AND lc_ck_caracteristica EQ abap_true.

                    "Busca item do Pedido de Compra
                    SELECT SINGLE * INTO @DATA(wa_eket) FROM eket WHERE ebeln EQ @wa_pedidos-ebeln AND ebelp EQ @wa_pedidos-ebelp.

                    IF wa_eket-charg IS INITIAL.
                      READ TABLE me->lotes INTO DATA(wa_lote)
                                           WITH KEY chave_nfe = wa_pedidos-chave_nfe
                                                    ebeln     = wa_pedidos-ebeln
                                                    ebelp     = wa_pedidos-ebelp.

                      "ZCX_OBRIGA_LOTE  Para Material &MSGV1& Deve entrar com as informações de Lote!
                      "ZCX_OBRIGA_LOTE_NUMERO	Para Material &MSGV1& é Obrigatório informar o campo Lote!
                      "ZCX_OBRIGA_LOTE_VENCIMENTO	Para Material &MSGV1& é Obrigatório informar o campo Dt. Vencimento!

                      IF ( sy-subrc IS NOT INITIAL ).
                        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                          EXPORTING
                            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgid
                                              msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgno
                                              attr1 = CONV #( wa_pedidos-matnr ) )
                            msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgid
                            msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgno
                            msgv1  = CONV #( wa_pedidos-matnr )
                            msgty  = 'E'.
                      ENDIF.

                      IF ( wa_lote-charg IS INITIAL ).
                        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                          EXPORTING
                            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                                              msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                                              attr1 = CONV #( wa_pedidos-matnr ) )
                            msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                            msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                            msgv1  = CONV #( wa_pedidos-matnr )
                            msgty  = 'E'.
                      ENDIF.

                      IF ( wa_lote-vfdat IS INITIAL ).
                        TRY.
                            CALL METHOD zcl_charg=>get_charg
                              EXPORTING
                                i_matnr = wa_itens-matnr
                                i_charg = wa_lote-charg
                              RECEIVING
                                r_mch1  = DATA(wa_mch1).
                          CATCH zcx_charg_exception .
                            "Caso não exista lote a data de vencimento é obrigatória
                            RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                              EXPORTING
                                textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgid
                                                  msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgno
                                                  attr1 = CONV #( wa_pedidos-matnr ) )
                                msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgid
                                msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgno
                                msgv1  = CONV #( wa_pedidos-matnr )
                                msgty  = 'E'.
                        ENDTRY.
                      ENDIF.

                    ENDIF.

                  ELSEIF wa_mara-xchpf EQ abap_true AND wa_ekpo-knttp IS INITIAL AND lc_ck_caracteristica EQ abap_false.

                    "Obrigado o Lote pelo fato do material ser controlado por lote e é uma entrada para estoque

                    IF wa_pedidos-vfdat IS INITIAL.
                      "VFDAT  VFDAT DATS  8 0 Data do vencimento
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgno
                                            attr1 = CONV #( wa_pedidos-ebeln )
                                            attr2 = CONV #( wa_pedidos-ebelp ) )
                          msgid  = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgno
                          msgv1  = CONV #( wa_pedidos-ebeln )
                          msgv2  = CONV #( wa_pedidos-ebelp )
                          msgty  = 'E'.
                    ELSEIF wa_pedidos-vfdat LT me->nota-dt_emissao.
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgno
                          msgty  = 'E'.
                    ENDIF.

                    IF wa_pedidos-hsdat IS INITIAL.
                      "HSDAT  HSDAT DATS  8 0 Data de produção
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgno
                                            attr1 = CONV #( wa_pedidos-ebeln )
                                            attr2 = CONV #( wa_pedidos-ebelp ) )
                          msgid  = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgno
                          msgv1  = CONV #( wa_pedidos-ebeln )
                          msgv2  = CONV #( wa_pedidos-ebelp )
                          msgty  = 'E'.
                    ELSEIF wa_pedidos-hsdat GT me->nota-dt_emissao.
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgno
                          msgty  = 'E'.
                    ENDIF.

                    IF wa_pedidos-charg IS INITIAL.
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                                            attr1 = wa_ekpo-matnr )
                          msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                          msgty  = 'E'
                          msgv1  = CONV #( wa_ekpo-matnr ).
                    ENDIF.

                  ELSEIF wa_mara-xchpf EQ abap_true AND wa_ekpo-knttp IS NOT INITIAL AND lc_ck_caracteristica EQ abap_false.

                    "Não é obrigado o Lote pelo fato do material ser controlado por lote e não é uma entrada para estoque (consumo)

                    IF wa_pedidos-vfdat IS INITIAL.
                      "VFDAT  VFDAT DATS  8 0 Data do vencimento
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgno
                                            attr1 = CONV #( wa_pedidos-ebeln )
                                            attr2 = CONV #( wa_pedidos-ebelp ) )
                          msgid  = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_inf_data_vencimento-msgno
                          msgv1  = CONV #( wa_pedidos-ebeln )
                          msgv2  = CONV #( wa_pedidos-ebelp )
                          msgty  = 'E'.
                    ELSEIF wa_pedidos-vfdat LT me->nota-dt_emissao.
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_dt_vencimento_erro-msgno
                          msgty  = 'E'.
                    ENDIF.

                    IF wa_pedidos-hsdat IS INITIAL.
                      "HSDAT  HSDAT DATS  8 0 Data de produção
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgno
                                            attr1 = CONV #( wa_pedidos-ebeln )
                                            attr2 = CONV #( wa_pedidos-ebelp ) )
                          msgid  = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_inf_data_producao-msgno
                          msgv1  = CONV #( wa_pedidos-ebeln )
                          msgv2  = CONV #( wa_pedidos-ebelp )
                          msgty  = 'E'.
                    ELSEIF wa_pedidos-hsdat GT me->nota-dt_emissao.
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_dt_producao_erro-msgno
                          msgty  = 'E'.
                    ENDIF.

                  ENDIF.

                  SELECT SINGLE * INTO wa_ekko FROM ekko WHERE ebeln EQ wa_pedidos-ebeln.
                  READ TABLE r_tipo_pedidos INTO DATA(wa_tipo_pedidos) WITH KEY bstyp = wa_ekko-bstyp bsart = wa_ekko-bsart BINARY SEARCH.

                  IF sy-subrc IS NOT INITIAL AND departamento->get_ck_sem_ret_pedido( ) EQ abap_false.

                    "Pedido de Compra &MSGV1& com Tipo &MSGV2& não Permitido p/ Depart. &MSGV3&!
                    RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                      EXPORTING
                        textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgid
                                          msgno = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgno
                                          attr1 = CONV #( wa_pedidos-ebeln )
                                          attr2 = CONV #( wa_ekko-bsart )
                                          attr3 = CONV #( me->nota-cd_departamento ) )
                        msgid  = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgid
                        msgno  = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgno
                        msgty  = 'E'
                        msgv1  = CONV #( wa_pedidos-ebeln )
                        msgv2  = CONV #( wa_ekko-bsart )
                        msgv3  = CONV #( me->nota-cd_departamento ).

                  ELSEIF me->nota-ck_compra_futura EQ abap_true AND wa_tipo_pedidos-ck_compra_futura EQ abap_false.

                    "100  Nota Fiscal é de Compra Futura e Pedido não é de Compra Futura!
                    RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                      EXPORTING
                        textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_nota_futura-msgid
                                          msgno = zcx_nfe_inbound_exception=>zcx_nota_futura-msgno )
                        msgid  = zcx_nfe_inbound_exception=>zcx_nota_futura-msgid
                        msgno  = zcx_nfe_inbound_exception=>zcx_nota_futura-msgno.

                  ELSEIF me->nota-ck_compra_futura EQ abap_false AND wa_tipo_pedidos-ck_compra_futura EQ abap_true.

                    "099  Nota Fiscal não é de Compra Futura e Pedido é de Compra Futura!
                    RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                      EXPORTING
                        textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgid
                                          msgno = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgno )
                        msgid  = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgid
                        msgno  = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgno.

                  ENDIF.

                  CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                    EXPORTING
                      i_ebeln = wa_pedidos-ebeln
                      i_ebelp = wa_pedidos-ebelp
                    RECEIVING
                      r_bstae = DATA(r_bstae).

                  "061  Pedido/Item &1/&2 com Aviso de Recebimento Previsto &3!
                  "062  Pedido/Item &1/&2 sem Aviso de Recebimento Previsto &3!

                  CASE r_bstae.
                    WHEN '0002'.
                      IF me->nota-ck_possui_frete EQ abap_true.

                        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                          EXPORTING
                            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgid
                                              msgno = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgno )
                            msgid  = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgid
                            msgno  = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgno
                            msgty  = 'E'.

                      ENDIF.
*                    WHEN '0001' OR '0004'.
*                      "Obrigatorio ter Custo de Frete
*                      IF ME->NOTA-CK_POSSUI_FRETE NE ABAP_TRUE.
*                        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
*                          EXPORTING
*                            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGID
*                                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGNO
*                                              ATTR1 = CONV #( WA_PEDIDOS-EBELN )
*                                              ATTR2 = CONV #( WA_PEDIDOS-EBELP )
*                                              ATTR3 = CONV #( R_BSTAE ) )
*                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGID
*                            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGNO
*                            MSGTY  = 'E'
*                            MSGV1  = CONV #( WA_PEDIDOS-EBELN )
*                            MSGV2  = CONV #( WA_PEDIDOS-EBELP )
*                            MSGV3  = CONV #( R_BSTAE ).
*                      ENDIF.
*                    WHEN OTHERS.
*                      "Não tem Custo de Frete
*                      IF ME->NOTA-CK_POSSUI_FRETE NE ABAP_FALSE.
*                        RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
*                          EXPORTING
*                            TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGID
*                                              MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGNO
*                                              ATTR1 = CONV #( WA_PEDIDOS-EBELN )
*                                              ATTR2 = CONV #( WA_PEDIDOS-EBELP )
*                                              ATTR3 = CONV #( R_BSTAE ) )
*                            MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGID
*                            MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGNO
*                            MSGTY  = 'E'
*                            MSGV1  = CONV #( WA_PEDIDOS-EBELN )
*                            MSGV2  = CONV #( WA_PEDIDOS-EBELP )
*                            MSGV3  = CONV #( R_BSTAE ).
*                      ENDIF.
                  ENDCASE.

                  lc_total_lote = wa_pedidos-menge.

                  LOOP AT me->lotes INTO DATA(wa_lote2) WHERE ebeln EQ wa_pedidos-ebeln AND ebelp EQ wa_pedidos-ebelp.
                    IF lc_total_lote EQ wa_pedidos-menge.
                      lc_total_lote = 0.
                    ENDIF.
                    ADD wa_lote2-menge TO lc_total_lote.
                  ENDLOOP.

                  DATA(lc_item) = wa_pedidos-ebeln && '-' && wa_pedidos-ebelp.

                  IF lc_total_lote NE wa_pedidos-menge.
                    RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                      EXPORTING
                        textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgid
                                          msgno = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgno
                                          attr1 = lc_item
                                          attr2 = CONV #( lc_total_lote    )
                                          attr3 = CONV #( wa_pedidos-menge ) )
                        msgid  = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgid
                        msgno  = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgno
                        msgty  = 'E'
                        msgv1  = CONV #( lc_item )
                        msgv2  = CONV #( lc_total_lote )
                        msgv3  = CONV #( wa_pedidos-menge ).
                  ENDIF.
                ENDLOOP.
              ELSE.
                LOOP AT me->itens INTO wa_itens.

                  SELECT SINGLE * INTO wa_mara
                    FROM mara
                   WHERE matnr EQ wa_itens-matnr.

                  "Código para administração em lote obrigatória
                  IF wa_mara-xchpf EQ abap_true.

                    "Busca item do Pedido de Compra
                    SELECT SINGLE * INTO wa_eket FROM eket WHERE ebeln EQ wa_itens-ebeln AND ebelp EQ wa_itens-ebelp.

                    IF wa_eket-charg IS INITIAL.
                      READ TABLE me->lotes INTO wa_lote
                                           WITH KEY chave_nfe = wa_itens-chave_nfe
                                                    prod_item = wa_itens-prod_item.

                      "ZCX_OBRIGA_LOTE  Para Material &MSGV1& Deve entrar com as informações de Lote!
                      "ZCX_OBRIGA_LOTE_NUMERO	Para Material &MSGV1& é Obrigatório informar o campo Lote!
                      "ZCX_OBRIGA_LOTE_VENCIMENTO	Para Material &MSGV1& é Obrigatório informar o campo Dt. Vencimento!

                      IF ( sy-subrc IS NOT INITIAL ).
                        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                          EXPORTING
                            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgid
                                              msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgno
                                              attr1 = CONV #( wa_itens-matnr ) )
                            msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgid
                            msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote-msgno
                            msgv1  = CONV #( wa_itens-matnr )
                            msgty  = 'E'.
                      ENDIF.

                      IF ( wa_lote-charg IS INITIAL ).
                        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                          EXPORTING
                            textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                                              msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                                              attr1 = CONV #( wa_itens-matnr ) )
                            msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgid
                            msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_numero-msgno
                            msgv1  = CONV #( wa_itens-matnr )
                            msgty  = 'E'.
                      ENDIF.


                      IF ( wa_lote-vfdat IS INITIAL ).

                        TRY.
                            CALL METHOD zcl_charg=>get_charg
                              EXPORTING
                                i_matnr = wa_itens-matnr
                                i_charg = wa_lote-charg
                              RECEIVING
                                r_mch1  = wa_mch1.
                          CATCH zcx_charg_exception .
                            "Caso não exista lote a data de vencimento é obrigatória
                            RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                              EXPORTING
                                textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgid
                                                  msgno = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgno
                                                  attr1 = CONV #( wa_itens-matnr ) )
                                msgid  = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgid
                                msgno  = zcx_nfe_inbound_exception=>zcx_obriga_lote_vencimento-msgno
                                msgv1  = CONV #( wa_itens-matnr )
                                msgty  = 'E'.
                        ENDTRY.
                      ENDIF.

                    ENDIF.
                  ENDIF.

                  IF wa_itens-ebeln IS NOT INITIAL AND wa_itens-ebelp IS NOT INITIAL.

                    SELECT SINGLE * INTO wa_ekko FROM ekko WHERE ebeln EQ wa_itens-ebeln.
                    READ TABLE r_tipo_pedidos INTO wa_tipo_pedidos WITH KEY bstyp = wa_ekko-bstyp bsart = wa_ekko-bsart BINARY SEARCH.

                    IF sy-subrc IS NOT INITIAL AND departamento->get_ck_sem_ret_pedido( ) EQ abap_false.

                      "Pedido de Compra &MSGV1& com Tipo &MSGV2& não Permitido p/ Depart. &MSGV3&!
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgno
                                            attr1 = CONV #( wa_itens-ebeln )
                                            attr2 = CONV #( wa_ekko-bsart )
                                            attr3 = CONV #( me->nota-cd_departamento ) )
                          msgid  = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_tipo_pedido_nao_permitido-msgno
                          msgty  = 'E'
                          msgv1  = CONV #( wa_itens-ebeln )
                          msgv2  = CONV #( wa_ekko-bsart )
                          msgv3  = CONV #( me->nota-cd_departamento ).

                    ELSEIF me->nota-ck_compra_futura EQ abap_true AND wa_tipo_pedidos-ck_compra_futura EQ abap_false.

                      "100  Nota Fiscal é de Compra Futura e Pedido não é de Compra Futura!
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_nota_futura-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_nota_futura-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_nota_futura-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_nota_futura-msgno.

                    ELSEIF me->nota-ck_compra_futura EQ abap_false AND wa_tipo_pedidos-ck_compra_futura EQ abap_true.

                      "099  Nota Fiscal não é de Compra Futura e Pedido é de Compra Futura!
                      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                        EXPORTING
                          textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgid
                                            msgno = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgno )
                          msgid  = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgid
                          msgno  = zcx_nfe_inbound_exception=>zcx_nota_futura_nao-msgno.

                    ENDIF.

                    CALL METHOD zcl_pedido_compra=>get_chave_controle_conf_item
                      EXPORTING
                        i_ebeln = wa_itens-ebeln
                        i_ebelp = wa_itens-ebelp
                      RECEIVING
                        r_bstae = r_bstae.

                    "061  Pedido/Item &1/&2 com Aviso de Recebimento Previsto &3!
                    "062  Pedido/Item &1/&2 sem Aviso de Recebimento Previsto &3!

                    CASE r_bstae.
                      WHEN '0002'.
                        IF me->nota-ck_possui_frete EQ abap_true.

                          RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                            EXPORTING
                              textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgid
                                                msgno = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgno )
                              msgid  = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgid
                              msgno  = zcx_nfe_inbound_exception=>zcx_possui_frete_erro_ped-msgno
                              msgty  = 'E'.

                        ENDIF.
*                      WHEN '0001' OR '0004'.
                        "Obrigatorio ter Custo de Frete
*                        IF ME->NOTA-CK_POSSUI_FRETE NE ABAP_TRUE.
*                          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
*                            EXPORTING
*                              TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGID
*                                                MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGNO
*                                                ATTR1 = CONV #( WA_ITENS-EBELN )
*                                                ATTR2 = CONV #( WA_ITENS-EBELP )
*                                                ATTR3 = CONV #( R_BSTAE ) )
*                              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGID
*                              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_POSSUI_AVISO_RECEB-MSGNO
*                              MSGTY  = 'E'
*                              MSGV1  = CONV #( WA_ITENS-EBELN )
*                              MSGV2  = CONV #( WA_ITENS-EBELP )
*                              MSGV3  = CONV #( R_BSTAE ).
*                        ENDIF.
*                      WHEN OTHERS.
                        "Não tem Custo de Frete
*                        IF ME->NOTA-CK_POSSUI_FRETE NE ABAP_FALSE.
*                          RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
*                            EXPORTING
*                              TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGID
*                                                MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGNO
*                                                ATTR1 = CONV #( WA_ITENS-EBELN )
*                                                ATTR2 = CONV #( WA_ITENS-EBELP )
*                                                ATTR3 = CONV #( R_BSTAE ) )
*                              MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGID
*                              MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_NPOSSUI_AVISO_RECEB-MSGNO
*                              MSGTY  = 'E'
*                              MSGV1  = CONV #( WA_ITENS-EBELN )
*                              MSGV2  = CONV #( WA_ITENS-EBELP )
*                              MSGV3  = CONV #( R_BSTAE ).
*                        ENDIF.
                    ENDCASE.
                  ENDIF.

                  lc_total_lote = wa_itens-menge.

                  LOOP AT me->lotes INTO wa_lote2 WHERE prod_item EQ wa_itens-prod_item .
                    IF lc_total_lote EQ wa_itens-menge.
                      lc_total_lote = 0.
                    ENDIF.
                    ADD wa_lote2-menge TO lc_total_lote.
                  ENDLOOP.

                  IF lc_total_lote NE wa_itens-menge.
                    RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                      EXPORTING
                        textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgid
                                          msgno = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgno
                                          attr1 = CONV #( wa_itens-prod_item )
                                          attr2 = CONV #( lc_total_lote      )
                                          attr3 = CONV #( wa_itens-menge     ) )
                        msgid  = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgid
                        msgno  = zcx_nfe_inbound_exception=>zcx_volume_lote_errado-msgno
                        msgty  = 'E'
                        msgv1  = CONV #( wa_itens-prod_item )
                        msgv2  = CONV #( lc_total_lote )
                        msgv3  = CONV #( wa_itens-menge ).
                  ENDIF.

                ENDLOOP.
              ENDIF.
              IF lc_sequencia IS INITIAL.
                lc_sequencia = me->get_sequencia_log( ).
              ENDIF.

            CATCH zcx_nfe_inbound_exception INTO zcx_nfe_inbound.
              zcx_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              sy-msgty = 'E'.
            CATCH zcx_pedido_compra_exception INTO zcx_pedido_compra.
              zcx_pedido_compra->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              sy-msgty = 'E'.
            CATCH zcx_cadastro INTO zcx_cad.
              zcx_cad->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              sy-msgty = 'E'.
          ENDTRY.

          IF sy-msgty EQ 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
            EXIT.
          ENDIF.

        ENDIF.

        IF me->ck_aceite_faturar EQ abap_true AND me->get_ck_cfop_retorno_armazena( ) EQ abap_false.

          IF me->nota-cancel EQ abap_true.
            MESSAGE s105 WITH me->nota-numero me->nota-serie DISPLAY LIKE 'E'.

            sy-msgty = 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

            EXIT.
          ENDIF.

          TRY .
              "Verificar Aceite Fiscal Concedido.
              IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_migo_gerada AND
                 me->nota-tp_compra_futura NE zcl_nfe_inbound=>tp_compra_futura_fatura AND
                 me->ck_aceite_fisico EQ abap_false. "Fazer Primeiro a MIGO

                DATA(lc_erro_aceite_fisico) = abap_true.
                IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_migo_gerada.
                  wa_migo = me->get_migo_valida( ).
                  IF wa_migo IS NOT INITIAL.
                    me->set_nr_documento_material( i_mblnr = wa_migo-mblnr i_mjahr = wa_migo-mjahr ).
                    me->nota-st_fisico = zcl_nfe_inbound=>st_fisico_migo_gerada.
                    lc_erro_aceite_fisico = abap_false.
                  ENDIF.

                ENDIF.

                IF lc_erro_aceite_fisico EQ abap_true AND me->ck_somente_validar_fatura EQ abap_false.
                  RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                    EXPORTING
                      textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_sem_aceite_fisico-msgid
                                        msgno = zcx_nfe_inbound_exception=>zcx_sem_aceite_fisico-msgno )
                      msgid  = zcx_nfe_inbound_exception=>zcx_sem_aceite_fisico-msgid
                      msgno  = zcx_nfe_inbound_exception=>zcx_sem_aceite_fisico-msgno
                      msgty  = 'E'.
                ENDIF.
              ENDIF.

              IF ( me->get_possui_adiantamento( ) EQ abap_true ) AND ( me->nota-zlspr IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                  EXPORTING
                    textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgid
                                      msgno = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgno )
                    msgid  = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgid
                    msgno  = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgno
                    msgty  = 'E'.
              ENDIF.

              DATA(r_zmmt0075) = me->get_config_tipo_pedido( ).

              IF ( r_zmmt0075-ck_obriga_bloqueio EQ abap_true ) AND ( me->nota-zlspr IS INITIAL ).
                RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                  EXPORTING
                    textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgid
                                      msgno = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgno )
                    msgid  = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgid
                    msgno  = zcx_nfe_inbound_exception=>zcx_bloqueio_pagamento-msgno
                    msgty  = 'E'.
              ENDIF.

              IF ( r_zmmt0075-ck_altera_bloqueio EQ abap_false ) AND ( me->nota-zlspr NE r_zmmt0075-zlspr ).
                RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                  EXPORTING
                    textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_bloqueio_parametrizado-msgid
                                      msgno = zcx_nfe_inbound_exception=>zcx_bloqueio_parametrizado-msgno
                                      attr1 = CONV #( r_zmmt0075-zlspr ) )
                    msgid  = zcx_nfe_inbound_exception=>zcx_bloqueio_parametrizado-msgid
                    msgno  = zcx_nfe_inbound_exception=>zcx_bloqueio_parametrizado-msgno
                    msgv1  = CONV #( r_zmmt0075-zlspr )
                    msgty  = 'E'.
              ENDIF.

              IF me->pedidos IS INITIAL OR me->ck_ignora_data_se_vencimento EQ abap_true.
                DATA(i_data_se) = abap_false.
              ELSE.
                i_data_se = abap_true.
              ENDIF.

              TRY .
                  zcl_miro=>verificar_vencimento_fatura(
                     EXPORTING
                       i_data_vencimento = me->nota-dt_vencimento
                       i_data_se         = i_data_se
                       i_pymt_meth       = me->nota-pymt_meth
                       i_ck_revisao      = me->nota-ck_revisao
                       i_valida_politica = abap_true               "*-CS2024000243-05.06.2024-#136397-JT
                       i_ck_fpol         = me->nota-ck_fpol        "*-CS2024000243-05.06.2024-#136397-JT
                       i_obs_financeira  = me->nota-obs_financeira "*-CS2024000243-05.06.2024-#136397-JT
                       ).
                CATCH zcx_miro_exception INTO DATA(ex_miro).       "*-CS2024000243-05.06.2024-#136397-JT
                  ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
                  RETURN.
                CATCH zcx_error INTO DATA(ex_error).
                  ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
                  RETURN.
              ENDTRY.

              IF r_zmmt0075-ck_nao_valida_venc_ped EQ abap_false.
                IF me->pedidos IS INITIAL.
                  READ TABLE me->itens INTO DATA(wa_item) INDEX 1.
                ELSE.
                  READ TABLE me->pedidos INTO wa_pedidos INDEX 1.
                  wa_item-ebeln = wa_pedidos-ebeln.
                ENDIF.
                IF me->get_ck_cfop_retorno_armazena( ) EQ abap_false OR wa_item-ebeln IS NOT INITIAL.
                  zcl_miro=>verificar_vencimento_pedido( i_ebeln = wa_item-ebeln i_data_base = me->nota-dt_emissao i_data_vencimento = me->nota-dt_vencimento ).
                ENDIF.
              ENDIF.
              zcl_miro=>verificar_criar( i_data = sy-datlo i_bukrs = me->nota-e_tomadora ).
              zcl_miro=>verificar_tipo_documento( i_bsart = r_zmmt0075-bsart i_blart = r_zmmt0075-blart ).
              zcl_miro=>verificar_chave_bloqueio( i_bsart = r_zmmt0075-bsart i_zlspr = me->nota-zlspr ).
              zcl_miro=>verificar_tipo_pedido( i_bsart = r_zmmt0075-bsart i_budat = sy-datlo ).

              "
              IF me->nota-pymt_meth NE zcl_miro=>st_forma_pagamento_boleto.
                CLEAR: me->nota-housebankid, me->nota-pymt_meth.
                TRY .
                    zcl_miro=>get_formapag_banco_empresa( EXPORTING i_bukrs = me->nota-bukrs
                                                                    i_lifnr = me->nota-p_emissor    " Nº conta do fornecedor
                                                                    i_bvtyp = me->nota-zbvtyp       " Tipo de banco do parceiro
                                                          IMPORTING e_banco_empresa   = me->nota-housebankid
                                                                    e_forma_pagamento = me->nota-pymt_meth ).
                    IF me->nota-housebankid IS INITIAL.
                      MESSAGE s836(sd) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'S'.
                      EXIT.
                    ENDIF.
                  CATCH zcx_miro_exception INTO ex_miro.  "
                    ex_miro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
                    RETURN.
                ENDTRY.
              ELSE.
                CLEAR: me->nota-housebankid, me->nota-pymt_meth.
                TRY .
                    zcl_miro=>get_formapag_banco_empresa( EXPORTING i_bukrs = me->nota-bukrs
                                                                    i_lifnr = me->nota-p_emissor    " Nº conta do fornecedor
                                                                    i_bvtyp = me->nota-zbvtyp       " Tipo de banco do parceiro
                                                                    i_zlsch = zcl_miro=>st_forma_pagamento_boleto
                                                          IMPORTING e_banco_empresa   = me->nota-housebankid
                                                                    e_forma_pagamento = me->nota-pymt_meth ).
                    IF me->nota-housebankid IS INITIAL.
                      MESSAGE s836(sd) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'S'.
                      EXIT.
                    ENDIF.
                  CATCH zcx_miro_exception INTO DATA(ex_miro2).  "
                    ex_miro2->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
                    RETURN.
                ENDTRY.
              ENDIF.

              "
              me->get_valor_nota_fiscal_fatura(
                IMPORTING
                  e_waers          = me->nota-ctr_waers
                  e_wkurs          = e_wkurs
                  e_kufix          = me->nota-ctr_kufix
                  e_sinal          = me->nota-ctr_sinal
                  e_valor_total    = e_valor_total
                  e_valor_produtos = e_valor_desconto
                  e_zterm	         = me->nota-ctr_zterm  ).

              IF me->get_ck_cfop_retorno_armazena( ) EQ abap_true.
                i_kursf	 = 0.
                i_wrbtr  = me->nota-vl_total.
                e_valor_desconto = me->nota-vl_desconto.
              ELSEIF r_zmmt0075-ck_altera_valor EQ abap_false OR me->nota-ctr_valor_total IS INITIAL.
                i_kursf	   = e_wkurs.
                i_wrbtr    = e_valor_total.
                e_valor_desconto = e_valor_desconto - e_valor_total.
              ELSE.
                i_kursf	 = me->nota-ctr_wkurs.
                i_wrbtr  = me->nota-ctr_valor_total.
                e_valor_desconto = e_valor_desconto - me->nota-ctr_valor_total.
              ENDIF.

              MOVE e_valor_desconto TO i_desconto.
              i_wrbtr = i_wrbtr.

              IF me->nota-ctr_waers IS INITIAL.
                me->nota-ctr_waers = 'BRL'.
              ENDIF.

              zcl_miro=>verificar_valor_nfe( i_chave_nfe = me->nota-chave_nfe i_wrbtr = i_wrbtr i_waers = me->nota-ctr_waers i_kursf = i_kursf ).

              zcl_miro=>verificar_forn_doc_fiscal(
                EXPORTING
                  i_lifnr  = me->nota-p_emissor
                  i_nftype = me->get_categoria_nota_fiscal( )
                  i_xblnr  = zcl_miro=>get_chave_referencia( i_series = me->nota-serie  i_nf_number9 = me->nota-numero )
                  i_data   = me->nota-dt_emissao
                  i_werks  = me->nota-f_tomadora ).

              IF me->nota-pymt_meth EQ zcl_miro=>st_forma_pagamento_boleto.
                zcl_miro=>verificar_cod_barra(
                  EXPORTING
                    i_boleto        = me->nota-boleto
                    i_valor         = CONV #( me->nota-vl_total ) " Montante em moeda interna
                    i_dt_vencimento = me->nota-dt_vencimento    " Data Vencimento.
                ).
              ELSEIF me->nota-zbvtyp IS INITIAL.
                RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
                  EXPORTING
                    textid = VALUE #( msgid = zcx_nfe_inbound_exception=>zcx_banco_parceiro-msgid
                                      msgno = zcx_nfe_inbound_exception=>zcx_banco_parceiro-msgno
                                      attr1 = CONV #( me->nota-pymt_meth ) )
                    msgid  = zcx_nfe_inbound_exception=>zcx_banco_parceiro-msgid
                    msgno  = zcx_nfe_inbound_exception=>zcx_banco_parceiro-msgno
                    msgv1  = CONV #( me->nota-pymt_meth )
                    msgty  = 'E'.
              ENDIF.

            CATCH zcx_nfe_inbound_exception INTO zcx_nfe_inbound.
              zcx_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              sy-msgty = 'E'.
            CATCH zcx_miro_exception INTO zcx_miro.
              zcx_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              sy-msgty = 'E'.
          ENDTRY.

          IF sy-msgty EQ 'E'.
            CALL METHOD me->set_add_log_nfe
              EXPORTING
                i_type         = sy-msgty
                i_id           = sy-msgid
                i_num          = sy-msgno
                i_message_v1   = sy-msgv1
                i_message_v2   = sy-msgv2
                i_message_v3   = sy-msgv3
                i_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.
            EXIT.
          ENDIF.

        ENDIF.

        e_validou = abap_true.

      CATCH zcx_cadastro INTO DATA(ex_cadastro).

        ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        sy-msgty = 'E'.

        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = sy-msgty
            i_id           = sy-msgid
            i_num          = sy-msgno
            i_message_v1   = sy-msgv1
            i_message_v2   = sy-msgv2
            i_message_v3   = sy-msgv3
            i_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.

      CATCH zcx_pedido_compra_exception INTO DATA(ex_pedido_compra_exception).
        ex_pedido_compra_exception->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        sy-msgty = 'E'.

        CALL METHOD me->set_add_log_nfe
          EXPORTING
            i_type         = sy-msgty
            i_id           = sy-msgid
            i_num          = sy-msgno
            i_message_v1   = sy-msgv1
            i_message_v2   = sy-msgv2
            i_message_v3   = sy-msgv3
            i_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.

    ENDTRY.


  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    DATA: r_permitido_manual TYPE char01.

    r_permitido = abap_false.

    TRY .

        IF i_campo EQ 'CK_TRANS_NF_PROPRI' AND me->nota-docnum_arm IS NOT INITIAL.
          EXIT.
        ELSEIF i_campo EQ 'CK_TRANS_NF_PROPRI' AND me->nota-docnum_arm IS INITIAL.
          r_permitido = abap_true.
          EXIT.
        ENDIF.

        IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
          EXIT.
        ENDIF.

*        IF "( ME->CK_COLETA_TUDO EQ ABAP_TRUE ) AND
*           ( ME->NOTA-ST_DOCUMENTO NE ZCL_NFE_INBOUND=>ST_DOCUMENTO_00 ) AND
*           ( ME->NOTA-ST_DOCUMENTO NE SPACE ).
*          EXIT.
*        ENDIF.

        IF me->nota-chave_nfe IS NOT INITIAL.

          IF me->nota-docnum_nfe IS NOT INITIAL.
            r_permitido = abap_false.
          ELSE.
            r_permitido = abap_true.
          ENDIF.

          IF me->nota-manual IS INITIAL.
            r_permitido_manual = abap_true.
          ELSE.
            r_permitido_manual = abap_false.
          ENDIF.

          DATA(r_zmmt0075) = me->get_config_tipo_pedido( ).

          CASE i_campo.
            WHEN 'DS_DEPARTAMENTO'.
              r_permitido = abap_false.
            WHEN 'DOCNUM_NFE'.
              r_permitido = abap_false.
            WHEN 'E_TOMADORA'.
              r_permitido = abap_false.
            WHEN 'F_TOMADORA'.
              r_permitido = abap_false.
            WHEN 'P_EMISSOR'.
              r_permitido = abap_false.
            WHEN 'BELNR'.
              r_permitido = abap_false.
            WHEN 'GJAHR'.
              r_permitido = abap_false.
            WHEN 'ST_FISCAL'.
              r_permitido = abap_false.
            WHEN 'ST_FISICO'.
              r_permitido = abap_false.
            WHEN 'ST_DOCUMENTO'.
              r_permitido = abap_false.
            WHEN 'CD_DEPARTAMENTO'.

              IF me->nota-ck_fiscal IS NOT INITIAL.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.

            WHEN 'MATNR' OR 'MEINS' OR 'MENGE' OR 'EBELN' OR 'EBELP'.
              IF me->nota-ck_fiscal IS NOT INITIAL.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'CK_POSSUI_FRETE'.
              IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_00 OR me->nota-vbeln IS NOT INITIAL.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'NR_FASE'.
              IF me->nota-ck_fiscal IS NOT INITIAL .
                r_permitido = abap_false.
              ELSEIF me->get_obrigatorio_nr_fase( ) EQ abap_true.
                r_permitido = r_permitido.
              ELSE.
                r_permitido = abap_false.
              ENDIF.
            WHEN 'F_TRANSPORTE'.
              IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_00 OR me->nota-vbeln IS NOT INITIAL.
                r_permitido = abap_false.
              ELSE.
                IF me->nota-ck_possui_frete IS NOT INITIAL AND me->nota-vbeln IS INITIAL.
                  r_permitido = r_permitido.
                ELSE.
                  r_permitido = abap_false.
                ENDIF.
              ENDIF.
            WHEN 'DT_VENCIMENTO' OR 'ZBVTYP'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'ZLSPR'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                IF r_zmmt0075 IS INITIAL.
                  r_permitido = abap_false.
                ELSE.
                  DATA(r_ck_possui) = me->get_possui_adiantamento( ).
                  IF ( r_ck_possui EQ abap_true ) AND ( r_zmmt0075-ck_altera_bloqueio = abap_false ) AND ( r_zmmt0075-zlspr IS INITIAL ).
                    r_permitido = r_permitido.
                  ELSE.
                    IF r_zmmt0075-ck_altera_bloqueio EQ abap_true.
                      r_permitido = r_permitido.
                    ELSE.
                      r_permitido = abap_false.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN 'PYMT_METH' OR 'HOUSEBANKID'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'BOLETO'.
              IF me->nota-pymt_meth = zcl_miro=>st_forma_pagamento_boleto.
                r_permitido = r_permitido.
              ELSE.
                r_permitido = abap_false.
              ENDIF.
            WHEN 'VLR_DESCONTO'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                IF r_zmmt0075 IS NOT INITIAL.
                  IF r_zmmt0075-ck_altera_valor EQ abap_true.
                    r_permitido = r_permitido.
                  ELSE.
                    r_permitido = abap_false.
                  ENDIF.
                ELSE.
                  r_permitido = abap_false.
                ENDIF.
              ENDIF.
            WHEN 'CTR_VALOR_TOTAL'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                IF r_zmmt0075 IS NOT INITIAL.
                  IF r_zmmt0075-ck_altera_valor EQ abap_true.
                    me->get_valor_nota_fiscal_fatura( IMPORTING e_waers =  DATA(e_waers) ).
                    IF e_waers EQ 'BRL'.
                      r_permitido = abap_false.
                    ELSE.
                      r_permitido = r_permitido.
                    ENDIF.
                  ELSE.
                    r_permitido = abap_false.
                  ENDIF.
                ELSE.
                  r_permitido = abap_false.
                ENDIF.
              ENDIF.
            WHEN 'F_ARMAZEM'.
              IF me->nota-st_fisico NE zcl_nfe_inbound=>st_fisico_00 OR me->nota-cd_romaneio IS NOT INITIAL OR me->nota-ck_armazem IS NOT INITIAL.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'ARMAZEM_CNPJ'.
              r_permitido = abap_false.
            WHEN 'ARMAZEM_IE'.
              r_permitido = abap_false.
            WHEN 'ARMAZEM_RAZAO'.
              r_permitido = abap_false.
            WHEN 'OBS_FINANCEIRA'.
              IF me->nota-st_documento EQ zcl_nfe_inbound=>st_documento_99.
                r_permitido = abap_false.
              ELSE.
                r_permitido = r_permitido.
              ENDIF.
            WHEN 'WAERK'.
              r_permitido = r_permitido_manual.
            WHEN 'CK_FPOL'.
              r_permitido = r_permitido_manual.
            WHEN OTHERS.
              r_permitido = abap_false.
          ENDCASE.
        ELSE.
          r_permitido = abap_true.
        ENDIF.

      CATCH zcx_cadastro.

    ENDTRY.


  ENDMETHOD.


  METHOD ZIF_PESQUISA~PESQUISAR.

    DATA: LC_FILTRO TYPE ZIB_NFE_DIST_TER_FILTRO,
          T_CFOPS   TYPE RANGE OF ZIB_NFE_DIST_ITM-PROD_CFOP,
          L_CFOPS   LIKE LINE OF T_CFOPS.

    DATA: IT_ACESSO TYPE TABLE OF ZDE_NFE_INBOUND_ACESSO,
          WA_ACESSO TYPE ZDE_NFE_INBOUND_ACESSO,
          IT_TER    TYPE TABLE OF ZIB_NFE_DIST_TER,
          E_RESG    TYPE ZDE_IB_NFE_DIST_TER_T.

    LC_FILTRO = I_FILTROS.

    IF LC_FILTRO-CD_DEPARTAMENTO IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_ZMMT0077)
        FROM ZMMT0077
       WHERE CD_DEPARTAMENTO IN @LC_FILTRO-CD_DEPARTAMENTO.

      LOOP AT IT_ZMMT0077 INTO DATA(WA_ZMMT0077).
        L_CFOPS-SIGN   = 'I'.
        L_CFOPS-OPTION = 'NE'.
        L_CFOPS-LOW    = WA_ZMMT0077-CFOP(4).
        L_CFOPS-HIGH   = WA_ZMMT0077-CFOP(4).
        APPEND L_CFOPS TO T_CFOPS.
      ENDLOOP.
    ENDIF.

    SELECT * INTO TABLE @DATA(IT_ZIB_NFE_DIST_TER)
      FROM ZIB_NFE_DIST_TER AS CAP
     WHERE DOCNUM_NFE      IN @LC_FILTRO-DOCNUM_NFE
       AND NUMERO          IN @LC_FILTRO-NUMERO
       AND DT_EMISSAO      IN @LC_FILTRO-DT_EMISSAO
       AND CHAVE_NFE       IN @LC_FILTRO-CHAVE_NFE
       AND E_TOMADORA      IN @LC_FILTRO-E_TOMADORA
       AND F_TOMADORA      IN @LC_FILTRO-F_TOMADORA
       AND P_EMISSOR       IN @LC_FILTRO-P_EMISSOR
       AND FORNE_CNPJ      IN @LC_FILTRO-FORNE_CNPJ
       AND FORNE_IE        IN @LC_FILTRO-FORNE_IE
       AND ST_DOCUMENTO    IN @LC_FILTRO-ST_DOCUMENTO
       AND ST_FISCAL       IN @LC_FILTRO-ST_FISCAL
       AND ST_FISICO       IN @LC_FILTRO-ST_FISICO
       AND ST_ARMAZEM      IN @LC_FILTRO-ST_ARMAZEM
       AND CD_DEPARTAMENTO IN @LC_FILTRO-CD_DEPARTAMENTO
       AND VBELN           IN @LC_FILTRO-VBELN
       AND EXISTS ( SELECT * FROM ZIB_NFE_DIST_ITM AS ITM
                     WHERE ITM~CHAVE_NFE EQ CAP~CHAVE_NFE
                       AND ITM~PROD_CFOP  IN @T_CFOPS
                       AND PROD_CFOP      IN @LC_FILTRO-PROD_CFOP
                       AND PROD_DESCRICAO IN @LC_FILTRO-PROD_DESCRICAO
                       AND ICMS_CST       IN @LC_FILTRO-ICMS_CST
                       AND IPI_CST        IN @LC_FILTRO-IPI_CST
                       AND PIS_CST        IN @LC_FILTRO-PIS_CST
                       AND COF_CST        IN @LC_FILTRO-COF_CST
                       AND PROD_NCM       IN @LC_FILTRO-PROD_NCM ).

    "Verificar Acesso
    MOVE IT_ZIB_NFE_DIST_TER[] TO IT_TER[].
    SORT IT_TER BY E_TOMADORA F_TOMADORA CD_DEPARTAMENTO.
    DELETE ADJACENT DUPLICATES FROM IT_TER COMPARING E_TOMADORA F_TOMADORA CD_DEPARTAMENTO.

    LOOP AT IT_TER INTO DATA(WA_TER).
      CLEAR: WA_ACESSO.
      WA_ACESSO-E_TOMADORA      = WA_TER-E_TOMADORA.
      WA_ACESSO-F_TOMADORA      = WA_TER-F_TOMADORA.
      WA_ACESSO-CD_DEPARTAMENTO = WA_TER-CD_DEPARTAMENTO.
      AUTHORITY-CHECK OBJECT 'ZNFE_INB' ID 'ZANFETER'   FIELD '00'
                                        ID 'ZNFETERMEP' FIELD WA_TER-E_TOMADORA
                                        ID 'ZNFETERFIL' FIELD WA_TER-F_TOMADORA
                                        ID 'ZNFETERDEP' FIELD WA_TER-CD_DEPARTAMENTO.
      IF SY-SUBRC IS INITIAL.
        WA_ACESSO-CK_ACESSO = ABAP_TRUE.
      ELSE.
        WA_ACESSO-CK_ACESSO = ABAP_FALSE.
        DELETE IT_ZIB_NFE_DIST_TER
         WHERE E_TOMADORA      EQ WA_TER-E_TOMADORA
           AND F_TOMADORA      EQ WA_TER-F_TOMADORA
           AND CD_DEPARTAMENTO EQ WA_TER-CD_DEPARTAMENTO.
      ENDIF.
      APPEND WA_ACESSO TO IT_ACESSO.
    ENDLOOP.

    DESCRIBE TABLE IT_ACESSO LINES DATA(QTD_ACESSO).
    DESCRIBE TABLE IT_ZIB_NFE_DIST_TER LINES DATA(QTD_NOTAS).

    IF QTD_ACESSO GT 0 AND QTD_NOTAS EQ 0.
      READ TABLE IT_ACESSO INDEX 1 INTO WA_ACESSO.
      RAISE EXCEPTION TYPE ZCX_NFE_INBOUND_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
                            MSGNO = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
                            ATTR1 = '00'
                            ATTR2 = CONV #( WA_ACESSO-E_TOMADORA )
                            ATTR3 = CONV #( WA_ACESSO-F_TOMADORA )
                            ATTR4 = CONV #( WA_ACESSO-CD_DEPARTAMENTO ) )
          MSGID  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGID
          MSGNO  = ZCX_NFE_INBOUND_EXCEPTION=>ZCX_PERMISSAO_ACESSO-MSGNO
          MSGV1  = '00'
          MSGV2  = CONV #( WA_ACESSO-E_TOMADORA )
          MSGV3  = CONV #( WA_ACESSO-F_TOMADORA )
          MSGV4  = CONV #( WA_ACESSO-CD_DEPARTAMENTO ).
    ENDIF.

    MOVE IT_ZIB_NFE_DIST_TER[] TO E_RESG.
    E_REGISTROS = E_RESG.

  ENDMETHOD.
ENDCLASS.
