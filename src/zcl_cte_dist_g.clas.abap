class ZCL_CTE_DIST_G definition
  public
  final
  create public .

*"* public components of class ZCL_CTE_DIST_G
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  constants TIPO_01 type ZDE_PROCESSO_CTE value '01' ##NO_TEXT.
  constants TIPO_02 type ZDE_PROCESSO_CTE value '02' ##NO_TEXT.
  constants TIPO_03 type ZDE_PROCESSO_CTE value '03' ##NO_TEXT.
  constants TIPO_04 type ZDE_PROCESSO_CTE value '04' ##NO_TEXT.
  constants TIPO_05 type ZDE_PROCESSO_CTE value '05' ##NO_TEXT.
  constants TIPO_06 type ZDE_PROCESSO_CTE value '06' ##NO_TEXT.
  constants TIPO_07 type ZDE_PROCESSO_CTE value '07' ##NO_TEXT.
  constants TIPO_08 type ZDE_PROCESSO_CTE value '08' ##NO_TEXT.
  constants TIPO_09 type ZDE_PROCESSO_CTE value '09' ##NO_TEXT.

  class-methods GET_MIRO_FRETE
    importing
      !I_CTE type ZIB_CTE_DIST_TER
    returning
      value(R_RBKP) type RBKP
    raising
      ZCX_CTE_DIST_G .
  class-methods ATRIBUI_DADOS_VT
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exceptions
      ERRO .
  class-methods ATUALIZA_CTE
    exceptions
      ERRO_SQL .
  class-methods AUTORIZACAO
    importing
      !I_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_CD_APROVACAO type ZDE_EST_APROVACAO
      !E_TP_AUTORIZADO type ZDE_TP_AUTORIZACAO
    changing
      value(I_E_DOMNAME) type CHAR30 optional
      value(I_TP_APROVACAO) type ZDE_TIP_APROVACAO optional
    exceptions
      ERRO
      CTE_FINALIZADO
      CTE_NAO_LOC
      CTE_BLOQUEADA
      SEM_AUTORIZACAO .
  class-methods AUTORIZACAO_VIEW
    importing
      !I_CD_APROVACAO type ZDE_EST_APROVACAO
    exceptions
      ID
      LANGUAGE
      NAME
      NOT_FOUND
      OBJECT
      REFERENCE_CHECK
      WRONG_ACCESS_TO_ARCHIVE
      NAO_ENCONTRADO .
    "! Verificar se Existe uma Autorizacao Especifica para a CT-e
    "! @parameter I_CD_CHAVE_CTE |Chave da CT-e
    "! @parameter I_TP_APROVACAO |Tipo de Aprovacao Desejada ZDM_TIP_APROVACAO
    "! @parameter R_AUTORIZADO |X - Autorizado
  class-methods AUTORIZADO
    importing
      !I_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !I_TP_APROVACAO type ZDE_TIP_APROVACAO
    returning
      value(R_AUTORIZADO) type CHAR01 .
  class-methods BUSCA_BANCO_PARCEIRO
    exporting
      !E_LFBK type LFBK
      !E_BNKA type BNKA
    changing
      !P_CTE type ZIB_CTE_DIST_TER
    exceptions
      ERRO_BANCO .
  class-methods BUSCA_IMPOSTOS_TAXAS
    importing
      !P_IVA type MWSKZ
      !P_DATA_DOCUMENTO type J_1BDOCDAT
      !P_SHIPFROM type J_1BTXSHPF
      !P_SHIPTO type J_1BTXSHPT
      !E_TOMADORA type BUKRS
      !F_TOMADORA type J_1BBRANC_
      !P_EMISSORA type LIFNR
      !P_MATNR type MATNR
    exporting
      !E_RATE_ICMS type J_1BTXRATE
      !E_RATE_PIS type J_1BTXRATE
      !E_RATE_COFINS type J_1BTXRATE
    exceptions
      SEM_IVA .
  class-methods BUSCA_MOTORISTAS
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_MOT_T type ZIB_CTE_DIST_MOT_T .
  class-methods BUSCA_NOTAS_C57
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_C57_T type ZIB_CTE_DIST_C57_T .
  class-methods BUSCA_NOTAS_ITENS
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_NIT_T type ZIB_CTE_DIST_NIT_T .
  class-methods BUSCA_NOTAS_N01
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_N01_T type ZIB_CTE_DIST_N01_T .
  class-methods BUSCA_NOTAS_N55
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_N55_T type ZIB_CTE_DIST_N55_T .
  class-methods BUSCA_PROXIMO_VENC_FATURA
    exporting
      !E_DATA_VENCIMENTO type DATUM
    exceptions
      ERRO .
  class-methods BUSCA_TOLERANCIA
    importing
      !P_COD_MERCADORIA type MATNR
    exporting
      !E_TOLERANCIA type ZDE_PERC_TOLERANCIA
    exceptions
      NAO_ACHOU .
  class-methods BUSCA_VEICULOS
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !E_VEI_T type ZIB_CTE_DIST_VEI_T .
  class-methods BUSCA_VOLUME_UTILIZADO_FERRO
    importing
      !I_CD_SEQ_LANC type ZDE_SEQ_LANC
    exporting
      !E_QTD_UTILIZADA type BRGEW_15
      !E_QTD_UTILIZADA_EMP type ZDE_SALDO_UTIL_EMP_T
      !E_QTD_UTILIZADA_PRD type ZDE_SALDO_UTIL_PRO_T
      !E_QTD_UTILIZADA_EMP_PROD type ZDE_SALDO_UTIL_EP_T .
  class-methods CALCULA_QUEBRA_PERDA
    importing
      !P_COD_MERCADORIA type MATNR
      !P_PESO_ORIGEM type ZDE_PESO_ORIGEM
      !P_PESO_DESTINO type ZDE_PESO_CHEGADA
      !P_VLR_FRETE type ZDE_VLR_FRETE
      !P_VLR_KG_TRASPORT type ZDE_VLR_KG_TRANS
      !P_VLR_KG_MERCADORIA type ZDE_VLR_KG_MERCA
    exporting
      !E_PESO_DIFERENCA type ZDE_PESO_DIF
      !E_PESO_QUEBRA type ZDE_QUEBRA
      !E_PESO_PERDA type ZDE_PERDA
      !E_VLR_QUEBRA type ZVLR_QUEBRA
      !E_VLR_PERDA type ZVLR_PERDA
      !E_VLR_LIQ_PAGAR type ZVLR_LIQ_PAGAR
      !E_PC_QUEBRA type ZDE_PERC_QUEBRA
      !E_PC_TOLERANCIA type ZDE_PERC_TOLERANCIA .
  class-methods DACTE
    importing
      !I_CTE type ZDE_CHAVE_DOC_E
    raising
      ZCX_CTE_INBOUND .
  class-methods GERAR_PAGAMENTO_AUTOMATICO
    exceptions
      ERRO_SQL .
  class-methods GET_VOLUME_VALOR_NOTA
    importing
      !I_N55T type ZIB_CTE_DIST_N55_T
      !I_N01T type ZIB_CTE_DIST_N01_T
    exporting
      !E_J_1BNFLIN_TAB type J_1BNFLIN_TAB .
  class-methods INFORMAR_DADOS_FATURAMENTO
    importing
      !I_CTE type ZIB_CTE_DIST_TER_T .
  class-methods PSQ_NFE
    exporting
      !E_DOCNUM type J_1BDOCNUM .
  class-methods VERIFICA_VENCIMENTO_FATURA
    importing
      !I_DATA_VENCIMENTO type DATUM
      !I_VALIDA_DIA_UTIL type CHAR01 default 'X'
    exceptions
      NAO_VALIDA .
  class-methods ADD_LOG_CTE_JOB
    importing
      !P_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !P_TYPE type BAPI_MTYPE
      !P_ID type SYMSGID default 'ZCTE_DISTRI'
      !P_NUM type SYMSGNO
      !P_MESSAGE_V1 type C optional
      !P_MESSAGE_V2 type C optional
      !P_MESSAGE_V3 type C optional
      !P_MESSAGE_V4 type C optional
      !P_ESTRATEGIA type CHAR01 optional
      !P_MENSAGEM type BAPI_MSG optional
    changing
      value(P_LC_SEQUENCIA) type ZDE_SEQ_LOG .
  class-methods VERIFICA_VOLUME_PRECO_FERRO
    importing
      !I_FORNECEDOR type LIFNR
      !I_PAIS type LAND1 default 'BR'
      !I_INICIO type J_1BTXJCD
      !I_FINAL type J_1BTXJCD
      !I_VLR_TARIFA type NETPR
      !I_UND_TARIFA type UNIT default 'TO'
      !I_MOEDA_TARIFA type WAERK default 'BRL'
      !I_TOMADOR type BUKRS
      !I_TOMADOR_CENTRO type J_1BBRANC_
      !I_DT_REFERENCIA type DATUM
      !I_QTD_FATURAR type BRGEW_15
      !I_MATERIAL type MATNR optional
      !I_TIPO_CONTRATO type SDABW default '0001'
    exporting
      !E_DISPONIVEL type ZDE_ZLEST0119_ALV
    exceptions
      SEM_ITINERARIO
      SEM_VOLUME
      SEM_VOLUME_EMPRESA
      SEM_VOLUME_DISPONIVEL
      SEM_MATERIAL .
  methods BUSCAR_CONTAS_MIRO
    importing
      !I_CTE type ZIB_CTE_DIST_TER
      !I_VSART type VSARTTR
      !I_SHTYP type SHTYP
      !I_ITEM_INVOICE type RBLGP
      !I_VALOR_ITEM type BAPIWRBTR
      !I_VALOR_PERDA type ZVLR_PERDA
      !I_VALOR_QUEBRA type ZVLR_QUEBRA
      !I_VALOR_VI type ZDE_VLR_VI
      !I_DT_REFERENCIA type BUDAT
    exporting
      !E_CONTAS type ZBAPI_INCINV_GL_ACCOUNT_T
    exceptions
      PARAM_CTB .
  methods BUSCAR_INFO_FERROVIARIO
    importing
      !P_CTE type ZIB_CTE_DIST_TER
      !P_GRAVAR type CHAR01 default 'X'
    exporting
      !E_REMESSAS type ZIB_CTE_DIST_N55_T
      !E_ORG_POINT type KNOTA
      !E_ORG_CUST type KUNNA
      !E_ORG_SUPPL type LIFNA
      !E_DEST_POINT type KNOTZ
      !E_DEST_CUST type KUNNZ
      !E_DEST_SUPPL type LIFNZ
      !E_ZLEST0044 type ZLEST0044
      !E_TIPO_CONTRATO type SDABW
      !E_MATNR_FATURADO type MATNR
    exceptions
      NAO_ACHOU .
  methods BUSCAR_TIPO_IMPOSTO
    importing
      !I_TAXGRP type J_1BTAXGRP
    exporting
      !E_KSCHL type KSCHL
    exceptions
      ERRO .
  methods BUSCA_FORNECEDOR
    importing
      !P_TP_DOC type ZDE_DOC_PARC
      !P_CNPJ type J_1BCGC
      !P_CPF type J_1BCPF
      !P_IE type J_1BSTAINS
    exporting
      !E_LFA1 type LFA1
    exceptions
      NAO_ACHOU .
  methods BUSCA_INFO_ALGODAO
    changing
      !P_CTE_DIST type ZIB_CTE_DIST_TER
    returning
      value(R_GMI_ALV) type ZIB_CTE_DIST_GMI_ALV_T .
  methods BUSCA_LOG_PROC_CTE
    importing
      !P_CHAVE type ZDE_CHAVE_DOC_E
    exporting
      !E_LOGS type ZDE_CTE_DIST_LOG_ALV_T .
  methods BUSCA_TEXTO_CIDADE
    importing
      !P_COUNTRY type LAND1 default 'BR'
      !P_UF type REGIO
      !P_IBGE type CHAR07
    exporting
      !E_NOME_CIDADE type TEXT60
    exceptions
      NAO_ACHOU .
  methods GERAR_DOC_TRANSPORTE
    importing
      !P_CTE_CHAVE type ZDE_CHAVE_DOC_E
    exporting
      !E_TKNUM type TKNUM
      !E_FKNUM type FKNUM
    exceptions
      DOC_TRANSP .
  methods GERAR_FATURA_FRETE
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E optional
      !P_ESTORNAR type CHAR01 default ' '
    changing
      !P_CTE type ZIB_CTE_DIST_TER
    exceptions
      NAO_ENC_FRETE
      FATURA
      PEDIDO
      COD_IVA
      BANCO_PARCEIRO
      PARAM_CTB
      BANCO_EMPRESA
      SEM_VT
      ERRO_ENTRADA_FISCAL
      MIRO_COMPENSADA
      PESO_CHEGADA
      NAO_AUTORIZADO .
  methods LER_DADOS_XI
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E optional
      !P_PENDENTES type CHAR01 optional
    exceptions
      FOREIGN_LOCK .
  methods PREPARA_REG_ALV_SAIDA
    importing
      !P_CTE_DIST type ZIB_CTE_DIST_TER_T
    exporting
      !E_ALV type ZDE_CTE_DIST_ALV_T .
  methods VALIDA_ETAPAS_CTE
    importing
      !P_TIPO type ZIB_CTE_DIST_FLG
      !P_CTE type ZIB_CTE_DIST_TER
      !P_CTE_N01 type ZIB_CTE_DIST_N01_T
      !P_CTE_N55 type ZIB_CTE_DIST_N55_T
      !P_CTE_C57 type ZIB_CTE_DIST_C57_T
      !P_GRAVAR_ETAPAS type CHAR01
    changing
      !P_CONCLUIDO type CHAR01 optional
      !P_PESO_CHEGADA type CHAR01 optional .
  class-methods GET_BASE_PIS_COFINS
    importing
      value(I_VALOR_FRETE) type J_1BNETVAL optional
      value(I_VALOR_ICMS) type J_1BNETVAL optional
    returning
      value(R_BASE) type J_1BBASE .
protected section.

  constants TRUE type CHAR01 value 'X' ##NO_TEXT.
  constants FALSE type CHAR01 value ' ' ##NO_TEXT.
  constants DS_TIPO_01 type SYMSGV value 'Frete Terceiro sobre Faturamento Próprio' ##NO_TEXT.
  constants DS_TIPO_02 type SYMSGV value 'Frete Próprio' ##NO_TEXT.
  constants DS_TIPO_03 type SYMSGV value 'Frete Próprio - Intercompany' ##NO_TEXT.
  constants DS_TIPO_04 type SYMSGV value 'Frete Próprio - Subcontratado' ##NO_TEXT.
  constants DS_TIPO_05 type SYMSGV value 'Frete Terceiro sobre Revisão de Fatura' ##NO_TEXT.
  constants DS_TIPO_06 type SYMSGV value 'Frete Terceiro sobre Nota Writer' ##NO_TEXT.
  constants DS_TIPO_07 type SYMSGV value 'Frete Terceiro sobre Mov. Mercadoria' ##NO_TEXT.
  constants DS_TIPO_08 type SYMSGV value 'Frete Remessa Conta Ordem de Terceiro' ##NO_TEXT.
  constants DS_TIPO_09 type SYMSGV value 'Frete Terceiro sobre Mov. Mercadoria (Sáida)' ##NO_TEXT.
  constants ERRO type CHAR01 value 'E' ##NO_TEXT.
  constants INFORMA type CHAR01 value 'I' ##NO_TEXT.
  constants SUCESSO type CHAR01 value 'S' ##NO_TEXT.

  class-methods INICIA_SEQUENCIA
    importing
      !P_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
    exporting
      !P_LC_SEQUENCIA type ZDE_SEQ_LOG .
  methods BUSCA_CTE_DADOS_ANTERIOR
    changing
      !E_CTE_DISTR type ZIB_CTE_DIST_TER
      !E_CTE_C57 type ZIB_CTE_DIST_C57
    exceptions
      NAO_ACHOU_CTE
      NAO_ESCRITURADO .
  methods BUSCA_DOCNUM_CHAVE_NT_PROP
    importing
      !P_CHAVE type ZDE_CHAVE_DOC_E
    exporting
      !E_ACTIVE type J_1BNFE_ACTIVE
      !E_DOCNUM type J_1BDOCNUM .
  methods BUSCA_DOCNUM_CHAVE
    importing
      !P_CTE_DIST type ZIB_CTE_DIST_TER optional
      !P_CHAVE type ZDE_CHAVE_DOC_E
      !P_CK_MANUAL type ZDE_CK_MANUAL optional
      !P_PSQ_CHAVE type CHAR01 optional
      !P_FORM type CHAR01 optional
    exporting
      !E_ACTIVE type J_1BNFE_ACTIVE
    changing
      !E_DOCNUM type J_1BDOCNUM
      !P_J_1BBRANCH type J_1BBRANCH optional
    exceptions
      NAO_ACHOU
      NAO_ACHOU_PARCEIRO .
  methods BUSCA_DOCNUM_NUMERO
    importing
      !P_CTE_DIST type ZIB_CTE_DIST_TER
      !P_NF01 type ZIB_CTE_DIST_N01
    exporting
      !E_J_1BNFDOC type J_1BNFDOC
    changing
      !E_DOCNUM type J_1BDOCNUM
      !P_J_1BBRANCH type J_1BBRANCH
    exceptions
      NAO_ACHOU
      NAO_ACHOU_PARCEIRO .
  methods BUSCA_ENTRADA_DO_FRETE
    importing
      !P_N01_T type ZIB_CTE_DIST_N01_T
      !P_N55_T type ZIB_CTE_DIST_N55_T
      !P_C57_T type ZIB_CTE_DIST_C57_T
    changing
      !E_CTE_DISTR type ZIB_CTE_DIST_TER
    exceptions
      ERRO_PEDIDO
      ERRO_MIRO_FRETE
      NAO_ACHOU
      SEM_IVA .
  methods BUSCA_FORN_EMPRESA
    changing
      !P_CTE_DIST type ZIB_CTE_DIST_TER
    exceptions
      NAO_ACHOU_FORNECEDOR .
  methods BUSCA_LINHA_NOTA
    importing
      !P_TIPO_CONTRATO type CHAR04 optional
      !P_J_1BNFDOC type J_1BNFDOC
    exporting
      !E_ZLEST0041 type ZLEST0041
    changing
      !E_CTE_DISTR type ZIB_CTE_DIST_TER
      !E_CTE_N55 type ZIB_CTE_DIST_N55 optional
      !E_CTE_N01 type ZIB_CTE_DIST_N01 optional
      !E_CTE_NIT type ZIB_CTE_DIST_NIT_T optional
    exceptions
      SEM_REMESSA
      SEM_DOC_TRANS
      SEM_RELACAO
      SEM_CUSTO .
  methods BUSCA_PESO_AUAVIARIO
    importing
      !P_DOCNUM type J_1BDOCNUM
    changing
      !E_CTE type ZIB_CTE_DIST_TER
    exceptions
      NAO_ACHOU .
  methods BUSCA_PESO_RODO_TERCEIRO
    importing
      !P_DOCNUM type J_1BDOCNUM
    changing
      !E_CTE type ZIB_CTE_DIST_TER
    exceptions
      NAO_ACHOU .
  methods BUSCA_PESO_RODO_TERCEIRO_VT
    importing
      !P_TKNUM type TKNUM
    changing
      !E_CTE_DISTR type ZIB_CTE_DIST_TER
      !E_CTE_N55 type ZIB_CTE_DIST_N55
      !E_CTE_N01 type ZIB_CTE_DIST_N01
      !E_CTE_NIT type ZIB_CTE_DIST_NIT_T
    exceptions
      NAO_ACHOU .
  methods BUSCA_PESO_TIP_FRETE
    importing
      !P_DOCNUM type J_1BDOCNUM
    changing
      !E_CTE type ZIB_CTE_DIST_TER
    exceptions
      NAO_ACHOU .
  methods BUSCA_PESO_TIP_FRETE_VT
    importing
      !P_TKNUM type TKNUM
    changing
      !E_CTE_N55 type ZIB_CTE_DIST_N55
      !E_CTE_N01 type ZIB_CTE_DIST_N01
      !E_CTE_NIT type ZIB_CTE_DIST_NIT_T
    exceptions
      NAO_ACHOU .
  methods BUSCA_TOMADOR_SERVICO
    importing
      !P_CTE_DIST type ZIB_CTE_DIST_TER
    exporting
      !E_J_1BBRANCH type J_1BBRANCH
    exceptions
      NAO_ACHOU_PARCEIRO .
  methods GERAR_COMPENSACAO
    importing
      !P_CTE type ZIB_CTE_DIST_TER
      !E_INVOICE_IN type RE_BELNR
      !E_YEAR_IN type GJAHR
      !E_INVOICE_OUT type RE_BELNR
      !E_YEAR_OUT type GJAHR
    exporting
      !I_BUKRS type BUKRS
      !I_BELNR type BELNR_D
    exceptions
      OUTROS .
  methods GERAR_ESCRIT_ENTRADA
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E optional
      !P_ZLEST0061 type ZLEST0061 optional
    changing
      !P_CTE type ZIB_CTE_DIST_TER
    exceptions
      NAO_ENC_FRETE
      NAO_CTE_FORN
      NAO_PARAM_IVA
      NAO_SERVICO_PARAM
      NAO_PARAM_CFOP
      ERRO .
  methods LER_DADOS_FERROVIARIO
    importing
      !P_TIPO_CONTRATO type CHAR04
    changing
      !P_CTE type ZIB_CTE_DIST_TER .
  methods LER_DADOS_RODOVIARIO
    importing
      !P_TIPO_CONTRATO type CHAR04 optional
    changing
      !P_CTE type ZIB_CTE_DIST_TER .
  methods BUSCA_GRP_MERCADORIA
    importing
      value(P_CHAVE_CTE) type ZDE_CHAVE_DOC_E optional
    changing
      value(P_MATKL) type J_1BNFLIN-MATKL .
private section.

*"* private components of class ZCL_CTE_DIST_G
*"* do not include other source files here!!!
  data IT_CTE type ZIB_CTE_DIST_TER_T .
  data IT_N01 type ZIB_CTE_DIST_N01_T .
  data IT_N55 type ZIB_CTE_DIST_N55_T .
  data IT_C57 type ZIB_CTE_DIST_C57_T .
  data IT_ANT type ZIB_CTE_DIST_ANT_T .
  data IT_NIT type ZIB_CTE_DIST_NIT_T .
  data IT_TIPOS type ZIB_CTE_DIST_FLG_T .
  data LC_SEQUENCIA type ZDE_SEQ_LOG .

  class-methods ADD_LOG_CTE_DIST
    importing
      !P_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !P_TYPE type BAPI_MTYPE
      !P_ID type SYMSGID default 'ZCTE_DISTRI'
      !P_NUM type SYMSGNO
      !P_MESSAGE_V1 type C optional
      !P_MESSAGE_V2 type C optional
      !P_MESSAGE_V3 type C optional
      !P_MESSAGE_V4 type C optional
      !P_ESTRATEGIA type CHAR01 optional
      !P_MENSAGEM type BAPI_MSG optional
    changing
      value(P_LC_SEQUENCIA) type ZDE_SEQ_LOG .
  class-methods GET_NFTYPE_ENTRADA
    returning
      value(E_NFTYPE) type J_1BNFTYPE .
  methods ATRIBUI_DADOS_CTE_07
    changing
      !P_CTE type ZIB_CTE_DIST_TER .
  methods ATRIBUI_DADOS_NOTA
    importing
      !P_TIPO_CONTRATO type CHAR04 optional
    exporting
      !E_DOCNUM type J_1BDOCNUM
      !E_CHAVE_ROM type ZCH_REF
    changing
      !P_CTE_DIST type ZIB_CTE_DIST_TER
      !P_NIT_T type ZIB_CTE_DIST_NIT_T
      !P_NF01 type ZIB_CTE_DIST_N01 optional
      !P_NF55 type ZIB_CTE_DIST_N55 optional
    exceptions
      ERRO .
  methods AUTORIZACAO_LOG
    changing
      !P_CTE type ZIB_CTE_DIST_TER .
  methods AUTORIZACAO_VERIFICAR
    importing
      !I_CTE type ZIB_CTE_DIST_TER
    exceptions
      BLOQUEADO .
  methods BUSCA_TROCA_NF_43
    importing
      !CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
    changing
      !E_N55 type ZIB_CTE_DIST_N55_T
      !E_N01 type ZIB_CTE_DIST_N01_T .
  methods ESTORNAR_DOC_CUSTO
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !I_FKNUM type FKNUM .
  methods ESTORNAR_DOC_TRANSPORTE
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !I_TKNUM type TKNUM .
  methods GERAR_DOC_CUSTO
    importing
      !P_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !P_TKNUM type TKNUM
      !P_DATA type DATUM
    exporting
      !E_FKNUM type FKNUM .
  methods MONTA_SHDB
    importing
      !P_DYNBEGIN type ANY
      !P_NAME type ANY
      !P_VALUE type ANY
    changing
      !E_SHDB type BDCDATA_TAB .
ENDCLASS.



CLASS ZCL_CTE_DIST_G IMPLEMENTATION.


  METHOD ADD_LOG_CTE_DIST.

    DATA: WA_LOG     TYPE ZIB_CTE_DIST_LOG,
          LC_MESSAGE TYPE BAPI_MSG.

    MOVE: P_MESSAGE_V1 TO WA_LOG-MESSAGE_V1,
          P_MESSAGE_V2 TO WA_LOG-MESSAGE_V2,
          P_MESSAGE_V3 TO WA_LOG-MESSAGE_V3,
          P_MESSAGE_V4 TO WA_LOG-MESSAGE_V4.

    WA_LOG-CD_CHAVE_CTE   = P_CD_CHAVE_CTE.
    WA_LOG-DT_ATUALIZACAO = SY-DATUM.
    WA_LOG-HR_ATUALIZACAO = SY-UZEIT.
    WA_LOG-NR_SEQUENCIA   = P_LC_SEQUENCIA.

    TRY .
        WA_LOG-TYPE           = P_TYPE.
        WA_LOG-ID             = P_ID.
        WA_LOG-NUM            = P_NUM.
        WA_LOG-MESSAGE_V1     = P_MESSAGE_V1.
        WA_LOG-MESSAGE_V2     = P_MESSAGE_V2.
        WA_LOG-MESSAGE_V3     = P_MESSAGE_V3.
        WA_LOG-MESSAGE_V4     = P_MESSAGE_V4.
        WA_LOG-BNAME          = SY-UNAME.
        WA_LOG-CK_ESTRATEGIA  = P_ESTRATEGIA.

        IF P_MENSAGEM IS INITIAL.
          IF P_ID IS NOT INITIAL AND P_TYPE IS NOT INITIAL.
            MESSAGE ID P_ID TYPE P_TYPE NUMBER P_NUM INTO WA_LOG-MESSAGE WITH WA_LOG-MESSAGE_V1 WA_LOG-MESSAGE_V2 WA_LOG-MESSAGE_V3 WA_LOG-MESSAGE_V4.
            MODIFY ZIB_CTE_DIST_LOG FROM WA_LOG.
          ELSE.
            CONCATENATE P_MESSAGE_V1 P_MESSAGE_V2 P_MESSAGE_V3 P_MESSAGE_V4 INTO WA_LOG-MESSAGE SEPARATED BY SPACE.
            MODIFY ZIB_CTE_DIST_LOG FROM WA_LOG.
          ENDIF.
        ELSE.
          WA_LOG-MESSAGE = P_MENSAGEM.
          MODIFY ZIB_CTE_DIST_LOG FROM WA_LOG.
        ENDIF.
      CATCH CX_ROOT.
    ENDTRY.

    ADD 1 TO P_LC_SEQUENCIA.


  ENDMETHOD.


  METHOD ATRIBUI_DADOS_CTE_07.

    DATA: WA_J_1BNFDOC TYPE J_1BNFDOC,
          IT_J_1BNFLIN TYPE TABLE OF J_1BNFLIN,
          WA_J_1BNFLIN TYPE J_1BNFLIN,
          WA_RBKP      TYPE RBKP,
          WA_RSEG      TYPE RSEG.

    IF P_CTE-DOCNUM_CTE IS NOT INITIAL.

      SELECT SINGLE *
        INTO WA_J_1BNFDOC
        FROM J_1BNFDOC
       WHERE DOCNUM EQ P_CTE-DOCNUM_CTE.

      P_CTE-BELNR = WA_J_1BNFDOC-BELNR.
      P_CTE-GJAHR = WA_J_1BNFDOC-GJAHR.

      SELECT * INTO TABLE IT_J_1BNFLIN
        FROM J_1BNFLIN
       WHERE DOCNUM EQ P_CTE-DOCNUM_CTE.

      READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN INDEX 1.

      P_CTE-MATNS        = WA_J_1BNFLIN-MATNR.
      P_CTE-PESO_ORIGEM	 = WA_J_1BNFLIN-MENGE.
      P_CTE-PESO_CHEGADA = WA_J_1BNFLIN-MENGE.

      CASE WA_J_1BNFLIN-REFTYP.
        WHEN 'LI'. "Logística: revisão de faturas

          SELECT SINGLE * INTO WA_RBKP
            FROM RBKP
           WHERE BELNR EQ P_CTE-BELNR
             AND GJAHR EQ P_CTE-GJAHR.

          P_CTE-DT_CHEGADA  = WA_RBKP-BUDAT.
          P_CTE-ZDT_MOV     = WA_RBKP-BUDAT.
          P_CTE-ZDT_VENCTO  = WA_RBKP-ZFBDT.
          P_CTE-ZBVTYP      = WA_RBKP-BVTYP.

          SELECT SINGLE * INTO WA_RSEG
            FROM RSEG
           WHERE BELNR EQ P_CTE-BELNR
             AND GJAHR EQ P_CTE-GJAHR
             AND BUZEI EQ WA_J_1BNFLIN-REFITM.

          IF SY-SUBRC IS INITIAL.
            P_CTE-EBELN = WA_RSEG-EBELN.
            P_CTE-EBELP = WA_RSEG-EBELP.
            P_CTE-MWSKZ = WA_RSEG-MWSKZ.
          ENDIF.
      ENDCASE.

    ENDIF.


  ENDMETHOD.


  METHOD ATRIBUI_DADOS_NOTA.

    DATA: WA_DOC        TYPE J_1BNFDOC,
          WA_J_1BBRANCH TYPE J_1BBRANCH,
          WA_ZLEST0041  TYPE ZLEST0041,
          WA_ACTIVE     TYPE J_1BNFE_ACTIVE,
          WA_NIT_T      TYPE ZIB_CTE_DIST_NIT.

    CLEAR: E_CHAVE_ROM.

    IF P_NF01 IS NOT INITIAL.

      CLEAR: P_NF01-BUKRS,
             P_NF01-BRANCH,
             P_NF01-FORM,
             P_NF01-PARVW,
             P_NF01-PARID,
             P_NF01-DIRECT,
             P_NF01-TKNUM,
             P_NF01-FKNUM,
             P_NF01-LBLNI,
             P_NF01-EBELN,
             P_NF01-EBELP,
             P_NF01-BELNR,
             P_NF01-GJAHR,
             P_NF01-VBELN_VF,
             P_NF01-VBELN_VL,
             P_NF01-VBELN_RE,
             P_NF01-GJAHR_RE,
             P_NF01-ZW_LCTO.

      CALL METHOD ME->BUSCA_DOCNUM_NUMERO
        EXPORTING
          P_CTE_DIST         = P_CTE_DIST
          P_NF01             = P_NF01
        IMPORTING
          E_J_1BNFDOC        = WA_DOC
        CHANGING
          E_DOCNUM           = P_NF01-DOCNUM_NF
          P_J_1BBRANCH       = WA_J_1BBRANCH
        EXCEPTIONS
          NAO_ACHOU          = 1
          NAO_ACHOU_PARCEIRO = 2
          OTHERS             = 3.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
      ELSE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE_DIST-CD_CHAVE_CTE
            P_TYPE         = SY-MSGTY
            P_ID           = SY-MSGID
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE_DIST-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_NUM          = 005
            P_MESSAGE_V1   = SY-MSGV1
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        P_NF01-BUKRS  = WA_DOC-BUKRS.
        P_NF01-BRANCH = WA_DOC-BRANCH.
        P_NF01-FORM   = WA_DOC-FORM.
        P_NF01-PARVW  = WA_DOC-PARVW.
        P_NF01-PARID  = WA_DOC-PARID.

        CLEAR: WA_ZLEST0041.

        CALL METHOD ME->BUSCA_LINHA_NOTA
          EXPORTING
            P_J_1BNFDOC   = WA_DOC
          IMPORTING
            E_ZLEST0041   = WA_ZLEST0041
          CHANGING
            E_CTE_DISTR   = P_CTE_DIST
            E_CTE_N01     = P_NF01
            E_CTE_NIT     = P_NIT_T
          EXCEPTIONS
            SEM_REMESSA   = 1
            SEM_DOC_TRANS = 2
            SEM_RELACAO   = 3
            SEM_CUSTO     = 4
            OTHERS        = 5.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
        ELSE.
          E_DOCNUM = WA_ZLEST0041-DOCNUM.
        ENDIF.

      ENDIF.

    ELSE.

      CLEAR: P_NF55-BUKRS,
             P_NF55-BRANCH,
             P_NF55-FORM,
             P_NF55-PARVW,
             P_NF55-PARID,
             P_NF55-DIRECT,
             P_NF55-TKNUM,
             P_NF55-FKNUM,
             P_NF55-LBLNI,
             P_NF55-EBELN,
             P_NF55-EBELP,
             P_NF55-BELNR,
             P_NF55-GJAHR,
             P_NF55-VBELN_VF,
             P_NF55-VBELN_VL,
             P_NF55-VBELN_RE,
             P_NF55-GJAHR_RE,
             P_NF55-ZW_LCTO.

      CALL METHOD ME->BUSCA_DOCNUM_CHAVE
        EXPORTING
          P_CTE_DIST   = P_CTE_DIST
          P_CHAVE      = P_NF55-N55_CHAVE_ACESSO
          P_CK_MANUAL  = P_NF55-CK_INC_MANUAL
        IMPORTING
          E_ACTIVE     = WA_ACTIVE
        CHANGING
          E_DOCNUM     = P_NF55-DOCNUM_NFE
          P_J_1BBRANCH = WA_J_1BBRANCH
        EXCEPTIONS
          NAO_ACHOU    = 1
          OTHERS       = 2.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
      ELSE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE_DIST-CD_CHAVE_CTE
            P_TYPE         = SY-MSGTY
            P_ID           = SY-MSGID
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        SELECT SINGLE * INTO WA_DOC FROM J_1BNFDOC WHERE DOCNUM EQ P_NF55-DOCNUM_NFE.
        MOVE WA_DOC-DOCNUM TO SY-MSGV1.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE_DIST-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_NUM          = 005
            P_MESSAGE_V1   = SY-MSGV1
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        P_NF55-BUKRS  = WA_DOC-BUKRS.
        P_NF55-BRANCH = WA_DOC-BRANCH.
        P_NF55-FORM   = WA_DOC-FORM.
        P_NF55-PARVW  = WA_DOC-PARVW.
        P_NF55-PARID  = WA_DOC-PARID.

        CALL METHOD ME->BUSCA_LINHA_NOTA
          EXPORTING
            P_J_1BNFDOC     = WA_DOC
            P_TIPO_CONTRATO = P_TIPO_CONTRATO
          IMPORTING
            E_ZLEST0041     = WA_ZLEST0041
          CHANGING
            E_CTE_DISTR     = P_CTE_DIST
            E_CTE_N55       = P_NF55
            E_CTE_NIT       = P_NIT_T
          EXCEPTIONS
            SEM_REMESSA     = 1
            SEM_DOC_TRANS   = 2
            SEM_RELACAO     = 3
            SEM_CUSTO       = 4
            OTHERS          = 5.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
        ELSE.
          E_DOCNUM = WA_ZLEST0041-DOCNUM.
        ENDIF.

      ENDIF.

    ENDIF.

    "Verificar se depara é de um faturamento agrupado - Filiais do Sul (Ex: 0160)
    IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZLEST0041 IS NOT INITIAL ) AND ( E_DOCNUM IS NOT INITIAL ).

      DATA: LVA_COUNT_DEPARA TYPE I.

      SELECT SINGLE COUNT(*)
        FROM ZLEST0041 INTO LVA_COUNT_DEPARA
       WHERE DOCNUM EQ E_DOCNUM.

      IF ( LVA_COUNT_DEPARA > 1 ) AND ( WA_ZLEST0041-CH_REFERENCIA IS NOT INITIAL ).
        SELECT SINGLE *
          FROM ZSDT0001 INTO @DATA(LWA_ZSDT0001_SAIDA)
         WHERE CH_REFERENCIA EQ @WA_ZLEST0041-CH_REFERENCIA
           AND TP_MOVIMENTO  EQ 'S'.

        IF ( SY-SUBRC EQ 0 ) AND ( LWA_ZSDT0001_SAIDA-DOC_REM IS NOT INITIAL ) AND ( LWA_ZSDT0001_SAIDA-CH_REFERENCIA IS NOT INITIAL ).
          E_CHAVE_ROM = LWA_ZSDT0001_SAIDA-CH_REFERENCIA.
        ENDIF.
      ENDIF.
    ENDIF.
    "Fim Verificar se depara é de um faturamento agrupado - Filiais do Sul (Ex: 0160)


  ENDMETHOD.


  METHOD ATRIBUI_DADOS_VT.

    DATA: P_CTE             TYPE ZIB_CTE_DIST_TER,
          IT_N01            TYPE TABLE OF ZIB_CTE_DIST_N01,
          IT_N55            TYPE TABLE OF ZIB_CTE_DIST_N55,
          WA_N01            TYPE ZIB_CTE_DIST_N01,
          WA_N55            TYPE ZIB_CTE_DIST_N55,
          IT_NIT            TYPE TABLE OF ZIB_CTE_DIST_NIT,
          WA_NIT            TYPE ZIB_CTE_DIST_NIT,
          WA_ZLEST0034      TYPE ZLEST0034,
          IT_ZLEST0034      TYPE TABLE OF ZLEST0034,
          WA_VTTK           TYPE VTTK,
          LV_ZVLR_KG_TRANSP TYPE ZDE_VLR_KG_TRANS.

    FIELD-SYMBOLS <FS34> TYPE ZLEST0034.

    SELECT SINGLE * INTO P_CTE
      FROM ZIB_CTE_DIST_TER
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E001 WITH P_CHAVE_CTE RAISING ERRO.
    ENDIF.

    SELECT * INTO TABLE IT_N01
      FROM ZIB_CTE_DIST_N01
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

    SELECT * INTO TABLE IT_NIT
      FROM ZIB_CTE_DIST_NIT
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

    SELECT * INTO TABLE IT_N55
      FROM ZIB_CTE_DIST_N55
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

    LOOP AT IT_N01 INTO WA_N01.
      IF WA_N01-TKNUM IS NOT INITIAL.
        WA_ZLEST0034-TKNUM = WA_N01-TKNUM.
        APPEND WA_ZLEST0034 TO IT_ZLEST0034.
      ENDIF.
    ENDLOOP.

    LOOP AT IT_N55 INTO WA_N55.
      IF WA_N55-TKNUM IS NOT INITIAL.
        WA_ZLEST0034-TKNUM = WA_N55-TKNUM.
        APPEND WA_ZLEST0034 TO IT_ZLEST0034.
      ENDIF.
    ENDLOOP.

    "Pagamento por Doc. de Transporte
    SORT IT_ZLEST0034 BY TKNUM.
    DELETE ADJACENT DUPLICATES FROM IT_ZLEST0034 COMPARING TKNUM.

    "Buscar dados gravados
    LOOP AT IT_ZLEST0034 ASSIGNING <FS34>.
      SELECT SINGLE * INTO <FS34>
        FROM ZLEST0034
       WHERE TKNUM EQ <FS34>-TKNUM.
    ENDLOOP.

    "Encontrar Tipo de transporte
    LOOP AT IT_ZLEST0034 ASSIGNING <FS34>.

      SELECT SINGLE * INTO WA_VTTK
        FROM VTTK WHERE TKNUM EQ <FS34>-TKNUM.

      <FS34>-EN_DOCNUM      = P_CTE-DOCNUM_CTE.
      <FS34>-EBELN          = P_CTE-EBELN.
      <FS34>-EBELP          = P_CTE-EBELP.
      <FS34>-TDLNR          = P_CTE-P_EMISSOR.
      <FS34>-LIFNR          = P_CTE-P_EMISSOR.
      <FS34>-BUKRS          = P_CTE-E_TOMADORA.
      <FS34>-WERKS          = P_CTE-F_TOMADORA.
      <FS34>-NR_CONHEC      = P_CTE-NUMR_CTE.
      <FS34>-ZDT_CONHEC     = P_CTE-DT_EMISSAO.
      <FS34>-SERIES         = P_CTE-NUMR_SERIE.
      <FS34>-ZDT_CHEGADA    = P_CTE-DT_CHEGADA.
      <FS34>-ZDT_MOV        = P_CTE-ZDT_MOV.
      <FS34>-ZDT_VENCTO     = P_CTE-ZDT_VENCTO.
      <FS34>-KALSM          = 'TAXBRA'.
      <FS34>-IVA            = P_CTE-MWSKZ.
      <FS34>-NFE            = TRUE.
      <FS34>-MATNS          = P_CTE-MATNS.
      <FS34>-BVTYP          = P_CTE-ZBVTYP.
      <FS34>-RE_BELNR       = P_CTE-BELNR.
      <FS34>-RE_GJAHR       = P_CTE-GJAHR.
      <FS34>-REGIO_EMISSOR  = P_CTE-INICIO_UF. "Origem
      <FS34>-REGIO_RECEPTOR = P_CTE-TERMINO_UF. "Destino
      <FS34>-SHTYP          = WA_VTTK-SHTYP.
      <FS34>-ADD03          = WA_VTTK-ADD03.

      "Documento de Fatura somente possui um item
      IF P_CTE-BELNR IS INITIAL.
        CLEAR: <FS34>-RE_ITEM.
      ELSE.
        <FS34>-RE_ITEM        = 1.
      ENDIF.

      "Nota Fiscal Normal
      READ TABLE IT_N01 INTO WA_N01 WITH KEY TKNUM = <FS34>-TKNUM.
      IF SY-SUBRC IS INITIAL.
        <FS34>-NFENUM           = WA_N01-N01_NR_NF.
        <FS34>-DMBTR            = WA_N01-ZVLR_VI.
        <FS34>-DMBTR_DOC        = WA_N01-ZVLR_FRETE.
        <FS34>-FKNUM            = WA_N01-FKNUM.
        <FS34>-LBLNI            = WA_N01-LBLNI.
        <FS34>-VALOR_MERCADORIA = WA_N01-ZVLR_MERCADORIA.
        <FS34>-DOCNUM           = WA_N01-DOCNUM_NF.
        <FS34>-ZPESO_ORIGEM     = WA_N01-PESO_ORIGEM.
        <FS34>-ZPESO_DESTINO    = WA_N01-PESO_CHEGADA.
        <FS34>-ZPESO_DIFERENCA  = WA_N01-ZPESO_DIFERENCA.
        <FS34>-ZQUEBRA          = WA_N01-ZQUEBRA.
        <FS34>-ZPERDA           = WA_N01-ZPERDA.
        <FS34>-ZVLR_QUEBRA      = WA_N01-ZVLR_QUEBRA.
        <FS34>-ZVLR_PERDA       = WA_N01-ZVLR_PERDA.
        <FS34>-ZVLR_LIQ_PAGAR   = WA_N01-ZVLR_LIQ_PAGAR.
        LV_ZVLR_KG_TRANSP	      = WA_N01-ZVLR_KG_TRANSP.
        READ TABLE IT_NIT INTO WA_NIT WITH KEY CD_CHAVE_CTE = WA_N01-CD_CHAVE_CTE
                                               DOCNUM       = WA_N01-DOCNUM_NF.
        IF SY-SUBRC IS INITIAL.
          <FS34>-GEWEI = WA_NIT-MEINS.
          <FS34>-MATNR = WA_NIT-ZMATNR_MERC.
          IF LV_ZVLR_KG_TRANSP IS INITIAL.
            LV_ZVLR_KG_TRANSP	= WA_NIT-ZVLR_KG_TRANSP.
          ENDIF.
        ENDIF.

        "<FS34>-LFGJA -- Exercício do documento de referência
      ELSE.
        READ TABLE IT_N55 INTO WA_N55 WITH KEY TKNUM = <FS34>-TKNUM.
        IF SY-SUBRC IS INITIAL.
          <FS34>-NFENUM           = WA_N55-N55_CHAVE_ACESSO+25(9).
          <FS34>-DMBTR            = WA_N55-ZVLR_VI.
          <FS34>-DMBTR_DOC        = WA_N55-ZVLR_FRETE.
          <FS34>-FKNUM            = WA_N55-FKNUM.
          <FS34>-LBLNI            = WA_N55-LBLNI.
          <FS34>-VALOR_MERCADORIA = WA_N55-ZVLR_MERCADORIA.
          <FS34>-DOCNUM           = WA_N55-DOCNUM_NFE.
          <FS34>-ZPESO_ORIGEM     = WA_N55-PESO_ORIGEM.
          <FS34>-ZPESO_DESTINO    = WA_N55-PESO_CHEGADA.
          <FS34>-ZPESO_DIFERENCA  = WA_N55-ZPESO_DIFERENCA.
          <FS34>-ZQUEBRA          = WA_N55-ZQUEBRA.
          <FS34>-ZPERDA           = WA_N55-ZPERDA.
          <FS34>-ZVLR_QUEBRA      = WA_N55-ZVLR_QUEBRA.
          <FS34>-ZVLR_PERDA       = WA_N55-ZVLR_PERDA.
          <FS34>-ZVLR_LIQ_PAGAR   = WA_N55-ZVLR_LIQ_PAGAR.
          LV_ZVLR_KG_TRANSP	      = WA_N55-ZVLR_KG_TRANSP.
          READ TABLE IT_NIT INTO WA_NIT WITH KEY CD_CHAVE_CTE = WA_N55-CD_CHAVE_CTE
                                                 DOCNUM       = WA_N55-DOCNUM_NFE.
          IF SY-SUBRC IS INITIAL.
            <FS34>-GEWEI            = WA_NIT-MEINS.
            <FS34>-MATNR            = WA_NIT-ZMATNR_MERC.
            IF LV_ZVLR_KG_TRANSP IS INITIAL.
              LV_ZVLR_KG_TRANSP	= WA_NIT-ZVLR_KG_TRANSP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF P_CTE-ZBASE_ICMS GT 0.
*---> 07/06/2023 - Migração S4 - JS
*            <FS34>-BASE_ICMS  = <FS34>-DMBTR_DOC.
             <FS34>-BASE_ICMS = CONV #( <FS34>-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS
        <FS34>-RATE_ICMS  = P_CTE-ZRATE_ICMS.
        <FS34>-VALOR_ICMS = <FS34>-DMBTR_DOC * ( P_CTE-ZRATE_ICMS / 100 ).
      ENDIF.

      IF P_CTE-ZBASE_PIS GT 0.
*---> 07/06/2023 - Migração S4 - JS
*            <FS34>-BASE_PIS  = <FS34>-DMBTR_DOC.
        <FS34>-BASE_PIS = CONV #( <FS34>-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS
        <FS34>-RATE_PIS  = P_CTE-ZRATE_PIS.
        <FS34>-VALOR_PIS = <FS34>-DMBTR_DOC * ( P_CTE-ZRATE_PIS / 100 ).
      ENDIF.

      IF P_CTE-ZBASE_COFINS GT 0.
*---> 07/06/2023 - Migração S4 - JS
*            <FS34>-BASE_COFINS  = <FS34>-DMBTR_DOC.
             <FS34>-BASE_COFINS = CONV #( <FS34>-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS

        <FS34>-RATE_COFINS  = P_CTE-ZRATE_COFINS.
        <FS34>-VALOR_COFINS = <FS34>-DMBTR_DOC * ( P_CTE-ZRATE_COFINS / 100 ).
      ENDIF.

      CASE <FS34>-GEWEI.
        WHEN 'KG'.
          LV_ZVLR_KG_TRANSP = LV_ZVLR_KG_TRANSP * 1000.
          MOVE LV_ZVLR_KG_TRANSP TO <FS34>-KBETR.
      ENDCASE.

      MODIFY ZLEST0034 FROM <FS34>.

      CALL FUNCTION 'Z_LES_REVISAO_FATURA_TERC'
        EXPORTING
          P_ZLEST0034 = <FS34>.

    ENDLOOP.

  ENDMETHOD.


  METHOD ATUALIZA_CTE.

    TYPES: BEGIN OF TY_RBKP_ESTORNO,
             BELNR TYPE RBKP-BELNR,
             STBLG TYPE RBKP-STBLG,
           END OF TY_RBKP_ESTORNO.

    DATA: CTE TYPE REF TO ZCL_CTE_DIST_G.

    DATA: EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
          ERROR_TEXT TYPE STRING,
          IT_CHAVE   TYPE TABLE OF ZDE_CHAVE_DOC_E,
          VL_CHAVE   TYPE ZDE_CHAVE_DOC_E,
          WA_CTE     TYPE ZIB_CTE_DIST_TER.

    DATA: LIT_RBKP_ESTORNO TYPE TABLE OF TY_RBKP_ESTORNO.

    DATA: LVA_CPU_DT TYPE RBKP-CPUDT.

    CREATE OBJECT CTE.

    " Verifica Miro Estornada """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    TRY.
*        EXEC SQL.
*          OPEN DOCUMENTOS FOR
*            SELECT T.CD_CHAVE_CTE
*              FROM SAPHANADB.ZIB_CTE_DIST_TER T
*             WHERE T.MANDT         = '300'
*               AND T.CK_FINALIZADO = 'X'
*               AND TRIM(T.BELNR) IS NOT NULL
*               AND TRIM(T.GJAHR) IS NOT NULL
*               AND T.TIMESTAMP   >= TO_CHAR(SYSDATE-360,'YYYYMMDDHH24MISS')
*               AND EXISTS ( SELECT * FROM SAPHANADB.RBKP R
*                             WHERE R.MANDT = T.MANDT
*                               AND R.BELNR = T.BELNR
*                               AND R.GJAHR = T.GJAHR
*                               AND TRIM(R.STBLG) IS NOT NULL )
*        ENDEXEC.
*      CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
*        ERROR_TEXT = EXC_REF->GET_TEXT( ).
*        MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
*    ENDTRY.
*
*    DO.
*      EXEC SQL.
*        FETCH NEXT DOCUMENTOS INTO
*          :VL_CHAVE
*      ENDEXEC.
*      IF SY-SUBRC <> 0.
*        EXIT.
*      ELSE.
*        APPEND VL_CHAVE TO IT_CHAVE.
*      ENDIF.
*    ENDDO.
*
*    EXEC SQL.
*      CLOSE DOCUMENTOS
*    ENDEXEC.

    CLEAR: IT_CHAVE[].

    LVA_CPU_DT = SY-DATUM - 10.

    SELECT BELNR STBLG
      FROM RBKP AS R INTO TABLE LIT_RBKP_ESTORNO
     WHERE CPUDT GE LVA_CPU_DT
       AND STBLG NE SPACE
       and STBLG NE '0000000000'
       AND EXISTS ( SELECT CD_CHAVE_CTE
                      FROM ZIB_CTE_DIST_TER AS X
                     WHERE X~BELNR = R~STBLG ).

    IF LIT_RBKP_ESTORNO[] IS NOT INITIAL.
      SELECT CD_CHAVE_CTE
        FROM ZIB_CTE_DIST_TER INTO TABLE @DATA(LIT_ZIB_TER)
         FOR ALL ENTRIES IN @LIT_RBKP_ESTORNO
       WHERE BELNR EQ @LIT_RBKP_ESTORNO-STBLG.

      LOOP AT LIT_ZIB_TER INTO DATA(LWA_ZIB_TER).

        SELECT SINGLE * INTO WA_CTE
          FROM ZIB_CTE_DIST_TER
         WHERE CD_CHAVE_CTE EQ LWA_ZIB_TER-CD_CHAVE_CTE.

        CHECK SY-SUBRC EQ 0.

        CLEAR: WA_CTE-DOCNUM_CTE,
               WA_CTE-CK_FINALIZADO,
               WA_CTE-TP_PROCESSO_CTE,
               WA_CTE-BELNR,
               WA_CTE-GJAHR.

        MODIFY ZIB_CTE_DIST_TER FROM WA_CTE.
      ENDLOOP.
    ENDIF.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Verifica Miro Estornada """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CALL METHOD CTE->LER_DADOS_XI
      EXPORTING
        P_PENDENTES = TRUE.

  ENDMETHOD.


  METHOD AUTORIZACAO.


    DATA: P_LC_SEQUENCIA TYPE ZDE_SEQ_LOG,
          E_DDTEXT       TYPE VAL_TEXT,
          IT_DD07V       TYPE TABLE OF DD07V,
          WA_DD07V       TYPE DD07V.
      " Initialize or clear the variables - 178025 CS2023000574 Job dinâmico PSA
  CLEAR: P_LC_SEQUENCIA, E_DDTEXT, IT_DD07V, WA_DD07V.

    "Selecionar Tipo de Aprovação/Recusa
    CALL FUNCTION 'ZCTE_DIST_SEL_APROVACAO'
      EXPORTING
        I_CD_CHAVE_CTE  = I_CD_CHAVE_CTE
      IMPORTING
        E_DOMNAME       = I_E_DOMNAME
        E_DDTEXT        = E_DDTEXT
      CHANGING
        E_TP_APROVACAO  = I_TP_APROVACAO
      EXCEPTIONS
        CTE_NAO_LOC     = 1
        SEM_AUTORIZACAO = 2
        OTHERS          = 3.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING CTE_NAO_LOC.
      WHEN 2.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING SEM_AUTORIZACAO.
      WHEN 3.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
    ENDCASE.

    CHECK I_E_DOMNAME EQ 'ZDM_TIP_APROVACAO'.

    CALL FUNCTION 'ZCTE_DIST_AUTORIZACAO'
      EXPORTING
        P_CD_CHAVE_CTE  = I_CD_CHAVE_CTE
        P_TP_APROVACAO  = I_TP_APROVACAO
      IMPORTING
        P_CD_APROVACAO  = E_CD_APROVACAO
      CHANGING
        P_TP_AUTORIZADO = E_TP_AUTORIZADO
      EXCEPTIONS
        ERRO            = 1
        CTE_FINALIZADO  = 2
        CTE_NAO_LOC     = 3
        CTE_BLOQUEADA   = 4
        CANCELADO       = 5
        OTHERS          = 6.

    CASE SY-SUBRC.
      WHEN 0.

        CALL METHOD ZCL_CTE_DIST_G=>INICIA_SEQUENCIA
          EXPORTING
            P_CD_CHAVE_CTE = I_CD_CHAVE_CTE
          IMPORTING
            P_LC_SEQUENCIA = P_LC_SEQUENCIA.

        CALL FUNCTION 'GET_DOMAIN_VALUES'
          EXPORTING
            DOMNAME    = 'ZDM_TP_AUTORIZACAO'
          TABLES
            VALUES_TAB = IT_DD07V.

        READ TABLE IT_DD07V INTO WA_DD07V WITH KEY DOMVALUE_L = E_TP_AUTORIZADO.

        CASE I_TP_APROVACAO.
          WHEN '01'. "01  Chegada de Documentos
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '02'. "02  Autorização de Pagamento Complemento
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '03'. "03  Travar Pagamento
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'E'.
              WHEN '02'.
                SY-MSGTY = 'S'.
            ENDCASE.
          WHEN '04'. "04  Peso e Data de Chegada ZFRL e ZRDC
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '05'. "05  Liberação de Pagamento de CT-e com Erro
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '06'. "06  Ignorar a Série na Busca de Nota Fiscal (Modelo 01/04)
            "01	Autorizado
            "02	Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '07'. "07 Liberar Edição/Inclusão/Exclusão de Nota Fiscal Modelos 01/1A/04.
            "01 Autorizado
            "02 Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
          WHEN '08'. "07 Liberar Edição/Inclusão/Exclusão de Nota Fiscal Modelo 55.
            "01 Autorizado
            "02 Negado
            CASE E_TP_AUTORIZADO.
              WHEN '01'.
                SY-MSGTY = 'S'.
              WHEN '02'.
                SY-MSGTY = 'E'.
            ENDCASE.
        ENDCASE.

        SY-MSGNO = '000'.
        SY-MSGV1 = WA_DD07V-DDTEXT.
        SY-MSGV2 = E_DDTEXT.
        SY-MSGV3 = E_TP_AUTORIZADO.
        SY-MSGV4 = E_CD_APROVACAO.

        CALL METHOD ZCL_CTE_DIST_G=>ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = I_CD_CHAVE_CTE
            P_TYPE         = SY-MSGTY
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
            P_ESTRATEGIA   = 'X'
          CHANGING
            P_LC_SEQUENCIA = P_LC_SEQUENCIA.

      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
      WHEN 2.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING CTE_FINALIZADO.
      WHEN 3.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING CTE_NAO_LOC.
      WHEN 4.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING CTE_BLOQUEADA.
      WHEN 5 OR 6.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO.
    ENDCASE.

  ENDMETHOD.


  METHOD AUTORIZACAO_LOG.


    DATA: IT_ZIB_CTE_DIST_EAP TYPE TABLE OF ZIB_CTE_DIST_EAP,
          WA_ZIB_CTE_DIST_EAP TYPE ZIB_CTE_DIST_EAP,
          IT_DD07V_AP         TYPE TABLE OF DD07V,
          IT_DD07V_AU         TYPE TABLE OF DD07V,
          WA_DD07V_AP         TYPE DD07V,
          WA_DD07V_AU         TYPE DD07V.

    SELECT * INTO TABLE IT_ZIB_CTE_DIST_EAP
      FROM ZIB_CTE_DIST_EAP
     WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE
     ORDER BY DT_AUTORIZACAO HR_AUTORIZACAO CD_APROVACAO.

    CHECK IT_ZIB_CTE_DIST_EAP[] IS NOT INITIAL.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        DOMNAME    = 'ZDM_TIP_APROVACAO'
      TABLES
        VALUES_TAB = IT_DD07V_AP.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        DOMNAME    = 'ZDM_TP_AUTORIZACAO'
      TABLES
        VALUES_TAB = IT_DD07V_AU.

    LOOP AT IT_ZIB_CTE_DIST_EAP INTO WA_ZIB_CTE_DIST_EAP.

      READ TABLE IT_DD07V_AP INTO WA_DD07V_AP WITH KEY DOMVALUE_L = WA_ZIB_CTE_DIST_EAP-TP_APROVACAO.

      READ TABLE IT_DD07V_AU INTO WA_DD07V_AU WITH KEY DOMVALUE_L = WA_ZIB_CTE_DIST_EAP-TP_AUTORIZADO.

      CASE WA_ZIB_CTE_DIST_EAP-TP_AUTORIZADO.
        WHEN '01'.
          SY-MSGTY = 'S'.
        WHEN '02'.
          SY-MSGTY = 'E'.
      ENDCASE.

      SY-MSGV1 = WA_DD07V_AU-DDTEXT.
      SY-MSGV2 = WA_DD07V_AP-DDTEXT.
      SY-MSGV3 = WA_ZIB_CTE_DIST_EAP-TP_APROVACAO.
      SY-MSGV4 = WA_ZIB_CTE_DIST_EAP-CD_APROVACAO.

      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = WA_ZIB_CTE_DIST_EAP-CD_CHAVE_CTE
          P_TYPE         = SY-MSGTY
          P_ID           = 'ZCTE_DISTRI'
          P_NUM          = 000
          P_MESSAGE_V1   = SY-MSGV1
          P_MESSAGE_V2   = SY-MSGV2
          P_MESSAGE_V3   = SY-MSGV3
          P_MESSAGE_V4   = SY-MSGV4
          P_ESTRATEGIA   = 'X'
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

    ENDLOOP.

  ENDMETHOD.


  METHOD AUTORIZACAO_VERIFICAR.


    DATA: IT_DOC_EAP TYPE TABLE OF ZIB_CTE_DIST_EAP,
          WA_DOC_EAP TYPE ZIB_CTE_DIST_EAP.

    SELECT * INTO TABLE IT_DOC_EAP
      FROM ZIB_CTE_DIST_EAP
     WHERE CD_CHAVE_CTE EQ I_CTE-CD_CHAVE_CTE
       AND CK_ULTIMO    EQ ABAP_TRUE.

    SORT IT_DOC_EAP BY TP_APROVACAO.

    "Verifica Trava de Pagamento
    READ TABLE IT_DOC_EAP INTO WA_DOC_EAP WITH KEY TP_APROVACAO = '03' BINARY SEARCH.
    IF SY-SUBRC IS INITIAL AND WA_DOC_EAP-TP_AUTORIZADO EQ '01'. "Travado o Pagamento
      MESSAGE E162 RAISING BLOQUEADO.
    ELSE.
      "Verificar Aprovação de Pagamento de Complemento
      IF I_CTE-CD_TIPO_CTE EQ '1'.

        READ TABLE IT_DOC_EAP INTO WA_DOC_EAP WITH KEY TP_APROVACAO = '02' BINARY SEARCH.
        IF SY-SUBRC IS INITIAL AND WA_DOC_EAP-TP_AUTORIZADO EQ '02'. "Não Autorizado o Pagamento
          MESSAGE E162 RAISING BLOQUEADO.
        ELSEIF  SY-SUBRC IS NOT INITIAL.
          MESSAGE E162 RAISING BLOQUEADO.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD AUTORIZACAO_VIEW.


    CALL FUNCTION 'ZCTE_DIST_VIEW_APROVACAO'
      EXPORTING
        P_CD_APROVACAO          = I_CD_APROVACAO
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        NAO_ENCONTRADO          = 8
        OTHERS                  = 9.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ID.
      WHEN 2.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING LANGUAGE.
      WHEN 3.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAME.
      WHEN 4.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NOT_FOUND.
      WHEN 5.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OBJECT.
      WHEN 6.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING REFERENCE_CHECK.
      WHEN 7.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING WRONG_ACCESS_TO_ARCHIVE.
      WHEN 8.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_ENCONTRADO.
    ENDCASE.

  ENDMETHOD.


  METHOD AUTORIZADO.


    DATA: WA_ZIB_CTE_DIST_EAP TYPE ZIB_CTE_DIST_EAP.

    WA_ZIB_CTE_DIST_EAP-CD_CHAVE_CTE  = I_CD_CHAVE_CTE.
    WA_ZIB_CTE_DIST_EAP-TP_APROVACAO  = I_TP_APROVACAO.
    CASE I_TP_APROVACAO.
      WHEN '03'.
        WA_ZIB_CTE_DIST_EAP-TP_AUTORIZADO = '02'.
      WHEN OTHERS.
        WA_ZIB_CTE_DIST_EAP-TP_AUTORIZADO = '01'.
    ENDCASE.
    WA_ZIB_CTE_DIST_EAP-CK_ULTIMO     = ABAP_TRUE.

    SELECT SINGLE * INTO WA_ZIB_CTE_DIST_EAP
      FROM ZIB_CTE_DIST_EAP
     WHERE CD_CHAVE_CTE  EQ WA_ZIB_CTE_DIST_EAP-CD_CHAVE_CTE
       AND TP_APROVACAO  EQ WA_ZIB_CTE_DIST_EAP-TP_APROVACAO
       AND TP_AUTORIZADO EQ WA_ZIB_CTE_DIST_EAP-TP_AUTORIZADO
       AND CK_ULTIMO     EQ WA_ZIB_CTE_DIST_EAP-CK_ULTIMO.

    IF SY-SUBRC IS INITIAL.
      R_AUTORIZADO = ABAP_TRUE.
    ELSE.
      R_AUTORIZADO = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD buscar_contas_miro.


    DATA: it_zlest0021  TYPE TABLE OF zlest0021,
          wa_zlest0021  TYPE zlest0021,
          wa_contas     TYPE bapi_incinv_create_gl_account,
          lc_zvlr_frete TYPE zde_vlr_frete,
          lc_vlr_ajuste TYPE zde_vlr_frete.

    CLEAR: e_contas.

    "Controle de desterminação conta razão
*      1  Cofins a Recuperar
*      2  Debito Frete
*      3  Desconto Seguro
*      4  Forn. Repon
*      5  Icms a Recuperar
*      6  Inss 20% empresa
*      7  INSS Retido
*      8  IRRF Retido
*      9  Pis a Recuperar
*      10	Seguro a Pagar
*      11	Sest/senat
*      12	Subcontratado/Fornecedor
*      13	Transitoria
*      14	Vale Pedágio
*      15	Outros
*      16	Quebra
*      17	Sobra
*      18	Perda
*      19	EM/EF Débito
*      20	EM/EF Crédito
*      21	Estadia
*      22	Compl Preço
*      23	Recuperação Seguro
*      24 Vlr total impostos
*      ...
*     31 Debito Posterior

    DATA: rgveicu TYPE RANGE OF zde_tp_prop_veiculo_ctb.
    rgveicu = VALUE #( sign = 'I' option = 'EQ' ( low = space high = space ) ( low = '0' high = '0' ) ).

    TRY .

          zcl_controle_conta_razao=>get_instance(
            )->get_conta_razao(
            EXPORTING
              i_shtyp                  = i_shtyp  " Tipo de transporte
              i_tcode                  = 'MIRO'   " Código de transação
              i_fatura                 = 'T'      " Emissor da fatura - Fretes
              i_tp_emissor             = 'T'      " Tipo de emissor
              i_operfrete_range        = VALUE #( sign = 'I' option = 'EQ'
                                                    ( low = '2' high = '2' )
                                                    ( low = '5' high = '5' )
                                                    ( low = '14' high = '14' )
                                                    ( low = '16' high = '16' )
                                                    ( low = '18' high = '18' )
                                                    ( low = '19' high = '19' )
                                                    ( low = '20' high = '20' )
                                                    ( low = '24' high = '24' ) ) " Ranges Operação de lançamento no razão - Frete
              i_tp_veiculo             = rgveicu    " Tipo de Proprietário de Veículo para Contabilização
              i_dt_referencia          = i_dt_referencia  " Data de lançamento no documento
            IMPORTING
              e_it_zlest0021           = it_zlest0021   " Controle de desterminação conta razão
          ).

      CATCH zcx_controle_conta_razao.    " .
        MESSAGE e001 RAISING param_ctb.
    ENDTRY.


*    IF ( I_VSART NE '01' ) AND ( I_VSART NE '07' ).
*      DATA(LC_METODO_ANTIGO) = ABAP_TRUE.
*    ELSE.
*      READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '2'.
*      IF SY-SUBRC IS INITIAL.
*        LC_METODO_ANTIGO = ABAP_TRUE.
*      ELSE.
*        LC_METODO_ANTIGO = ABAP_FALSE.
*      ENDIF.
*    ENDIF.

    READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '2'.
    IF sy-subrc IS INITIAL.
      DATA(lc_metodo_antigo) = abap_true.
    ELSE.
      lc_metodo_antigo       = abap_false.
    ENDIF.

    "16	Quebra
    IF i_valor_quebra GT 0.
      READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '16'.
      IF sy-subrc IS INITIAL.
        CLEAR: wa_contas.
        wa_contas-invoice_doc_item = i_item_invoice.
        wa_contas-gl_account       = wa_zlest0021-razaocred.
        wa_contas-item_amount      = i_valor_quebra.
        wa_contas-db_cr_ind        = 'H'.
        wa_contas-comp_code        = i_cte-e_tomadora.
        wa_contas-bus_area         = i_cte-f_tomadora.
        CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
        CONCATENATE 'Quebra de Frete' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
        APPEND wa_contas TO e_contas.
      ELSE.
        MESSAGE e077 WITH '16' RAISING param_ctb.
      ENDIF.
    ENDIF.

    "18	Perda
    IF i_valor_perda GT 0.
      READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '18'.
      IF sy-subrc IS INITIAL.
        CLEAR: wa_contas.
        wa_contas-invoice_doc_item = i_item_invoice.
        wa_contas-gl_account       = wa_zlest0021-razaocred.
        wa_contas-item_amount      = i_valor_perda.
        wa_contas-db_cr_ind        = 'H'.
        wa_contas-comp_code        = i_cte-e_tomadora.
        wa_contas-bus_area         = i_cte-f_tomadora.
        CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
        CONCATENATE 'Perda de Frete' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
        APPEND wa_contas TO e_contas.
      ELSE.
        MESSAGE e077 WITH '18' RAISING param_ctb.
      ENDIF.
    ENDIF.

      lc_zvlr_frete = i_valor_vi - i_valor_item.

*    IF i_valor_item GT 0.
*
*      READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '31'.
*      IF sy-subrc IS INITIAL.
*        CLEAR: wa_contas.
*        wa_contas-invoice_doc_item = i_item_invoice.
*        wa_contas-gl_account       = wa_zlest0021-razaodeb.
*        wa_contas-item_amount      = i_valor_item.
*        wa_contas-db_cr_ind        = 'S'.
*        wa_contas-comp_code        = i_cte-e_tomadora.
*        wa_contas-bus_area         = i_cte-f_tomadora.
*        CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
*        CONCATENATE 'Débito Posterior' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
*        APPEND wa_contas TO e_contas.
*
*        wa_contas-db_cr_ind        = 'H'.
*        wa_contas-gl_account       = wa_zlest0021-razaocred.
*        wa_contas-item_amount      = i_valor_item. "I_VALOR_VI.
*        CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
*        CONCATENATE 'Débito Posterior' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
*        APPEND wa_contas TO e_contas.
*      ELSE.
*        MESSAGE e077 WITH '2' RAISING param_ctb.
*      ENDIF.
*
*    ENDIF.


    CASE lc_metodo_antigo.
      WHEN abap_true.

        "2 - Debito Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF i_valor_item GT 0.
          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '2'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-gl_account       = wa_zlest0021-razaodeb.
            wa_contas-item_amount      = i_valor_item.
            wa_contas-db_cr_ind        = 'S'.
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'Provisão de Frete' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.

            wa_contas-db_cr_ind        = 'H'.
            wa_contas-gl_account       = wa_zlest0021-razaocred.
            wa_contas-item_amount      = i_valor_vi.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'Folha de Serviço' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '2' RAISING param_ctb.
          ENDIF.
        ENDIF.

        "19  EM/EF Débito
        IF lc_zvlr_frete GT 0.
          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '19'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-gl_account       = wa_zlest0021-razaodeb.
            wa_contas-item_amount      = lc_zvlr_frete.
            wa_contas-db_cr_ind        = 'S'.
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'EM/EF Débito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '19' RAISING param_ctb.
          ENDIF.
        ENDIF.

        "20  EM/EF Crédito
        IF lc_zvlr_frete LT 0.
          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '20'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-gl_account       = wa_zlest0021-razaocred.
            wa_contas-item_amount      = abs( lc_zvlr_frete ).
            wa_contas-db_cr_ind        = 'H'.
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'EM/EF Crédito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '20' RAISING param_ctb.
          ENDIF.
        ENDIF.

      WHEN abap_false.

*        IF LC_ZVLR_FRETE GT 0.
*          READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '19'.
*          IF SY-SUBRC IS INITIAL.
*            CLEAR: WA_CONTAS.
*            WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*            WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAODEB.
*            WA_CONTAS-ITEM_AMOUNT      = LC_ZVLR_FRETE.
*            WA_CONTAS-DB_CR_IND        = 'S'.
*            WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*            WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*            CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*            CONCATENATE 'EM/EF Crédito' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*            APPEND WA_CONTAS TO E_CONTAS.
*          ELSE.
*            MESSAGE E077 WITH '19' RAISING PARAM_CTB.
*          ENDIF.
*        ENDIF.
*
*        "20  EM/EF Crédito
*        IF LC_ZVLR_FRETE LT 0.
*          READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '20'.
*          IF SY-SUBRC IS INITIAL.
*            CLEAR: WA_CONTAS.
*            WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*            WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAOCRED.
*            WA_CONTAS-ITEM_AMOUNT      = ABS( LC_ZVLR_FRETE ).
*            WA_CONTAS-DB_CR_IND        = 'H'.
*            WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*            WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*            CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*            CONCATENATE 'EM/EF Crédito' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*            APPEND WA_CONTAS TO E_CONTAS.
*          ELSE.
*            MESSAGE E077 WITH '20' RAISING PARAM_CTB.
*          ENDIF.
*        ENDIF.
        lc_vlr_ajuste = lc_zvlr_frete.

        "LC_VLR_AJUSTE = I_VALOR_VI - I_CTE-ZVLR_FRETE.

        IF abs( lc_vlr_ajuste ) NE 0.
          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '24'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-item_amount      = abs( lc_vlr_ajuste ).
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'Ajuste de Custo' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.

            IF lc_vlr_ajuste GT 0.
              wa_contas-gl_account = wa_zlest0021-razaocred.
              wa_contas-db_cr_ind  = 'H'.
            ELSE.
              wa_contas-gl_account = wa_zlest0021-razaodeb.
              wa_contas-db_cr_ind  = 'S'.
            ENDIF.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '24' RAISING param_ctb.
          ENDIF.
        ENDIF.

        IF lc_vlr_ajuste LT 0.

          "20  EM/EF Crédito
          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '20'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-gl_account       = wa_zlest0021-razaocred.
            wa_contas-item_amount      = abs( lc_vlr_ajuste ).
            wa_contas-db_cr_ind        = 'H'.
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'EM/EF Crédito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '20' RAISING param_ctb.
          ENDIF.

        ELSEIF lc_vlr_ajuste GT 0.

          READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY operfrete = '19'.
          IF sy-subrc IS INITIAL.
            CLEAR: wa_contas.
            wa_contas-invoice_doc_item = i_item_invoice.
            wa_contas-gl_account       = wa_zlest0021-razaodeb.
            wa_contas-item_amount      = lc_vlr_ajuste.
            wa_contas-db_cr_ind        = 'S'.
            wa_contas-comp_code        = i_cte-e_tomadora.
            wa_contas-bus_area         = i_cte-f_tomadora.
            CONCATENATE i_cte-numr_cte '/' i_cte-numr_serie INTO wa_contas-item_text.
            CONCATENATE 'EM/EF Crédito' wa_contas-item_text INTO wa_contas-item_text SEPARATED BY space.
            APPEND wa_contas TO e_contas.
          ELSE.
            MESSAGE e077 WITH '19' RAISING param_ctb.
          ENDIF.

        ENDIF.


    ENDCASE.



*      "14  Vale Pedágio
*      IF I_CTE-ZVALOR_PEDAGIO GT 0.
*        READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '14'.
*        IF SY-SUBRC IS INITIAL.
*          CLEAR: WA_CONTAS.
*          WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*          WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAODEB.
*          WA_CONTAS-ITEM_AMOUNT      = I_CTE-ZVALOR_PEDAGIO.
*          WA_CONTAS-DB_CR_IND        = 'S'.
*          WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*          WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*          CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*          CONCATENATE 'Pedágio' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*          APPEND WA_CONTAS TO E_CONTAS.
*        ELSE.
*          MESSAGE E077 WITH '14' RAISING SEM_OPERFRETE.
*        ENDIF.
*      ENDIF.



    "19  EM/EF Débito
*    IF LC_ZVLR_FRETE GT 0 AND I_VSART NE '01'.
*
*      READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '19'.
*      IF SY-SUBRC IS INITIAL.
*        CLEAR: WA_CONTAS.
*        WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*        WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAODEB.
*        WA_CONTAS-ITEM_AMOUNT      = LC_ZVLR_FRETE.
*        WA_CONTAS-DB_CR_IND        = 'S'.
*        WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*        WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*        CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*        CONCATENATE 'EM/EF Débito' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*        APPEND WA_CONTAS TO E_CONTAS.
*      ELSE.
*        MESSAGE E077 WITH '19' RAISING PARAM_CTB.
*      ENDIF.
*
*    ELSEIF LC_ZVLR_FRETE GT 0 AND I_VSART EQ '01'.
*
*      READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '19'.
*      IF SY-SUBRC IS INITIAL.
*        CLEAR: WA_CONTAS.
*        WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*        WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAOCRED.
*        WA_CONTAS-ITEM_AMOUNT      = LC_ZVLR_FRETE.
*        WA_CONTAS-DB_CR_IND        = 'H'.
*        WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*        WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*        CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*        CONCATENATE 'EM/EF Crédito' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*        APPEND WA_CONTAS TO E_CONTAS.
*      ELSE.
*        MESSAGE E077 WITH '19' RAISING PARAM_CTB.
*      ENDIF.
*
*    ENDIF.

*    "20  EM/EF Crédito
*    IF LC_ZVLR_FRETE LT 0 AND I_VSART NE '01'.
*      READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '20'.
*      IF SY-SUBRC IS INITIAL.
*        CLEAR: WA_CONTAS.
*        WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*        WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAOCRED.
*        WA_CONTAS-ITEM_AMOUNT      = ABS( LC_ZVLR_FRETE ).
*        WA_CONTAS-DB_CR_IND        = 'H'.
*        WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*        WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*        CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*        CONCATENATE 'EM/EF Crédito' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*        APPEND WA_CONTAS TO E_CONTAS.
*      ELSE.
*        MESSAGE E077 WITH '20' RAISING PARAM_CTB.
*      ENDIF.
*    ENDIF.
*
*
*    "Valor dos Impostos
*    IF I_VSART EQ '01'.
*
*      LC_VLR_IMPOSTOS = I_CTE-ZVLR_FRETE - I_ITEM_INVOICE.
*
*      IF LC_VLR_IMPOSTOS GT 0.
*        READ TABLE IT_ZLEST0021 INTO WA_ZLEST0021 WITH KEY OPERFRETE = '24'.
*        IF SY-SUBRC IS INITIAL.
*          CLEAR: WA_CONTAS.
*          WA_CONTAS-INVOICE_DOC_ITEM = I_ITEM_INVOICE.
*          WA_CONTAS-GL_ACCOUNT       = WA_ZLEST0021-RAZAODEB.
*          WA_CONTAS-ITEM_AMOUNT      = ABS( LC_ZVLR_FRETE ).
*          WA_CONTAS-DB_CR_IND        = 'S'.
*          WA_CONTAS-COMP_CODE        = I_CTE-E_TOMADORA.
*          WA_CONTAS-BUS_AREA         = I_CTE-F_TOMADORA.
*          CONCATENATE I_CTE-NUMR_CTE '/' I_CTE-NUMR_SERIE INTO WA_CONTAS-ITEM_TEXT.
*          CONCATENATE 'Vlr total impostos' WA_CONTAS-ITEM_TEXT INTO WA_CONTAS-ITEM_TEXT SEPARATED BY SPACE.
*          APPEND WA_CONTAS TO E_CONTAS.
*        ELSE.
*          MESSAGE E077 WITH '24' RAISING PARAM_CTB.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.


  METHOD buscar_info_ferroviario.

    DATA: wa_zlest0118          TYPE zlest0118,
          wl_peso_faturar       TYPE zlest0044-peso_bruto,
          wl_peso_faturado      TYPE zlest0044-peso_bruto,
          wl_peso_faturad2      TYPE zlest0044-peso_bruto,
          wa_n55                TYPE zib_cte_dist_n55,
          wa_n55_aux            TYPE zib_cte_dist_n55,
          "IT_VGA           TYPE TABLE OF ZIB_CTE_DIST_VGA,
          "WA_VGA           TYPE ZIB_CTE_DIST_VGA,
          it_d55                TYPE TABLE OF zib_cte_dist_d55,
          wa_d55                TYPE zib_cte_dist_d55,
          lc_qt_carga_cte	      TYPE zde_qt_carga_cte,
          lc_qt_carga_dif	      TYPE zde_qt_carga_cte,
          lc_qt_carga_lbp	      TYPE zde_qt_carga_cte,
          it_n55_aux            TYPE zib_cte_dist_n55_t,
          it_zlest0041          TYPE TABLE OF zlest0041,
          wa_zlest0041          TYPE zlest0041,
          it_j_1bnfnad          TYPE TABLE OF j_1bnfnad,
*==============Inicio LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
          it_ekes_aux           TYPE TABLE OF ekes,
          it_ekbe_aux           TYPE TABLE OF ekbe,
          it_LIN_aux            TYPE TABLE OF j_1bnflin,
          it_j_1bndlin_aux      TYPE TABLE OF j_1bnflin,
*==============Fim LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
          wa_j_1bnfnad          TYPE j_1bnfnad,
          wa_tvkn               TYPE tvkn,
          it_vttp               TYPE TABLE OF vttp,
          it_vttk               TYPE TABLE OF vttk,
          wa_vttk               TYPE vttk,
          it_vtts               TYPE TABLE OF vtts,
          wa_vtts               TYPE vtts,
          wa_a910               TYPE a910,
          wa_a933               TYPE a933,
          wa_konp               TYPE konp,
          it_zlest0045          TYPE TABLE OF zlest0045,
          wa_zlest0045          TYPE zlest0045,
          lc_saldo_nota         TYPE j_1bnetqty,
          lc_nr_nf              TYPE zlest0035-nr_nf,
          lc_serie_nf           TYPE  zlest0035-serie_nf,
          lc_cnpj               TYPE  zlest0035-cnpj,
          lc_werks              TYPE zlest0035-werks,
          wa_peso_liberado      TYPE zib_cte_dist_lbp,
          wa_dif_peso           TYPE zib_cte_dist_pld,
          it_cte_vga            TYPE TABLE OF zib_cte_dist_vga,
          wa_cte_vga            TYPE zib_cte_dist_vga,
          it_lbp                TYPE TABLE OF zib_cte_dist_lbp,
          wa_lbp                TYPE zib_cte_dist_lbp,
          it_j_1bndlin          TYPE TABLE OF j_1bnflin,
          it_j_1bnfdoc          TYPE TABLE OF j_1bnfdoc,
          wa_j_1bndlin          TYPE j_1bnflin,
          wa_ultima_carta       TYPE zcarta_correcao,
          ck_tem_seguro         TYPE char01,
          vl_total_notas        TYPE j_1bnetval,
          wa_a917               TYPE a917,
          _job                  TYPE sy-batch,
          ls_message_automation TYPE string,
          ck_achou_preco        TYPE boolean.

    DATA: vg_lines         TYPE i,
          vg_check_entrada TYPE c,
*==============Inicio LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
          vg_refkey        TYPE j_1brefkey.
*==============Fim LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING

    CLEAR: vg_lines, vg_check_entrada,_job.
    e_tipo_contrato = '0001'.
    ck_tem_seguro   = abap_false.

    _job = sy-batch.

    IF _job IS INITIAL AND sy-cprog = 'ZMMR183'.
      _job = abap_true.
    ENDIF.


    FIELD-SYMBOLS: <fs_vga> TYPE zib_cte_dist_vga,
                   <fs_d55> TYPE zib_cte_dist_d55.

    SELECT SINGLE * INTO e_zlest0044
      FROM zlest0044
     WHERE chave_cte EQ p_cte-cd_chave_cte.

    IF sy-subrc IS NOT INITIAL.
      IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
        MESSAGE e125 RAISING nao_achou.
      ENDIF.
    ENDIF.

    SELECT * INTO TABLE it_zlest0045
      FROM zlest0045
     WHERE chave_cte EQ p_cte-cd_chave_cte.

    SELECT * INTO TABLE it_n55
      FROM zib_cte_dist_n55
     WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

    SELECT * INTO TABLE @DATA(it_011)
      FROM zib_cte_dist_001
     WHERE cd_chave_cte EQ @p_cte-cd_chave_cte.

    CONCATENATE p_cte-inicio_uf  p_cte-inicio_ibge  INTO wa_zlest0118-domicilio_origem SEPARATED BY space.
    CONCATENATE p_cte-termino_uf p_cte-termino_ibge INTO wa_zlest0118-domicilio_destin SEPARATED BY space.

    "Verificar Intinerário Cadastrada """""""""""""""""""""""""""""""""""""""""""""""" >>>>>
    CLEAR: it_j_1bndlin.
    LOOP AT it_n55 INTO wa_n55.
      IF wa_n55-docnum_nfe IS INITIAL.
        CONTINUE.
      ENDIF.
      SELECT * APPENDING TABLE it_j_1bndlin
        FROM j_1bnflin
       WHERE docnum EQ wa_n55-docnum_nfe.

*==============Inicio LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
      "Seleciona dados da j_1bnfdoc
      IF it_j_1bndlin IS NOT INITIAL.
        SELECT * FROM j_1bnfdoc
        APPENDING TABLE it_j_1bnfdoc
        WHERE docnum EQ wa_n55-docnum_nfe.
      ENDIF.
*==============Fim LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING

      "Seleciona dados aviso com mais de um item.
*     loop at it_j_1bndlin into data(ws_lin) WHERE docnum EQ wa_n55-docnum_nfe.

*     ENDLOOP.
    ENDLOOP.

*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO

*==============Inicio LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
    FREE: it_ekes_aux.
    IF it_j_1bndlin IS NOT INITIAL.

      it_ekes_aux = VALUE #( FOR y IN it_j_1bndlin ( ebeln = y-xped
                                                     ebelp = y-nitemped
      ) ).

      IF it_ekes_aux IS NOT INITIAL.
        SELECT * FROM ekes
        INTO TABLE @DATA(it_ekes)
          FOR ALL ENTRIES IN @it_ekes_aux
        WHERE ebeln EQ @it_ekes_aux-ebeln
          AND ebelp EQ @it_ekes_aux-ebelp.
        IF sy-subrc EQ 0.
          vg_check_entrada = abap_true.


          it_ekbe_aux = VALUE #( FOR t IN it_ekes ( ebeln = t-ebeln
                                                    ebelp = t-ebelp
                                                    xblnr = t-xblnr
         ) ).

          SELECT * FROM ekbe
          INTO TABLE @DATA(it_ekbe)
          FOR ALL ENTRIES IN  @it_ekbe_aux
          WHERE ebeln EQ @it_ekbe_aux-ebeln
            AND ebelp EQ @it_ekbe_aux-ebelp
            AND xblnr EQ @it_ekbe_aux-xblnr
            AND bewtp EQ 'Q'.

          IF sy-subrc EQ 0.
            SELECT * FROM rbkp INTO TABLE @DATA(it_rbkp)
              FOR ALL ENTRIES IN @it_ekbe
              WHERE belnr EQ @it_ekbe-belnr
                AND gjahr EQ @it_ekbe-gjahr
                AND stblg EQ @space.

            IF sy-subrc EQ 0.
              "IR187295  - ZMM0079 - Ferroviário nota estornada #145213 - INICIO

              "registros da ekbe que não estão estornados
              FREE: it_ekbe.
              SELECT * FROM ekbe
        INTO TABLE @it_ekbe
        FOR ALL ENTRIES IN  @it_rbkp
        WHERE belnr = @it_rbkp-belnr
              AND gjahr = @it_rbkp-gjahr.
              "IR187295  - ZMM0079 - Ferroviário nota estornada #145213 - FIM

              it_LIN_aux = VALUE #( FOR e IN it_rbkp ( refkey = |{ e-belnr }{ e-gjahr }|
                  ) ).

              SELECT * FROM j_1bnflin INTO TABLE it_j_1bndlin_AUX
              FOR ALL ENTRIES IN it_LIN_aux
              WHERE refkey EQ it_LIN_aux-refkey.

            ENDIF.

          ENDIF.

        ENDIF.
*==============Fim LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING

      ENDIF.
    ENDIF.
*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO

    READ TABLE it_j_1bndlin INTO wa_j_1bndlin INDEX 1.

    e_matnr_faturado = wa_j_1bndlin-matnr.

    "Liberar Provisão de Pagamento da CT-e com Erro
    SELECT SINGLE * INTO @DATA(wa_zib_cte_dist_eap)
      FROM zib_cte_dist_eap
     WHERE cd_chave_cte  EQ @p_cte-cd_chave_cte
       AND tp_aprovacao  EQ '05'
       AND ck_ultimo     EQ 'X'
       AND tp_autorizado EQ '01'.

    IF sy-subrc IS INITIAL.
      DATA(ck_pagamento_com_erro) = abap_true.
    ELSE.
      ck_pagamento_com_erro = abap_false.
    ENDIF.

    "Liberar Provisão de Pagamento da CT-e com Erro
    SELECT SINGLE * INTO @wa_zib_cte_dist_eap
      FROM zib_cte_dist_eap
     WHERE cd_chave_cte  EQ @p_cte-cd_chave_cte
       AND tp_aprovacao  EQ '09'
       AND ck_ultimo     EQ 'X'
       AND tp_autorizado EQ '01'.

    IF sy-subrc IS INITIAL.
      DATA(ck_pagamento_praca) = abap_true.
    ELSE.
      ck_pagamento_praca = abap_false.
    ENDIF.

    "Verificar Parâmetro de Frete Peso/Lotação
    LOOP AT it_n55 INTO wa_n55.

      IF wa_n55-docnum_nfe IS INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_itens)
        FROM j_1bnflin
       WHERE docnum EQ @wa_n55-docnum_nfe.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
      SELECT SINGLE * INTO @DATA(wa_zlest0154)
        FROM zlest0154
       WHERE bukrs EQ @p_cte-e_tomadora
         AND lifnr EQ @p_cte-p_emissor
         AND matkl EQ @wa_itens-matkl.

      IF sy-subrc IS INITIAL.
        e_tipo_contrato        = '0002'.
        e_zlest0044-tarifa     = p_cte-valor_receber.
        e_zlest0044-peso_bruto = 1.
        APPEND wa_n55 TO e_remessas.
      ENDIF.

      wa_a917-kappl  = 'F'.
      wa_a917-kschl  = 'ZSEG'.
      wa_a917-tdlnr  = p_cte-p_emissor.
      wa_a917-matnr  = wa_itens-matnr.
      wa_a917-datbi  = e_zlest0044-dt_referencia.
      wa_a917-datab  = e_zlest0044-dt_referencia.

      SELECT SINGLE * INTO wa_a917
        FROM a917
       WHERE kappl  EQ wa_a917-kappl
         AND kschl  EQ wa_a917-kschl
         AND tdlnr  EQ wa_a917-tdlnr
         AND matnr  EQ wa_a917-matnr
         AND datbi  GE wa_a917-datbi
         AND datab  LE wa_a917-datab.

      IF sy-subrc IS INITIAL.
        ck_tem_seguro = abap_true.
        wa_konp-knumh =  wa_a917-knumh.
        SELECT SINGLE * INTO wa_konp
          FROM konp
         WHERE knumh    EQ wa_konp-knumh
           AND loevm_ko EQ abap_false.
      ENDIF.

    ENDLOOP.

    "Deduzir Seguro da Tarifa
    IF e_tipo_contrato EQ '0002' AND ck_tem_seguro EQ abap_true.

      IF wa_konp-konwa EQ '%'.
        vl_total_notas = 0.
        LOOP AT it_n55 INTO wa_n55.
          SELECT li~netwr INTO TABLE @DATA(it_itens)
            FROM j_1bnflin AS li
           INNER JOIN j_1bnfdoc AS nf ON nf~docnum EQ li~docnum
           WHERE li~docnum EQ @wa_n55-docnum_nfe
             AND nf~form NE @space.
          LOOP AT it_itens INTO DATA(wa_itens_nota).
            ADD wa_itens_nota-netwr TO vl_total_notas.
          ENDLOOP.
        ENDLOOP.

        e_zlest0044-vlr_seguro = ( vl_total_notas * wa_konp-kbetr ) / 1000.
      ENDIF.

      IF  wa_konp-krech EQ 'B'.
        e_zlest0044-vlr_seguro = wa_konp-kbetr.
      ENDIF.
      e_zlest0044-tarifa = e_zlest0044-tarifa - e_zlest0044-vlr_seguro.
    ENDIF.

*    IF E_TIPO_CONTRATO NE '0002'.
*
*      CALL METHOD ZCL_CTE_DIST_G=>VERIFICA_VOLUME_PRECO_FERRO
*        EXPORTING
*          I_FORNECEDOR          = P_CTE-P_EMISSOR
*          I_INICIO              = WA_ZLEST0118-DOMICILIO_ORIGEM
*          I_FINAL               = WA_ZLEST0118-DOMICILIO_DESTIN
*          I_VLR_TARIFA          = E_ZLEST0044-TARIFA
*          I_TOMADOR             = P_CTE-E_TOMADORA
*          I_TOMADOR_CENTRO      = P_CTE-F_TOMADORA
*          I_DT_REFERENCIA       = E_ZLEST0044-DT_REFERENCIA
*          I_QTD_FATURAR         = E_ZLEST0044-PESO_BRUTO
*          I_MATERIAL            = WA_J_1BNDLIN-MATNR
*          I_TIPO_CONTRATO       = E_TIPO_CONTRATO
*        EXCEPTIONS
*          SEM_ITINERARIO        = 1
*          SEM_VOLUME            = 2
*          SEM_VOLUME_EMPRESA    = 3
*          SEM_VOLUME_DISPONIVEL = 4
*          OTHERS                = 5.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        IF E_TIPO_CONTRATO EQ '0002'.
*          WRITE E_ZLEST0044-TARIFA TO SY-MSGV1.
*          WRITE E_ZLEST0044-VLR_SEGURO TO SY-MSGV2.
*          CONDENSE SY-MSGV1 NO-GAPS.
*          CONDENSE SY-MSGV2 NO-GAPS.
*          IF P_GRAVAR IS NOT INITIAL.
*            CALL METHOD ME->ADD_LOG_CTE_DIST
*              EXPORTING
*                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
*                P_TYPE         = 'E'
*                P_NUM          = 196
*                P_MESSAGE_V1   = SY-MSGV1
*                P_MESSAGE_V2   = SY-MSGV2
*                P_MESSAGE_V3   = SY-MSGV3
*              CHANGING
*                P_LC_SEQUENCIA = LC_SEQUENCIA.
*          ENDIF.
*          MESSAGE I196 WITH SY-MSGV1 SY-MSGV2.
*        ENDIF.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_ACHOU.
*      ELSE.
*        SY-MSGV1 = P_CTE-P_EMISSOR.
*        SY-MSGV2 = WA_ZLEST0118-DOMICILIO_ORIGEM.
*        SY-MSGV3 = WA_ZLEST0118-DOMICILIO_DESTIN.
*        IF P_GRAVAR IS NOT INITIAL.
*          CALL METHOD ME->ADD_LOG_CTE_DIST
*            EXPORTING
*              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
*              P_TYPE         = 'S'
*              P_NUM          = 121
*              P_MESSAGE_V1   = SY-MSGV1
*              P_MESSAGE_V2   = SY-MSGV2
*              P_MESSAGE_V3   = SY-MSGV3
*            CHANGING
*              P_LC_SEQUENCIA = LC_SEQUENCIA.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    "Verifica remessa """"""""""""""""""""""""""""""""""""""""""""""""""""""""""" >>>>>
    "SELECT * INTO TABLE IT_VGA
    "  FROM ZIB_CTE_DIST_VGA
    " WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

    "LOOP AT IT_VGA ASSIGNING <FS_VGA>.
    "  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    "    EXPORTING
    "      INPUT  = <FS_VGA>-NUMR_IDENT_VAGAO
    "    IMPORTING
    "      OUTPUT = <FS_VGA>-NUMR_IDENT_VAGAO.
    "ENDLOOP.

    SELECT * INTO TABLE it_d55
      FROM zib_cte_dist_d55
     WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

    LOOP AT it_d55 ASSIGNING <fs_d55>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_d55>-id_unid_transp
        IMPORTING
          output = <fs_d55>-id_unid_transp.
    ENDLOOP.

    SELECT * INTO TABLE it_n55
      FROM zib_cte_dist_n55
     WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

    it_n55_aux = it_n55.
    DELETE it_n55_aux WHERE docnum_nfe EQ false.

    SELECT * INTO TABLE it_zlest0041
      FROM zlest0041
      FOR ALL ENTRIES IN it_n55_aux
     WHERE docnum EQ it_n55_aux-docnum_nfe.

    lc_qt_carga_cte = 0.

    "Vagões
    "LOOP AT IT_VGA INTO WA_VGA.
    "Rateios

    DATA(lc_erro_nota_terceiro) = abap_false.

*    clear: vg_lines.
*    vg_lines = LINES( it_j_1bndlin ).
    IF vg_check_entrada EQ abap_false.

      LOOP AT it_d55 INTO wa_d55. "WHERE ID_UNID_TRANSP EQ WA_VGA-NUMR_IDENT_VAGAO.
        "Notas Pertencentes ao CT-e
        LOOP AT it_n55 INTO wa_n55 WHERE n55_chave_acesso EQ wa_d55-n55_chave_acesso.

          IF wa_n55-vbeln_vl IS NOT INITIAL.

            READ TABLE it_zlest0041 INTO wa_zlest0041
            WITH KEY centro_comprador = wa_n55-branch
                     nr_nf            = wa_n55-n55_chave_acesso+25(9)
                     cod_cliente      = wa_n55-parid
                     serie            = wa_n55-n55_chave_acesso+22(3).

            IF sy-subrc IS INITIAL.
              READ TABLE it_n55_aux INTO wa_n55_aux WITH KEY docnum_nfe = wa_zlest0041-docnum.
              IF ( sy-subrc IS INITIAL ) AND ( wa_n55_aux-vbeln_vl IS NOT INITIAL ).
                ADD wa_d55-valr_peso_rate TO lc_qt_carga_cte.
                APPEND wa_n55_aux TO e_remessas.
              ENDIF.
            ELSE.
              ADD wa_d55-valr_peso_rate TO lc_qt_carga_cte.
              APPEND wa_n55 TO e_remessas.
            ENDIF.

          ELSE.

            "Se nota não tiver remessa, procurar uma no processo de compra e ordem de terceiro que possua
            READ TABLE it_zlest0041 INTO wa_zlest0041
            WITH KEY centro_comprador = wa_n55-branch
                     nr_nf            = wa_n55-n55_chave_acesso+25(9)
                     cod_cliente      = wa_n55-parid
                     serie            = wa_n55-n55_chave_acesso+22(3).

            IF sy-subrc IS INITIAL.

              READ TABLE it_n55_aux INTO wa_n55_aux WITH KEY docnum_nfe = wa_zlest0041-docnum.
              IF ( sy-subrc IS INITIAL ) AND ( wa_n55_aux-vbeln_vl IS NOT INITIAL ).
                ADD wa_d55-valr_peso_rate TO lc_qt_carga_cte.
                APPEND wa_n55_aux TO e_remessas.
              ENDIF.

            ELSE.

              IF CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                    )->set_parceiro( i_parceiro = CONV #( zcl_string=>lpad( i_str = CONV #( p_cte-f_tomadora )  i_qtd = 10  i_char = '0' ) )
                    ) )->at_lfa1-stcd1 NE wa_n55-n55_chave_acesso+6(14).

                lc_erro_nota_terceiro = abap_true.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = p_cte-cd_chave_cte
                    p_type         = 'E'
                    p_num          = 217
                    p_message_v1   = wa_n55-n55_chave_acesso+25(9)
                    p_message_v2   = wa_n55-n55_chave_acesso+22(3)
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                DATA(wa_nota_nao_encontrada) = wa_n55.

              ENDIF.

            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ELSE.
*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO

*LOOP AT it_n55 INTO wa_n55. "WHERE ID_UNID_TRANSP EQ WA_VGA-NUMR_IDENT_VAGAO.
      "Notas Pertencentes ao CT-e
      LOOP AT  it_ekes  INTO DATA(wa_ekes).

*==============Inicio LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
        READ TABLE  it_ekbe INTO DATA(ws_ekbe) WITH KEY ebeln = wa_ekes-ebeln
                                                        ebelp = wa_ekes-ebelp
                                                        xblnr = wa_ekes-xblnr.
        IF sy-subrc EQ 0.
          READ TABLE it_rbkp INTO DATA(ws_rbkp) WITH KEY belnr = ws_ekbe-belnr
                                                         gjahr = ws_ekbe-gjahr
                                                         stblg = space.

          IF sy-subrc EQ 0.
            CLEAR: vg_refkey.
            vg_refkey = |{ ws_rbkp-belnr }{ ws_rbkp-gjahr }|.
            READ TABLE it_j_1bndlin_AUX INTO DATA(WS_1bndlin_AUX) WITH KEY refkey = vg_refkey.
          ENDIF.
        ENDIF.

*       READ TABLE it_j_1bndlin into data(wa_1bndlin) with key xped = wa_ekes-ebeln
*                                                          nitemped = wa_ekes-ebelp.
*==============Fim LES - Ajuste ZMM0079 #IR186597/BUG 144405 / AOENNING
        IF sy-subrc EQ 0.
          READ TABLE it_j_1bnfdoc INTO DATA(wa_bnfdoc) WITH KEY docnum = WS_1bndlin_AUX-docnum.

          IF wa_bnfdoc-nfenum IS NOT INITIAL.
            DATA(vg_nfenum) = |{ wa_bnfdoc-nfenum }-{ wa_bnfdoc-series }|.
            DATA(vg_nfenum_aux) = |{ wa_bnfdoc-nfenum ALPHA = OUT }-{ wa_bnfdoc-series }|.
            CONDENSE: vg_nfenum_aux NO-GAPS.
          ENDIF.

          IF wa_ekes-vbeln IS NOT INITIAL AND wa_ekes-menge > 0 AND ( wa_ekes-xblnr EQ vg_nfenum OR wa_ekes-xblnr EQ vg_nfenum_aux ).


*          READ TABLE it_zlest0041 INTO wa_zlest0041
*          WITH KEY centro_comprador = wa_n55-branch
*                   nr_nf            = wa_n55-n55_chave_acesso+25(9)
*                   cod_cliente      = wa_n55-parid
*                   serie            = wa_n55-n55_chave_acesso+22(3).
*
*          IF sy-subrc IS INITIAL.
*            READ TABLE it_n55_aux INTO wa_n55_aux WITH KEY docnum_nfe = wa_zlest0041-docnum.
*            IF ( sy-subrc IS INITIAL ) AND ( wa_n55_aux-vbeln_vl IS NOT INITIAL ).
*              ADD wa_d55-valr_peso_rate TO lc_qt_carga_cte.
*              APPEND wa_n55_aux TO e_remessas.
*            ENDIF.
*          ELSE.
            ADD wa_ekes-menge TO lc_qt_carga_cte.
            wa_n55-vbeln_vl = wa_ekes-vbeln.
            wa_n55-ebeln    = wa_ekes-ebeln.
            APPEND wa_n55 TO e_remessas.
*          ENDIF.
          ENDIF.
          CLEAR: wa_bnfdoc, vg_nfenum, vg_nfenum_aux, WS_1bndlin_AUX, ws_ekbe, ws_rbkp." wa_1bndlin.
        ELSE.

          "Se nota não tiver remessa, procurar uma no processo de compra e ordem de terceiro que possua
*          READ TABLE it_zlest0041 INTO wa_zlest0041
*          WITH KEY centro_comprador = wa_n55-branch
*                   nr_nf            = wa_n55-n55_chave_acesso+25(9)
*                   cod_cliente      = wa_n55-parid
*                   serie            = wa_n55-n55_chave_acesso+22(3).
*
*          IF sy-subrc IS INITIAL.
*
*            READ TABLE it_n55_aux INTO wa_n55_aux WITH KEY docnum_nfe = wa_zlest0041-docnum.
*            IF ( sy-subrc IS INITIAL ) AND ( wa_n55_aux-vbeln_vl IS NOT INITIAL ).
*              ADD wa_d55-valr_peso_rate TO lc_qt_carga_cte.
*              APPEND wa_n55_aux TO e_remessas.
*            ENDIF.
*
*          ELSE.
*
*            IF CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
*                  )->set_parceiro( i_parceiro = CONV #( zcl_string=>lpad( i_str = CONV #( p_cte-f_tomadora )  i_qtd = 10  i_char = '0' ) )
*                  ) )->at_lfa1-stcd1 NE wa_n55-n55_chave_acesso+6(14).
*
*              lc_erro_nota_terceiro = abap_true.
*
*              CALL METHOD me->add_log_cte_dist
*                EXPORTING
*                  p_cd_chave_cte = p_cte-cd_chave_cte
*                  p_type         = 'E'
*                  p_num          = 217
*                  p_message_v1   = wa_n55-n55_chave_acesso+25(9)
*                  p_message_v2   = wa_n55-n55_chave_acesso+22(3)
*                CHANGING
*                  p_lc_sequencia = lc_sequencia.
*
*              clear: wa_nota_nao_encontrada.
*              wa_nota_nao_encontrada = wa_n55.
*
*            ENDIF.

*          ENDIF.

        ENDIF.
      ENDLOOP.
*    ENDLOOP.
*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO

    ENDIF.


    IF lc_erro_nota_terceiro EQ abap_true.
      IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
        MESSAGE e217 WITH wa_nota_nao_encontrada-n55_chave_acesso+25(9) wa_nota_nao_encontrada-n55_chave_acesso+22(3) RAISING nao_achou.
      ENDIF.
    ENDIF.

*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO
    IF vg_check_entrada EQ abap_false.
      lc_qt_carga_cte = lc_qt_carga_cte * 1000.
    ENDIF.
*&======================================================================LES - Ajuste pgt de frente terc/ notas  entrada #IR181478 AO

    IF lc_qt_carga_cte <> p_cte-qt_carga_cte AND e_tipo_contrato NE '0002'.

      SELECT SINGLE * INTO wa_peso_liberado
        FROM zib_cte_dist_lbp
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      IF ( sy-subrc IS NOT INITIAL ) OR ( wa_peso_liberado-qt_carga_cte_lb NE p_cte-qt_carga_cte ).
        "MESSAGE E126 RAISING NAO_ACHOU.

        SELECT SINGLE * INTO wa_dif_peso
          FROM zib_cte_dist_pld
         WHERE lifnr EQ p_cte-p_emissor.

        IF sy-subrc IS INITIAL.

          "Quantidade do CT-e menos
          lc_qt_carga_dif = p_cte-qt_carga_cte - lc_qt_carga_cte.
          lc_qt_carga_dif = abs( lc_qt_carga_dif ).

          IF lc_qt_carga_dif LE wa_dif_peso-qt_diferenca.
            "Liberar Automático"
            lc_qt_carga_dif = p_cte-qt_carga_cte - lc_qt_carga_cte. "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
            IF _job = abap_false. "Se não for por job mostra a menssagemj!
              MESSAGE w178 WITH p_cte-qt_carga_cte lc_qt_carga_dif.
              "Liberar Diferença de Peso """""""""""""""""""""""""""""""""""""""""""""""""""""""""
            ENDIF.

            "1º Verificar se tem nota que ainda não foi utilizada na CT-e
            SELECT * INTO TABLE it_d55
              FROM zib_cte_dist_d55 AS d
             WHERE cd_chave_cte EQ p_cte-cd_chave_cte
               AND NOT EXISTS ( SELECT * FROM zib_cte_dist_lbp AS n WHERE n~n55_chave_acesso EQ d~n55_chave_acesso ).

            IF sy-subrc IS NOT INITIAL.
              "Se todas já foram utilizadas pega a primeira
              SELECT * INTO TABLE it_d55
                FROM zib_cte_dist_d55 AS d
               WHERE cd_chave_cte EQ p_cte-cd_chave_cte.
            ENDIF.

            IF sy-subrc IS INITIAL.
              READ TABLE it_d55 INDEX 1 INTO wa_d55.
              wa_peso_liberado-cd_chave_cte     = p_cte-cd_chave_cte.
              wa_peso_liberado-qt_carga_cte     = p_cte-qt_carga_cte.
              wa_peso_liberado-qt_carga_cte_lb  = p_cte-qt_carga_cte.
              wa_peso_liberado-dt_autorizacao   = sy-datum.
              wa_peso_liberado-ds_name_usuario  = sy-uname.
              wa_peso_liberado-hr_autorizacao   = sy-uzeit.
              wa_peso_liberado-qt_diferenca     = lc_qt_carga_dif.
              wa_peso_liberado-n55_chave_acesso = wa_d55-n55_chave_acesso.

              SELECT * INTO TABLE it_cte_vga
                FROM zib_cte_dist_vga
               WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

              wa_peso_liberado-qt_vagoes = 0.
              LOOP AT it_cte_vga INTO wa_cte_vga.
                ADD wa_cte_vga-valr_peso_real TO wa_peso_liberado-qt_vagoes.
              ENDLOOP.
              MULTIPLY wa_peso_liberado-qt_vagoes BY 1000.

              SELECT * INTO TABLE it_d55
                FROM zib_cte_dist_d55 AS d
               WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

              wa_peso_liberado-qt_nfe = 0.
              LOOP AT it_d55 INTO wa_d55.
                ADD wa_d55-valr_peso_rate TO wa_peso_liberado-qt_nfe.
              ENDLOOP.
              MULTIPLY wa_peso_liberado-qt_nfe BY 1000.

              wa_peso_liberado-qt_nf = 0.
              MODIFY zib_cte_dist_lbp FROM wa_peso_liberado.
              COMMIT WORK.
            ELSE.
              IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                MESSAGE w179 WITH lc_qt_carga_dif.
              ENDIF.
            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          ELSE.
            IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
              MESSAGE e177 WITH lc_qt_carga_dif p_cte-p_emissor RAISING nao_achou.
            ENDIF.
          ENDIF.
        ELSE.
          IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
            MESSAGE e211 WITH p_cte-p_emissor RAISING nao_achou.
          ENDIF.
        ENDIF.
      ELSE.
        IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
          MESSAGE w150 WITH p_cte-qt_carga_cte wa_peso_liberado-qt_diferenca wa_peso_liberado-ds_name_usuario.
        ENDIF.
      ENDIF.

      IF p_gravar IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = 'W'
            p_num          = 150
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

    ELSE.
      IF p_gravar IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = 'S'
            p_num          = 127
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.
    ENDIF.
    "Verifica remessa """"""""""""""""""""""""""""""""""""""""""""""""""""""""""" <<<<<

    DATA: wa_active TYPE j_1bnfe_active.

    CASE p_cte-cd_modal.
      WHEN '06'.

        CASE e_tipo_contrato.
          WHEN '0002'.

            DATA: lc_nm_quantidade TYPE zde_quant_containe,
                  tx_quant_01      TYPE c LENGTH 30,
                  tx_quant_02      TYPE c LENGTH 30.

            "Vinculo de consumo por nota fiscal do CT-e
            SELECT * INTO TABLE @DATA(it_cte_uso)
              FROM zib_cte_dist_001
             WHERE cd_chave_cte     EQ @p_cte-cd_chave_cte
               AND n55_chave_acesso NE @space.

            "Agrupar por NF-e, pois pode ter mais de um conteiner e a nota fiscal pode estar em ambos
            DATA(it_agrupa) = it_cte_uso[].
            SORT it_agrupa BY n55_chave_acesso.
            DELETE ADJACENT DUPLICATES FROM it_agrupa COMPARING n55_chave_acesso.

            LOOP AT it_agrupa INTO DATA(wa_agrupa).

              wa_active-regio     = wa_agrupa-n55_chave_acesso(2).
              wa_active-nfyear    = wa_agrupa-n55_chave_acesso+2(2).
              wa_active-nfmonth   = wa_agrupa-n55_chave_acesso+4(2).
              wa_active-stcd1     = wa_agrupa-n55_chave_acesso+6(14).
              wa_active-model     = wa_agrupa-n55_chave_acesso+20(2).
              wa_active-serie     = wa_agrupa-n55_chave_acesso+22(3).
              wa_active-nfnum9    = wa_agrupa-n55_chave_acesso+25(9).
              wa_active-docnum9   = wa_agrupa-n55_chave_acesso+34(9).
              wa_active-cdv       = wa_agrupa-n55_chave_acesso+43(1).

              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     NE space
                 AND bukrs    EQ p_cte-e_tomadora
                 AND branch   EQ p_cte-f_tomadora
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

              IF sy-subrc IS NOT INITIAL.
                IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                  MESSAGE e223 WITH wa_active-nfnum9 wa_active-serie p_cte-f_tomadora.
                ENDIF.
              ENDIF.

              SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
                FROM j_1bnflin
               WHERE docnum EQ @wa_active-docnum.

              "Debitar consumo atual
              LOOP AT it_cte_uso INTO DATA(wa_cte_uso) WHERE n55_chave_acesso EQ wa_agrupa-n55_chave_acesso.

                CASE wa_j_1bnflin-reftyp.
                  WHEN 'BI'.

                    "Fatura
                    SELECT SINGLE * INTO @DATA(wa_vbrp) FROM vbrp WHERE vbeln EQ @wa_j_1bnflin-refkey.
                    IF sy-subrc IS INITIAL.
                      "Peso Bruto
                      wa_j_1bnflin-menge = wa_vbrp-brgew.
                    ENDIF.

                  WHEN 'MD'.

                    "Documento de Material
                    SELECT SINGLE * INTO @DATA(wa_mseg) FROM mseg
                     WHERE mblnr EQ @wa_j_1bnflin-refkey(10)
                       AND mjahr EQ @wa_j_1bnflin-refkey+10(4)
                       AND xblnr_mkpf NE @space
                       AND tcode2_mkpf EQ 'VL02N'.

                    IF sy-subrc IS INITIAL.

                      SELECT SINGLE * INTO @DATA(wa_lips) FROM lips
                       WHERE vbeln EQ @wa_mseg-xblnr_mkpf.

                      IF sy-subrc IS INITIAL.
                        "Peso Bruto
                        wa_j_1bnflin-menge = wa_lips-brgew.
                      ENDIF.
                    ENDIF.

                ENDCASE.

                wa_j_1bnflin-menge = wa_j_1bnflin-menge - wa_cte_uso-nm_quantidade.
              ENDLOOP.

              "Busca Consumos Anteriores
              SELECT * INTO TABLE @DATA(it_nfe_uso_anterior)
                FROM zib_cte_dist_001
               WHERE n55_chave_acesso EQ @wa_cte_uso-n55_chave_acesso
                 AND cd_chave_cte     NE @p_cte-cd_chave_cte.

              "Existe Anterior
              IF sy-subrc IS INITIAL.
                LOOP AT it_nfe_uso_anterior INTO DATA(wa_nfe_uso_anterior).
                  SELECT SINGLE * INTO @DATA(wa_zlest0044)
                    FROM zlest0044
                   WHERE chave_cte EQ @wa_nfe_uso_anterior-cd_chave_cte
                     AND nr_trans  NE @space.

                  IF sy-subrc IS INITIAL.
                    "Remove Saldo anteior utilizado
                    wa_j_1bnflin-menge = wa_j_1bnflin-menge - wa_nfe_uso_anterior-nm_quantidade.
                  ENDIF.
                ENDLOOP.
              ENDIF.

              "Verificar Ferroviário
              SELECT * INTO TABLE @DATA(it_zlest0045_anterior) "PESO_RATEADO
                FROM zlest0045 AS i
               WHERE i~chave     EQ @wa_agrupa-n55_chave_acesso
                 AND i~chave_cte NE @wa_agrupa-cd_chave_cte
                 AND EXISTS ( SELECT * FROM zlest0044 AS s WHERE s~chave_cte EQ i~chave_cte AND s~nr_trans  NE @space ).

              IF sy-subrc IS INITIAL.

                "CT-es ferroviário anteriores
                SELECT * INTO TABLE @DATA(it_cte_ferro)
                  FROM zib_cte_dist_ter
                   FOR ALL ENTRIES IN @it_zlest0045_anterior
                 WHERE cd_chave_cte EQ @it_zlest0045_anterior-chave_cte
                   AND cd_modal     EQ '04'. "Ferroviário

                SORT it_cte_ferro BY cd_chave_cte.

                LOOP AT it_zlest0045_anterior INTO DATA(wa_zlest0045_anterior).
                  READ TABLE it_cte_ferro INTO DATA(wa_cte_ferro) WITH KEY cd_chave_cte = wa_zlest0045_anterior-chave_cte.
                  IF sy-subrc IS INITIAL.
                    wa_j_1bnflin-menge = wa_j_1bnflin-menge - wa_zlest0045_anterior-peso_rateado.
                  ENDIF.
                ENDLOOP.

              ENDIF.

              IF wa_j_1bnflin-menge LT 0.
                IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                  MESSAGE e224 WITH wa_active-nfnum9 wa_active-serie.
                ENDIF.
              ENDIF.

              CLEAR: wa_j_1bnflin.

            ENDLOOP.

            "Totalizar Vinculações """"""""""""""""""""""""""""""""""""""""""
            lc_nm_quantidade = 0.
            LOOP AT it_cte_uso INTO wa_cte_uso.
              ADD wa_cte_uso-nm_quantidade TO lc_nm_quantidade.
            ENDLOOP.

            DATA: lc_quantidade_cte  TYPE i.
            DATA: lc_quantidade_rate TYPE i.

            lc_quantidade_cte  = p_cte-qt_carga_cte.
            lc_quantidade_rate = lc_nm_quantidade.

            IF lc_quantidade_cte NE lc_quantidade_rate.
              WRITE lc_quantidade_cte  TO tx_quant_01.
              WRITE lc_quantidade_rate TO tx_quant_02.
              CONDENSE tx_quant_01 NO-GAPS.
              CONDENSE tx_quant_02 NO-GAPS.
              IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                MESSAGE e225 WITH tx_quant_01 tx_quant_02.
              ENDIF.
            ENDIF.

          WHEN OTHERS.
            "226  Modal &1 com tipo de contratação &2 não previsto!
            IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
              MESSAGE e226 WITH p_cte-cd_modal e_tipo_contrato.
            ENDIF.
        ENDCASE.

      WHEN OTHERS.

        "Sempre após a autorização automática de peso """"""""""""""""""""""""""""""""""""
        "Verificar Saldo de Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""" >>>>>
        CASE e_tipo_contrato.
          WHEN '0001' OR '0002'.
            "Verifica saldo por Peso
            LOOP AT it_zlest0045 INTO wa_zlest0045.

              READ TABLE it_n55 INTO wa_n55 WITH KEY n55_chave_acesso = wa_zlest0045-chave.

              IF wa_n55-zw_lcto IS NOT INITIAL OR wa_n55-vbeln_re IS NOT INITIAL.

                SELECT SINGLE c~* INTO @DATA(wa_zlest0045_ant)
                  FROM zlest0045 AS c
                 INNER JOIN zlest0044 AS d ON d~chave_cte EQ c~chave_cte
                 WHERE c~chave    EQ @wa_n55-n55_chave_acesso
                   AND d~nr_trans NE @space.

                IF sy-subrc IS INITIAL.
                  IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                    MESSAGE e221 WITH wa_zlest0045_ant-chave_cte.
                  ENDIF.
                ENDIF.

              ELSE.

                SELECT SINGLE * INTO wa_zlest0041
                  FROM zlest0041
                 WHERE centro_comprador EQ wa_n55-branch
                   AND nr_nf            EQ wa_n55-n55_chave_acesso+25(9)
                   AND cod_cliente      EQ wa_n55-parid
                   AND serie            EQ wa_n55-n55_chave_acesso+22(3).

                IF sy-subrc IS NOT INITIAL.

                  lc_werks    =  wa_n55-branch.
                  lc_nr_nf    =  wa_zlest0045-chave+25(9).
                  lc_serie_nf =  wa_zlest0045-chave+22(3).
                  lc_cnpj     =  wa_zlest0045-chave+06(14).

                  CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
                    EXPORTING
                      nr_nf                = lc_nr_nf
                      serie_nf             = lc_serie_nf
                      cnpj                 = lc_cnpj
                      werks                = lc_werks
                      i_chave_nfe          = wa_zlest0045-chave
                      i_fornecedor_servico = p_cte-p_emissor
                      i_docnum             = wa_n55-docnum_nfe
                    IMPORTING
                      saldo                = lc_saldo_nota
                    EXCEPTIONS
                      qtd_cheg_not_found   = 1.

                  IF sy-subrc IS NOT INITIAL.
                    lc_saldo_nota = 0.
                  ENDIF.

                  MOVE: wa_zlest0045-chave+25(9)  TO sy-msgv1,
                        wa_zlest0045-peso_rateado TO sy-msgv2,
                        lc_saldo_nota             TO sy-msgv3.

                  CONDENSE: sy-msgv2, sy-msgv3.

                  SELECT SINGLE * INTO wa_dif_peso
                    FROM zib_cte_dist_pld
                   WHERE lifnr EQ p_cte-p_emissor.

                  IF sy-subrc IS INITIAL.
                    ADD wa_dif_peso-qt_diferenca TO lc_saldo_nota.
                  ENDIF.

                  IF lc_saldo_nota LT wa_zlest0045-peso_rateado.
                    CASE ck_pagamento_com_erro.
                      WHEN abap_false.
                        MESSAGE e142 WITH sy-msgv1 sy-msgv2 sy-msgv3 RAISING nao_achou.
                      WHEN abap_true.
                        IF p_gravar IS NOT INITIAL.
                          CALL METHOD me->add_log_cte_dist
                            EXPORTING
                              p_cd_chave_cte = p_cte-cd_chave_cte
                              p_type         = 'S'
                              p_num          = 195
                              p_message_v1   = sy-msgv1
                              p_message_v2   = sy-msgv2
                              p_message_v3   = sy-msgv3
                            CHANGING
                              p_lc_sequencia = lc_sequencia.
                        ENDIF.
                    ENDCASE.
                  ELSE.
                    IF p_gravar IS NOT INITIAL.
                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = p_cte-cd_chave_cte
                          p_type         = 'S'
                          p_num          = 143
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                          p_message_v3   = sy-msgv3
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ENDIF.
                  ENDIF.
                ELSE.
                  READ TABLE it_n55 INTO wa_n55 WITH KEY docnum_nfe = wa_zlest0041-docnum.
                  IF sy-subrc IS INITIAL.

                    SELECT SINGLE stcd1 INTO lc_cnpj FROM lfa1 WHERE lifnr EQ wa_zlest0041-cod_cliente.

                    lc_werks    =  wa_n55-branch.
                    lc_nr_nf    =  wa_n55-n55_chave_acesso+25(9).
                    lc_serie_nf =  wa_n55-n55_chave_acesso+22(3).
                    lc_cnpj     =  lc_cnpj.
                    "LC_CNPJ     =  WA_N55-N55_CHAVE_ACESSO+06(14).

                    CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
                      EXPORTING
                        nr_nf              = lc_nr_nf
                        serie_nf           = lc_serie_nf
                        cnpj               = lc_cnpj
                        werks              = lc_werks
                        i_chave_nfe        = wa_n55-n55_chave_acesso
                      IMPORTING
                        saldo              = lc_saldo_nota
                      EXCEPTIONS
                        qtd_cheg_not_found = 1.

                    IF sy-subrc IS NOT INITIAL.
                      lc_saldo_nota = 0.
                    ENDIF.

                    MOVE: wa_zlest0045-chave+25(9)  TO sy-msgv1,
                          wa_zlest0045-peso_rateado TO sy-msgv2,
                          lc_saldo_nota             TO sy-msgv3.

                    CONDENSE: sy-msgv2, sy-msgv3.

                    SELECT SINGLE * INTO wa_dif_peso
                      FROM zib_cte_dist_pld
                     WHERE lifnr EQ p_cte-p_emissor.

                    IF sy-subrc IS INITIAL.
                      ADD wa_dif_peso-qt_diferenca TO lc_saldo_nota.
                    ENDIF.

                    IF lc_saldo_nota LT wa_zlest0045-peso_rateado.
                      CASE ck_pagamento_com_erro.
                        WHEN abap_false.
                          MESSAGE e142 WITH sy-msgv1 sy-msgv2 sy-msgv3 RAISING nao_achou.
                        WHEN abap_true.
                          IF p_gravar IS NOT INITIAL.
                            CALL METHOD me->add_log_cte_dist
                              EXPORTING
                                p_cd_chave_cte = p_cte-cd_chave_cte
                                p_type         = 'S'
                                p_num          = 195
                                p_message_v1   = sy-msgv1
                                p_message_v2   = sy-msgv2
                                p_message_v3   = sy-msgv3
                              CHANGING
                                p_lc_sequencia = lc_sequencia.
                          ENDIF.
                      ENDCASE.
                    ELSE.
                      IF p_gravar IS NOT INITIAL.
                        CALL METHOD me->add_log_cte_dist
                          EXPORTING
                            p_cd_chave_cte = p_cte-cd_chave_cte
                            p_type         = 'S'
                            p_num          = 143
                            p_message_v1   = sy-msgv1
                            p_message_v2   = sy-msgv2
                            p_message_v3   = sy-msgv3
                          CHANGING
                            p_lc_sequencia = lc_sequencia.
                      ENDIF.
                    ENDIF.
                  ELSE.
                    MESSAGE e144 WITH wa_zlest0041-docnum RAISING nao_achou.
                  ENDIF.
                ENDIF.

              ENDIF.

            ENDLOOP.
*      WHEN '0002'.
*        "Verifica se Já existe outr CTe Ferroviária para esta nota Fiscal
*        LOOP AT IT_N55 INTO WA_N55.
*
*          SELECT SINGLE TE~CD_CHAVE_CTE INTO @DATA(WA_CTE_ENTEIOR)
*            FROM ZIB_CTE_DIST_N55 AS NF
*           INNER JOIN ZIB_CTE_DIST_TER AS TE ON TE~CD_CHAVE_CTE EQ NF~CD_CHAVE_CTE
*           WHERE NF~N55_CHAVE_ACESSO EQ @WA_N55-N55_CHAVE_ACESSO
*             AND TE~CK_FINALIZADO EQ @ABAP_TRUE
*             AND TE~CD_CHAVE_CTE  NE @WA_N55-CD_CHAVE_CTE
*             AND TE~CD_MODAL      EQ @P_CTE-CD_MODAL.
*
*          IF SY-SUBRC IS INITIAL.
*            MESSAGE E193 WITH WA_N55-N55_CHAVE_ACESSO+26(9) WA_CTE_ENTEIOR+26(9) RAISING NAO_ACHOU.
*          ENDIF.
*        ENDLOOP.
        ENDCASE.

    ENDCASE.

    "Verificar Saldo de Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""" <<<<<

    "Busca Dados de Local de Partida e Chegada """""""""""""""""""""""""""""""""" >>>>>
    READ TABLE e_remessas INTO wa_n55 INDEX 1.

    IF sy-subrc IS INITIAL.

      sy-msgv1 = wa_n55-n55_chave_acesso+25(9).
      sy-msgv2 = wa_n55-vbeln_vl.

      CALL METHOD me->add_log_cte_dist
        EXPORTING
          p_cd_chave_cte = p_cte-cd_chave_cte
          p_type         = 'I'
          p_num          = 163
          p_message_v1   = sy-msgv1
          p_message_v2   = sy-msgv2
        CHANGING
          p_lc_sequencia = lc_sequencia.

    ENDIF.

    CLEAR: wa_vtts.

    SELECT * INTO TABLE it_vttp
      FROM vttp
     WHERE vbeln EQ wa_n55-vbeln_vl.

    IF sy-subrc IS INITIAL.
      "Etapa Rodoviária
      SELECT * INTO TABLE it_vttk
        FROM vttk
         FOR ALL ENTRIES IN it_vttp
       WHERE tknum EQ it_vttp-tknum
         AND vsart EQ '01'.

      IF sy-subrc IS INITIAL.
        SELECT * INTO TABLE it_vtts
          FROM vtts
           FOR ALL ENTRIES IN it_vttk
         WHERE tknum EQ it_vttk-tknum.

        IF sy-subrc IS INITIAL.
          READ TABLE it_vtts INTO wa_vtts INDEX 1.
        ENDIF.
      ENDIF.
    ENDIF.

    "Não tem Etapa Anterior
    "E_ZLEST0044-SHTYP = 'Z003'. "Form.Lote Ferro.
    "E_ZLEST0044-SHTYP = 'Z028'. "Form.Lote Ferro. Direto

    DATA: r_parvw TYPE TABLE OF shp_parvw_range,
          w_parvw TYPE shp_parvw_range.
    w_parvw-low    = 'LR'.
    w_parvw-sign   = 'I' .
    w_parvw-option = 'EQ'.
    APPEND w_parvw TO r_parvw.
    w_parvw-low    = 'Z1'.
    APPEND w_parvw TO r_parvw.
    w_parvw-low    = 'WE'.
    APPEND w_parvw TO r_parvw.
    w_parvw-low    = 'PC'.
    APPEND w_parvw TO r_parvw.

*131  Não Encontrado Parceiro p/ Ponto de Coleta (Parc. LR da Remessa)!
*132  Não Encontrado Parceiro p/ Local de Entrega (Parc. Z1 da Remessa)!
*133  Encontrado Parceiro &1 p/ Ponto de Coleta (Parc. LR da Remessa)!
*134  Encontrado Parceiro &1 p/ Local de Entrega (Parc. Z1 da Remessa)!

    CLEAR: it_j_1bnfnad, it_j_1bnfnad[].

    LOOP AT it_n55 INTO wa_n55.
      IF it_j_1bnfnad IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF wa_n55-form IS NOT INITIAL.
        SELECT *
          FROM j_1bnfnad
          INTO TABLE it_j_1bnfnad
         WHERE docnum EQ wa_n55-docnum_nfe
           AND parvw  IN r_parvw.

        SELECT *
          FROM j_1bnflin
          INTO TABLE @DATA(it_j_1bnflin)
         WHERE docnum EQ @wa_n55-docnum_nfe.

      ENDIF.
    ENDLOOP.

    IF it_j_1bnfnad IS INITIAL.
      READ TABLE it_n55 INTO wa_n55 INDEX 1.
      IF sy-subrc IS INITIAL.
        SELECT *
          FROM j_1bnfnad
          INTO TABLE it_j_1bnfnad
         WHERE docnum EQ wa_n55-docnum_nfe
           AND parvw  IN r_parvw.

        SELECT *
          FROM j_1bnflin
          INTO TABLE @it_j_1bnflin
         WHERE docnum EQ @wa_n55-docnum_nfe.

      ENDIF.
    ENDIF.

    READ TABLE it_j_1bnflin INDEX 1 INTO wa_j_1bnflin.
    IF sy-subrc IS INITIAL AND wa_j_1bnflin-reftyp EQ 'ZW' AND wa_n55-vbeln_vl IS NOT INITIAL.
      SELECT SINGLE *
         FROM likp
         INTO @DATA(w_likp2)
         WHERE vbeln = @wa_n55-vbeln_vl.
      IF w_likp2-vbtyp = '7'.
        REFRESH it_j_1bnfnad.
      ENDIF.
    ENDIF.

    IF it_j_1bnfnad IS INITIAL AND wa_n55-vbeln_vl IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_vbpa)
        FROM vbpa
       WHERE vbeln EQ @wa_n55-vbeln_vl.

      LOOP AT it_vbpa INTO DATA(wa_vbpa).
        CLEAR: wa_j_1bnfnad.
        wa_j_1bnfnad-parvw = wa_vbpa-parvw.
        IF wa_vbpa-kunnr IS NOT INITIAL.
          wa_j_1bnfnad-parid  = wa_vbpa-kunnr.
          wa_j_1bnfnad-partyp = 'C'.
        ELSE.
          wa_j_1bnfnad-parid  = wa_vbpa-lifnr.
          wa_j_1bnfnad-partyp = 'V'.
        ENDIF.
        APPEND wa_j_1bnfnad TO it_j_1bnfnad.
      ENDLOOP.

    ENDIF.

    "Verificar Local de Entrega Não Alfandegado
    DATA(lc_coleta)   = 'LR'.
    DATA(lc_entrega)  = 'Z1'.

    e_zlest0044-shtyp = 'Z003'.

    READ TABLE it_j_1bnflin INDEX 1 INTO wa_j_1bnflin.
    IF sy-subrc IS INITIAL AND wa_j_1bnflin-reftyp EQ 'BI'.

      SELECT SINGLE * INTO @DATA(wa_vbrk)
        FROM vbrk
       WHERE vbeln EQ @wa_j_1bnflin-refkey.

      IF sy-subrc IS INITIAL AND wa_vbrk-fkart EQ 'ZRAN'.
        lc_entrega = 'WE'.
      ENDIF.

    ELSEIF sy-subrc IS INITIAL  AND ( wa_j_1bnflin-reftyp EQ 'ZW' OR wa_j_1bnflin-reftyp EQ 'LI' ).
      lc_coleta  = 'PC'.
      lc_entrega = 'LR'.
      e_zlest0044-shtyp = 'Z028'.
      IF wa_n55-vbeln_vl IS NOT INITIAL.
        SELECT SINGLE *
          FROM likp
          INTO @DATA(w_likp)
          WHERE vbeln = @wa_n55-vbeln_vl.
        IF w_likp-vbtyp = '7'.
          e_zlest0044-shtyp = 'Z031'.
        ENDIF.
      ENDIF.
    ENDIF.
    DATA(lc_material) = wa_j_1bnflin-matnr.
    DATA(lc_material_aux) = wa_j_1bnflin-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lc_material_aux
      IMPORTING
        output = lc_material_aux.

    """""""""""""""""""""""""""""""""""""""""""

    READ TABLE it_j_1bnfnad ASSIGNING FIELD-SYMBOL(<wa_j_1bnfnad_lr>) WITH KEY parvw = lc_coleta.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e131 RAISING nao_achou.
    ELSEIF lc_coleta EQ 'PC'.
      DATA(wa_j_1bnfnad_pc) = <wa_j_1bnfnad_lr>.
    ENDIF.

    READ TABLE it_j_1bnfnad ASSIGNING FIELD-SYMBOL(<wa_j_1bnfnad_z1>) WITH KEY parvw = lc_entrega.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e132 RAISING nao_achou.
      "ELSEIF LC_ENTREGA EQ 'LR'.
      "  <WA_J_1BNFNAD_LR> = <WA_J_1BNFNAD_Z1>.
    ENDIF.

    CLEAR: wa_ultima_carta.

    SELECT * INTO TABLE @DATA(it_carta_correcao)
      FROM zcarta_correcao AS ca
     WHERE docnum           EQ @<wa_j_1bnfnad_lr>-docnum
       AND novo_loc_entrega NE @space.

    LOOP AT it_carta_correcao INTO DATA(wa_carta_correcao).
      IF wa_ultima_carta IS INITIAL.
        wa_ultima_carta = wa_carta_correcao.
      ELSEIF wa_ultima_carta-id_cc LT wa_carta_correcao-id_cc.
        wa_ultima_carta = wa_carta_correcao.
      ENDIF.
    ENDLOOP.

    "Possui Carta de Correção
    IF wa_ultima_carta IS NOT INITIAL.
      "Novo Local de Entrega (Transbordo)
      IF wa_ultima_carta-novo_loc_entrega NE <wa_j_1bnfnad_lr>-parid AND wa_ultima_carta-novo_loc_entrega IS NOT INITIAL.
        <wa_j_1bnfnad_lr>-parid = wa_ultima_carta-novo_loc_entrega.
        SELECT SINGLE stcd1 INTO @DATA(lc_stcd1) FROM kna1 WHERE kunnr EQ @wa_ultima_carta-novo_loc_entrega.
        MOVE lc_stcd1 TO <wa_j_1bnfnad_lr>-cgc.
      ENDIF.
    ENDIF.

    CLEAR: wa_ultima_carta, it_carta_correcao[], it_carta_correcao.

    SELECT * INTO TABLE it_carta_correcao
      FROM zcarta_correcao AS ca
     WHERE docnum        EQ <wa_j_1bnfnad_lr>-docnum
       AND novo_terminal NE space.

    LOOP AT it_carta_correcao INTO wa_carta_correcao.
      IF wa_ultima_carta IS INITIAL.
        wa_ultima_carta = wa_carta_correcao.
      ELSEIF wa_ultima_carta-id_cc LT wa_carta_correcao-id_cc.
        wa_ultima_carta = wa_carta_correcao.
      ENDIF.
    ENDLOOP.

    "Possui Carta de Correção
    IF wa_ultima_carta IS NOT INITIAL.
      "Novo Terminal (parceiro Z1)
      IF wa_ultima_carta-novo_terminal NE <wa_j_1bnfnad_z1>-parid AND wa_ultima_carta-novo_terminal IS NOT INITIAL.
        <wa_j_1bnfnad_z1>-parid = wa_ultima_carta-novo_terminal.
        SELECT SINGLE stcd1 INTO lc_stcd1 FROM lfa1 WHERE lifnr EQ wa_ultima_carta-novo_terminal.
        MOVE lc_stcd1 TO <wa_j_1bnfnad_z1>-cgc.
      ENDIF.
    ENDIF.

    READ TABLE it_j_1bnfnad INTO DATA(wa_j_1bnfnad_lr) WITH KEY parvw = lc_coleta.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e131 RAISING nao_achou.
    ELSEIF lc_coleta EQ 'PC'.
      wa_j_1bnfnad_pc = wa_j_1bnfnad_lr.
    ENDIF.

    READ TABLE it_j_1bnfnad INTO DATA(wa_j_1bnfnad_z1) WITH KEY parvw = lc_entrega.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e132 RAISING nao_achou.
    ELSEIF lc_entrega EQ 'LR'.
      wa_j_1bnfnad_lr = wa_j_1bnfnad_z1.
    ENDIF.

    "Frete não teve transbordo
    IF wa_j_1bnfnad_lr-cgc EQ wa_j_1bnfnad_z1-cgc AND e_zlest0044-shtyp NE 'Z031'.

      lc_entrega = 'LR'.
      lc_coleta  = 'PC'.
      e_zlest0044-shtyp = 'Z028'.

      READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad_pc WITH KEY parvw = 'PC'.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e132 RAISING nao_achou.
      ENDIF.

    ENDIF.

    CLEAR: wa_tvkn.

    CASE lc_coleta.
      WHEN 'LR'.

        IF wa_j_1bnfnad_lr IS NOT INITIAL.
          IF p_gravar IS NOT INITIAL.
            sy-msgv1 = wa_j_1bnfnad_lr-parid.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'S'
                p_num          = 133
                p_message_v1   = sy-msgv1
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.
          e_org_cust  = wa_j_1bnfnad_lr-parid.
          SELECT SINGLE * FROM tvkn INTO wa_tvkn WHERE kunnr EQ wa_j_1bnfnad_lr-parid.
          IF ( sy-subrc IS INITIAL ) AND ( wa_tvkn-knote IS NOT INITIAL ).
            e_org_point = wa_tvkn-knote.
          ENDIF.
        ENDIF.

        IF ( e_org_point IS INITIAL ) AND ( wa_vtts IS NOT INITIAL ).
          e_org_point = wa_vtts-knotz.
          IF e_org_cust IS INITIAL.
            e_org_cust  = wa_vtts-kunnz.
          ENDIF.
          e_org_suppl = wa_vtts-lifnz.
        ENDIF.

      WHEN 'PC'.

        IF wa_j_1bnfnad_pc IS NOT INITIAL.
          IF p_gravar IS NOT INITIAL.
            sy-msgv1 = wa_j_1bnfnad_pc-parid.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'S'
                p_num          = 189
                p_message_v1   = sy-msgv1
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.
          e_org_suppl = wa_j_1bnfnad_pc-parid.
          SELECT SINGLE * FROM tvkn INTO wa_tvkn WHERE lifnr EQ wa_j_1bnfnad_pc-parid.
          IF ( sy-subrc IS INITIAL ) AND ( wa_tvkn-knote IS NOT INITIAL ).
            e_org_point = wa_tvkn-knote.
          ENDIF.
        ENDIF.

    ENDCASE.

    CLEAR: wa_tvkn.

    CASE lc_entrega.
      WHEN 'LR'.

        IF wa_j_1bnfnad_lr IS NOT INITIAL.
          IF p_gravar IS NOT INITIAL.
            sy-msgv1 = wa_j_1bnfnad_lr-parid.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'S'
                p_num          = 133
                p_message_v1   = sy-msgv1
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.
          e_dest_cust  = wa_j_1bnfnad_lr-parid.
          SELECT SINGLE * FROM tvkn INTO wa_tvkn WHERE kunnr EQ wa_j_1bnfnad_lr-parid.
          IF ( sy-subrc IS INITIAL ) AND ( wa_tvkn-knote IS NOT INITIAL ).
            e_dest_point = wa_tvkn-knote.
          ENDIF.
        ENDIF.

      WHEN 'Z1' OR 'WE'.

        IF wa_j_1bnfnad_z1 IS NOT INITIAL.

          IF p_gravar IS NOT INITIAL.
            sy-msgv1 = wa_j_1bnfnad_z1-parid.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'S'
                p_num          = 134
                p_message_v1   = sy-msgv1
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.

          CASE lc_entrega.
            WHEN 'WE'.
              TRY .
                  zcl_fornecedores=>zif_parceiros~get_instance(
                   )->set_parceiro_cnpj_cpf_ie(
                    EXPORTING
                      i_cnpj          = wa_j_1bnfnad_z1-cgc
                      i_cpf           = wa_j_1bnfnad_z1-cpf
                      i_insc_estatual = wa_j_1bnfnad_z1-stains
                   )->get_id_parceiro( IMPORTING e_parceiro = e_dest_suppl
                   ).
                CATCH zcx_parceiros.    " .
              ENDTRY.
            WHEN 'Z1'.
              e_dest_suppl = wa_j_1bnfnad_z1-parid.
          ENDCASE.

          SELECT SINGLE * FROM tvkn INTO wa_tvkn WHERE lifnr EQ wa_j_1bnfnad_z1-parid.
          IF ( sy-subrc IS INITIAL ) AND ( wa_tvkn-knote IS NOT INITIAL ).
            e_dest_point = wa_tvkn-knote.
          ENDIF.
        ENDIF.

    ENDCASE.

    "Busca Dados de Local de Partida e Chegada """""""""""""""""""""""""""""""""" <<<<<

    "Busca Zonas de Transporte """""""""""""""""""""""""""""""""""""""""""""""""" >>>>>
    LOOP AT it_j_1bnfnad INTO wa_j_1bnfnad.
      CASE wa_j_1bnfnad-partyp.
        WHEN 'B'. "*B Local de negócios
          wa_j_1bnfnad-parid = wa_j_1bnfnad-parid+6(4).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_j_1bnfnad-parid
            IMPORTING
              output = wa_j_1bnfnad-parid.
          SELECT SINGLE lzone INTO wa_j_1bnfnad-teltx
            FROM kna1
           WHERE kunnr EQ wa_j_1bnfnad-parid.
        WHEN 'C'. "*C Cliente
          SELECT SINGLE lzone INTO wa_j_1bnfnad-teltx
            FROM kna1
           WHERE kunnr EQ wa_j_1bnfnad-parid.
        WHEN 'V'. "*V Forneced.
          SELECT SINGLE lzone INTO wa_j_1bnfnad-teltx
            FROM lfa1
           WHERE lifnr EQ wa_j_1bnfnad-parid.
      ENDCASE.

      CASE wa_j_1bnfnad-parvw.
        WHEN lc_coleta. "Ponto de Coleta
          wa_a910-lzonea = wa_j_1bnfnad-teltx.
        WHEN lc_entrega. "Local de Entrega
          wa_a910-lzonez = wa_j_1bnfnad-teltx.
      ENDCASE.
    ENDLOOP.
    "Busca Zonas de Transporte """""""""""""""""""""""""""""""""""""""""""""""""" <<<<<

    "Busca ZFRE de Zona Origem e Destino """""""""""""""""""""""""""""""""""""""" >>>>>
    CASE e_zlest0044-shtyp.

      WHEN 'Z003' OR 'Z028' OR 'Z031' . "Tranporte com tranbordo

        wa_a933-kappl  = 'F'.
        wa_a933-kschl  = 'ZFRE'.
        wa_a933-shtyp  = e_zlest0044-shtyp.
        wa_a933-tdlnr  = p_cte-p_emissor.
        wa_a933-datbi  = e_zlest0044-dt_referencia.
        wa_a933-datab  = e_zlest0044-dt_referencia.
        wa_a933-vstel  = p_cte-f_tomadora.
        wa_a933-lzonea = wa_a910-lzonea.
        wa_a933-lzonez = wa_a910-lzonez.

        READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad_pc WITH KEY parvw = 'PC'.
        IF sy-subrc IS INITIAL AND wa_j_1bnfnad_pc-partyp EQ 'V'.
          "01	Logística - Tabelamento de Preço Ferroviário

          TRY .

              DATA(r_id_cidade_base) =
                zcl_calc_frete=>get_cidade_tabela_mesorregiao(
                EXPORTING
                  i_agente_frete = p_cte-p_emissor " Nº conta do fornecedor
                  i_ponto_coleta = wa_j_1bnfnad_pc-parid    " Nº conta do fornecedor
              ).

              "Achou Tabela de Preço para o Fornecedor
              TRY .
                  zcl_calc_frete=>get_valor_frete(
                    EXPORTING
                      i_kappl           = 'F'                " Aplicação
                      i_kschl           = 'ZFRE'             " Tipo de condição
                      i_tdlnr           = wa_a933-tdlnr      " Nº do agente de frete
                      i_shtyp           = wa_a933-shtyp      " Tipo de transporte
                      i_lzonea          = wa_a933-lzonea     " Zona de partida
                      i_lzonez          = wa_a933-lzonez     " Zona de chegada
                      i_id_cidade_base  = r_id_cidade_base   " Cidade Base (Região)
                      i_data_referencia = wa_a933-datbi      " Data de Referência
                      i_matnr           = lc_material        " Material
                    IMPORTING
                      e_kbetr           = DATA(e_kbetr)               " Montante/porcentagem de condição no caso de não haver escala
                      e_konwa           = DATA(e_konwa)               " Unidade de condição (moeda ou porcentagem)
                      e_krech           = DATA(e_krech)               " Regra de cálculo de condição
                      e_texto           = DATA(e_texto)
                  ).

                  IF e_kbetr NE e_zlest0044-tarifa.

                    SELECT low FROM tvarvc WHERE name = 'Z_BUKRS_FERR_TARIFA_XML' AND low <> '' INTO TABLE @DATA(Lt_tvarvc). "127460 CS2023000574 Automação Tarifa PSA
                    IF sy-subrc = 0.
                      READ TABLE Lt_tvarvc INTO DATA(ls_tvarvc) WITH KEY low = e_zlest0044-bukrs.
                    ENDIF.

                    IF ls_tvarvc IS NOT INITIAL.

                      IF r_id_cidade_base IS NOT INITIAL AND wa_a933-shtyp = 'Z003' OR wa_a933-shtyp = 'Z028'. "2 Mesoregiao (2.2 - Se fornecedor estiver previsto para mesorregião , vamos  cadastrar a tarifa de  frete na tabela  A938)
                        DATA _pstlza TYPE pstlza.
                        _pstlza = r_id_cidade_base.
                        CLEAR: ls_message_automation.
                        CALL FUNCTION 'Z_AUTO_CAD_TARIFA_FERR'
                          EXPORTING
                            i_condicao = '2'
                            i_shtyp    = wa_a933-shtyp
                            i_tdlnr    = wa_a933-tdlnr
                            i_lzonea   = wa_a933-lzonea
                            i_lzonez   = wa_a933-lzonez
                            i_kbetr    = e_zlest0044-tarifa
                            i_matnr    = lc_material
                            i_pstlza   = _pstlza
                            "I_VSTEL    = p_cte-f_tomadora
                            i_datab    = e_zlest0044-dt_referencia
                          IMPORTING
                            e_message  = ls_message_automation.

                        IF ls_message_automation = 'SUCESSO'.
                          CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
                          ck_achou_preco = abap_true.
                          e_kbetr = e_zlest0044-tarifa.
                          wa_konp-kbetr = e_zlest0044-tarifa.
                        ENDIF.

                        IF sy-subrc = 0.
                          CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
                          ck_achou_preco = abap_true.
                          e_kbetr = e_zlest0044-tarifa.
                          wa_konp-kbetr = e_zlest0044-tarifa.
                        ENDIF.


                      ENDIF.

                    ELSE.
                      ck_achou_preco = abap_false.
                      DATA(lc_texto) = |{ wa_a933-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.
                      MESSAGE s135 WITH wa_a933-shtyp wa_a933-tdlnr wa_a933-lzonea lc_texto.

                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = p_cte-cd_chave_cte
                          p_id           = sy-msgid
                          p_type         = 'E'
                          p_num          = sy-msgno
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                          p_message_v3   = sy-msgv3
                          p_message_v4   = sy-msgv4
                          p_mensagem     = CONV #( e_texto )
                        CHANGING
                          p_lc_sequencia = lc_sequencia.

                      MESSAGE e139 WITH e_zlest0044-tarifa e_kbetr RAISING nao_achou.

                    ENDIF.



                  ELSE.

                    ck_achou_preco = abap_true.
                    lc_texto = |{ wa_a933-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.

                    IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                      MESSAGE s136 WITH wa_a933-shtyp wa_a933-tdlnr wa_a933-lzonea lc_texto.
                    ENDIF.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = p_cte-cd_chave_cte
                        p_id           = sy-msgid
                        p_type         = 'S'
                        p_num          = sy-msgno
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                        p_message_v3   = sy-msgv3
                        p_message_v4   = sy-msgv4
                        p_mensagem     = CONV #( e_texto )
                      CHANGING
                        p_lc_sequencia = lc_sequencia.

                  ENDIF.

                CATCH zcx_calc_frete INTO DATA(ex_frete).

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = p_cte-cd_chave_cte
                      p_id           = ex_frete->msgid
                      p_type         = 'E'
                      p_num          = ex_frete->msgno
                      p_message_v1   = ex_frete->msgv1
                      p_message_v2   = ex_frete->msgv2
                      p_message_v3   = ex_frete->msgv3
                      p_message_v4   = ex_frete->msgv4
                      p_mensagem     = CONV #( e_texto )
                    CHANGING
                      p_lc_sequencia = lc_sequencia.

                  lc_texto = |{ wa_a933-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.

                  IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                    MESSAGE e135 WITH wa_a933-shtyp wa_a933-tdlnr wa_a933-lzonea lc_texto RAISING nao_achou.
                  ENDIF.

              ENDTRY.

            CATCH zcx_calc_frete INTO ex_frete.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_id           = ex_frete->msgid
                  p_type         = 'W'
                  p_num          = ex_frete->msgno
                  p_message_v1   = ex_frete->msgv1
                  p_message_v2   = ex_frete->msgv2
                  p_message_v3   = ex_frete->msgv3
                  p_message_v4   = ex_frete->msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
          ENDTRY.
        ENDIF.

        IF ck_achou_preco EQ abap_false.
          SELECT SINGLE * INTO wa_a933
            FROM a933
           WHERE kappl  EQ wa_a933-kappl
             AND kschl  EQ wa_a933-kschl
             AND shtyp  EQ wa_a933-shtyp
             AND tdlnr  EQ wa_a933-tdlnr
             AND lzonea EQ wa_a933-lzonea
             AND lzonez EQ wa_a933-lzonez
             AND vstel  EQ wa_a933-vstel
             AND datbi  GE wa_a933-datbi
             AND datab  LE wa_a933-datab
             AND lzonea NE space
             AND lzonez NE space.

          IF sy-subrc IS NOT INITIAL.

            wa_a910-kappl  = 'F'.
            wa_a910-kschl  = 'ZFRE'.
            wa_a910-shtyp  = e_zlest0044-shtyp.
            wa_a910-tdlnr  = p_cte-p_emissor.
            wa_a910-datbi  = e_zlest0044-dt_referencia.
            wa_a910-datab  = e_zlest0044-dt_referencia.

            SELECT SINGLE * INTO wa_a910
              FROM a910
             WHERE kappl  EQ wa_a910-kappl
               AND kschl  EQ wa_a910-kschl
               AND shtyp  EQ wa_a910-shtyp
               AND tdlnr  EQ wa_a910-tdlnr
               AND lzonea EQ wa_a910-lzonea
               AND lzonez EQ wa_a910-lzonez
               AND datbi  GE wa_a910-datbi
               AND datab  LE wa_a910-datab
               AND lzonea NE space
               AND lzonez NE space.

          ELSE.
            MOVE-CORRESPONDING wa_a933 TO wa_a910.
          ENDIF.

          IF sy-subrc IS NOT INITIAL.
            lc_texto = |{ wa_a910-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.

            IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
              MESSAGE e135 WITH wa_a910-shtyp wa_a910-tdlnr wa_a910-lzonea lc_texto RAISING nao_achou.
            ENDIF.

          ELSE.
            IF p_gravar IS NOT INITIAL.

              lc_texto = |{ wa_a910-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.

              MOVE: wa_a910-shtyp  TO sy-msgv1,
                    wa_a910-tdlnr  TO sy-msgv2,
                    wa_a910-lzonea TO sy-msgv3,
                    lc_texto       TO sy-msgv4.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 136
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDIF.
          ENDIF.
          "Busca ZFRE de Zona Origem e Destino """""""""""""""""""""""""""""""""""""""" <<<<<

          "Busca Valor do ZFRE da Condição de Zona """""""""""""""""""""""""""""""""""" >>>>>
          wa_konp-knumh = wa_a910-knumh.

          IF wa_konp-knumh IS NOT INITIAL.

            CASE e_tipo_contrato.
              WHEN '0001'.
                wa_konp-kmein = 'TO'.
                SELECT SINGLE * INTO wa_konp
                  FROM konp
                 WHERE knumh    EQ wa_konp-knumh
                   AND kmein    EQ wa_konp-kmein
                   AND loevm_ko EQ abap_false.

              WHEN '0002'.
                SELECT SINGLE * INTO wa_konp
                  FROM konp
                 WHERE knumh    EQ wa_konp-knumh
                   AND krech    EQ 'B' "freço fixo
                   AND loevm_ko EQ abap_false.
            ENDCASE.

            IF sy-subrc IS NOT INITIAL.
              lc_texto = |{ wa_a910-lzonez }/Material: { lc_material_aux }/Mesorregião: { r_id_cidade_base }|.
              IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
                MESSAGE e135 WITH wa_a910-shtyp wa_a910-tdlnr wa_a910-lzonea lc_texto RAISING nao_achou.
              ENDIF.
            ELSE.

*              IF e_tipo_contrato EQ '0002'. "127460 CS2023000574 Automação Tarifa PSA
*                e_zlest0044-tarifa = wa_konp-kbetr.
*              ENDIF.

              IF p_gravar IS NOT INITIAL.
                CASE e_tipo_contrato.
                  WHEN '0001'.
                    MOVE: wa_konp-kbetr  TO sy-msgv1,
                          wa_konp-kmein  TO sy-msgv2.
                    CONDENSE sy-msgv1.
                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = p_cte-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 138
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  WHEN '0002'.
                    MOVE: wa_konp-kbetr  TO sy-msgv1.
                    CONDENSE sy-msgv1.
                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = p_cte-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 194
                        p_message_v1   = sy-msgv1
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                ENDCASE.
              ENDIF.

            ENDIF.
          ELSE.
            IF _job = abap_false. "Se não for por job mostra a menssagemj! "174411 Job: ZMM0079_FRETE_FERROVIARIO Cancelado PSA, esta junta na request DEVK9A2IAM
              MESSAGE e137 RAISING nao_achou.
            ENDIF.
          ENDIF.
          "Busca Valor do ZFRE da Condição de Zona """""""""""""""""""""""""""""""""""" <<<<<

        ELSE.
          wa_konp-kbetr = e_kbetr.
        ENDIF.

        "WHEN 'Z028'. "tranporte sem tranbordo
    ENDCASE.

    "Verifica Tarifa CT-e com Condição SAP """""""""""""""""""""""""""""""""""""" >>>>>
    MOVE: e_zlest0044-tarifa TO sy-msgv1,
          wa_konp-kbetr      TO sy-msgv2.

    CONDENSE sy-msgv1.
    CONDENSE sy-msgv2.

    IF wa_konp-kbetr NE e_zlest0044-tarifa.

      FREE: Lt_tvarvc.
      CLEAR: ls_tvarvc.
      SELECT low FROM tvarvc WHERE name = 'Z_BUKRS_FERR_TARIFA_XML' AND low <> '' INTO TABLE @Lt_tvarvc. "127460 CS2023000574 Automação Tarifa PSA
      IF sy-subrc = 0.
        READ TABLE Lt_tvarvc INTO ls_tvarvc WITH KEY low = e_zlest0044-bukrs.
      ENDIF.

      IF ls_tvarvc IS NOT INITIAL.

        IF wa_a933-shtyp = 'Z031'.  "4 - Se e_zlest0044-shtyp = 'Z031' vamos fazer o cadastro na tabela A910
          CLEAR: ls_message_automation.
          CALL FUNCTION 'Z_AUTO_CAD_TARIFA_FERR'
            EXPORTING
              i_condicao = '4'
              i_shtyp    = wa_a933-shtyp
              i_tdlnr    = wa_a933-tdlnr
              i_lzonea   = wa_a933-lzonea
              i_lzonez   = wa_a933-lzonez
              i_kbetr    = e_zlest0044-tarifa
              "i_matnr    = lc_material
              "i_pstlza   = r_id_cidade_base
              "I_VSTEL    = p_cte-f_tomadora
              i_datab    = e_zlest0044-dt_referencia
            IMPORTING
              e_message  = ls_message_automation.

          IF ls_message_automation = 'SUCESSO'.
            CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
            ck_achou_preco = abap_true.
            e_kbetr = e_zlest0044-tarifa.
            wa_konp-kbetr = e_zlest0044-tarifa.
          ENDIF.

        ELSE.

          IF wa_zlest0154 IS NOT INITIAL. "4  Se wa_zlest0154-bukrs,wa_zlest0154-lifnr e wa_zlest0154-matkl  localizado  então iremos cadastrar a tarifa de frete na tabela A910
            "Lotação
            CLEAR: ls_message_automation.
            CALL FUNCTION 'Z_AUTO_CAD_TARIFA_FERR'
              EXPORTING
                i_condicao = '4'
                i_shtyp    = wa_a933-shtyp
                i_tdlnr    = wa_a933-tdlnr
                i_lzonea   = wa_a933-lzonea
                i_lzonez   = wa_a933-lzonez
                i_kbetr    = e_zlest0044-tarifa
                "i_matnr    = lc_material
                "i_pstlza   = r_id_cidade_base
                "I_VSTEL    = p_cte-f_tomadora
                i_datab    = e_zlest0044-dt_referencia
              IMPORTING
                e_message  = ls_message_automation.

            IF ls_message_automation = 'SUCESSO'.
              CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
              ck_achou_preco = abap_true.
              e_kbetr = e_zlest0044-tarifa.
              wa_konp-kbetr = e_zlest0044-tarifa.
            ENDIF.

          ELSE.

            IF wa_a933-shtyp = 'Z003' OR wa_a933-shtyp = 'Z028'. "3 - Se fornecedor NÃO estiver previsto para mesorregião,  , vamos  cadastrar a tarifa de  frete na tabela  A933

              CLEAR: ls_message_automation.
              CALL FUNCTION 'Z_AUTO_CAD_TARIFA_FERR'
                EXPORTING
                  i_condicao = '3'
                  i_shtyp    = wa_a933-shtyp
                  i_tdlnr    = wa_a933-tdlnr
                  i_lzonea   = wa_a933-lzonea
                  i_lzonez   = wa_a933-lzonez
                  i_kbetr    = e_zlest0044-tarifa
                  "i_matnr    = lc_material
                  "i_pstlza   = r_id_cidade_base
                  i_vstel    = p_cte-f_tomadora
                  i_datab    = e_zlest0044-dt_referencia
                IMPORTING
                  e_message  = ls_message_automation.


              IF ls_message_automation = 'SUCESSO'.
                CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
                ck_achou_preco = abap_true.
                e_kbetr = e_zlest0044-tarifa.
                wa_konp-kbetr = e_zlest0044-tarifa.
              ENDIF.

            ELSE. "1  Se wa_zlest0154-bukrs,wa_zlest0154-lifnr e wa_zlest0154-matkl  localizado  então iremos cadastrar a tarifa de frete na tabela A910

              CALL FUNCTION 'Z_AUTO_CAD_TARIFA_FERR'
                EXPORTING
                  i_condicao = '1'
                  i_shtyp    = wa_a933-shtyp
                  i_tdlnr    = wa_a933-tdlnr
                  i_lzonea   = wa_a933-lzonea
                  i_lzonez   = wa_a933-lzonez
                  i_kbetr    = e_zlest0044-tarifa
                  "i_matnr    = lc_material
                  "i_pstlza   = r_id_cidade_base
                  "I_VSTEL    = p_cte-f_tomadora
                  i_datab    = e_zlest0044-dt_referencia.

              IF sy-subrc = 0.
                CLEAR: e_kbetr,wa_konp-kbetr,ck_achou_preco.
                ck_achou_preco = abap_true.
                e_kbetr = e_zlest0044-tarifa.
                wa_konp-kbetr = e_zlest0044-tarifa.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

        MOVE: e_zlest0044-tarifa TO sy-msgv1,
wa_konp-kbetr      TO sy-msgv2.

        CONDENSE sy-msgv1.
        CONDENSE sy-msgv2.

        IF wa_konp-kbetr EQ e_zlest0044-tarifa.
          IF p_gravar IS NOT INITIAL.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'S'
                p_num          = 140
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.
        ENDIF.

      ELSE.
        "**********************************************************************
        IF e_tipo_contrato EQ '0002'. "127460 CS2023000574 Automação Tarifa PSA
          WRITE e_zlest0044-tarifa     TO sy-msgv1.
          WRITE e_zlest0044-vlr_seguro TO sy-msgv2.
          CONDENSE sy-msgv1 NO-GAPS.
          CONDENSE sy-msgv2 NO-GAPS.
          IF p_gravar IS NOT INITIAL.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = 'E'
                p_num          = 196
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDIF.
          MESSAGE i196 WITH sy-msgv1 sy-msgv2.
        ENDIF.
        MESSAGE e139 WITH sy-msgv1 sy-msgv2 RAISING nao_achou.
        "**********************************************************************
      ENDIF.

    ELSE.
      IF p_gravar IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = 'S'
            p_num          = 140
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.
    ENDIF.
    "Verifica Tarifa CT-e com Condição SAP """""""""""""""""""""""""""""""""""""" <<<<<

  ENDMETHOD.


  METHOD BUSCAR_TIPO_IMPOSTO.


    DATA: IT_J_1BAJ TYPE TABLE OF J_1BAJ,
          WA_J_1BAJ TYPE J_1BAJ,
          IT_T683S  TYPE TABLE OF T683S,
          WA_T683S  TYPE T683S.

    SELECT * INTO TABLE IT_J_1BAJ FROM J_1BAJ WHERE TAXGRP EQ I_TAXGRP.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE IT_T683S
      FROM T683S
       FOR ALL ENTRIES IN IT_J_1BAJ
     WHERE KVEWE EQ 'A'
       AND KAPPL EQ 'TX'
       AND KALSM EQ 'TAXBRA'
       AND KSCHL EQ IT_J_1BAJ-TAXTYP
       AND KSTAT EQ 'X'.

    CHECK SY-SUBRC IS INITIAL.

    READ TABLE IT_T683S INDEX 1 INTO WA_T683S.
    E_KSCHL = WA_T683S-KSCHL.

  ENDMETHOD.


  METHOD BUSCA_BANCO_PARCEIRO.


    IF P_CTE-ZBVTYP IS INITIAL.
      CALL FUNCTION 'FI_F4_BVTYP'
        EXPORTING
          I_KUNNR        = SPACE
          I_LIFNR        = P_CTE-P_EMISSOR
          I_XSHOW        = SPACE
        IMPORTING
          E_BVTYP        = P_CTE-ZBVTYP
        EXCEPTIONS
          NO_BVTYP_FOUND = 1
          INVALID_CALL   = 2
          OTHERS         = 3.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO_BANCO.
      ENDIF.
    ENDIF.

    CHECK ( P_CTE-ZBVTYP IS NOT INITIAL ) AND ( P_CTE-P_EMISSOR IS NOT INITIAL ).

    SELECT SINGLE *
      INTO E_LFBK
      FROM LFBK
     WHERE LIFNR EQ P_CTE-P_EMISSOR
       AND BVTYP EQ P_CTE-ZBVTYP.

    SELECT SINGLE * INTO E_BNKA
      FROM BNKA
     WHERE BANKS EQ E_LFBK-BANKS
       AND BANKL EQ E_LFBK-BANKL.

  ENDMETHOD.


  METHOD BUSCA_CTE_DADOS_ANTERIOR.

*0  CT-e Normal
*1  CT-e de Complemento de Valores
*2  CT-e de Anulação de Valores
*3  CT-e Substituto

    DATA: ZCTE_ANT TYPE ZIB_CTE_DIST_TER.

    SELECT SINGLE * INTO ZCTE_ANT FROM ZIB_CTE_DIST_TER WHERE CD_CHAVE_CTE EQ E_CTE_C57-C57_CHAVE_ACESSO.
    IF ( SY-SUBRC IS NOT INITIAL ) OR ( SY-SUBRC IS INITIAL AND ZCTE_ANT-DOCNUM_CTE IS INITIAL ).
      "Verifica se Existe Outro CT-e que pagou a mesma nota fiscal do CT-e complementado
      "CT-e que Complementado que foi pago
      SELECT T~CD_CHAVE_CTE INTO TABLE @DATA(IT_CTES)
        FROM ZIB_CTE_DIST_C57 AS O
       INNER JOIN ZIB_CTE_DIST_N55 AS N ON N~CD_CHAVE_CTE     EQ O~C57_CHAVE_ACESSO
       INNER JOIN ZIB_CTE_DIST_N55 AS M ON M~N55_CHAVE_ACESSO EQ N~N55_CHAVE_ACESSO
       INNER JOIN ZIB_CTE_DIST_TER AS T ON T~CD_CHAVE_CTE     EQ M~CD_CHAVE_CTE
       WHERE O~CD_CHAVE_CTE   EQ @E_CTE_DISTR-CD_CHAVE_CTE
         AND T~P_EMISSOR      EQ @E_CTE_DISTR-P_EMISSOR
         AND T~INICIO_IBGE    EQ @E_CTE_DISTR-INICIO_IBGE
         AND T~TERMINO_IBGE	  EQ @E_CTE_DISTR-TERMINO_IBGE
         AND T~EMIT_CNPJ      EQ @E_CTE_DISTR-EMIT_CNPJ
         AND T~EMIT_CPF       EQ @E_CTE_DISTR-EMIT_CPF
         AND T~EMIT_IE        EQ @E_CTE_DISTR-EMIT_IE
         AND T~REME_CNPJ      EQ @E_CTE_DISTR-REME_CNPJ
         AND T~REME_CPF       EQ @E_CTE_DISTR-REME_CPF
         AND T~REME_IE        EQ @E_CTE_DISTR-REME_IE
         AND T~DEST_CNPJ      EQ @E_CTE_DISTR-DEST_CNPJ
         AND T~DEST_CPF       EQ @E_CTE_DISTR-DEST_CPF
         AND T~DEST_IE        EQ @E_CTE_DISTR-DEST_IE
         AND T~CK_FINALIZADO  EQ @ABAP_TRUE
         AND T~CD_TIPO_CTE    EQ '0'.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_CTES INTO DATA(WA_CTES) INDEX 1.
        SELECT SINGLE * INTO ZCTE_ANT FROM ZIB_CTE_DIST_TER WHERE CD_CHAVE_CTE EQ WA_CTES-CD_CHAVE_CTE.
      ENDIF.
    ENDIF.

    IF SY-SUBRC IS INITIAL.

      E_CTE_C57-DOCNUM_CTE        = ZCTE_ANT-DOCNUM_CTE.

      CASE E_CTE_DISTR-CD_TIPO_CTE.
        WHEN '1'. "CT-e de Complemento de Valores
          E_CTE_DISTR-DOCNUM_CTE_C = ZCTE_ANT-DOCNUM_CTE.
        WHEN '2'. "CT-e de Anulação de Valores
          E_CTE_DISTR-DOCNUM_CTE_A = ZCTE_ANT-DOCNUM_CTE.
        WHEN '3'. "CT-e Substituto
          E_CTE_DISTR-DOCNUM_CTE_S = ZCTE_ANT-DOCNUM_CTE.
      ENDCASE.

      IF ZCTE_ANT-DOCNUM_CTE IS INITIAL.
        MESSAGE E021 RAISING NAO_ESCRITURADO.
      ENDIF.
      IF E_CTE_DISTR-CD_TIPO_CTE = '1'. "Complemento de Valor
        E_CTE_DISTR-TP_PROCESSO_CTE = ZCTE_ANT-TP_PROCESSO_CTE.
        E_CTE_DISTR-EBELN	          = ZCTE_ANT-EBELN.
        E_CTE_DISTR-EBELP	          = ZCTE_ANT-EBELP.
        "E_CTE_DISTR-MWSKZ            = ZCTE_ANT-MWSKZ.
        E_CTE_DISTR-DT_CHEGADA      = E_CTE_DISTR-DT_EMISSAO.
        E_CTE_DISTR-ZBVTYP          = ZCTE_ANT-ZBVTYP.
      ENDIF.

    ELSE.
      MESSAGE E020 RAISING NAO_ACHOU_CTE.
    ENDIF.

  ENDMETHOD.


  METHOD busca_docnum_chave.


    DATA: wa_active    TYPE j_1bnfe_active,
          wa_lfa1      TYPE lfa1,
          qtd          TYPE i,
          opcao        TYPE char01,
          reme_cpf_aux TYPE j_1bcgc.

    CLEAR: e_active.

    IF e_docnum IS NOT INITIAL.
      IF p_j_1bbranch IS INITIAL.
        CALL METHOD me->busca_tomador_servico
          EXPORTING
            p_cte_dist         = p_cte_dist
          IMPORTING
            e_j_1bbranch       = p_j_1bbranch
          EXCEPTIONS
            nao_achou_parceiro = 1
            OTHERS             = 2.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING nao_achou_parceiro.
        ENDIF.
      ENDIF.

      SELECT SINGLE * INTO e_active
        FROM j_1bnfe_active AS a
       WHERE docnum EQ e_docnum
         AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ 'X' ).

      IF sy-subrc IS NOT INITIAL.
        CLEAR: e_docnum.
      ELSE.
        MESSAGE s004 WITH e_active-nfnum9 e_active-serie e_active-model e_active-stcd1.
      ENDIF.
    ENDIF.

    CHECK e_docnum IS INITIAL.

    IF p_psq_chave NE true.

      IF p_j_1bbranch IS INITIAL.

        CALL METHOD me->busca_tomador_servico
          EXPORTING
            p_cte_dist         = p_cte_dist
          IMPORTING
            e_j_1bbranch       = p_j_1bbranch
          EXCEPTIONS
            nao_achou_parceiro = 1
            OTHERS             = 2.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING nao_achou_parceiro.
        ENDIF.

      ENDIF.

      qtd = strlen( p_chave ).

      IF qtd LT 44.
        MESSAGE e002 WITH p_chave RAISING nao_achou.
      ENDIF.

      wa_active-regio     = p_chave(2).
      wa_active-nfyear    = p_chave+2(2).
      wa_active-nfmonth   = p_chave+4(2).
      wa_active-stcd1     = p_chave+6(14).
      wa_active-model     = p_chave+20(2).
      wa_active-serie     = p_chave+22(3).
      wa_active-nfnum9    = p_chave+25(9).
      wa_active-docnum9   = p_chave+34(9).
      wa_active-cdv       = p_chave+43(1).

      "Entrada Manual ou Remetente Igual ao Destinatário (Remesso Formação de Lote)
      IF ( p_ck_manual IS NOT INITIAL ) OR ( p_cte_dist-reme_cnpj EQ p_cte_dist-dest_cnpj ).
        opcao = '4'.
      ELSE.
        opcao = p_cte_dist-cd_tomador.

        "Se Tomador Receber e for igual ao destinatário, trocar tomador para Destinatário
        IF opcao EQ '2' AND p_cte_dist-dest_cnpj EQ p_cte_dist-receb_cnpj.
          opcao = '3'.
        ENDIF.

      ENDIF.

      CASE opcao.
        WHEN '0'. "Remetente  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

          "Busca nota fiscal de emissão própria
          SELECT SINGLE * INTO wa_active
            FROM j_1bnfe_active AS a
           WHERE regio    EQ wa_active-regio
             AND nfyear   EQ wa_active-nfyear
             AND nfmonth  EQ wa_active-nfmonth
             AND stcd1    EQ wa_active-stcd1
             AND model    EQ wa_active-model
             AND serie    EQ wa_active-serie
             AND nfnum9   EQ wa_active-nfnum9
             AND docnum9  EQ wa_active-docnum9
             AND cdv      EQ wa_active-cdv
             AND form     NE space
             AND stcd1    EQ p_j_1bbranch-stcd1
             AND cancel   NE abap_true
             AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

          IF sy-subrc IS NOT INITIAL.
            CALL METHOD me->busca_docnum_chave_nt_prop
              EXPORTING
                p_chave  = p_chave
              IMPORTING
                e_docnum = e_docnum
                e_active = e_active.

            IF e_docnum IS NOT INITIAL.
              e_docnum  = e_docnum.
              e_active  = e_active.
              MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
            ELSE.
              MESSAGE e001 WITH p_chave RAISING nao_achou.
            ENDIF.
          ELSE.
            e_docnum = wa_active-docnum.
            e_active = wa_active.
            MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
          ENDIF.

        WHEN '3'. "Destinatário ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

          CALL METHOD me->busca_fornecedor
            EXPORTING
              p_tp_doc  = p_cte_dist-reme_tp_doc
              p_cnpj    = p_cte_dist-reme_cnpj
              p_cpf     = p_cte_dist-reme_cpf
              p_ie      = p_cte_dist-reme_ie
            IMPORTING
              e_lfa1    = wa_lfa1
            EXCEPTIONS
              nao_achou = 1
              OTHERS    = 2.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE e013 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING nao_achou_parceiro.
          ENDIF.

          IF wa_lfa1-ktokk EQ 'ZFIC'.
            "Busca nota fiscal de emissão de terceiro

            IF ( wa_active-stcd1 EQ p_cte_dist-reme_cnpj ). " Nota Fiscal de Emissão Própria

              "Busca nota fiscal de emissão própria
              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     NE space
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

              IF sy-subrc IS NOT INITIAL.
                CALL METHOD me->busca_docnum_chave_nt_prop
                  EXPORTING
                    p_chave  = p_chave
                  IMPORTING
                    e_docnum = e_docnum
                    e_active = e_active.

                IF e_docnum IS NOT INITIAL.
                  e_docnum  = e_docnum.
                  e_active  = e_active.
                  MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
                ELSE.
                  MESSAGE e001 WITH p_chave RAISING nao_achou.
                ENDIF.
              ELSE.
                e_docnum = wa_active-docnum.
                e_active = wa_active.
                MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
              ENDIF.

            ELSE.

              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     EQ space
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

            ENDIF.
          ELSE.
            "Busca nota fiscal de emissão de terceiro
            SELECT SINGLE * INTO wa_active
              FROM j_1bnfe_active AS a
             WHERE regio    EQ wa_active-regio
               AND nfyear   EQ wa_active-nfyear
               AND nfmonth  EQ wa_active-nfmonth
               AND stcd1    EQ wa_active-stcd1
               AND model    EQ wa_active-model
               AND serie    EQ wa_active-serie
               AND nfnum9   EQ wa_active-nfnum9
               AND docnum9  EQ wa_active-docnum9
               AND cdv      EQ wa_active-cdv
               AND form     EQ space
               AND parid    EQ wa_lfa1-lifnr
               AND cancel   NE abap_true
               AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).
          ENDIF.

          IF sy-subrc IS NOT INITIAL.
            CALL METHOD me->busca_docnum_chave_nt_prop
              EXPORTING
                p_chave  = p_chave
              IMPORTING
                e_docnum = e_docnum
                e_active = e_active.

            IF e_docnum IS NOT INITIAL.
              e_docnum  = e_docnum.
              e_active  = e_active.
              MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
            ELSE.
              MESSAGE e001 WITH p_chave RAISING nao_achou.
            ENDIF.
          ELSE.
            e_docnum = wa_active-docnum.
            e_active = wa_active.
            MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
          ENDIF.

        WHEN '4'. "Outros ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

          CLEAR: wa_lfa1.

          CALL METHOD me->busca_fornecedor
            EXPORTING
              p_tp_doc  = p_cte_dist-reme_tp_doc
              p_cnpj    = p_cte_dist-reme_cnpj
              p_cpf     = p_cte_dist-reme_cpf
              p_ie      = p_cte_dist-reme_ie
            IMPORTING
              e_lfa1    = wa_lfa1
            EXCEPTIONS
              nao_achou = 1
              OTHERS    = 2.

          "NFE CNPJ = REMETENTE NOTA = REMENTENTE LOCAL DE NEGOCIO (ZFIC).
          "Busca Nota Fiscal de Saida
          CLEAR: reme_cpf_aux.
          reme_cpf_aux = p_cte_dist-reme_cpf.
          reme_cpf_aux = | { reme_cpf_aux ALPHA = IN }|.
          IF ( wa_active-stcd1 EQ p_cte_dist-reme_cnpj ) OR ( wa_active-stcd1 EQ reme_cpf_aux ). " Nota Fiscal de Emissão Própria

            IF wa_lfa1-ktokk EQ 'ZFIC'.
              "Busca nota fiscal de emissão própria
              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     NE space
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

              IF sy-subrc IS NOT INITIAL.
                CALL METHOD me->busca_docnum_chave_nt_prop
                  EXPORTING
                    p_chave  = p_chave
                  IMPORTING
                    e_docnum = e_docnum
                    e_active = e_active.

                IF e_docnum IS NOT INITIAL.
                  e_docnum  = e_docnum.
                  e_active  = e_active.
                  MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
                ELSE.
                  MESSAGE e001 WITH p_chave RAISING nao_achou.
                ENDIF.
              ELSE.
                e_docnum = wa_active-docnum.
                e_active = wa_active.
                MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
              ENDIF.

            ELSEIF wa_lfa1-ktokk NE 'ZFIC'. " Nota Fiscal de Terceiro
              "Busca nota fiscal de emissão própria
              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     EQ space
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).

              IF sy-subrc IS NOT INITIAL.
                CALL METHOD me->busca_docnum_chave_nt_prop
                  EXPORTING
                    p_chave  = p_chave
                  IMPORTING
                    e_docnum = e_docnum
                    e_active = e_active.

                IF e_docnum IS NOT INITIAL.
                  e_docnum  = e_docnum.
                  e_active  = e_active.
                  MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
                ELSE.
                  MESSAGE e001 WITH p_chave RAISING nao_achou.
                ENDIF.
              ELSE.
                e_docnum = wa_active-docnum.
                e_active = wa_active.
                MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
              ENDIF.
            ENDIF.

          ELSEIF wa_active-stcd1 EQ p_cte_dist-dest_cnpj. " Nota Fiscal de Emissão Terceiro

            CLEAR: wa_lfa1.

            CALL METHOD me->busca_fornecedor
              EXPORTING
                p_tp_doc  = p_cte_dist-dest_tp_doc
                p_cnpj    = p_cte_dist-dest_cnpj
                p_cpf     = p_cte_dist-dest_cpf
                p_ie      = p_cte_dist-dest_ie
              IMPORTING
                e_lfa1    = wa_lfa1
              EXCEPTIONS
                nao_achou = 1
                OTHERS    = 2.

            IF sy-subrc IS NOT INITIAL.
              MESSAGE e013 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING nao_achou_parceiro.
            ENDIF.

            "Somente procura a nota se o Destinatário for uma empresa do grupo
            IF wa_lfa1-ktokk EQ 'ZFIC'.
              "Busca nota fiscal de emissão de terceiro
              SELECT SINGLE * INTO wa_active
                FROM j_1bnfe_active AS a
               WHERE regio    EQ wa_active-regio
                 AND nfyear   EQ wa_active-nfyear
                 AND nfmonth  EQ wa_active-nfmonth
                 AND stcd1    EQ wa_active-stcd1
                 AND model    EQ wa_active-model
                 AND serie    EQ wa_active-serie
                 AND nfnum9   EQ wa_active-nfnum9
                 AND docnum9  EQ wa_active-docnum9
                 AND cdv      EQ wa_active-cdv
                 AND form     NE space
                 AND cancel   NE abap_true
                 AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).
            ENDIF.

            IF sy-subrc IS NOT INITIAL.
              CALL METHOD me->busca_docnum_chave_nt_prop
                EXPORTING
                  p_chave  = p_chave
                IMPORTING
                  e_docnum = e_docnum
                  e_active = e_active.

              IF e_docnum IS NOT INITIAL.
                e_docnum  = e_docnum.
                e_active  = e_active.
                MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
              ELSE.
                MESSAGE e001 WITH p_chave RAISING nao_achou.
              ENDIF.
            ELSE.
              e_docnum = wa_active-docnum.
              e_active = wa_active.
              MESSAGE s004 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
            ENDIF.

          ENDIF.

      ENDCASE.

    ELSE.

      wa_active-regio     = p_chave(2).
      wa_active-nfyear    = p_chave+2(2).
      wa_active-nfmonth   = p_chave+4(2).
      wa_active-stcd1     = p_chave+6(14).
      wa_active-model     = p_chave+20(2).
      wa_active-serie     = p_chave+22(3).
      wa_active-nfnum9    = p_chave+25(9).
      wa_active-docnum9   = p_chave+34(9).
      wa_active-cdv       = p_chave+43(1).

      IF p_form EQ true.
        "Busca nota fiscal de emissão própria
        SELECT SINGLE * INTO wa_active
          FROM j_1bnfe_active AS a
         WHERE regio    EQ wa_active-regio
           AND nfyear   EQ wa_active-nfyear
           AND nfmonth  EQ wa_active-nfmonth
           AND stcd1    EQ wa_active-stcd1
           AND model    EQ wa_active-model
           AND serie    EQ wa_active-serie
           AND nfnum9   EQ wa_active-nfnum9
           AND docnum9  EQ wa_active-docnum9
           AND cdv      EQ wa_active-cdv
           AND form     NE space
           AND cancel   NE abap_true
           AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).
      ELSE.
        "Busca nota fiscal de emissão própria
        SELECT SINGLE * INTO wa_active
          FROM j_1bnfe_active AS a
         WHERE regio    EQ wa_active-regio
           AND nfyear   EQ wa_active-nfyear
           AND nfmonth  EQ wa_active-nfmonth
           AND stcd1    EQ wa_active-stcd1
           AND model    EQ wa_active-model
           AND serie    EQ wa_active-serie
           AND nfnum9   EQ wa_active-nfnum9
           AND docnum9  EQ wa_active-docnum9
           AND cdv      EQ wa_active-cdv
           AND form     EQ space
           AND cancel   NE abap_true
           AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ abap_true ).
      ENDIF.

*023  Doc. Fiscal Saída Loc.: Nr.: &1 Sr.: &2 Md.: &3 CNPJ: &4
*024  Doc. Fiscal Entrada Loc.: Nr.: &1 Sr.: &2 Md.: &3 CNPJ: &4
*025  (Doc. Entrada) Chave &1 não localizada!
*026  (Doc. Saída) Chave &1 não localizada!

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->busca_docnum_chave_nt_prop
          EXPORTING
            p_chave  = p_chave
          IMPORTING
            e_docnum = e_docnum
            e_active = e_active.

        IF e_docnum IS NOT INITIAL.
          e_docnum  = e_docnum.
          e_active  = e_active.
          CASE p_form.
            WHEN true.
              MESSAGE s023 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
            WHEN false.
              MESSAGE s024 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
          ENDCASE.
        ELSE.
          CASE p_form.
            WHEN true.
              MESSAGE e026 WITH p_chave RAISING nao_achou.
            WHEN false.
              MESSAGE e025 WITH p_chave RAISING nao_achou.
          ENDCASE.
        ENDIF.
      ELSE.
        e_docnum = wa_active-docnum.
        e_active = wa_active.
        CASE p_form.
          WHEN true.
            MESSAGE s023 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
          WHEN false.
            MESSAGE s024 WITH wa_active-nfnum9 wa_active-serie p_chave+20(2) wa_active-stcd1.
        ENDCASE.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD busca_docnum_chave_nt_prop.

    DATA lva_candat_null     TYPE j_1bnfdoc-candat.

    CLEAR e_docnum .
    IF p_chave IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(lwa_zsdt0001)
        FROM zsdt0001
        WHERE tp_movimento EQ 'E'
          AND chave_nfe = @p_chave.
      IF lwa_zsdt0001-ch_referencia IS NOT INITIAL.
        SELECT *
          FROM zmmt_ee_zgr INTO TABLE @DATA(lit_zmm_ee_zgr)
         WHERE ch_referencia EQ @lwa_zsdt0001-ch_referencia.

        IF lit_zmm_ee_zgr[] IS NOT INITIAL.

          SELECT *
            FROM zmmt_ee_zgr_docs INTO TABLE @DATA(lit_zmmt_ee_zgr_docs)
             FOR ALL ENTRIES IN @lit_zmm_ee_zgr
           WHERE obj_key EQ @lit_zmm_ee_zgr-obj_key.

        ENDIF.

        LOOP AT lit_zmmt_ee_zgr_docs INTO DATA(lwa_zmmt_ee_zgr_docs) WHERE docnum IS NOT INITIAL.
          SELECT SINGLE *
            FROM j_1bnfdoc INTO @DATA(lwa_j_1bnfdoc)
           WHERE docnum   EQ @lwa_zmmt_ee_zgr_docs-docnum
             AND candat   EQ @lva_candat_null
             AND cancel   EQ @space
             AND doctyp   IN ('1','2','6').

          CHECK ( sy-subrc EQ 0 ) AND ( lwa_j_1bnfdoc-entrad EQ abap_true ). "Entrada Propria.

          e_docnum       = lwa_j_1bnfdoc-docnum.
          EXIT.
        ENDLOOP.
        IF e_docnum IS NOT INITIAL.
          SELECT SINGLE * INTO e_active
           FROM j_1bnfe_active
            WHERE docnum = e_docnum.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_DOCNUM_NUMERO.


    DATA: WA_LFA1             TYPE LFA1,
          WA_ZIB_CTE_DIST_EAP TYPE ZIB_CTE_DIST_EAP,
          RG_SERIES           TYPE ZDE_SERIES_RANGE_T,
          WA_SERIES           TYPE ZDE_SERIES_RANGE,
          OPCAO               TYPE CHAR01,
          LVA_SERIES          TYPE J_1BNFDOC-SERIES,
          VNR_NF              TYPE ZLEST0041-NR_NF,
          LVA_NFNUM_ROM       TYPE ZSDT0001-NFNUM,
          LVA_CANDAT_NULL     TYPE J_1BNFDOC-CANDAT.

    CLEAR: E_J_1BNFDOC.

    SELECT SINGLE * INTO WA_ZIB_CTE_DIST_EAP
      FROM ZIB_CTE_DIST_EAP
     WHERE CD_CHAVE_CTE  EQ P_CTE_DIST-CD_CHAVE_CTE
       AND TP_APROVACAO  EQ '06'
       AND TP_AUTORIZADO EQ '01'
       AND CK_ULTIMO     EQ ABAP_TRUE.

    IF SY-SUBRC IS NOT INITIAL.
      LVA_SERIES = P_NF01-N01_NUMR_SERIE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT    = LVA_SERIES
        IMPORTING
          OUTPUT   = LVA_SERIES.

      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LVA_SERIES ) TO RG_SERIES.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT    = LVA_SERIES
        IMPORTING
          OUTPUT   = LVA_SERIES.

      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LVA_SERIES ) TO RG_SERIES.
    ENDIF.

    IF P_J_1BBRANCH IS INITIAL.
      CALL METHOD ME->BUSCA_TOMADOR_SERVICO
        EXPORTING
          P_CTE_DIST         = P_CTE_DIST
        IMPORTING
          E_J_1BBRANCH       = P_J_1BBRANCH
        EXCEPTIONS
          NAO_ACHOU_PARCEIRO = 1
          OTHERS             = 2.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_ACHOU_PARCEIRO.
      ENDIF.
    ENDIF.

    OPCAO = P_CTE_DIST-CD_TOMADOR.

    "Se Tomador Receber e for igual ao destinatário, trocar tomador para Destinatário
    IF OPCAO EQ '2' AND P_CTE_DIST-DEST_CNPJ EQ P_CTE_DIST-RECEB_CNPJ.
      OPCAO = '3'.
    ENDIF.

    CASE OPCAO.
      WHEN '0'. "Remetente  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        MOVE P_NF01-N01_NR_NF TO E_J_1BNFDOC-NFNUM.

        IF P_NF01-N01_MODELO_NF EQ '04'.
          SELECT SINGLE * INTO E_J_1BNFDOC
            FROM J_1BNFDOC
           WHERE BUKRS   EQ P_J_1BBRANCH-BUKRS
             AND BRANCH  EQ P_J_1BBRANCH-BRANCH
             AND FORM    NE SPACE
             AND DOCTYP  EQ '1' "Nota Fiscal
             "AND MODEL   EQ P_NF01-N01_MODELO_NF
             AND SERIES  IN RG_SERIES
             AND NFNUM   EQ E_J_1BNFDOC-NFNUM
             AND CANCEL  NE 'X'.
        ELSE.
          SELECT SINGLE * INTO E_J_1BNFDOC
            FROM J_1BNFDOC
           WHERE BUKRS   EQ P_J_1BBRANCH-BUKRS
             AND BRANCH  EQ P_J_1BBRANCH-BRANCH
             AND FORM    NE SPACE
             AND DOCTYP  EQ '1' "Nota Fiscal
             AND MODEL   EQ P_NF01-N01_MODELO_NF
             AND SERIES  IN RG_SERIES
             AND NFNUM   EQ E_J_1BNFDOC-NFNUM
             AND CANCEL  NE 'X'.
        ENDIF.

        IF SY-SUBRC IS INITIAL.
          MOVE: P_NF01-N01_NR_NF     TO SY-MSGV1,
                P_NF01-N01_MODELO_NF TO SY-MSGV3,
                E_J_1BNFDOC-DOCNUM   TO E_DOCNUM.

          MESSAGE S004 WITH SY-MSGV1 P_NF01-N01_NUMR_SERIE SY-MSGV3 P_J_1BBRANCH-STCD1.
        ELSE.
          MESSAGE E010 WITH P_NF01-N01_NR_NF P_NF01-N01_NUMR_SERIE P_NF01-N01_MODELO_NF P_J_1BBRANCH-STCD1 RAISING NAO_ACHOU.
        ENDIF.

      WHEN '3'. "Destinatário ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        CALL METHOD ME->BUSCA_FORNECEDOR
          EXPORTING
            P_TP_DOC  = P_CTE_DIST-REME_TP_DOC
            P_CNPJ    = P_CTE_DIST-REME_CNPJ
            P_CPF     = P_CTE_DIST-REME_CPF
            P_IE      = P_CTE_DIST-REME_IE
          IMPORTING
            E_LFA1    = WA_LFA1
          EXCEPTIONS
            NAO_ACHOU = 1
            OTHERS    = 2.

        MOVE P_NF01-N01_NR_NF TO E_J_1BNFDOC-NFNUM.
        MOVE P_NF01-N01_NR_NF TO LVA_NFNUM_ROM.

        IF P_NF01-N01_MODELO_NF EQ '04'.
          SELECT SINGLE * INTO E_J_1BNFDOC
            FROM J_1BNFDOC
           WHERE PARID   EQ WA_LFA1-LIFNR
             AND FORM    EQ SPACE
             AND DOCTYP  EQ '1' "Nota Fiscal
             "AND MODEL   EQ P_NF01-N01_MODELO_NF
             AND SERIES  IN RG_SERIES
             AND NFNUM   EQ E_J_1BNFDOC-NFNUM
             AND CANCEL  NE 'X'.
        ELSE.
          SELECT SINGLE * INTO E_J_1BNFDOC
            FROM J_1BNFDOC
           WHERE PARID   EQ WA_LFA1-LIFNR
             AND FORM    EQ SPACE
             AND DOCTYP  EQ '1' "Nota Fiscal
             AND MODEL   EQ P_NF01-N01_MODELO_NF
             AND SERIES  IN RG_SERIES
             AND NFNUM   EQ E_J_1BNFDOC-NFNUM
             AND CANCEL  NE 'X'.
          IF SY-SUBRC NE 0 AND  P_CTE_DIST-REME_CPF IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = E_J_1BNFDOC-NFNUM
              IMPORTING
                OUTPUT = VNR_NF.
            SELECT SINGLE *
               FROM ZLEST0041 INTO @DATA(W41)
              WHERE NR_NF             EQ @VNR_NF
                AND COD_CLIENTE       EQ @WA_LFA1-LIFNR
                AND CENTRO_COMPRADOR  EQ @P_J_1BBRANCH-BRANCH.
            IF SY-SUBRC = 0.
              SELECT SINGLE * INTO E_J_1BNFDOC
                  FROM J_1BNFDOC
                WHERE DOCNUM = W41-DOCNUM.
            ENDIF.

          ENDIF.
        ENDIF.

        IF SY-SUBRC IS NOT INITIAL. "Caso não encontrou, buscar por fluxo de entrada propria

          DATA(LVA_ENT_PROP_FOUND) = ABAP_FALSE.

          "Localizar Romaneio de Entrada
          SELECT SINGLE * FROM ZSDT0001 INTO @DATA(LWA_ZSDT0001)
           WHERE TP_MOVIMENTO EQ 'E'
             AND SERIES       IN @RG_SERIES
             AND PARID        EQ @WA_LFA1-LIFNR
             AND BRANCH       EQ @P_J_1BBRANCH-BRANCH
             AND NFNUM        EQ @LVA_NFNUM_ROM.

          IF SY-SUBRC EQ 0.

            SELECT *
              FROM ZMMT_EE_ZGR INTO TABLE @DATA(LIT_ZMM_EE_ZGR)
             WHERE CH_REFERENCIA EQ @LWA_ZSDT0001-CH_REFERENCIA.

            IF LIT_ZMM_EE_ZGR[] IS NOT INITIAL.

              SELECT *
                FROM ZMMT_EE_ZGR_DOCS INTO TABLE @DATA(LIT_ZMMT_EE_ZGR_DOCS)
                 FOR ALL ENTRIES IN @LIT_ZMM_EE_ZGR
               WHERE OBJ_KEY EQ @LIT_ZMM_EE_ZGR-OBJ_KEY.

            ENDIF.

            LOOP AT LIT_ZMMT_EE_ZGR_DOCS INTO DATA(LWA_ZMMT_EE_ZGR_DOCS) WHERE DOCNUM IS NOT INITIAL.
              SELECT SINGLE *
                FROM J_1BNFDOC INTO @DATA(LWA_J_1BNFDOC)
               WHERE DOCNUM   EQ @LWA_ZMMT_EE_ZGR_DOCS-DOCNUM
                 AND CANDAT   EQ @LVA_CANDAT_NULL
                 AND CANCEL   EQ @SPACE
                 AND DOCTYP   IN ('1','2','6').

               CHECK ( SY-SUBRC EQ 0 ) AND ( LWA_J_1BNFDOC-ENTRAD EQ ABAP_TRUE ). "Entrada Propria.

               E_J_1BNFDOC        = LWA_J_1BNFDOC.
               LVA_ENT_PROP_FOUND = ABAP_TRUE.
               EXIT.
            ENDLOOP.

          ENDIF.

          IF LVA_ENT_PROP_FOUND = FALSE.
            SY-SUBRC = 4.
          ELSE.
            SY-SUBRC = 0.
          ENDIF.

        ENDIF.

        IF SY-SUBRC IS INITIAL.
          MOVE: P_NF01-N01_NR_NF     TO SY-MSGV1,
                P_NF01-N01_MODELO_NF TO SY-MSGV3,
                E_J_1BNFDOC-DOCNUM   TO E_DOCNUM.

          MESSAGE S004 WITH SY-MSGV1 P_NF01-N01_NUMR_SERIE SY-MSGV3 P_J_1BBRANCH-STCD1.
        ELSE.
          MESSAGE E010 WITH P_NF01-N01_NR_NF P_NF01-N01_NUMR_SERIE P_NF01-N01_MODELO_NF P_J_1BBRANCH-STCD1 RAISING NAO_ACHOU.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD BUSCA_ENTRADA_DO_FRETE.


    DATA: WA_ZLEST0032  TYPE ZLEST0032,
          WA_ZLEST0034  TYPE ZLEST0034,
          IT_ZLEST0032  TYPE TABLE OF ZLEST0032,
          IT_ZLEST0032A TYPE TABLE OF ZLEST0032,
          WA_N01_T      TYPE ZIB_CTE_DIST_N01,
          WA_N55_T      TYPE ZIB_CTE_DIST_N55,
          WA_C57_T      TYPE ZIB_CTE_DIST_C57,
          LC_LINHAS     TYPE I,
          LC_EKPO       TYPE EKPO,
          IT_NOTAS_P    TYPE TABLE OF ZCTE_INFO_NOTA,
          WA_NOTAS_P    TYPE ZCTE_INFO_NOTA,
          WA_VTTK       TYPE VTTK,
          WA_CTE_IVA    TYPE ZIB_CTE_DIST_IVA,
          WA_C57        TYPE ZIB_CTE_DIST_C57,
          WA_TER_CMPL   TYPE ZIB_CTE_DIST_TER,
          IT_EKKO       TYPE TABLE OF EKKO,
          WA_EKKO       TYPE EKKO.

    IF E_CTE_DISTR-DOCNUM_CTE IS INITIAL.

      CALL METHOD ME->BUSCA_DOCNUM_CHAVE
        EXPORTING
          P_CHAVE     = E_CTE_DISTR-CD_CHAVE_CTE
          P_PSQ_CHAVE = TRUE
          P_FORM      = FALSE
        CHANGING
          E_DOCNUM    = E_CTE_DISTR-DOCNUM_CTE
        EXCEPTIONS
          NAO_ACHOU   = 1
          OTHERS      = 2.

      IF SY-SUBRC IS INITIAL.
        E_CTE_DISTR-CK_FINALIZADO = TRUE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_ID           = SY-MSGID
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Verificar documento subcontratado emitido por terceiro
      "Pagamentos pelo documento emitido de forma própria
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""

    ELSE.
      E_CTE_DISTR-CK_FINALIZADO = TRUE.
    ENDIF.

    IF E_CTE_DISTR-CD_TIPO_CTE NE '1'. "CT-e de Complemento de Valores
      CASE E_CTE_DISTR-TP_PROCESSO_CTE.
        WHEN TIPO_01 OR TIPO_02 OR TIPO_03 OR TIPO_05 OR TIPO_08 OR TIPO_09 OR TIPO_06. "Frete terceiro sobre faturamento

          LOOP AT P_N01_T INTO WA_N01_T.
            WA_ZLEST0032-TKNUM = WA_N01_T-TKNUM.
            WA_ZLEST0032-FKNUM = WA_N01_T-FKNUM.
            WA_ZLEST0032-EBELN = WA_N01_T-EBELN.
            WA_ZLEST0032-EBELP = WA_N01_T-EBELP.
            WA_ZLEST0032-LBLNI = WA_N01_T-LBLNI.
            WA_ZLEST0032-BELNR = WA_N01_T-BELNR.
            WA_ZLEST0032-GJAHR = WA_N01_T-GJAHR.
            APPEND WA_ZLEST0032 TO IT_ZLEST0032.
          ENDLOOP.

          LOOP AT P_N55_T INTO WA_N55_T.
            WA_ZLEST0032-TKNUM = WA_N55_T-TKNUM.
            WA_ZLEST0032-FKNUM = WA_N55_T-FKNUM.
            WA_ZLEST0032-EBELN = WA_N55_T-EBELN.
            WA_ZLEST0032-EBELP = WA_N55_T-EBELP.
            WA_ZLEST0032-LBLNI = WA_N55_T-LBLNI.
            WA_ZLEST0032-BELNR = WA_N55_T-BELNR.
            WA_ZLEST0032-GJAHR = WA_N55_T-GJAHR.
            APPEND WA_ZLEST0032 TO IT_ZLEST0032.
          ENDLOOP.

          MOVE IT_ZLEST0032 TO IT_ZLEST0032A.
          SORT IT_ZLEST0032A BY EBELN EBELP.
          DELETE ADJACENT DUPLICATES FROM IT_ZLEST0032A COMPARING EBELN EBELP.
          DELETE IT_ZLEST0032A WHERE EBELN IS INITIAL.
          DESCRIBE TABLE IT_ZLEST0032A LINES LC_LINHAS.

          IF ( LC_LINHAS GT 1 ) AND ( E_CTE_DISTR-DOCNUM_CTE IS INITIAL ).
            SELECT * INTO TABLE IT_EKKO
              FROM EKKO AS K
               FOR ALL ENTRIES IN IT_ZLEST0032A
             WHERE K~EBELN EQ IT_ZLEST0032A-EBELN
               AND K~AEDAT EQ ( SELECT MAX( L~AEDAT ) FROM EKKO AS L WHERE L~EBELN EQ K~EBELN ).

            IF SY-SUBRC IS INITIAL.
              READ TABLE IT_EKKO INDEX 1 INTO WA_EKKO.
              DELETE IT_ZLEST0032A WHERE EBELN NE WA_EKKO-EBELN.
            ENDIF.
          ENDIF.

          READ TABLE IT_ZLEST0032A INTO WA_ZLEST0032 INDEX 1.

          IF WA_ZLEST0032-EBELN IS NOT INITIAL AND WA_ZLEST0032-EBELP IS NOT INITIAL.

            SY-MSGV1 = WA_ZLEST0032-EBELN.
            SY-MSGV2 = WA_ZLEST0032-EBELP.

            E_CTE_DISTR-EBELN = WA_ZLEST0032-EBELN.
            E_CTE_DISTR-EBELP = WA_ZLEST0032-EBELP.

            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 029
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.

          ELSEIF ( WA_ZLEST0032-EBELN IS INITIAL OR WA_ZLEST0032-EBELP IS INITIAL ) AND ( E_CTE_DISTR-DOCNUM_CTE IS INITIAL ).
            MESSAGE E032 RAISING ERRO_PEDIDO.
          ENDIF.

          MOVE IT_ZLEST0032 TO IT_ZLEST0032A.
          SORT IT_ZLEST0032A BY BELNR GJAHR.
          DELETE ADJACENT DUPLICATES FROM IT_ZLEST0032A COMPARING BELNR GJAHR.
          DELETE IT_ZLEST0032A WHERE BELNR IS INITIAL.
          DESCRIBE TABLE IT_ZLEST0032A LINES LC_LINHAS.
          READ TABLE IT_ZLEST0032A INTO WA_ZLEST0032 INDEX 1.

          IF ( LC_LINHAS GT 1 ) AND ( E_CTE_DISTR-DOCNUM_CTE IS INITIAL ).
            MESSAGE I033 RAISING ERRO_MIRO_FRETE.
          ELSEIF WA_ZLEST0032-BELNR IS NOT INITIAL AND WA_ZLEST0032-GJAHR IS NOT INITIAL.

            SY-MSGV1 = WA_ZLEST0032-BELNR.
            SY-MSGV2 = WA_ZLEST0032-GJAHR.

            E_CTE_DISTR-BELNR = WA_ZLEST0032-BELNR.
            E_CTE_DISTR-GJAHR = WA_ZLEST0032-GJAHR.

            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 030
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.

          ELSEIF ( WA_ZLEST0032-BELNR IS INITIAL OR WA_ZLEST0032-GJAHR IS INITIAL )  AND ( E_CTE_DISTR-DOCNUM_CTE IS NOT INITIAL ).
            MESSAGE E034 RAISING ERRO_MIRO_FRETE.
          ENDIF.

          SELECT SINGLE * INTO LC_EKPO
            FROM EKPO
           WHERE EBELN EQ E_CTE_DISTR-EBELN
             AND EBELP EQ E_CTE_DISTR-EBELP.

          IF LC_EKPO-MWSKZ IS NOT INITIAL.
            E_CTE_DISTR-MWSKZ = LC_EKPO-MWSKZ.
          ENDIF.

        WHEN TIPO_04. "Frete Próprio -  Subcontratado

          "O CT-e Normal emitido antes é de emissão da empresa transportadora.
          IF P_N55_T IS NOT INITIAL.

            SELECT * INTO TABLE IT_NOTAS_P
              FROM ZCTE_INFO_NOTA
               FOR ALL ENTRIES IN P_N55_T
             WHERE CHAVE EQ P_N55_T-N55_CHAVE_ACESSO.

            CLEAR: IT_ZLEST0032.

            LOOP AT IT_NOTAS_P INTO WA_NOTAS_P.
              WA_ZLEST0032-DOCNUM = WA_NOTAS_P-DOCNUM.
              APPEND WA_ZLEST0032 TO IT_ZLEST0032.
            ENDLOOP.

            SORT IT_ZLEST0032 BY DOCNUM.
            DELETE ADJACENT DUPLICATES FROM IT_ZLEST0032 COMPARING DOCNUM.
            DESCRIBE TABLE IT_ZLEST0032 LINES LC_LINHAS.
            READ TABLE IT_ZLEST0032 INTO WA_ZLEST0032 INDEX 1.

            IF LC_LINHAS GT 1.
              MESSAGE E055 RAISING NAO_ACHOU.
            ELSEIF WA_ZLEST0032-DOCNUM IS NOT INITIAL.

              E_CTE_DISTR-DOCNUM_CTE_SUB = WA_ZLEST0032-DOCNUM.

              CALL METHOD ME->ADD_LOG_CTE_DIST
                EXPORTING
                  P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                  P_TYPE         = 'S'
                  P_NUM          = 056
                  P_MESSAGE_V1   = SY-MSGV1
                  P_MESSAGE_V2   = SY-MSGV2
                CHANGING
                  P_LC_SEQUENCIA = LC_SEQUENCIA.

            ELSEIF WA_ZLEST0032-DOCNUM IS INITIAL.
              MESSAGE E055 RAISING NAO_ACHOU.
            ENDIF.

          ENDIF.

        WHEN TIPO_06.

        WHEN TIPO_07. "Frete Terceiro sobre Mov. Mercadoria
          "Buscar Informações do Processo
          CALL METHOD ME->ATRIBUI_DADOS_CTE_07
            CHANGING
              P_CTE = E_CTE_DISTR.

          IF E_CTE_DISTR-DOCNUM_CTE IS INITIAL.

            LOOP AT P_N01_T INTO WA_N01_T.
              WA_ZLEST0032-TKNUM = WA_N01_T-TKNUM.
              WA_ZLEST0032-FKNUM = WA_N01_T-FKNUM.
              WA_ZLEST0032-EBELN = WA_N01_T-EBELN.
              WA_ZLEST0032-EBELP = WA_N01_T-EBELP.
              WA_ZLEST0032-LBLNI = WA_N01_T-LBLNI.
              WA_ZLEST0032-BELNR = WA_N01_T-BELNR.
              WA_ZLEST0032-GJAHR = WA_N01_T-GJAHR.
              APPEND WA_ZLEST0032 TO IT_ZLEST0032.
            ENDLOOP.

            LOOP AT P_N55_T INTO WA_N55_T.
              WA_ZLEST0032-TKNUM = WA_N55_T-TKNUM.
              WA_ZLEST0032-FKNUM = WA_N55_T-FKNUM.
              WA_ZLEST0032-EBELN = WA_N55_T-EBELN.
              WA_ZLEST0032-EBELP = WA_N55_T-EBELP.
              WA_ZLEST0032-LBLNI = WA_N55_T-LBLNI.
              WA_ZLEST0032-BELNR = WA_N55_T-BELNR.
              WA_ZLEST0032-GJAHR = WA_N55_T-GJAHR.
              APPEND WA_ZLEST0032 TO IT_ZLEST0032.
            ENDLOOP.

            MOVE IT_ZLEST0032 TO IT_ZLEST0032A.
            SORT IT_ZLEST0032A BY EBELN EBELP.
            DELETE ADJACENT DUPLICATES FROM IT_ZLEST0032A COMPARING EBELN EBELP.
            DELETE IT_ZLEST0032A WHERE EBELN IS INITIAL.
            DESCRIBE TABLE IT_ZLEST0032A LINES LC_LINHAS.

            IF ( LC_LINHAS GT 1 ) AND ( E_CTE_DISTR-DOCNUM_CTE IS INITIAL ).
              SELECT * INTO TABLE IT_EKKO
                FROM EKKO AS K
                 FOR ALL ENTRIES IN IT_ZLEST0032A
               WHERE K~EBELN EQ IT_ZLEST0032A-EBELN
                 AND K~AEDAT EQ ( SELECT MAX( L~AEDAT ) FROM EKKO AS L WHERE L~EBELN EQ K~EBELN ).

              IF SY-SUBRC IS INITIAL.
                READ TABLE IT_EKKO INDEX 1 INTO WA_EKKO.
                DELETE IT_ZLEST0032A WHERE EBELN NE WA_EKKO-EBELN.
              ENDIF.
            ENDIF.

            READ TABLE IT_ZLEST0032A INTO WA_ZLEST0032 INDEX 1.

            IF WA_ZLEST0032-EBELN IS NOT INITIAL AND WA_ZLEST0032-EBELP IS NOT INITIAL.

              SY-MSGV1 = WA_ZLEST0032-EBELN.
              SY-MSGV2 = WA_ZLEST0032-EBELP.

              E_CTE_DISTR-EBELN = WA_ZLEST0032-EBELN.
              E_CTE_DISTR-EBELP = WA_ZLEST0032-EBELP.

              CALL METHOD ME->ADD_LOG_CTE_DIST
                EXPORTING
                  P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                  P_TYPE         = 'S'
                  P_NUM          = 029
                  P_MESSAGE_V1   = SY-MSGV1
                  P_MESSAGE_V2   = SY-MSGV2
                CHANGING
                  P_LC_SEQUENCIA = LC_SEQUENCIA.

              SELECT SINGLE * INTO LC_EKPO
                FROM EKPO
               WHERE EBELN EQ E_CTE_DISTR-EBELN
                 AND EBELP EQ E_CTE_DISTR-EBELP.

              IF LC_EKPO-MWSKZ IS NOT INITIAL.
                E_CTE_DISTR-MWSKZ = LC_EKPO-MWSKZ.
              ENDIF.

            ELSEIF ( WA_ZLEST0032-EBELN IS INITIAL OR WA_ZLEST0032-EBELP IS INITIAL ) AND ( E_CTE_DISTR-DOCNUM_CTE IS INITIAL ).
              MESSAGE E032 RAISING ERRO_PEDIDO.
            ENDIF.

          ENDIF.

      ENDCASE.
    ENDIF.

    CASE E_CTE_DISTR-TP_PROCESSO_CTE.
      WHEN TIPO_01 OR TIPO_05 OR TIPO_06 OR TIPO_07 OR TIPO_08 OR TIPO_09.

        IF ( E_CTE_DISTR-DOCNUM_CTE IS NOT INITIAL ) AND ( E_CTE_DISTR-CD_MODAL EQ '01' ). "Rodoviário

          CALL METHOD ME->BUSCA_PESO_RODO_TERCEIRO
            EXPORTING
              P_DOCNUM  = E_CTE_DISTR-DOCNUM_CTE
            CHANGING
              E_CTE     = E_CTE_DISTR
            EXCEPTIONS
              NAO_ACHOU = 1
              OTHERS    = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'E'
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 071
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.
        ENDIF.

      WHEN TIPO_02 OR TIPO_03 OR TIPO_04.

        IF ( E_CTE_DISTR-DOCNUM_CTE_SUB IS NOT INITIAL ) AND ( E_CTE_DISTR-CD_MODAL EQ '01' ). " Rodoviário

          CALL METHOD ME->BUSCA_PESO_TIP_FRETE
            EXPORTING
              P_DOCNUM  = E_CTE_DISTR-DOCNUM_CTE_SUB
            CHANGING
              E_CTE     = E_CTE_DISTR
            EXCEPTIONS
              NAO_ACHOU = 1
              OTHERS    = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'E'
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 064
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.

        ELSEIF ( E_CTE_DISTR-DOCNUM_CTE_SUB IS NOT INITIAL ) AND ( E_CTE_DISTR-CD_MODAL EQ '03' ). "Aquaviário

          CALL METHOD ME->BUSCA_PESO_AUAVIARIO
            EXPORTING
              P_DOCNUM  = E_CTE_DISTR-DOCNUM_CTE_SUB
            CHANGING
              E_CTE     = E_CTE_DISTR
            EXCEPTIONS
              NAO_ACHOU = 1
              OTHERS    = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'E'
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 064
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.
        ENDIF.

    ENDCASE.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Determinar IVA do Frete de Terceiro. """""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF E_CTE_DISTR-CD_TIPO_CTE EQ '1'. "CT-e de Complemento de Valores

      SELECT SINGLE * INTO WA_C57
        FROM ZIB_CTE_DIST_C57
       WHERE CD_CHAVE_CTE EQ E_CTE_DISTR-CD_CHAVE_CTE.

*  092  CT-e de Complemento e não encontrado CT-e Complementada!
*  093  CT-e Complementada não foi escriturada!

      IF ( SY-SUBRC IS NOT INITIAL ).
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
            P_TYPE         = 'E'
            P_NUM          = 092
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ELSE.

        IF ( WA_C57-DOCNUM_CTE IS INITIAL ).
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
              P_TYPE         = 'E'
              P_NUM          = 093
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ELSE.
          SELECT SINGLE * INTO WA_TER_CMPL
            FROM ZIB_CTE_DIST_TER
           WHERE CD_CHAVE_CTE EQ WA_C57-C57_CHAVE_ACESSO.

          IF WA_TER_CMPL-MWSKZ IS INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'E'
                P_NUM          = 094
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            E_CTE_DISTR-MWSKZ = WA_TER_CMPL-MWSKZ.
            SY-MSGV1          = E_CTE_DISTR-MWSKZ.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 095
                P_MESSAGE_V1   = SY-MSGV1
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      CLEAR: IT_ZLEST0032.
      LOOP AT P_N01_T INTO WA_N01_T.
        WA_ZLEST0032-TKNUM = WA_N01_T-TKNUM.
        APPEND WA_ZLEST0032 TO IT_ZLEST0032.
      ENDLOOP.

      LOOP AT P_N55_T INTO WA_N55_T.
        WA_ZLEST0032-TKNUM = WA_N55_T-TKNUM.
        APPEND WA_ZLEST0032 TO IT_ZLEST0032.
      ENDLOOP.

      SORT IT_ZLEST0032 BY TKNUM.
      DELETE ADJACENT DUPLICATES FROM IT_ZLEST0032 COMPARING TKNUM.
      DELETE IT_ZLEST0032 WHERE TKNUM IS INITIAL.
      READ TABLE IT_ZLEST0032 INTO WA_ZLEST0032 INDEX 1.

      IF SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO WA_ZLEST0034
          FROM ZLEST0034
         WHERE TKNUM EQ WA_ZLEST0032-TKNUM.

        IF SY-SUBRC IS INITIAL AND  WA_ZLEST0034-IVA IS NOT INITIAL.
          "Já existe um IVA Determinado
          WA_CTE_IVA-MWSKZ = WA_ZLEST0034-IVA.
        ELSE.

          SELECT SINGLE SHTYP
            INTO WA_CTE_IVA-SHTYP
            FROM VTTK
           WHERE TKNUM EQ WA_ZLEST0032-TKNUM.

          SELECT SINGLE INDUSTRY
            INTO WA_CTE_IVA-INDUSTRY
            FROM J_1BBRANCH
           WHERE BUKRS  EQ E_CTE_DISTR-E_TOMADORA
             AND BRANCH EQ E_CTE_DISTR-F_TOMADORA.

          WA_CTE_IVA-TDLNR = E_CTE_DISTR-P_EMISSOR.

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

          CASE E_CTE_DISTR-CST_ICMS.
            WHEN '00' OR '10' OR '20' OR '30' OR '70'.
              WA_CTE_IVA-ICMS = TRUE.
            WHEN '40' OR '41' OR '50' OR '51' OR '60' OR '90'.
              WA_CTE_IVA-ICMS = FALSE.
          ENDCASE.

          "Bucar por Fornecedor (Excessão)
          SELECT SINGLE * INTO WA_CTE_IVA
            FROM ZIB_CTE_DIST_IVA
           WHERE BUKRS    EQ E_CTE_DISTR-E_TOMADORA
             AND SHTYP    EQ WA_CTE_IVA-SHTYP
             AND TDLNR    EQ WA_CTE_IVA-TDLNR
             AND INDUSTRY EQ WA_CTE_IVA-INDUSTRY
             AND ICMS     EQ WA_CTE_IVA-ICMS.

          "Bucar sem Fornecedor
          IF SY-SUBRC IS NOT INITIAL.
            SELECT SINGLE * INTO WA_CTE_IVA
              FROM ZIB_CTE_DIST_IVA
             WHERE BUKRS    EQ E_CTE_DISTR-E_TOMADORA
               AND SHTYP    EQ WA_CTE_IVA-SHTYP
               AND INDUSTRY EQ WA_CTE_IVA-INDUSTRY
               AND ICMS     EQ WA_CTE_IVA-ICMS.
          ENDIF.
        ENDIF.
      ENDIF.

      IF WA_CTE_IVA-MWSKZ IS INITIAL.

        SY-MSGNO = 086.
        SY-MSGV1 = WA_CTE_IVA-SHTYP.
        SY-MSGV2 = WA_CTE_IVA-TDLNR.
        SY-MSGV3 = WA_CTE_IVA-INDUSTRY.
        SY-MSGV4 = E_CTE_DISTR-CST_ICMS.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
            P_TYPE         = 'E'
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

      ELSE.
        E_CTE_DISTR-MWSKZ = WA_CTE_IVA-MWSKZ.

        SY-MSGNO = 085.
        SY-MSGV1 = WA_CTE_IVA-MWSKZ.
        CONCATENATE WA_CTE_IVA-SHTYP ';' WA_CTE_IVA-TDLNR ';' WA_CTE_IVA-INDUSTRY ';' E_CTE_DISTR-CST_ICMS INTO SY-MSGV2.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

      ENDIF.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.


  METHOD BUSCA_FORNECEDOR.


    DATA: IT_LFA1_AUX TYPE TABLE OF LFA1,
          IT_LFA1     TYPE TABLE OF LFA1,
          WA_LFA1     TYPE LFA1.

    DATA: LC_IE	TYPE J_1BSTAINS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = P_IE
      IMPORTING
        OUTPUT = LC_IE.

    CONCATENATE '%' LC_IE INTO LC_IE.

    CASE P_TP_DOC.
      WHEN 1.

        SELECT * INTO TABLE IT_LFA1_AUX FROM LFA1 WHERE STCD1 EQ P_CNPJ AND STCD3 LIKE LC_IE.
        IF SY-SUBRC IS NOT INITIAL.
          SELECT * INTO TABLE IT_LFA1_AUX FROM LFA1 WHERE STCD1 EQ P_CNPJ.
        ENDIF.

        LOOP AT IT_LFA1_AUX INTO WA_LFA1.

          CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
            EXPORTING
              P_KOART      = 'K'
              P_FORNECEDOR = WA_LFA1-LIFNR
            EXCEPTIONS
              ERROR        = 1
              BRANCH       = 2
              OTHERS       = 3.

          IF SY-SUBRC IS INITIAL.
            APPEND WA_LFA1 TO IT_LFA1.
          ENDIF.

        ENDLOOP.

        READ TABLE IT_LFA1 INTO E_LFA1 INDEX 1.

        IF SY-SUBRC IS INITIAL.
          MESSAGE S014 WITH P_CNPJ P_IE.
        ELSE.
          MESSAGE E013 WITH P_CNPJ P_IE RAISING NAO_ACHOU.
        ENDIF.

      WHEN 2.


        SELECT * INTO TABLE IT_LFA1_AUX FROM LFA1 WHERE STCD2 EQ P_CPF  AND STCD3 LIKE LC_IE.
        IF SY-SUBRC IS NOT INITIAL.
          SELECT * INTO TABLE IT_LFA1_AUX FROM LFA1 WHERE STCD2 EQ P_CPF.
        ENDIF.

        LOOP AT IT_LFA1_AUX INTO WA_LFA1.

          CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
            EXPORTING
              P_KOART      = 'K'
              P_FORNECEDOR = WA_LFA1-LIFNR
            EXCEPTIONS
              ERROR        = 1
              BRANCH       = 2
              OTHERS       = 3.

          IF SY-SUBRC IS INITIAL.
            APPEND WA_LFA1 TO IT_LFA1.
          ENDIF.

        ENDLOOP.

        READ TABLE IT_LFA1 INTO E_LFA1 INDEX 1.

        IF SY-SUBRC IS INITIAL.
          MESSAGE S014 WITH P_CPF P_IE.
        ELSE.
          MESSAGE E013 WITH P_CPF P_IE RAISING NAO_ACHOU.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD BUSCA_FORN_EMPRESA.


    DATA: WA_J_1BBRANCH TYPE J_1BBRANCH,
          IT_LFA1_AUX   TYPE TABLE OF LFA1,
          IT_LFA1       TYPE TABLE OF LFA1,
          WA_LFA1       TYPE LFA1,
          LC_QTD        TYPE I,
          LC_TXJCD      TYPE LFA1-TXJCD,
          LC_LIFNR      TYPE LFA1-LIFNR,
          LC_STCD3      TYPE STCD3.

    MOVE: P_CTE_DIST-EMIT_CNPJ TO WA_J_1BBRANCH-STCD1,
          P_CTE_DIST-EMIT_IE   TO WA_J_1BBRANCH-STATE_INSC.

    SELECT SINGLE * INTO WA_J_1BBRANCH FROM J_1BBRANCH WHERE STCD1 EQ WA_J_1BBRANCH-STCD1 AND STATE_INSC EQ WA_J_1BBRANCH-STATE_INSC.
    IF SY-SUBRC IS INITIAL.
      P_CTE_DIST-E_EMISSOR = WA_J_1BBRANCH-BUKRS.
      P_CTE_DIST-F_EMISSOR = WA_J_1BBRANCH-BRANCH.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_J_1BBRANCH-STATE_INSC
      IMPORTING
        OUTPUT = WA_J_1BBRANCH-STATE_INSC.

    LC_STCD3 = '%' && WA_J_1BBRANCH-STATE_INSC.

    SELECT * INTO TABLE IT_LFA1_AUX
      FROM LFA1
     WHERE STCD1 EQ WA_J_1BBRANCH-STCD1
       AND STCD3 LIKE LC_STCD3
       AND LOEVM NE TRUE and SPERR NE TRUE and SPERM NE TRUE and NODEL ne true. "BG #133852

    LOOP AT IT_LFA1_AUX INTO WA_LFA1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_LFA1-STCD3
        IMPORTING
          OUTPUT = LC_STCD3.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_J_1BBRANCH-STATE_INSC
        IMPORTING
          OUTPUT = WA_J_1BBRANCH-STATE_INSC.

      IF LC_STCD3 NE WA_J_1BBRANCH-STATE_INSC.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
        EXPORTING
          P_KOART      = 'K'
          P_EMPRESA    = P_CTE_DIST-E_TOMADORA
          P_FORNECEDOR = WA_LFA1-LIFNR
        EXCEPTIONS
          ERROR        = 1
          OTHERS       = 2.

      IF SY-SUBRC IS INITIAL.
        APPEND WA_LFA1 TO IT_LFA1.
      ENDIF.

    ENDLOOP.

    DESCRIBE TABLE IT_LFA1 LINES LC_QTD.

    IF LC_QTD GT 1.
      SY-SUBRC = 1.
      CONCATENATE P_CTE_DIST-INICIO_UF P_CTE_DIST-INICIO_IBGE INTO LC_TXJCD SEPARATED BY SPACE.
      LOOP AT IT_LFA1 INTO WA_LFA1.
        IF WA_LFA1-TXJCD EQ LC_TXJCD.
          SY-SUBRC = 0.
          LC_LIFNR = WA_LFA1-LIFNR.
        ENDIF.
      ENDLOOP.
      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = LC_LIFNR.
      ENDIF.
    ELSE.
      READ TABLE IT_LFA1 INDEX 1 INTO WA_LFA1.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      P_CTE_DIST-P_EMISSOR = WA_LFA1-LIFNR.
      MESSAGE S018 WITH WA_J_1BBRANCH-STCD1 WA_J_1BBRANCH-STATE_INSC.
    ELSE.
      MESSAGE E017 WITH WA_J_1BBRANCH-STCD1 WA_J_1BBRANCH-STATE_INSC RAISING NAO_ACHOU_FORNECEDOR.
    ENDIF.

  ENDMETHOD.


  METHOD busca_grp_mercadoria.

    DATA: vl_docnum_nfe  TYPE zib_cte_dist_n55-docnum_nfe.

*-CS2023000491-07.11.2024-JT-#127456-inicio
    SELECT SINGLE cd_tipo_cte
      INTO @DATA(_cd_tipo_cte)
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte = @p_chave_cte.
*-CS2023000491-07.11.2024-JT-#127456-fim

*-CS2023000491-07.11.2024-JT-#127456-inicio
    IF _cd_tipo_cte = '1'. "CT-e de Complemento de Valores
      SELECT SINGLE c57_chave_acesso
        INTO @DATA(_c57_chave_acesso)
        FROM zib_cte_dist_c57
       WHERE cd_chave_cte = @p_chave_cte.

      IF sy-subrc = 0.
        SELECT SINGLE docnum_nfe
          INTO @DATA(_docnum_nfe)
          FROM zib_cte_dist_n55
         WHERE cd_chave_cte = @_c57_chave_acesso.

        IF sy-subrc = 0.
          vl_docnum_nfe = _docnum_nfe.
        ELSE.
          SELECT SINGLE docnum_nf
            INTO @DATA(_docnum_nf)
            FROM zib_cte_dist_n01
           WHERE cd_chave_cte = @_c57_chave_acesso.

          IF sy-subrc = 0.
            vl_docnum_nfe = _docnum_nf.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.    "CT-e Normal de Valores
*-CS2023000491-07.11.2024-JT-#127456-fim

      SELECT SINGLE docnum_nfe
        INTO vl_docnum_nfe
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ p_chave_cte.

**** IR071493 - BG -  INICIO
      IF sy-subrc NE 0.
        SELECT SINGLE docnum_nf
        INTO vl_docnum_nfe
        FROM zib_cte_dist_n01
        WHERE cd_chave_cte EQ p_chave_cte.
      ENDIF.
**** IR071493 - BG -  FIM

    ENDIF.  "*-CS2023000491-07.11.2024-JT-#127456

    IF sy-subrc EQ 0 AND NOT vl_docnum_nfe IS INITIAL.
      SELECT SINGLE matkl
        INTO @DATA(vl_matkl)
        FROM j_1bnflin
        WHERE docnum EQ @vl_docnum_nfe.

      IF sy-subrc EQ 0 AND NOT vl_matkl IS INITIAL.
        SELECT SINGLE matkl
          FROM zlest0037
          INTO @DATA(vl_matkl_aux)
          WHERE matkl EQ @vl_matkl.

        IF sy-subrc EQ 0 AND NOT vl_matkl_aux IS INITIAL.
          p_matkl = vl_matkl_aux.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_IMPOSTOS_TAXAS.


    DATA: WA_IVA           TYPE T007A,
          WA_J_1BATL1      TYPE J_1BATL1,
          WA_J_1BATL4A     TYPE J_1BATL4A,
          WA_J_1BATL5      TYPE J_1BATL5,
          IT_J_1BTXIC1     TYPE TABLE OF J_1BTXIC1,
          WA_J_1BTXIC1     TYPE J_1BTXIC1,
          LV_VENCIMENTO    TYPE C LENGTH 10,
          LC_DT_VENCIMENTO TYPE SY-DATUM,
          IT_J_1BTXPIS     TYPE TABLE OF J_1BTXPIS,
          IT_J_1BTXCOF     TYPE TABLE OF J_1BTXCOF,
          WA_J_1BTXPIS     TYPE J_1BTXPIS,
          WA_J_1BTXCOF     TYPE J_1BTXCOF,
          LC_TAX_ICMS      TYPE C LENGTH 2,
          LC_DATA_INV      TYPE J_1BTXDATF.

    IF P_IVA IS INITIAL.
      MESSAGE E069 RAISING SEM_IVA.
    ENDIF.

    SELECT SINGLE *
      INTO WA_IVA
      FROM T007A
     WHERE KALSM EQ 'TAXBRA'
       AND MWSKZ EQ P_IVA.

    "J_1BTAXLW1 J_1BTAXLW1  CHAR  3 Direito fiscal: ICMS
    "J_1BTAXLW2 J_1BTAXLW2  CHAR  3 Direito fiscal: IPI
    "J_1BTAXLW4  J_1BTAXLW4  CHAR  3 Lei tributária COFINS
    "J_1BTAXLW5  J_1BTAXLW5  CHAR  3 Lei tributária PIS

    "ICMS -----------------------------------------------------------------------------------------------------
    E_RATE_ICMS = 0.

    IF WA_IVA-J_1BTAXLW1 IS NOT INITIAL.

      SELECT SINGLE * INTO WA_J_1BATL1 FROM J_1BATL1 WHERE TAXLAW EQ WA_IVA-J_1BTAXLW1.

      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          INPUT  = WA_J_1BATL1-TAXSIT
        IMPORTING
          OUTPUT = LC_TAX_ICMS.

      CASE LC_TAX_ICMS.
        WHEN '40' OR '41' OR '51' OR '50'.
          "40 - Isento
          "41 - Não tributada
          "51 - Diferimento
          "50 - Suspensão do ICMS
          E_RATE_ICMS = 0.
        WHEN '00'.
          "00 - Tributada integralmente
          E_RATE_ICMS = 0.

          "Data Invertida
          LC_DATA_INV = P_DATA_DOCUMENTO.
          LC_DATA_INV = 99999999 - LC_DATA_INV.

          "Pesquisa de Exceção de ICMS de Fornecedor/Material
          SELECT SINGLE * INTO @DATA(WA_J_1BTXIC3)
            FROM J_1BTXIC3
           WHERE LAND1     EQ 'BR'
             AND GRUOP     EQ '30'
             AND SHIPFROM  EQ @P_SHIPFROM
             AND SHIPTO    EQ @P_SHIPTO
             AND VALIDTO   LE @LC_DATA_INV
             AND VALIDFROM GE @LC_DATA_INV
             AND VALUE     EQ @P_EMISSORA
             AND VALUE2    EQ @P_MATNR.

          IF SY-SUBRC IS INITIAL.
            E_RATE_ICMS = WA_J_1BTXIC3-RATE.
          ELSE.
            SELECT * INTO TABLE IT_J_1BTXIC1
              FROM J_1BTXIC1
             WHERE LAND1    EQ 'BR'
               AND SHIPFROM EQ P_SHIPFROM
               AND SHIPTO   EQ P_SHIPTO.

            IF SY-SUBRC IS INITIAL.
              LOOP AT IT_J_1BTXIC1 INTO WA_J_1BTXIC1.
                CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
                  EXPORTING
                    INPUT  = WA_J_1BTXIC1-VALIDFROM
                  IMPORTING
                    OUTPUT = LV_VENCIMENTO.

                WRITE LV_VENCIMENTO TO LC_DT_VENCIMENTO.

                IF ( LC_DT_VENCIMENTO LE P_DATA_DOCUMENTO ) AND ( E_RATE_ICMS EQ 0 ).
                  E_RATE_ICMS = WA_J_1BTXIC1-RATE.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

        WHEN  '10' OR '20' OR '30' OR '60' OR '70' OR '90'.
          "10 - Tributada e com cobrança do ICMS por substituição tributária
          "20 - Com redução de base de cálculo
          "30 - Isenta ou não tributada e com cobrança do ICMS por substitui
          "60 - ICMS cobrado anteriormente por substituição tributária
          "70 - Com redução de base de cálculo e cobrança do ICMS por Sub.T.
          "90 - Outros/as
          E_RATE_ICMS = 0.
      ENDCASE.

    ENDIF.

    "COFINS ----------------------------------------------------------------------------------------------------
    E_RATE_COFINS = 0.

    IF WA_IVA-J_1BTAXLW4 IS NOT INITIAL.

      SELECT SINGLE * INTO WA_J_1BATL4A
        FROM J_1BATL4A
       WHERE TAXLAW EQ WA_IVA-J_1BTAXLW4.

      CASE WA_J_1BATL4A-TAXSIT.
        WHEN '70'. "Operação de Aquisição sem Direito a Crédito
        WHEN OTHERS.
          "COFINS
          SELECT * INTO TABLE IT_J_1BTXCOF
            FROM J_1BTXCOF
           WHERE COUNTRY   EQ 'BR'
             AND GRUOP     EQ '71'
             AND VALUE     EQ F_TOMADORA
             AND VALIDFROM GE P_DATA_DOCUMENTO
             AND VALIDTO   LE P_DATA_DOCUMENTO.

          IF SY-SUBRC IS INITIAL.
            READ TABLE IT_J_1BTXCOF INTO WA_J_1BTXCOF INDEX 1.
            E_RATE_COFINS = WA_J_1BTXCOF-RATE.
          ENDIF.
      ENDCASE.

    ENDIF.

    "PIS -------------------------------------------------------------------------------------------------------
    E_RATE_PIS = 0.

    IF WA_IVA-J_1BTAXLW5 IS NOT INITIAL.

      SELECT SINGLE * INTO WA_J_1BATL5
        FROM J_1BATL5
       WHERE TAXLAW EQ WA_IVA-J_1BTAXLW5.

      CASE WA_J_1BATL5-TAXSIT.
        WHEN '70'. "Operação de Aquisição sem Direito a Crédito
        WHEN OTHERS.
          SELECT * INTO TABLE IT_J_1BTXPIS
            FROM J_1BTXPIS
           WHERE COUNTRY   EQ 'BR'
             AND GRUOP     EQ '72'
             AND VALUE     EQ F_TOMADORA
             AND VALIDFROM GE P_DATA_DOCUMENTO
             AND VALIDTO   LE P_DATA_DOCUMENTO.

          IF SY-SUBRC IS INITIAL.
            READ TABLE IT_J_1BTXPIS INTO WA_J_1BTXPIS INDEX 1.
            E_RATE_PIS = WA_J_1BTXPIS-RATE.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_INFO_ALGODAO.

    DATA: WA_GMI_ALV TYPE ZIB_CTE_DIST_GMI_ALV,
          WA_ACTIVE  TYPE J_1BNFE_ACTIVE,
          IT_LC_GMI  TYPE TABLE OF ZIB_CTE_DIST_GMI.

    LOOP AT IT_NIT INTO DATA(WA_CTE_NIT) WHERE CD_CHAVE_CTE EQ P_CTE_DIST-CD_CHAVE_CTE.
      IF WA_CTE_NIT-DOCNUM IS NOT INITIAL.

        SELECT SINGLE MATNR INTO @DATA(LC_MATNR)
          FROM J_1BNFLIN
         WHERE DOCNUM EQ @WA_CTE_NIT-DOCNUM
           AND ITMNUM EQ @WA_CTE_NIT-ITMNUM.

        SELECT SINGLE MATKL INTO @DATA(LC_GRUPO)
          FROM MARA
         WHERE MATNR EQ @LC_MATNR.

        SELECT SINGLE TP_AUT_FRETE INTO @DATA(LC_TIPO)
          FROM ZIB_CTE_DIST_GM
         WHERE MATKL EQ @LC_GRUPO.
      ENDIF.
    ENDLOOP.

    CHECK LC_TIPO EQ '01'.

    DATA(LC_SALVAR) = ABAP_TRUE.

    SELECT * INTO TABLE @DATA(IT_CTE_GMI)
      FROM ZIB_CTE_DIST_GMI
     WHERE CD_CHAVE_CTE EQ @P_CTE_DIST-CD_CHAVE_CTE.

    SORT IT_CTE_GMI BY DOCNUM_NFE ITMNUM_NFE.

    LOOP AT IT_NIT INTO WA_CTE_NIT WHERE CD_CHAVE_CTE EQ P_CTE_DIST-CD_CHAVE_CTE.

      CLEAR: WA_GMI_ALV.

      READ TABLE ME->IT_N55 INTO DATA(WA_CTE_N55) WITH
       KEY DOCNUM_NFE = WA_CTE_NIT-DOCNUM
           CD_CHAVE_CTE = P_CTE_DIST-CD_CHAVE_CTE.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(LC_J_1BNFDOC)
        FROM J_1BNFDOC
       WHERE DOCNUM EQ @WA_CTE_NIT-DOCNUM.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(LC_J_1BNFLIN)
        FROM J_1BNFLIN
       WHERE DOCNUM EQ @WA_CTE_NIT-DOCNUM
         AND ITMNUM EQ @WA_CTE_NIT-ITMNUM.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      WA_GMI_ALV-BUKRS    = LC_J_1BNFDOC-BUKRS.
      WA_GMI_ALV-BRANCH   = LC_J_1BNFDOC-BRANCH.
      WA_GMI_ALV-NFENUM   = LC_J_1BNFDOC-NFENUM.
      WA_GMI_ALV-SAFRA    = LC_J_1BNFLIN-CHARG.
      WA_GMI_ALV-MAKTX    = LC_J_1BNFLIN-MAKTX.
      WA_GMI_ALV-VBELN_VF = WA_CTE_N55-VBELN_VF.

      IF WA_GMI_ALV-TERMINAL_ENTREGA IS INITIAL.
        "Busca Terminal
        SELECT SINGLE * INTO @DATA(LC_J_1BNFNAD)
          FROM J_1BNFNAD
         WHERE DOCNUM EQ @WA_CTE_NIT-DOCNUM
           AND PARVW  EQ 'Z1'.

        IF SY-SUBRC IS INITIAL.
          WA_GMI_ALV-TERMINAL_ENTREGA = LC_J_1BNFNAD-PARID.
        ENDIF.
      ENDIF.

      IF WA_GMI_ALV-TERMINAL_ENTREGA IS NOT INITIAL.
        SELECT SINGLE * INTO @DATA(LC_LFA1)
          FROM LFA1
         WHERE LIFNR EQ @WA_GMI_ALV-TERMINAL_ENTREGA.

        IF SY-SUBRC IS INITIAL.
          WA_GMI_ALV-NAME1 = LC_LFA1-NAME1.
          WA_GMI_ALV-ORT01 = LC_LFA1-ORT01.
          WA_GMI_ALV-REGIO = LC_LFA1-REGIO.
        ENDIF.
      ENDIF.

      "Buscar Remessa da Saída
      IF LC_J_1BNFDOC-FORM IS INITIAL.
        WA_ACTIVE-REGIO     = WA_CTE_N55-N55_CHAVE_ACESSO(2).
        WA_ACTIVE-NFYEAR    = WA_CTE_N55-N55_CHAVE_ACESSO+2(2).
        WA_ACTIVE-NFMONTH   = WA_CTE_N55-N55_CHAVE_ACESSO+4(2).
        WA_ACTIVE-STCD1     = WA_CTE_N55-N55_CHAVE_ACESSO+6(14).
        WA_ACTIVE-MODEL     = WA_CTE_N55-N55_CHAVE_ACESSO+20(2).
        WA_ACTIVE-SERIE     = WA_CTE_N55-N55_CHAVE_ACESSO+22(3).
        WA_ACTIVE-NFNUM9    = WA_CTE_N55-N55_CHAVE_ACESSO+25(9).
        WA_ACTIVE-DOCNUM9   = WA_CTE_N55-N55_CHAVE_ACESSO+34(9).
        WA_ACTIVE-CDV       = WA_CTE_N55-N55_CHAVE_ACESSO+43(1).

        SELECT SINGLE * INTO WA_ACTIVE
          FROM J_1BNFE_ACTIVE AS A
         WHERE REGIO    EQ WA_ACTIVE-REGIO
           AND NFYEAR   EQ WA_ACTIVE-NFYEAR
           AND NFMONTH  EQ WA_ACTIVE-NFMONTH
           AND STCD1    EQ WA_ACTIVE-STCD1
           AND MODEL    EQ WA_ACTIVE-MODEL
           AND SERIE    EQ WA_ACTIVE-SERIE
           AND NFNUM9   EQ WA_ACTIVE-NFNUM9
           AND DOCNUM9  EQ WA_ACTIVE-DOCNUM9
           AND CDV      EQ WA_ACTIVE-CDV
           AND FORM     NE SPACE
           AND NOT EXISTS ( SELECT * FROM J_1BNFDOC AS D WHERE D~DOCNUM EQ A~DOCNUM AND D~CANCEL EQ 'X' ).

        IF SY-SUBRC IS NOT INITIAL.
          WA_ACTIVE-DOCNUM = LC_J_1BNFDOC-DOCNUM.
        ENDIF.
      ELSE.
        WA_ACTIVE-DOCNUM = LC_J_1BNFDOC-DOCNUM.
      ENDIF.

      WA_GMI_ALV-DOCNUM_SAIDA = WA_ACTIVE-DOCNUM.

      READ TABLE IT_CTE_GMI
      INTO DATA(WA_CTE_GMI)
      WITH KEY DOCNUM_NFE = WA_CTE_NIT-DOCNUM
               ITMNUM_NFE = WA_CTE_NIT-ITMNUM BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING WA_CTE_GMI TO WA_GMI_ALV.
      ELSE.

        WA_GMI_ALV-CD_CHAVE_CTE	    = P_CTE_DIST-CD_CHAVE_CTE.
        WA_GMI_ALV-DOCNUM_NFE	      = WA_CTE_NIT-DOCNUM.
        WA_GMI_ALV-ITMNUM_NFE	      = WA_CTE_NIT-ITMNUM.
        WA_GMI_ALV-QT_CARGA_ORG     = P_CTE_DIST-QT_CARGA_CTE.
        WA_GMI_ALV-QT_CARGA_CHEGADA = P_CTE_DIST-QT_CARGA_CTE.
        WA_GMI_ALV-DT_SAIDA         = P_CTE_DIST-DT_EMISSAO.
        WA_GMI_ALV-ZVLR_FRETE       = WA_CTE_NIT-ZVLR_FRETE.

        IF WA_CTE_N55-VBELN_VL IS NOT INITIAL.

          WA_GMI_ALV-VBELN_VL  = WA_CTE_N55-VBELN_VL.

          SELECT SINGLE * INTO @DATA(LC_VBFA)
            FROM VBFA
           WHERE VBELN   EQ @WA_GMI_ALV-VBELN_VL
             AND VBTYP_N EQ 'J'
             AND VBTYP_V EQ 'C'.

          IF SY-SUBRC IS INITIAL.

            SELECT SINGLE * INTO @DATA(LC_LIPS)
              FROM LIPS
             WHERE VBELN EQ @LC_VBFA-VBELN
               AND POSNR EQ @LC_VBFA-POSNN.

            IF SY-SUBRC IS INITIAL.
              WA_GMI_ALV-QTD_FARDOS = LC_LIPS-VOLUM.
              WA_GMI_ALV-UND_VOLEH  = LC_LIPS-VOLEH.
            ENDIF.

            WA_GMI_ALV-VBELN_VA = LC_VBFA-VBELV.

            SELECT SINGLE * INTO @DATA(LC_ZSDT0066)
              FROM ZSDT0066
             WHERE VBELN EQ @LC_VBFA-VBELV.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE * INTO @DATA(LC_ZSDT0053)
                FROM ZSDT0053
               WHERE VBELN EQ @LC_VBFA-VBELV.
              IF SY-SUBRC IS INITIAL.
                WA_GMI_ALV-INSTRUCAO     = LC_ZSDT0053-INSTRUCAO.
                WA_GMI_ALV-NRO_SOL_OV    = LC_ZSDT0053-NRO_SOL_OV.
                WA_GMI_ALV-NRO_SOL_POSNR = LC_ZSDT0053-POSNR.
              ENDIF.
            ELSE.
              WA_GMI_ALV-INSTRUCAO     = LC_ZSDT0066-INSTRUCAO.
              WA_GMI_ALV-NRO_SOL_OV    = LC_ZSDT0066-NRO_SOL_OV.
              WA_GMI_ALV-NRO_SOL_POSNR = LC_ZSDT0066-POSNR.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF P_CTE_DIST-CK_FINALIZADO EQ ABAP_FALSE.
        IF WA_GMI_ALV-CK_AUTORIZADO EQ ABAP_FALSE.
          WA_GMI_ALV-IC_EDITAR = ICON_CHANGE_NUMBER.
        ELSE.
          WA_GMI_ALV-IC_EDITAR = ICON_SET_STATE.
        ENDIF.
      ELSE.
        WA_GMI_ALV-IC_EDITAR = ICON_COMPLETE.
      ENDIF.

      APPEND WA_GMI_ALV TO R_GMI_ALV.
    ENDLOOP.

    "Busca Informações do Romaneio de Saída
    LOOP AT R_GMI_ALV ASSIGNING FIELD-SYMBOL(<FS_GMI>) .

      IF WA_CTE_GMI-CK_AUTORIZADO EQ ABAP_TRUE AND WA_CTE_GMI-DS_NAME_USUARIO IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
        FROM ZSDT0001
       WHERE DOC_REM      EQ @<FS_GMI>-VBELN_VL
         AND TP_MOVIMENTO EQ 'S'.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0001OVRO)
        FROM ZSDT0001OVRO
       WHERE CH_REFERENCIA_SAI EQ @WA_ZSDT0001-CH_REFERENCIA.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      SELECT * INTO TABLE @DATA(IT_ZLEST0173)
        FROM ZLEST0173
       WHERE DOCNUM EQ @<FS_GMI>-DOCNUM_NFE.

      IF SY-SUBRC IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      <FS_GMI>-QTD_FARDOS = 0.
      LOOP AT IT_ZLEST0173 INTO DATA(WA_ZLEST0173).
        ADD WA_ZLEST0173-QTD_FARDOS TO <FS_GMI>-QTD_FARDOS.
      ENDLOOP.

      "SORT IT_ZLEST0173 BY DT_CHEGADA DESCENDING HR_REGISTRO DESCENDING.

      "READ TABLE IT_ZLEST0173 INDEX 1 INTO DATA(WA_ZLEST0173).

      IF WA_ZSDT0001OVRO-NM_QTD_EMBALAGENS NE <FS_GMI>-QTD_FARDOS AND WA_ZSDT0001OVRO-NM_QTD_EMBALAGENS IS NOT INITIAL.
        LC_SALVAR = ABAP_FALSE.
        CONTINUE.
      ENDIF.

      <FS_GMI>-DT_CHEGADA       = WA_ZLEST0173-DT_CHEGADA.
      <FS_GMI>-INSTRUCAO        = WA_ZLEST0173-INSTRUCAO.

      "Da maggi pega da nota fiscal pois não está correto no OPUS.
      SELECT SINGLE * INTO @DATA(WA_J1BNFDOC)
        FROM J_1BNFDOC
       WHERE DOCNUM EQ @<FS_GMI>-DOCNUM_NFE
         AND DOCNUM NE @SPACE.

      "NM_QTD_EMBALAGENS = vazio é da maggi
      IF SY-SUBRC IS INITIAL AND WA_ZSDT0001OVRO-NM_QTD_EMBALAGENS IS INITIAL.
        <FS_GMI>-QT_CARGA_ORG     = WA_J1BNFDOC-BRGEW. "Peso Bruto
        <FS_GMI>-QT_CARGA_CHEGADA = WA_J1BNFDOC-BRGEW. "Peso Bruto
      ELSE.
        <FS_GMI>-QT_CARGA_ORG     = WA_ZSDT0001OVRO-NM_PESO_SUBTOTAL.
        <FS_GMI>-QT_CARGA_CHEGADA = WA_ZSDT0001OVRO-NM_PESO_SUBTOTAL.
      ENDIF.

    ENDLOOP.

    IF LC_SALVAR EQ ABAP_TRUE.

      CLEAR: IT_LC_GMI.

      LOOP AT R_GMI_ALV ASSIGNING <FS_GMI>.
        P_CTE_DIST-DT_CHEGADA = <FS_GMI>-DT_CHEGADA.
        MOVE-CORRESPONDING <FS_GMI> TO WA_CTE_GMI.
        IF WA_CTE_GMI-CK_AUTORIZADO EQ ABAP_TRUE AND WA_CTE_GMI-DS_NAME_USUARIO IS NOT INITIAL.
          APPEND WA_CTE_GMI TO IT_LC_GMI.
          CONTINUE.
        ENDIF.
        WA_CTE_GMI-DT_AUTORIZACAO  = SY-DATUM.
        WA_CTE_GMI-HR_AUTORIZACAO  = SY-UZEIT.
        WA_CTE_GMI-CK_AUTORIZADO   = ABAP_TRUE.
        APPEND WA_CTE_GMI TO IT_LC_GMI.

        LOOP AT IT_NIT ASSIGNING FIELD-SYMBOL(<FS_NIT>)
           WHERE DOCNUM EQ WA_CTE_GMI-DOCNUM_NFE
             AND CD_CHAVE_CTE EQ P_CTE_DIST-CD_CHAVE_CTE.
          <FS_NIT>-CK_AUTORIZADO    = WA_CTE_GMI-CK_AUTORIZADO.
          <FS_NIT>-PESO_ORIGEM_APRO = WA_CTE_GMI-QT_CARGA_ORG.
          <FS_NIT>-PESO_CHEGADA_APR = WA_CTE_GMI-QT_CARGA_CHEGADA.
          <FS_NIT>-PESO_DIFERE_APRO = 0.
          <FS_NIT>-ZVLR_FRETE_APRO  = WA_CTE_GMI-ZVLR_FRETE.
        ENDLOOP.
      ENDLOOP.

      DELETE FROM ZIB_CTE_DIST_GMI WHERE CD_CHAVE_CTE EQ P_CTE_DIST-CD_CHAVE_CTE.
      MODIFY ZIB_CTE_DIST_GMI FROM TABLE IT_LC_GMI.
      P_CTE_DIST-CK_AUTORIZADO = ABAP_TRUE.

    ENDIF.

  ENDMETHOD.


  METHOD busca_linha_nota.

    TYPES: r_nr_nf TYPE RANGE OF zlest0041-nr_nf,
           r_serie TYPE RANGE OF zlest0041-serie.


    DATA: r_vbelv TYPE RANGE OF vbfa-vbelv,
          r_posnn TYPE RANGE OF vbfa-posnn.

    DATA: it_lin            TYPE TABLE OF j_1bnflin,
          wa_lin            TYPE j_1bnflin,
          wa_doc            TYPE j_1bnfdoc,
          wa_vbfa           TYPE vbfa,
          wa_vbrp           TYPE vbrp,
          wa_vbak           TYPE vbak,
          wa_vttp           TYPE vttp,
          wa_vfkp           TYPE vfkp,
          wa_zcte_info_nota TYPE zcte_info_nota,
          wa_zlest0032      TYPE zlest0032,
          wa_rbkp           TYPE rbkp,
          wa_rseg           TYPE rseg,
          wa_mseg           TYPE mseg,
          wa_likp           TYPE likp,
          wa_lips           TYPE lips,
          wa_zmmt           TYPE zmmt_ee_zgr_docs,
          wa_nit            TYPE zib_cte_dist_nit,
          lc_xblnr          TYPE xblnr1,
          lc_lifex          TYPE lifex,
          wa_mkpf           TYPE mkpf,
          it_gmi            TYPE TABLE OF zib_cte_dist_gmi,
          wa_gmi            TYPE zib_cte_dist_gmi,
          wa_zlest0044      TYPE zlest0044,
          it_d55            TYPE TABLE OF zib_cte_dist_d55,
          wa_d55            TYPE zib_cte_dist_d55,
          lc_peso_original  TYPE j_1bnetqty,
          lv_tabix          TYPE sy-tabix,
          wa_zlest0041      TYPE zlest0041,
          it_zlest0041      TYPE TABLE OF zlest0041,
          lit_depara_cen    TYPE TABLE OF zsdt_depara_cen,
          lc_exti1          TYPE exti1,
          lc_serie          TYPE char03,
          lc_xblnr_long_v   TYPE xblnr_long,  " Alteracao feita Alexandre Rimini - 13.04.2023
          lc_xblnr_long     TYPE xblnr_long.

    DATA: lc_series     TYPE j_1bseries,
          lc_nf_number9 TYPE j_1bnfnum9.

    DATA: wlifex TYPE shp_lifex_range,
          rlifex TYPE TABLE OF shp_lifex_range.


    DATA: lv_nfnum TYPE xblnr_long,           "*-IR211684-12.12.2024-#160693-JT
          lv_serie TYPE xblnr_long,           "*-IR211684-12.12.2024-#160693-JT
          lr_xblnr TYPE RANGE OF xblnr_long,  "*-IR211684-12.12.2024-#160693-JT
          lw_xblnr LIKE LINE OF lr_xblnr.     "*-IR211684-12.12.2024-#160693-JT

    SELECT * INTO TABLE it_lin
      FROM j_1bnflin
     WHERE docnum EQ p_j_1bnfdoc-docnum.

    SELECT *
      INTO TABLE it_d55
      FROM zib_cte_dist_d55
     WHERE cd_chave_cte EQ e_cte_distr-cd_chave_cte.

    SELECT *
      INTO TABLE it_zlest0041
      FROM zlest0041
     WHERE docnum EQ p_j_1bnfdoc-docnum.

    e_cte_n55-zvlr_frete = e_cte_distr-valor_receber.
    e_cte_n01-zvlr_frete = e_cte_distr-valor_receber.

    e_cte_n55-zvlr_mercadoria = 0.
    e_cte_n01-zvlr_mercadoria = 0.

    lc_exti1 = e_cte_distr-numr_cte.
    lc_serie = e_cte_distr-numr_serie.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lc_serie
      IMPORTING
        output = lc_serie.

    lc_exti1 = lc_exti1 && '-' && lc_serie.

    IF it_lin IS NOT INITIAL.
      SELECT * INTO TABLE it_gmi
        FROM zib_cte_dist_gmi
         FOR ALL ENTRIES IN it_lin
       WHERE cd_chave_cte  EQ e_cte_distr-cd_chave_cte
         AND docnum_nfe    EQ it_lin-docnum
         AND itmnum_nfe    EQ it_lin-itmnum
         AND ck_autorizado EQ 'X'.
    ENDIF.

    LOOP AT it_lin INTO wa_lin.

      READ TABLE e_cte_nit INTO wa_nit WITH KEY docnum = wa_lin-docnum
                                                itmnum = wa_lin-itmnum.
      IF ( sy-subrc IS NOT INITIAL ) OR ( wa_nit-ck_peso_digitado EQ false ).
        IF sy-subrc IS INITIAL.
          lv_tabix = sy-tabix.
        ELSE.
          lv_tabix = 0.
        ENDIF.
        "Ajusta Peso e Valor Rateio """"""""""""""""""""""""""""""""""""""""""""""""""""""
        CLEAR: wa_d55.
        READ TABLE it_d55 INTO wa_d55 INDEX 1.
        IF wa_d55 IS NOT INITIAL.
          lc_peso_original = wa_lin-menge.
          wa_lin-menge = 0.
          LOOP AT it_d55 INTO wa_d55 WHERE n55_chave_acesso EQ e_cte_n55-n55_chave_acesso.
            ADD wa_d55-valr_peso_rate TO wa_lin-menge.
          ENDLOOP.

          LOOP AT it_zlest0041 INTO wa_zlest0041.
            LOOP AT it_d55 INTO wa_d55.
              IF wa_d55-n55_chave_acesso+25(9) = wa_zlest0041-nr_nf AND
                 wa_d55-n55_chave_acesso+22(3) = wa_zlest0041-serie.
                ADD wa_d55-valr_peso_rate TO wa_lin-menge.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          wa_lin-menge = wa_lin-menge * 1000.
          wa_lin-netwr = ( wa_lin-netwr * wa_lin-menge ) / lc_peso_original.

        ELSEIF ( e_cte_n55-chave_rom_saida IS NOT INITIAL ) AND lines( it_zlest0041[] ) > 1.

          SELECT SINGLE *
            FROM zsdt0001 INTO @DATA(lwa_rom_saida)
           WHERE ch_referencia EQ @e_cte_n55-chave_rom_saida
             AND tp_movimento  EQ 'S'.

          IF ( sy-subrc EQ 0 ) AND ( lwa_rom_saida-doc_rem IS NOT INITIAL ).
            SELECT SINGLE *
              FROM lips INTO @DATA(lwa_lips_nf)
             WHERE vbeln EQ @lwa_rom_saida-doc_rem.

            IF sy-subrc EQ 0.
              wa_lin-menge = lwa_lips_nf-ntgew.
              wa_lin-netwr = wa_lin-menge * wa_lin-netpr.
            ENDIF.
          ENDIF.
        ENDIF.

        wa_nit-cd_chave_cte     = e_cte_distr-cd_chave_cte.
        wa_nit-docnum           = wa_lin-docnum.
        wa_nit-itmnum           = wa_lin-itmnum.
        wa_nit-zmatnr_merc      = wa_lin-matnr.
        wa_nit-zvlr_frete       = e_cte_distr-valor_receber.
        wa_nit-zvlr_mercadoria  = wa_lin-netwr.
        wa_nit-peso_origem      = wa_lin-menge.
        wa_nit-meins            = wa_lin-meins.
        wa_nit-zvlr_kg_mercad   = wa_lin-netpr.
        wa_nit-ck_peso_digitado = false.

        IF lv_tabix IS INITIAL.
          APPEND wa_nit TO e_cte_nit.
        ELSE.
          MODIFY e_cte_nit FROM wa_nit INDEX lv_tabix.
        ENDIF.
      ENDIF.

      ADD wa_lin-netwr TO e_cte_n55-zvlr_mercadoria.
      ADD wa_lin-netwr TO e_cte_n01-zvlr_mercadoria.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      """ REGISTRA APROVAÇÃO A NÍVEL DE ITEM """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE it_gmi INTO wa_gmi
      WITH KEY docnum_nfe = wa_lin-docnum
               itmnum_nfe = wa_lin-itmnum.
      IF sy-subrc IS INITIAL.
        e_cte_distr-dt_chegada = wa_gmi-dt_chegada.
        READ TABLE e_cte_nit INTO wa_nit WITH KEY docnum = wa_lin-docnum
                                                  itmnum = wa_lin-itmnum.
        IF sy-subrc IS INITIAL.
          wa_nit-ck_autorizado    = wa_gmi-ck_autorizado.
          wa_nit-peso_origem_apro = wa_gmi-qt_carga_org.
          wa_nit-peso_chegada_apr = wa_gmi-qt_carga_chegada.
          wa_nit-peso_difere_apro = wa_gmi-qt_carga_org - wa_gmi-qt_carga_chegada.
          wa_nit-zvlr_frete_apro  = wa_gmi-zvlr_frete.
          MODIFY e_cte_nit INDEX sy-tabix FROM wa_nit TRANSPORTING ck_autorizado peso_origem_apro peso_chegada_apr peso_difere_apro zvlr_frete_apro.

          sy-msgv1 = wa_lin-docnum.
          sy-msgv2 = wa_lin-itmnum.
          sy-msgv3 = wa_gmi-ds_name_usuario.

          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'W'
              p_num          = 106
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDIF.
      ENDIF.
      """ REGISTRA APROVAÇÃO A NÍVEL DE ITEM """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDLOOP.

    READ TABLE it_lin INTO wa_lin INDEX 1.

    "Frete Próprio - Subcontratado
    IF ( e_cte_distr-tp_processo_cte EQ tipo_04 ) AND ( sy-subrc IS INITIAL ).

      SELECT SINGLE * INTO wa_zcte_info_nota FROM zcte_info_nota
        WHERE docnum_nf EQ wa_lin-docnum.

      IF sy-subrc IS INITIAL.
        e_cte_distr-docnum_cte_sub = wa_zcte_info_nota-docnum.
      ENDIF.

    ELSEIF wa_lin IS NOT INITIAL.

      "Pag. de Frete Terceiro sobre Faturamento Próprio.
      IF ( wa_lin-reftyp EQ 'BI' ) AND ( p_j_1bnfdoc-direct EQ '2' OR p_j_1bnfdoc-direct EQ '4' ). "AND ( E_CTE_DISTR-CD_MODAL EQ '01' ).

        IF e_cte_distr-tp_processo_cte IS INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 006
              p_message_v1   = me->ds_tipo_01
            CHANGING
              p_lc_sequencia = lc_sequencia.

          e_cte_distr-tp_processo_cte = me->tipo_01.
        ENDIF.

        e_cte_n55-vbeln_vf  = wa_lin-refkey(10).
        e_cte_n01-vbeln_vf  = wa_lin-refkey(10).

        "Buscar Fatura
        SELECT SINGLE * INTO wa_vbrp
          FROM vbrp
         WHERE vbeln EQ e_cte_n55-vbeln_vf.

        "Buscar Ordem de Venda
        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO wa_vbak
            FROM vbak
           WHERE vbeln EQ wa_vbrp-aubel.

          IF sy-subrc IS INITIAL.
            e_cte_n55-vbeln_va  = wa_vbak-vbeln.
            e_cte_n01-vbeln_va  = wa_vbak-vbeln.
            e_cte_n55-auart_va  = wa_vbak-auart.
            e_cte_n01-auart_va  = wa_vbak-auart.
          ENDIF.
        ENDIF.

        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = e_cte_distr-cd_chave_cte
            p_type         = 'S'
            p_num          = 007
            p_message_v1   = e_cte_n55-vbeln_vf
          CHANGING
            p_lc_sequencia = lc_sequencia.

        CLEAR: r_vbelv[], r_posnn[].

        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_lin-refitm ) TO r_posnn.

        IF e_cte_n55-chave_rom_saida IS NOT INITIAL.
          SELECT SINGLE *
            FROM zsdt0001 INTO lwa_rom_saida
           WHERE ch_referencia EQ e_cte_n55-chave_rom_saida
             AND tp_movimento  EQ 'S'.

          IF ( sy-subrc EQ 0 ) AND ( lwa_rom_saida-doc_rem IS NOT INITIAL ).
            APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_rom_saida-doc_rem ) TO r_vbelv.
            CLEAR: r_posnn[].
          ENDIF.
        ENDIF.

        SELECT SINGLE * INTO wa_vbfa
          FROM vbfa
         WHERE vbtyp_n EQ 'M'
           AND vbtyp_v EQ 'J'
           AND vbeln   EQ e_cte_n55-vbeln_vf
           AND posnn   IN r_posnn
           AND vbelv   IN r_vbelv.

        IF sy-subrc IS INITIAL.

          e_cte_n55-vbeln_vl = wa_vbfa-vbelv.
          e_cte_n01-vbeln_vl = wa_vbfa-vbelv.

          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 008
              p_message_v1   = e_cte_n55-vbeln_vl
            CHANGING
              p_lc_sequencia = lc_sequencia.

          CASE e_cte_distr-cd_modal.
            WHEN '01'.
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
            WHEN '02'. "Aéreo
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
            WHEN '03'. "Aquaviário
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT * FROM vttk AS k
                               WHERE k~tknum EQ i~tknum
                                 AND k~vsart EQ '03'
                                 AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
            WHEN '04'. "Ferroviário

              SELECT SINGLE * INTO wa_zlest0044
                FROM zlest0044
               WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

              IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND tknum EQ wa_zlest0044-nr_trans
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
              ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                sy-subrc = 4.
              ENDIF.

            WHEN '05'. "Dutoviário
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl.
            WHEN '06'. "Multimodal
              CASE p_tipo_contrato.
                WHEN '0002'.

                  SELECT SINGLE * INTO wa_zlest0044
                    FROM zlest0044
                   WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                  IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND tknum EQ wa_zlest0044-nr_trans
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                  ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                    sy-subrc = 4.
                  ENDIF.

                WHEN OTHERS.

                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '07' ). "07	Multimodal

              ENDCASE.
          ENDCASE.

          IF sy-subrc IS INITIAL.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = e_cte_distr-cd_chave_cte
                p_type         = 'S'
                p_num          = 009
                p_message_v1   = wa_vttp-tknum
              CHANGING
                p_lc_sequencia = lc_sequencia.

            e_cte_n55-tknum = wa_vttp-tknum.
            e_cte_n01-tknum = wa_vttp-tknum.

            SELECT SINGLE * INTO wa_zlest0032
              FROM zlest0032
             WHERE tknum EQ wa_vttp-tknum.

            IF sy-subrc IS INITIAL.

              "Documento de Custo
              IF wa_zlest0032-fknum IS NOT INITIAL.

                e_cte_n55-fknum = wa_zlest0032-fknum.
                e_cte_n01-fknum = wa_zlest0032-fknum.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 027
                    p_message_v1   = wa_zlest0032-fknum
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                CASE e_cte_distr-cd_modal.

                  WHEN '01' OR '04' OR '06'. "Rodoviário/Ferroviário/Multimodal
                    """"" Buscar Valor do Frete;
                    SELECT SUM( netwr ) INTO e_cte_n55-zvlr_vi
                      FROM vfkp WHERE fknum EQ e_cte_n55-fknum.
                    MOVE e_cte_n55-zvlr_vi TO e_cte_n01-zvlr_vi.

                    e_cte_n55-waerk_vi = 'BRL'.
                    e_cte_n01-waerk_vi = 'BRL'.

                  WHEN '03'. "Aquaviário

                    SELECT SINGLE * INTO wa_vfkp
                      FROM vfkp
                     WHERE fknum EQ e_cte_n55-fknum.

                    e_cte_n55-waerk_vi = wa_vfkp-waers.
                    e_cte_n01-waerk_vi = wa_vfkp-waers.

                    MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
                    MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.

                    IF ( wa_vfkp-waers <> 'BRL' ) AND ( e_cte_n55-zvlr_vi NE 0 ).
                      e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                      e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                    ENDIF.

                ENDCASE.

              ELSE.
                MESSAGE e037 RAISING sem_custo.
              ENDIF.

              "Pedido de Compra
              IF wa_zlest0032-ebeln IS NOT INITIAL AND wa_zlest0032-ebelp IS NOT INITIAL.
                e_cte_n55-ebeln = wa_zlest0032-ebeln.
                e_cte_n01-ebeln = wa_zlest0032-ebeln.
                e_cte_n55-ebelp = wa_zlest0032-ebelp.
                e_cte_n01-ebelp = wa_zlest0032-ebelp.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 029
                    p_message_v1   = wa_zlest0032-ebeln
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.

              "Folha de Serviço
              IF wa_zlest0032-lblni IS NOT INITIAL.
                e_cte_n55-lblni = wa_zlest0032-lblni.
                e_cte_n01-lblni = wa_zlest0032-lblni.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 028
                    p_message_v1   = wa_zlest0032-lblni
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.

              "Miro do Frete
              IF wa_zlest0032-belnr IS NOT INITIAL AND wa_zlest0032-gjahr IS NOT INITIAL.

                SELECT SINGLE * INTO wa_rbkp
                  FROM rbkp
                 WHERE belnr EQ wa_zlest0032-belnr
                   AND gjahr EQ wa_zlest0032-gjahr.

                CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
                  EXPORTING
                    ref_number   = wa_rbkp-xblnr
                    i_nfeflag    = true
                  IMPORTING
                    series       = lc_series
                    nf_number9   = lc_nf_number9
                  EXCEPTIONS
                    number_error = 1
                    OTHERS       = 2.

                IF sy-subrc IS NOT INITIAL.
                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = sy-msgty
                      p_id           = sy-msgid
                      p_num          = sy-msgno
                      p_message_v1   = sy-msgv1
                      p_message_v2   = sy-msgv2
                      p_message_v3   = sy-msgv3
                      p_message_v4   = sy-msgv4
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ELSE.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lc_series
                    IMPORTING
                      output = lc_series.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lc_nf_number9
                    IMPORTING
                      output = lc_nf_number9.

                  IF ( e_cte_distr-numr_cte EQ lc_nf_number9 AND e_cte_distr-numr_serie EQ lc_series ) AND
                     ( ( e_cte_distr-p_emissor EQ wa_rbkp-lifnr ) OR ( e_cte_distr-tp_processo_cte EQ me->tipo_02 ) ).

                    e_cte_n55-belnr = wa_zlest0032-belnr.
                    e_cte_n55-gjahr = wa_zlest0032-gjahr.

                    e_cte_n01-belnr = wa_zlest0032-belnr.
                    e_cte_n01-gjahr = wa_zlest0032-gjahr.

                    sy-msgv1 = wa_zlest0032-belnr.
                    sy-msgv2 = wa_zlest0032-gjahr.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 030
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ELSE.
                    sy-msgv1 = wa_zlest0032-belnr.
                    sy-msgv2 = wa_zlest0032-gjahr.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'E'
                        p_num          = 057
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                      CHANGING
                        p_lc_sequencia = lc_sequencia.

                    MESSAGE e057 WITH sy-msgv1 sy-msgv2 RAISING sem_relacao.

                  ENDIF.
                ENDIF.

              ENDIF.
            ELSE.
              MESSAGE e039 RAISING sem_relacao.
            ENDIF.
          ELSE.
            "Ferroviário não tem documento de transporte
            IF e_cte_distr-cd_modal NE '04' AND NOT ( e_cte_distr-cd_modal EQ '06' AND p_tipo_contrato EQ '0002' ).
              MESSAGE e036 RAISING sem_doc_trans.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE e038 RAISING sem_remessa.
        ENDIF.

      ELSEIF ( wa_lin-reftyp EQ 'ZW' ).

        IF e_cte_distr-tp_processo_cte IS INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 006
              p_message_v1   = me->ds_tipo_06
            CHANGING
              p_lc_sequencia = lc_sequencia.

          e_cte_distr-tp_processo_cte = me->tipo_06.
        ENDIF.

        e_cte_n55-zw_lcto = wa_lin-refkey(10).
        e_cte_n01-zw_lcto = wa_lin-refkey(10).

        SELECT SINGLE * INTO @DATA(wa_zfiwrt0008)
          FROM zfiwrt0008
         WHERE seq_lcto EQ @e_cte_n55-zw_lcto.

        IF sy-subrc IS INITIAL.

          SELECT SINGLE * INTO @DATA(wa_zfiwrt0009)
            FROM zfiwrt0009
           WHERE seq_lcto EQ @e_cte_n55-zw_lcto.

          e_cte_n55-mblnr = wa_zfiwrt0008-mblnr.
          e_cte_n55-mjahr = wa_zfiwrt0008-mjahr.

          e_cte_n01-mblnr = wa_zfiwrt0008-mblnr.
          e_cte_n01-mjahr = wa_zfiwrt0008-mjahr.

          IF wa_zfiwrt0009-vbeln IS NOT INITIAL OR wa_zfiwrt0009-vbeln_r IS NOT INITIAL.

            IF e_cte_distr-tp_processo_cte IS INITIAL.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 006
                  p_message_v1   = me->ds_tipo_01
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_distr-tp_processo_cte = me->tipo_01.
            ENDIF.

            "E_CTE_N55-VBELN_VF  = WA_LIN-REFKEY(10).
            "E_CTE_N01-VBELN_VF  = WA_LIN-REFKEY(10).

            "Buscar Fatura
            "SELECT SINGLE * INTO WA_VBRP
            "  FROM VBRP
            " WHERE VBELN EQ E_CTE_N55-VBELN_VF.

            "Buscar Ordem de Venda
            "IF SY-SUBRC IS INITIAL.
            SELECT SINGLE * INTO wa_vbak
              FROM vbak
             WHERE vbeln EQ wa_zfiwrt0009-vbeln.

            IF sy-subrc IS INITIAL.
              e_cte_n55-vbeln_va  = wa_vbak-vbeln.
              e_cte_n01-vbeln_va  = wa_vbak-vbeln.
              e_cte_n55-auart_va  = wa_vbak-auart.
              e_cte_n01-auart_va  = wa_vbak-auart.
            ENDIF.
            "ENDIF.

*            CALL METHOD ME->ADD_LOG_CTE_DIST
*              EXPORTING
*                P_CD_CHAVE_CTE = E_CTE_DISTR-CD_CHAVE_CTE
*                P_TYPE         = 'S'
*                P_NUM          = 007
*                P_MESSAGE_V1   = E_CTE_N55-VBELN_VF
*              CHANGING
*                P_LC_SEQUENCIA = LC_SEQUENCIA.
*
*            SELECT SINGLE * INTO WA_VBFA
*              FROM VBFA
*             WHERE VBTYP_N EQ 'M'
*               AND VBTYP_V EQ 'J'
*               AND VBELN   EQ E_CTE_N55-VBELN_VF
*               AND POSNN   EQ WA_LIN-REFITM.
*
            IF wa_zfiwrt0009-vbeln_r IS NOT INITIAL.

              e_cte_n55-vbeln_vl = wa_zfiwrt0009-vbeln_r.
              e_cte_n01-vbeln_vl = wa_zfiwrt0009-vbeln_r.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 008
                  p_message_v1   = e_cte_n55-vbeln_vl
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              CASE e_cte_distr-cd_modal.
                WHEN '01'.
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
                WHEN '02'. "Aéreo
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
                WHEN '03'. "Aquaviário
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k
                                   WHERE k~tknum EQ i~tknum
                                     AND k~vsart EQ '03'
                                     AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
                WHEN '04'. "Ferroviário

                  SELECT SINGLE * INTO wa_zlest0044
                    FROM zlest0044
                   WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                  IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND tknum EQ wa_zlest0044-nr_trans
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                  ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                    sy-subrc = 4.
                  ENDIF.

                WHEN '05'. "Dutoviário
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl.
                WHEN '06'. "Multimodal
                  CASE p_tipo_contrato.
                    WHEN '0002'.

                      SELECT SINGLE * INTO wa_zlest0044
                        FROM zlest0044
                       WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                      IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                        SELECT SINGLE * INTO wa_vttp
                          FROM vttp AS i
                         WHERE vbeln EQ e_cte_n55-vbeln_vl
                           AND tknum EQ wa_zlest0044-nr_trans
                           AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                      ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                        sy-subrc = 4.
                      ENDIF.

                    WHEN OTHERS.

                      SELECT SINGLE * INTO wa_vttp
                        FROM vttp AS i
                       WHERE vbeln EQ e_cte_n55-vbeln_vl
                         AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '07' ). "07	Multimodal

                  ENDCASE.
              ENDCASE.

              IF sy-subrc IS INITIAL.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 009
                    p_message_v1   = wa_vttp-tknum
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                e_cte_n55-tknum = wa_vttp-tknum.
                e_cte_n01-tknum = wa_vttp-tknum.

                SELECT SINGLE * INTO wa_zlest0032
                  FROM zlest0032
                 WHERE tknum EQ wa_vttp-tknum.

                IF sy-subrc IS INITIAL.

                  "Documento de Custo
                  IF wa_zlest0032-fknum IS NOT INITIAL.

                    e_cte_n55-fknum = wa_zlest0032-fknum.
                    e_cte_n01-fknum = wa_zlest0032-fknum.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 027
                        p_message_v1   = wa_zlest0032-fknum
                      CHANGING
                        p_lc_sequencia = lc_sequencia.

                    CASE e_cte_distr-cd_modal.

                      WHEN '01' OR '04' OR '06'. "Rodoviário/Ferroviário/Multimodal
                        """"" Buscar Valor do Frete;
                        SELECT SUM( netwr ) INTO e_cte_n55-zvlr_vi
                          FROM vfkp WHERE fknum EQ e_cte_n55-fknum.
                        MOVE e_cte_n55-zvlr_vi TO e_cte_n01-zvlr_vi.

                        e_cte_n55-waerk_vi = 'BRL'.
                        e_cte_n01-waerk_vi = 'BRL'.

                      WHEN '03'. "Aquaviário

                        SELECT SINGLE * INTO wa_vfkp
                          FROM vfkp
                         WHERE fknum EQ e_cte_n55-fknum.

                        e_cte_n55-waerk_vi = wa_vfkp-waers.
                        e_cte_n01-waerk_vi = wa_vfkp-waers.

                        MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
                        MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.

                        IF ( wa_vfkp-waers <> 'BRL' ) AND ( e_cte_n55-zvlr_vi NE 0 ).
                          e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                          e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                        ENDIF.

                    ENDCASE.

                  ELSE.
                    MESSAGE e037 RAISING sem_custo.
                  ENDIF.

                  "Pedido de Compra
                  IF wa_zlest0032-ebeln IS NOT INITIAL AND wa_zlest0032-ebelp IS NOT INITIAL.
                    e_cte_n55-ebeln = wa_zlest0032-ebeln.
                    e_cte_n01-ebeln = wa_zlest0032-ebeln.
                    e_cte_n55-ebelp = wa_zlest0032-ebelp.
                    e_cte_n01-ebelp = wa_zlest0032-ebelp.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 029
                        p_message_v1   = wa_zlest0032-ebeln
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ENDIF.

                  "Folha de Serviço
                  IF wa_zlest0032-lblni IS NOT INITIAL.
                    e_cte_n55-lblni = wa_zlest0032-lblni.
                    e_cte_n01-lblni = wa_zlest0032-lblni.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 028
                        p_message_v1   = wa_zlest0032-lblni
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ENDIF.

                  "Miro do Frete
                  IF wa_zlest0032-belnr IS NOT INITIAL AND wa_zlest0032-gjahr IS NOT INITIAL.

                    SELECT SINGLE * INTO wa_rbkp
                      FROM rbkp
                     WHERE belnr EQ wa_zlest0032-belnr
                       AND gjahr EQ wa_zlest0032-gjahr.

                    CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
                      EXPORTING
                        ref_number   = wa_rbkp-xblnr
                        i_nfeflag    = true
                      IMPORTING
                        series       = lc_series
                        nf_number9   = lc_nf_number9
                      EXCEPTIONS
                        number_error = 1
                        OTHERS       = 2.

                    IF sy-subrc IS NOT INITIAL.
                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = sy-msgty
                          p_id           = sy-msgid
                          p_num          = sy-msgno
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                          p_message_v3   = sy-msgv3
                          p_message_v4   = sy-msgv4
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ELSE.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lc_series
                        IMPORTING
                          output = lc_series.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lc_nf_number9
                        IMPORTING
                          output = lc_nf_number9.

                      IF ( e_cte_distr-numr_cte EQ lc_nf_number9 AND e_cte_distr-numr_serie EQ lc_series ) AND
                         ( ( e_cte_distr-p_emissor EQ wa_rbkp-lifnr ) OR ( e_cte_distr-tp_processo_cte EQ me->tipo_02 ) ).

                        e_cte_n55-belnr = wa_zlest0032-belnr.
                        e_cte_n55-gjahr = wa_zlest0032-gjahr.

                        e_cte_n01-belnr = wa_zlest0032-belnr.
                        e_cte_n01-gjahr = wa_zlest0032-gjahr.

                        sy-msgv1 = wa_zlest0032-belnr.
                        sy-msgv2 = wa_zlest0032-gjahr.

                        CALL METHOD me->add_log_cte_dist
                          EXPORTING
                            p_cd_chave_cte = e_cte_distr-cd_chave_cte
                            p_type         = 'S'
                            p_num          = 030
                            p_message_v1   = sy-msgv1
                            p_message_v2   = sy-msgv2
                          CHANGING
                            p_lc_sequencia = lc_sequencia.
                      ELSE.
                        sy-msgv1 = wa_zlest0032-belnr.
                        sy-msgv2 = wa_zlest0032-gjahr.

                        CALL METHOD me->add_log_cte_dist
                          EXPORTING
                            p_cd_chave_cte = e_cte_distr-cd_chave_cte
                            p_type         = 'E'
                            p_num          = 057
                            p_message_v1   = sy-msgv1
                            p_message_v2   = sy-msgv2
                          CHANGING
                            p_lc_sequencia = lc_sequencia.

                        MESSAGE e057 WITH sy-msgv1 sy-msgv2 RAISING sem_relacao.

                      ENDIF.
                    ENDIF.

                  ENDIF.
                ELSE.
                  MESSAGE e039 RAISING sem_relacao.
                ENDIF.
              ELSE.
                "Ferroviário não tem documento de transporte
                IF e_cte_distr-cd_modal NE '04' AND NOT ( e_cte_distr-cd_modal EQ '06' AND p_tipo_contrato EQ '0002' ).
                  MESSAGE e036 RAISING sem_doc_trans.
                ENDIF.
              ENDIF.
            ELSE.
              MESSAGE e038 RAISING sem_remessa.
            ENDIF.


          ELSE.

            e_cte_n01-ebeln	= wa_zfiwrt0008-ebeln.
            e_cte_n55-ebeln	= wa_zfiwrt0008-ebeln.

            "Referencia
            lc_xblnr_long =
            zcl_miro=>get_chave_referencia(
              EXPORTING
                "I_NF_NUMBER  =     " Nº nota fiscal
                i_series     = e_cte_n55-n55_chave_acesso+22(3)    " Séries
                "I_SUBSERIES  =     " Subséries
                i_nf_number9 = e_cte_n55-n55_chave_acesso+25(9)    " Número de documento de nove posições
            ).

            SELECT SINGLE * INTO @DATA(wa_ekes)
              FROM ekes
             WHERE ebeln EQ @wa_zfiwrt0008-ebeln
               AND xblnr EQ @lc_xblnr_long.

*-IR211684-12.12.2024-#160693-JT-inicio
            IF sy-subrc <> 0.
              FREE: lr_xblnr.
              SPLIT lc_xblnr_long AT '-' INTO lv_nfnum lv_serie.

              lw_xblnr-sign     = 'I'.
              lw_xblnr-option   = 'CP'.
              lw_xblnr-low      = '*' && lv_nfnum && '*-*' && lv_serie && '*'.
              APPEND lw_xblnr  TO lr_xblnr.

              SELECT SINGLE * INTO wa_ekes
                FROM ekes
               WHERE ebeln EQ wa_zfiwrt0008-ebeln
                 AND xblnr IN lr_xblnr.
            ENDIF.
*-IR211684-12.12.2024-#160693-JT-fim

*** Alteracao feita por Alexandre Rimini 13.04.2023

            IF sy-subrc IS NOT INITIAL.

              CONCATENATE   e_cte_n55-n55_chave_acesso+25(9) '-'  e_cte_n55-n55_chave_acesso+22(3) INTO lc_xblnr_long_v.

              SELECT SINGLE * INTO @DATA(wa_ekes2)
                FROM ekes
               WHERE ebeln EQ @wa_zfiwrt0008-ebeln
               AND xblnr EQ @lc_xblnr_long_v.

            ENDIF.
*** Fim Alteracao Alexandre Rimini


            IF sy-subrc IS INITIAL.

              IF wa_ekes-vbeln IS NOT INITIAL. "Alteracao Feita por Alexandre Rimini.
                e_cte_n55-vbeln_vl = wa_ekes-vbeln.
                e_cte_n01-vbeln_vl = wa_ekes-vbeln.
              ELSE. "Alexandre Rimini
                e_cte_n55-vbeln_vl = wa_ekes2-vbeln. "Alexandre Rimini
                e_cte_n01-vbeln_vl = wa_ekes2-vbeln. "Alexandre Rimini
              ENDIF. "Alexandre Rimini

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 008
                  p_message_v1   = e_cte_n55-vbeln_vl
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              CASE e_cte_distr-cd_modal.
                WHEN '01'. " Rodoviário
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
                WHEN '02'. "Aéreo
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
                WHEN '03'. "Aquaviário
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT *
                                    FROM vttk AS k
                                   WHERE k~tknum EQ i~tknum
                                     AND k~vsart EQ '03'
                                     AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
                WHEN '04'. "Ferroviário

                  SELECT SINGLE * INTO wa_zlest0044
                    FROM zlest0044
                   WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                  IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND tknum EQ wa_zlest0044-nr_trans
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                  ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                    sy-subrc = 4.
                  ENDIF.

                WHEN '05'. "Dutoviário
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl.
                WHEN '06'. "Multimodal

                  CASE p_tipo_contrato.
                    WHEN '0002'.
                      SELECT SINGLE * INTO wa_zlest0044
                        FROM zlest0044
                       WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                      IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                        SELECT SINGLE * INTO wa_vttp
                          FROM vttp AS i
                         WHERE vbeln EQ e_cte_n55-vbeln_vl
                           AND tknum EQ wa_zlest0044-nr_trans
                           AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                      ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                        sy-subrc = 4.
                      ENDIF.
                    WHEN OTHERS.
                      SELECT SINGLE * INTO wa_vttp
                        FROM vttp AS i
                       WHERE vbeln EQ e_cte_n55-vbeln_vl
                         AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
                  ENDCASE.
              ENDCASE.

              IF sy-subrc IS INITIAL.

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 009
                    p_message_v1   = wa_vttp-tknum
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                e_cte_n55-tknum = wa_vttp-tknum.
                e_cte_n01-tknum = wa_vttp-tknum.

                SELECT SINGLE * INTO wa_zlest0032
                  FROM zlest0032
                 WHERE tknum EQ wa_vttp-tknum.

                IF sy-subrc IS INITIAL.

                  "Documento de Custo
                  IF wa_zlest0032-fknum IS NOT INITIAL.

                    e_cte_n55-fknum = wa_zlest0032-fknum.
                    e_cte_n01-fknum = wa_zlest0032-fknum.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 027
                        p_message_v1   = wa_zlest0032-fknum
                      CHANGING
                        p_lc_sequencia = lc_sequencia.

                    CASE e_cte_distr-cd_modal.

                      WHEN '01' OR '04' OR '06'. "Rodoviário/Ferroviário/Multimodal
                        """"" Buscar Valor do Frete;
                        SELECT SUM( netwr ) INTO e_cte_n55-zvlr_vi
                          FROM vfkp WHERE fknum EQ e_cte_n55-fknum.
                        MOVE e_cte_n55-zvlr_vi TO e_cte_n01-zvlr_vi.

                        e_cte_n55-waerk_vi = 'BRL'.
                        e_cte_n01-waerk_vi = 'BRL'.

                      WHEN '03'. "Aquaviário

                        SELECT SINGLE * INTO wa_vfkp
                          FROM vfkp
                         WHERE fknum EQ e_cte_n55-fknum.

                        e_cte_n55-waerk_vi = wa_vfkp-waers.
                        e_cte_n01-waerk_vi = wa_vfkp-waers.

                        MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
                        MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.

                        IF ( wa_vfkp-waers <> 'BRL' ) AND ( e_cte_n55-zvlr_vi NE 0 ).
                          e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                          e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                        ENDIF.

                    ENDCASE.

                  ELSE.
                    MESSAGE e037 RAISING sem_custo.
                  ENDIF.

                  "Pedido de Compra
                  IF wa_zlest0032-ebeln IS NOT INITIAL AND wa_zlest0032-ebelp IS NOT INITIAL.
                    e_cte_n55-ebeln = wa_zlest0032-ebeln.
                    e_cte_n01-ebeln = wa_zlest0032-ebeln.
                    e_cte_n55-ebelp = wa_zlest0032-ebelp.
                    e_cte_n01-ebelp = wa_zlest0032-ebelp.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 029
                        p_message_v1   = wa_zlest0032-ebeln
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ENDIF.

                  "Folha de Serviço
                  IF wa_zlest0032-lblni IS NOT INITIAL.
                    e_cte_n55-lblni = wa_zlest0032-lblni.
                    e_cte_n01-lblni = wa_zlest0032-lblni.

                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = 'S'
                        p_num          = 028
                        p_message_v1   = wa_zlest0032-lblni
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ENDIF.

                  "Miro do Frete
                  IF wa_zlest0032-belnr IS NOT INITIAL AND wa_zlest0032-gjahr IS NOT INITIAL.

                    SELECT SINGLE * INTO wa_rbkp
                      FROM rbkp
                     WHERE belnr EQ wa_zlest0032-belnr
                       AND gjahr EQ wa_zlest0032-gjahr.

                    CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
                      EXPORTING
                        ref_number   = wa_rbkp-xblnr
                        i_nfeflag    = true
                      IMPORTING
                        series       = lc_series
                        nf_number9   = lc_nf_number9
                      EXCEPTIONS
                        number_error = 1
                        OTHERS       = 2.

                    IF sy-subrc IS NOT INITIAL.
                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = sy-msgty
                          p_id           = sy-msgid
                          p_num          = sy-msgno
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                          p_message_v3   = sy-msgv3
                          p_message_v4   = sy-msgv4
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ELSE.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lc_series
                        IMPORTING
                          output = lc_series.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lc_nf_number9
                        IMPORTING
                          output = lc_nf_number9.

                      IF ( e_cte_distr-numr_cte EQ lc_nf_number9 AND e_cte_distr-numr_serie EQ lc_series ) AND
                         ( ( e_cte_distr-p_emissor EQ wa_rbkp-lifnr ) OR ( e_cte_distr-tp_processo_cte EQ me->tipo_02 ) ).

                        e_cte_n55-belnr = wa_zlest0032-belnr.
                        e_cte_n55-gjahr = wa_zlest0032-gjahr.

                        e_cte_n01-belnr = wa_zlest0032-belnr.
                        e_cte_n01-gjahr = wa_zlest0032-gjahr.

                        sy-msgv1 = wa_zlest0032-belnr.
                        sy-msgv2 = wa_zlest0032-gjahr.

                        CALL METHOD me->add_log_cte_dist
                          EXPORTING
                            p_cd_chave_cte = e_cte_distr-cd_chave_cte
                            p_type         = 'S'
                            p_num          = 030
                            p_message_v1   = sy-msgv1
                            p_message_v2   = sy-msgv2
                          CHANGING
                            p_lc_sequencia = lc_sequencia.
                      ELSE.
                        sy-msgv1 = wa_zlest0032-belnr.
                        sy-msgv2 = wa_zlest0032-gjahr.

                        CALL METHOD me->add_log_cte_dist
                          EXPORTING
                            p_cd_chave_cte = e_cte_distr-cd_chave_cte
                            p_type         = 'E'
                            p_num          = 057
                            p_message_v1   = sy-msgv1
                            p_message_v2   = sy-msgv2
                          CHANGING
                            p_lc_sequencia = lc_sequencia.
                      ENDIF.
                    ENDIF.

                  ENDIF.
                ELSE.
                  MESSAGE e039 RAISING sem_relacao.
                ENDIF.
              ELSE.
                "Ferroviário não tem documento de transporte
                IF e_cte_distr-cd_modal NE '04' AND NOT ( e_cte_distr-cd_modal EQ '06' AND p_tipo_contrato EQ '0002' ).
                  MESSAGE e036 RAISING sem_doc_trans.
                ENDIF.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ELSEIF ( wa_lin-reftyp EQ 'MD' ) AND ( p_j_1bnfdoc-direct EQ '1' OR p_j_1bnfdoc-direct EQ '3' ). "AND ( E_CTE_DISTR-CD_MODAL EQ '01' ).

        IF e_cte_distr-tp_processo_cte IS INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 006
              p_message_v1   = me->ds_tipo_07
            CHANGING
              p_lc_sequencia = lc_sequencia.

          e_cte_distr-tp_processo_cte = me->tipo_07.

          e_cte_n55-mblnr = wa_lin-refkey(10).
          e_cte_n55-mjahr = wa_lin-refkey+10(4).
          MOVE wa_lin-refitm TO e_cte_n55-zeile.

          e_cte_n01-mblnr = wa_lin-refkey(10).
          e_cte_n01-mjahr = wa_lin-refkey+10(4).
          MOVE wa_lin-refitm TO e_cte_n01-zeile.

          SELECT SINGLE * INTO wa_mseg
            FROM mseg
           WHERE mblnr EQ e_cte_n01-mblnr
             AND mjahr EQ e_cte_n01-mjahr.

          IF sy-subrc IS INITIAL AND wa_mseg-vbeln_im IS NOT INITIAL.

            SELECT SINGLE *
              FROM zmmt_ee_zgr_docs INTO @DATA(lwa_ee_zgr_docs)
             WHERE mm_mblnr = @e_cte_n01-mblnr.

            IF ( sy-subrc EQ 0 ) AND
               ( lwa_ee_zgr_docs-av_vbeln IS NOT INITIAL      ) AND
               ( lwa_ee_zgr_docs-av_vbeln NE wa_mseg-vbeln_im ).

              e_cte_n55-vbeln_vl = lwa_ee_zgr_docs-av_vbeln.
              e_cte_n01-vbeln_vl = lwa_ee_zgr_docs-av_vbeln.
            ELSE.
              e_cte_n55-vbeln_vl = wa_mseg-vbeln_im.
              e_cte_n01-vbeln_vl = wa_mseg-vbeln_im.
            ENDIF.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = e_cte_distr-cd_chave_cte
                p_type         = 'S'
                p_num          = 008
                p_message_v1   = e_cte_n55-vbeln_vl
              CHANGING
                p_lc_sequencia = lc_sequencia.

            CASE e_cte_distr-cd_modal.
              WHEN '01'. " Rodoviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
              WHEN '02'. "Aéreo
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
              WHEN '03'. "Aquaviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT *
                                  FROM vttk AS k
                                 WHERE k~tknum EQ i~tknum
                                   AND k~vsart EQ '03'
                                   AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
              WHEN '04'. "Ferroviário

                SELECT SINGLE * INTO wa_zlest0044
                  FROM zlest0044
                 WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND tknum EQ wa_zlest0044-nr_trans
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                  sy-subrc = 4.
                ENDIF.

              WHEN '05'. "Dutoviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl.
              WHEN '06'. "Multimodal

                CASE p_tipo_contrato.
                  WHEN '0002'.
                    SELECT SINGLE * INTO wa_zlest0044
                      FROM zlest0044
                     WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                    IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                      SELECT SINGLE * INTO wa_vttp
                        FROM vttp AS i
                       WHERE vbeln EQ e_cte_n55-vbeln_vl
                         AND tknum EQ wa_zlest0044-nr_trans
                         AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                    ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                      sy-subrc = 4.
                    ENDIF.
                  WHEN OTHERS.
                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
                ENDCASE.
            ENDCASE.

            IF sy-subrc IS INITIAL.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 009
                  p_message_v1   = wa_vttp-tknum
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_n55-tknum = wa_vttp-tknum.
              e_cte_n01-tknum = wa_vttp-tknum.

              SELECT SINGLE * INTO wa_zlest0032
                FROM zlest0032
               WHERE tknum EQ wa_vttp-tknum.

              IF sy-subrc IS INITIAL.

                "Documento de Custo
                IF wa_zlest0032-fknum IS NOT INITIAL.

                  e_cte_n55-fknum = wa_zlest0032-fknum.
                  e_cte_n01-fknum = wa_zlest0032-fknum.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 027
                      p_message_v1   = wa_zlest0032-fknum
                    CHANGING
                      p_lc_sequencia = lc_sequencia.

                  CASE e_cte_distr-cd_modal.

                    WHEN '01' OR '04' OR '06'. "Rodoviário/Ferroviário/Multimodal
                      """"" Buscar Valor do Frete;
                      SELECT SUM( netwr ) INTO e_cte_n55-zvlr_vi
                        FROM vfkp WHERE fknum EQ e_cte_n55-fknum.
                      MOVE e_cte_n55-zvlr_vi TO e_cte_n01-zvlr_vi.

                      e_cte_n55-waerk_vi = 'BRL'.
                      e_cte_n01-waerk_vi = 'BRL'.

                    WHEN '03'. "Aquaviário

                      SELECT SINGLE * INTO wa_vfkp
                        FROM vfkp
                       WHERE fknum EQ e_cte_n55-fknum.

                      e_cte_n55-waerk_vi = wa_vfkp-waers.
                      e_cte_n01-waerk_vi = wa_vfkp-waers.

                      MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
                      MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.

                      IF ( wa_vfkp-waers <> 'BRL' ) AND ( e_cte_n55-zvlr_vi NE 0 ).
                        e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                        e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                      ENDIF.

                  ENDCASE.

                ELSE.
                  MESSAGE e037 RAISING sem_custo.
                ENDIF.

                "Pedido de Compra
                IF wa_zlest0032-ebeln IS NOT INITIAL AND wa_zlest0032-ebelp IS NOT INITIAL.
                  e_cte_n55-ebeln = wa_zlest0032-ebeln.
                  e_cte_n01-ebeln = wa_zlest0032-ebeln.
                  e_cte_n55-ebelp = wa_zlest0032-ebelp.
                  e_cte_n01-ebelp = wa_zlest0032-ebelp.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 029
                      p_message_v1   = wa_zlest0032-ebeln
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDIF.

                "Folha de Serviço
                IF wa_zlest0032-lblni IS NOT INITIAL.
                  e_cte_n55-lblni = wa_zlest0032-lblni.
                  e_cte_n01-lblni = wa_zlest0032-lblni.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 028
                      p_message_v1   = wa_zlest0032-lblni
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDIF.

                "Miro do Frete
                IF wa_zlest0032-belnr IS NOT INITIAL AND wa_zlest0032-gjahr IS NOT INITIAL.

                  SELECT SINGLE * INTO wa_rbkp
                    FROM rbkp
                   WHERE belnr EQ wa_zlest0032-belnr
                     AND gjahr EQ wa_zlest0032-gjahr.

                  CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
                    EXPORTING
                      ref_number   = wa_rbkp-xblnr
                      i_nfeflag    = true
                    IMPORTING
                      series       = lc_series
                      nf_number9   = lc_nf_number9
                    EXCEPTIONS
                      number_error = 1
                      OTHERS       = 2.

                  IF sy-subrc IS NOT INITIAL.
                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = sy-msgty
                        p_id           = sy-msgid
                        p_num          = sy-msgno
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                        p_message_v3   = sy-msgv3
                        p_message_v4   = sy-msgv4
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ELSE.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = lc_series
                      IMPORTING
                        output = lc_series.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = lc_nf_number9
                      IMPORTING
                        output = lc_nf_number9.

                    IF ( e_cte_distr-numr_cte EQ lc_nf_number9 AND e_cte_distr-numr_serie EQ lc_series ) AND
                       ( ( e_cte_distr-p_emissor EQ wa_rbkp-lifnr ) OR ( e_cte_distr-tp_processo_cte EQ me->tipo_02 ) ).

                      e_cte_n55-belnr = wa_zlest0032-belnr.
                      e_cte_n55-gjahr = wa_zlest0032-gjahr.

                      e_cte_n01-belnr = wa_zlest0032-belnr.
                      e_cte_n01-gjahr = wa_zlest0032-gjahr.

                      sy-msgv1 = wa_zlest0032-belnr.
                      sy-msgv2 = wa_zlest0032-gjahr.

                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = 'S'
                          p_num          = 030
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ELSE.
                      sy-msgv1 = wa_zlest0032-belnr.
                      sy-msgv2 = wa_zlest0032-gjahr.

                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = 'E'
                          p_num          = 057
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ELSE.
                MESSAGE e039 RAISING sem_relacao.
              ENDIF.
            ELSE.
              "Ferroviário não tem documento de transporte
              IF e_cte_distr-cd_modal NE '04' AND NOT ( e_cte_distr-cd_modal EQ '06' AND p_tipo_contrato EQ '0002' ).
                MESSAGE e036 RAISING sem_doc_trans.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE e038 RAISING sem_remessa.
          ENDIF.

        ENDIF.

      ELSEIF ( wa_lin-reftyp EQ 'MD' ) AND ( p_j_1bnfdoc-direct EQ '2' OR p_j_1bnfdoc-direct EQ '4' ). "AND ( E_CTE_DISTR-CD_MODAL EQ '01' ).

*       "// se for processo Aquaviario limpar o Tipo de Processo
        IF e_cte_distr-cd_modal EQ '03' AND e_cte_distr-tp_processo_cte IS NOT INITIAL.
          CLEAR e_cte_distr-tp_processo_cte.
        ENDIF.

        "" Frete Terceiro sobre Mov. Mercadoria (Sáida)
        IF e_cte_distr-tp_processo_cte IS INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 006
              p_message_v1   = me->ds_tipo_09
            CHANGING
              p_lc_sequencia = lc_sequencia.

          e_cte_distr-tp_processo_cte = me->tipo_09.

          e_cte_n55-mblnr = wa_lin-refkey(10).
          e_cte_n55-mjahr = wa_lin-refkey+10(4).
          MOVE wa_lin-refitm TO e_cte_n55-zeile.

          e_cte_n01-mblnr = wa_lin-refkey(10).
          e_cte_n01-mjahr = wa_lin-refkey+10(4).
          MOVE wa_lin-refitm TO e_cte_n01-zeile.

          SELECT SINGLE * INTO wa_mkpf FROM mkpf
           WHERE mblnr EQ e_cte_n55-mblnr
             AND mjahr EQ e_cte_n55-mjahr.

          IF sy-subrc IS INITIAL AND wa_mkpf-le_vbeln IS NOT INITIAL.

            e_cte_n55-vbeln_vl = wa_mkpf-le_vbeln.
            e_cte_n01-vbeln_vl = wa_mkpf-le_vbeln.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = e_cte_distr-cd_chave_cte
                p_type         = 'S'
                p_num          = 008
                p_message_v1   = e_cte_n55-vbeln_vl
              CHANGING
                p_lc_sequencia = lc_sequencia.

            CASE e_cte_distr-cd_modal.
              WHEN '01'. " Rodoviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
              WHEN '02'. "Aéreo
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
              WHEN '03'. "Aquaviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND EXISTS ( SELECT *
                                  FROM vttk AS k
                                 WHERE k~tknum EQ i~tknum
                                   AND k~vsart EQ '03'
                                   AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
              WHEN '04'. "Ferroviário

                SELECT SINGLE * INTO wa_zlest0044
                  FROM zlest0044
                 WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND tknum EQ wa_zlest0044-nr_trans
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                  sy-subrc = 4.
                ENDIF.

              WHEN '05'. "Dutoviário
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl.

              WHEN '06'. "Multimodal

                CASE p_tipo_contrato.
                  WHEN '0002'.

                    SELECT SINGLE * INTO wa_zlest0044
                      FROM zlest0044
                     WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                    IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                      SELECT SINGLE * INTO wa_vttp
                        FROM vttp AS i
                       WHERE vbeln EQ e_cte_n55-vbeln_vl
                         AND tknum EQ wa_zlest0044-nr_trans
                         AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                    ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                      sy-subrc = 4.
                    ENDIF.

                  WHEN OTHERS.

                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario

                ENDCASE.

            ENDCASE.

            IF sy-subrc IS INITIAL.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 009
                  p_message_v1   = wa_vttp-tknum
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_n55-tknum = wa_vttp-tknum.
              e_cte_n01-tknum = wa_vttp-tknum.

              SELECT SINGLE * INTO wa_zlest0032
                FROM zlest0032
               WHERE tknum EQ wa_vttp-tknum.

              IF sy-subrc IS INITIAL.

                "Documento de Custo
                IF wa_zlest0032-fknum IS NOT INITIAL.

                  e_cte_n55-fknum = wa_zlest0032-fknum.
                  e_cte_n01-fknum = wa_zlest0032-fknum.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 027
                      p_message_v1   = wa_zlest0032-fknum
                    CHANGING
                      p_lc_sequencia = lc_sequencia.

                  CASE e_cte_distr-cd_modal.

                    WHEN '01' OR '04' OR '06'. "Rodoviário/Ferroviário/Multimodal
                      """"" Buscar Valor do Frete;
                      SELECT SUM( netwr ) INTO e_cte_n55-zvlr_vi
                        FROM vfkp WHERE fknum EQ e_cte_n55-fknum.
                      MOVE e_cte_n55-zvlr_vi TO e_cte_n01-zvlr_vi.

                      e_cte_n55-waerk_vi = 'BRL'.
                      e_cte_n01-waerk_vi = 'BRL'.

                    WHEN '03'. "Aquaviário

                      SELECT SINGLE * INTO wa_vfkp
                        FROM vfkp
                       WHERE fknum EQ e_cte_n55-fknum.

                      e_cte_n55-waerk_vi = wa_vfkp-waers.
                      e_cte_n01-waerk_vi = wa_vfkp-waers.

                      MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
                      MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.

                      IF ( wa_vfkp-waers <> 'BRL' ) AND ( e_cte_n55-zvlr_vi NE 0 ).
                        e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                        e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                      ENDIF.

                  ENDCASE.

                ELSE.
                  MESSAGE e037 RAISING sem_custo.
                ENDIF.

                "Pedido de Compra
                IF wa_zlest0032-ebeln IS NOT INITIAL AND wa_zlest0032-ebelp IS NOT INITIAL.
                  e_cte_n55-ebeln = wa_zlest0032-ebeln.
                  e_cte_n01-ebeln = wa_zlest0032-ebeln.
                  e_cte_n55-ebelp = wa_zlest0032-ebelp.
                  e_cte_n01-ebelp = wa_zlest0032-ebelp.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 029
                      p_message_v1   = wa_zlest0032-ebeln
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDIF.

                "Folha de Serviço
                IF wa_zlest0032-lblni IS NOT INITIAL.
                  e_cte_n55-lblni = wa_zlest0032-lblni.
                  e_cte_n01-lblni = wa_zlest0032-lblni.

                  CALL METHOD me->add_log_cte_dist
                    EXPORTING
                      p_cd_chave_cte = e_cte_distr-cd_chave_cte
                      p_type         = 'S'
                      p_num          = 028
                      p_message_v1   = wa_zlest0032-lblni
                    CHANGING
                      p_lc_sequencia = lc_sequencia.
                ENDIF.

                "Miro do Frete
                IF wa_zlest0032-belnr IS NOT INITIAL AND wa_zlest0032-gjahr IS NOT INITIAL.

                  SELECT SINGLE * INTO wa_rbkp
                    FROM rbkp
                   WHERE belnr EQ wa_zlest0032-belnr
                     AND gjahr EQ wa_zlest0032-gjahr.

                  CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
                    EXPORTING
                      ref_number   = wa_rbkp-xblnr
                      i_nfeflag    = true
                    IMPORTING
                      series       = lc_series
                      nf_number9   = lc_nf_number9
                    EXCEPTIONS
                      number_error = 1
                      OTHERS       = 2.

                  IF sy-subrc IS NOT INITIAL.
                    CALL METHOD me->add_log_cte_dist
                      EXPORTING
                        p_cd_chave_cte = e_cte_distr-cd_chave_cte
                        p_type         = sy-msgty
                        p_id           = sy-msgid
                        p_num          = sy-msgno
                        p_message_v1   = sy-msgv1
                        p_message_v2   = sy-msgv2
                        p_message_v3   = sy-msgv3
                        p_message_v4   = sy-msgv4
                      CHANGING
                        p_lc_sequencia = lc_sequencia.
                  ELSE.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = lc_series
                      IMPORTING
                        output = lc_series.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = lc_nf_number9
                      IMPORTING
                        output = lc_nf_number9.

                    IF ( e_cte_distr-numr_cte EQ lc_nf_number9 AND e_cte_distr-numr_serie EQ lc_series ) AND
                       ( ( e_cte_distr-p_emissor EQ wa_rbkp-lifnr ) OR ( e_cte_distr-tp_processo_cte EQ me->tipo_02 ) ).

                      e_cte_n55-belnr = wa_zlest0032-belnr.
                      e_cte_n55-gjahr = wa_zlest0032-gjahr.

                      e_cte_n01-belnr = wa_zlest0032-belnr.
                      e_cte_n01-gjahr = wa_zlest0032-gjahr.

                      sy-msgv1 = wa_zlest0032-belnr.
                      sy-msgv2 = wa_zlest0032-gjahr.

                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = 'S'
                          p_num          = 030
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ELSE.
                      sy-msgv1 = wa_zlest0032-belnr.
                      sy-msgv2 = wa_zlest0032-gjahr.

                      CALL METHOD me->add_log_cte_dist
                        EXPORTING
                          p_cd_chave_cte = e_cte_distr-cd_chave_cte
                          p_type         = 'E'
                          p_num          = 057
                          p_message_v1   = sy-msgv1
                          p_message_v2   = sy-msgv2
                        CHANGING
                          p_lc_sequencia = lc_sequencia.
                    ENDIF.
                  ENDIF.

                ENDIF.
              ELSE.
                MESSAGE e039 RAISING sem_relacao.
              ENDIF.
            ELSE.
              "Ferroviário não tem documento de transporte
              IF e_cte_distr-cd_modal NE '04' AND NOT ( e_cte_distr-cd_modal EQ '06' AND p_tipo_contrato EQ '0002' ).
                MESSAGE e036 RAISING sem_doc_trans.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE e038 RAISING sem_remessa.
          ENDIF.

        ENDIF.

      ELSEIF ( wa_lin-reftyp EQ 'LI' ) AND ( p_j_1bnfdoc-direct EQ '1' OR p_j_1bnfdoc-direct EQ '3' ). "AND ( E_CTE_DISTR-CD_MODAL EQ '01' ).

        CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
          EXPORTING
            nf_number  = p_j_1bnfdoc-nfnum
            series     = p_j_1bnfdoc-series
            nf_number9 = p_j_1bnfdoc-nfenum
          IMPORTING
            ref_number = lc_xblnr.

        MOVE lc_xblnr TO lc_lifex.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lc_lifex
          IMPORTING
            output = lc_lifex.

        "Processo de Compra de Insumos """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE * INTO wa_likp
          FROM likp
         WHERE lifex EQ lc_lifex
           AND lifnr EQ p_j_1bnfdoc-parid
           AND werks EQ p_j_1bnfdoc-branch.

        IF sy-subrc IS NOT INITIAL.

          "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP --->>>>
          DATA: lra_lifex_check TYPE RANGE OF likp-lifex.
          DATA: lva_lifex_tmp   TYPE likp-lifex.
          "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP <<----

          DATA: vl_nf_number9 TYPE j_1bnfnum9.
          DATA: vl_nf_number  TYPE j_1bnfdoc-nfnum.
          DATA: vl_series     TYPE j_1bnfdoc-series.

          vl_nf_number9 = p_j_1bnfdoc-nfenum.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = vl_nf_number9
            IMPORTING
              output = vl_nf_number9.

          CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
            EXPORTING
              nf_number  = p_j_1bnfdoc-nfnum
              series     = p_j_1bnfdoc-series
              nf_number9 = vl_nf_number9
            IMPORTING
              ref_number = lc_xblnr.

          CLEAR: rlifex.
          wlifex-sign   = 'I'.
          wlifex-option = 'CP'.
          CONCATENATE '*' lc_xblnr INTO wlifex-low.
          APPEND wlifex TO rlifex.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = lc_xblnr ) TO lra_lifex_check. "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP --->>>>

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = p_j_1bnfdoc-series
            IMPORTING
              output = vl_series.

          CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
            EXPORTING
              nf_number  = p_j_1bnfdoc-nfnum
              series     = vl_series
              nf_number9 = vl_nf_number9
            IMPORTING
              ref_number = lc_xblnr.

          wlifex-sign   = 'I'.
          wlifex-option = 'CP'.
          CONCATENATE '*' lc_xblnr INTO wlifex-low.
          APPEND wlifex TO rlifex.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = lc_xblnr ) TO lra_lifex_check. "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP --->>>>

          SELECT SINGLE * INTO wa_likp
            FROM likp
           WHERE lifex IN rlifex
             AND lifnr EQ p_j_1bnfdoc-parid.

          "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP --->>>>
          IF sy-subrc NE 0.
            SELECT SINGLE * INTO wa_likp
             FROM likp AS a
            WHERE lifex IN rlifex
              AND EXISTS ( SELECT vbeln
                             FROM vbpa AS b
                            WHERE b~vbeln EQ a~vbeln
                              AND b~parvw EQ 'WL'
                              AND b~lifnr EQ p_j_1bnfdoc-parid ).
          ENDIF.

*          IF sy-subrc EQ 0.
*            LOOP AT lra_lifex_check ASSIGNING FIELD-SYMBOL(<fs_lifex_check>).
*              <fs_lifex_check>-low = |{ <fs_lifex_check>-low ALPHA = OUT }|.
*            ENDLOOP.
*            lva_lifex_tmp = wa_likp-lifex.
*            lva_lifex_tmp = |{ lva_lifex_tmp ALPHA = OUT }|.
*            READ TABLE lra_lifex_check WITH KEY low = lva_lifex_tmp TRANSPORTING NO FIELDS.
*            IF sy-subrc NE 0.
*              CLEAR: wa_likp.
*            ENDIF.
*          ENDIF.
          "MM - Ajuste Busca NF ZMM0079 IR211102 - WPP <<<----

          IF ( sy-subrc IS NOT INITIAL ) AND ( p_j_1bnfdoc-docnum IS NOT INITIAL ) AND ( p_j_1bnfdoc-entrad EQ abap_true ). "Buscar Remessa/Aviso por Fluxo de Entrada Propria.

            DATA(lva_found_remessa_nf_ent_prop) = abap_false.

            SELECT SINGLE *
              FROM zmmt_ee_zgr_docs INTO @DATA(lwa_zmm_zgr_docs_prop)
             WHERE docnum EQ @p_j_1bnfdoc-docnum.

            IF sy-subrc EQ 0.
              SELECT SINGLE *
                FROM zmmt_ee_zgr INTO @DATA(lwa_zmm_zgr_prop)
               WHERE obj_key EQ @lwa_zmm_zgr_docs_prop-obj_key.

              IF ( sy-subrc EQ 0 ) AND ( lwa_zmm_zgr_prop-ch_referencia IS NOT INITIAL ).

                SELECT SINGLE *
                  FROM zsdt0001 INTO @DATA(lwa_zsdt001_nf_prod)
                 WHERE ch_referencia EQ @lwa_zmm_zgr_prop-ch_referencia.

                IF sy-subrc EQ 0.

                  CLEAR: vl_nf_number9, vl_nf_number, vl_series, lc_xblnr.

                  IF lwa_zsdt001_nf_prod-nfe IS NOT INITIAL.
                    vl_nf_number9 = lwa_zsdt001_nf_prod-nfnum.
                  ELSE.
                    vl_nf_number  = lwa_zsdt001_nf_prod-nfnum.
                  ENDIF.

                  vl_series = lwa_zsdt001_nf_prod-series.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      input  = vl_series
                    IMPORTING
                      output = vl_series.

                  CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
                    EXPORTING
                      nf_number  = vl_nf_number
                      series     = vl_series
                      nf_number9 = vl_nf_number9
                    IMPORTING
                      ref_number = lc_xblnr.

                  CLEAR: rlifex[], rlifex.
                  wlifex-sign   = 'I'.
                  wlifex-option = 'CP'.
                  CONCATENATE '*' lc_xblnr INTO wlifex-low.
                  APPEND wlifex TO rlifex.

*                  SELECT SINGLE * INTO wa_likp
*                    FROM likp
*                   WHERE lifex IN rlifex
*                     AND lifnr EQ p_j_1bnfdoc-parid.

                  SELECT * INTO TABLE @DATA(lit_likp)
                    FROM likp
                   WHERE lifex IN @rlifex
                     AND lifnr EQ @p_j_1bnfdoc-parid.

                  IF ( lit_likp[] IS NOT INITIAL ).

                    IF ( lines( lit_likp[] ) > 1 ) AND
                       ( e_cte_distr-docnum_cte_p IS NOT INITIAL ) AND
                       ( e_cte_distr-cd_modal = '03' ). "Aquaviario

                      LOOP AT lit_likp INTO wa_likp.
                        SELECT SINGLE *
                          FROM zlest0060 INTO @DATA(lwa_zlest0060)
                         WHERE docnum       = @e_cte_distr-docnum_cte_p
                           AND vbeln_aviso  = @wa_likp-vbeln.

                        IF sy-subrc EQ 0.
                          lva_found_remessa_nf_ent_prop = abap_true.
                          EXIT.
                        ENDIF.
                      ENDLOOP.

                    ENDIF.

                    IF lva_found_remessa_nf_ent_prop EQ abap_false.
                      READ TABLE lit_likp INTO wa_likp INDEX 1.
                      IF sy-subrc EQ 0.
                        lva_found_remessa_nf_ent_prop = abap_true.
                      ENDIF.
                    ENDIF.

                  ENDIF.

                ENDIF.
              ENDIF.
            ENDIF.

            IF lva_found_remessa_nf_ent_prop EQ abap_false.
              sy-subrc = 4.
            ENDIF.
          ENDIF. "Buscar por Fluxo de Entrada Propria - Fim

          IF sy-subrc IS INITIAL.

            SELECT SINGLE * INTO wa_lips
              FROM lips
             WHERE vbeln EQ wa_likp-vbeln
               AND werks EQ p_j_1bnfdoc-branch.

            IF sy-subrc IS NOT INITIAL.
              CLEAR: lit_depara_cen[].

              SELECT *
                FROM zsdt_depara_cen INTO TABLE lit_depara_cen
               WHERE centro_real       EQ p_j_1bnfdoc-branch
                 AND tp_centro_virtual EQ '1'.

              DATA(lva_achou_centro_af) = abap_false.

              LOOP AT lit_depara_cen INTO DATA(lwa_depara_cen).
                SELECT SINGLE * INTO wa_lips
                  FROM lips
                 WHERE vbeln EQ wa_likp-vbeln
                   AND werks EQ lwa_depara_cen-centrov_1.

                IF sy-subrc EQ 0.
                  lva_achou_centro_af = abap_true.
                  EXIT.
                ENDIF.
              ENDLOOP.

              IF lva_achou_centro_af EQ abap_false.
                sy-subrc = 4.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        "Processo de Compra de Grãos """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE * INTO wa_zmmt
            FROM zmmt_ee_zgr_docs
           WHERE docnum EQ p_j_1bnfdoc-docnum.

          IF ( sy-subrc IS INITIAL ) AND ( wa_zmmt-av_vbeln IS NOT INITIAL ).
            SELECT SINGLE * INTO wa_likp
              FROM likp
             WHERE vbeln EQ wa_zmmt-av_vbeln.
          ELSEIF ( sy-subrc IS INITIAL ) AND ( wa_zmmt-mm_mblnr IS NOT INITIAL AND wa_zmmt-mm_mjahr IS NOT INITIAL ).
            "IF E_CTE_DISTR-TP_PROCESSO_CTE IS INITIAL.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = e_cte_distr-cd_chave_cte
                p_type         = 'S'
                p_num          = 006
                p_message_v1   = me->ds_tipo_07
              CHANGING
                p_lc_sequencia = lc_sequencia.

            e_cte_distr-tp_processo_cte = me->tipo_07.

            e_cte_n55-mblnr    = wa_zmmt-mm_mblnr.
            e_cte_n55-mjahr    = wa_zmmt-mm_mjahr.
            e_cte_n55-vbeln_re = wa_zmmt-ft_belnr.
            e_cte_n55-gjahr_re = wa_zmmt-ft_gjahr.

            e_cte_n01-mblnr = wa_zmmt-mm_mblnr.
            e_cte_n01-mjahr = wa_zmmt-mm_mjahr.
            e_cte_n01-vbeln_re = wa_zmmt-ft_belnr.
            e_cte_n01-gjahr_re = wa_zmmt-ft_gjahr.
            "ENDIF.
          ELSEIF sy-subrc IS NOT INITIAL AND p_j_1bnfdoc-belnr IS NOT INITIAL AND p_j_1bnfdoc-gjahr IS NOT INITIAL.

            SELECT SINGLE * INTO wa_rseg
              FROM rseg
             WHERE belnr EQ p_j_1bnfdoc-belnr
               AND gjahr EQ p_j_1bnfdoc-gjahr.

            IF sy-subrc IS INITIAL AND
               wa_rseg-lfbnr IS NOT INITIAL AND
               wa_rseg-lfgja IS NOT INITIAL AND
               wa_rseg-lfpos IS NOT INITIAL.

              SELECT SINGLE * INTO wa_mseg
                FROM mseg
               WHERE mblnr EQ wa_rseg-lfbnr
                 AND mjahr EQ wa_rseg-lfgja
                 AND zeile EQ wa_rseg-lfpos.

              IF sy-subrc IS INITIAL .

                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 006
                    p_message_v1   = me->ds_tipo_07
                  CHANGING
                    p_lc_sequencia = lc_sequencia.

                e_cte_distr-tp_processo_cte = me->tipo_07.

                e_cte_n55-vbeln_re = p_j_1bnfdoc-belnr.
                e_cte_n55-gjahr_re = p_j_1bnfdoc-gjahr.
                e_cte_n01-vbeln_re = p_j_1bnfdoc-belnr.
                e_cte_n01-gjahr_re = p_j_1bnfdoc-gjahr.

                e_cte_n55-mblnr    = wa_mseg-mblnr.
                e_cte_n55-mjahr    = wa_mseg-mjahr.
                e_cte_n01-mblnr    = wa_mseg-mblnr.
                e_cte_n01-mjahr    = wa_mseg-mjahr.

                e_cte_n55-ebeln    = wa_mseg-ebeln.
                e_cte_n55-ebelp    = wa_mseg-ebelp.
                e_cte_n01-ebeln    = wa_mseg-ebeln.
                e_cte_n01-ebelp    = wa_mseg-ebelp.

                IF wa_mseg-tcode2_mkpf EQ 'VL32N'.
                  wa_likp-vbeln  = wa_mseg-xblnr_mkpf.
                ENDIF.

              ENDIF.
            ENDIF.

          ELSE.
            sy-subrc = 1.
          ENDIF.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        IF ( sy-subrc IS INITIAL ) AND ( wa_likp-vbeln IS NOT INITIAL ).
          e_cte_n55-vbeln_vl = wa_likp-vbeln.
          e_cte_n01-vbeln_vl = wa_likp-vbeln.
        ENDIF.

        IF e_cte_n55-vbeln_vl IS NOT INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = e_cte_distr-cd_chave_cte
              p_type         = 'S'
              p_num          = 008
              p_message_v1   = e_cte_n55-vbeln_vl
            CHANGING
              p_lc_sequencia = lc_sequencia.

          CASE e_cte_distr-cd_modal.
            WHEN '01'. " Rodoviário
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario
            WHEN '02'. "Aéreo
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '05' ). "05	Aéreo
            WHEN '03'. "Aquaviário
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl
                 AND EXISTS ( SELECT *
                                FROM vttk AS k
                               WHERE k~tknum EQ i~tknum
                                 AND k~vsart EQ '03'
                                 AND k~exti1 EQ lc_exti1 ). "03	Navegação fluvial
            WHEN '04'. "Ferroviário

              SELECT SINGLE * INTO wa_zlest0044
                FROM zlest0044
               WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

              IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                SELECT SINGLE * INTO wa_vttp
                  FROM vttp AS i
                 WHERE vbeln EQ e_cte_n55-vbeln_vl
                   AND tknum EQ wa_zlest0044-nr_trans
                   AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
              ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                sy-subrc = 4.
              ENDIF.

            WHEN '05'. "Dutoviário
              SELECT SINGLE * INTO wa_vttp
                FROM vttp AS i
               WHERE vbeln EQ e_cte_n55-vbeln_vl.

            WHEN '06'.

              CASE p_tipo_contrato.
                WHEN '0002'.

                  SELECT SINGLE * INTO wa_zlest0044
                    FROM zlest0044
                   WHERE chave_cte EQ e_cte_distr-cd_chave_cte.

                  IF sy-subrc IS INITIAL AND wa_zlest0044-nr_trans IS NOT INITIAL.
                    SELECT SINGLE * INTO wa_vttp
                      FROM vttp AS i
                     WHERE vbeln EQ e_cte_n55-vbeln_vl
                       AND tknum EQ wa_zlest0044-nr_trans
                       AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '02' ). "02	Ferroviário
                  ELSEIF wa_zlest0044-nr_trans IS INITIAL.
                    sy-subrc = 4.
                  ENDIF.

                WHEN OTHERS.

                  SELECT SINGLE * INTO wa_vttp
                    FROM vttp AS i
                   WHERE vbeln EQ e_cte_n55-vbeln_vl
                     AND EXISTS ( SELECT * FROM vttk AS k WHERE k~tknum EQ i~tknum AND k~vsart EQ '01' ). "01	Rodoviario

              ENDCASE.

          ENDCASE.

          "*********************************************************************************
          "*********************************************************************************

          IF sy-subrc IS INITIAL.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = e_cte_distr-cd_chave_cte
                p_type         = 'S'
                p_num          = 009
                p_message_v1   = wa_vttp-tknum
              CHANGING
                p_lc_sequencia = lc_sequencia.

            e_cte_n55-tknum = wa_vttp-tknum.
            e_cte_n01-tknum = wa_vttp-tknum.

            SELECT SINGLE * INTO wa_vfkp
              FROM vfkp
             WHERE rebel = wa_vttp-tknum.

            IF sy-subrc IS INITIAL.
              e_cte_n55-fknum    = wa_vfkp-fknum.
              e_cte_n01-fknum    = wa_vfkp-fknum.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 027
                  p_message_v1   = wa_vfkp-fknum
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_n55-waerk_vi = wa_vfkp-waers.
              e_cte_n01-waerk_vi = wa_vfkp-waers.
              MOVE wa_vfkp-netwr TO e_cte_n01-zvlr_vi.
              MOVE wa_vfkp-netwr TO e_cte_n55-zvlr_vi.
              IF wa_vfkp-waers <> 'BRL'.
                e_cte_n55-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
                e_cte_n01-kursk_vi = e_cte_n55-zvlr_frete / e_cte_n55-zvlr_vi.
              ENDIF.
              e_cte_n55-ebeln = wa_vfkp-ebeln.
              e_cte_n01-ebeln = wa_vfkp-ebeln.
              e_cte_n55-ebelp = wa_vfkp-ebelp.
              e_cte_n01-ebelp = wa_vfkp-ebelp.
              e_cte_n55-lblni = wa_vfkp-lblni.
              e_cte_n01-lblni = wa_vfkp-lblni.

              "Pedido de Compra
              IF wa_vfkp-ebeln IS NOT INITIAL AND wa_vfkp-ebelp IS NOT INITIAL.
                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 029
                    p_message_v1   = wa_vfkp-ebeln
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.

              "Folha de Serviço
              IF wa_vfkp-lblni IS NOT INITIAL.
                CALL METHOD me->add_log_cte_dist
                  EXPORTING
                    p_cd_chave_cte = e_cte_distr-cd_chave_cte
                    p_type         = 'S'
                    p_num          = 028
                    p_message_v1   = wa_vfkp-lblni
                  CHANGING
                    p_lc_sequencia = lc_sequencia.
              ENDIF.
            ELSE.
              MESSAGE e037 RAISING sem_custo.
            ENDIF.
            "*********************************************************************************
            "*********************************************************************************
          ENDIF.

        ENDIF.

        IF p_j_1bnfdoc-nfenum IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_j_1bnfdoc-nfnum
            IMPORTING
              output = e_zlest0041-nr_nf.
        ELSE.
          e_zlest0041-nr_nf = p_j_1bnfdoc-nfenum.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = p_j_1bnfdoc-series
          IMPORTING
            output = e_zlest0041-serie.

        DATA(r_nr_nf) = VALUE r_nr_nf( ( sign = 'I' option = 'EQ' low = |{ e_zlest0041-nr_nf ALPHA = IN  }| )
                                       ( sign = 'I' option = 'EQ' low = |{ e_zlest0041-nr_nf ALPHA = OUT }| ) ).

        DATA(r_serie) = VALUE r_serie( ( sign = 'I' option = 'EQ' low = |{ e_zlest0041-serie ALPHA = IN  }| )
                                       ( sign = 'I' option = 'EQ' low = |{ e_zlest0041-serie ALPHA = OUT }| ) ).

        IF p_j_1bnfdoc-entrad EQ abap_true .
          CLEAR lwa_zmm_zgr_docs_prop.
          SELECT SINGLE *
            FROM zmmt_ee_zgr_docs INTO lwa_zmm_zgr_docs_prop
           WHERE docnum EQ p_j_1bnfdoc-docnum.
          IF lwa_zmm_zgr_docs_prop-obj_key IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(lwa_zmmt_ee_zgr)
              FROM zmmt_ee_zgr
              WHERE obj_key = @lwa_zmm_zgr_docs_prop-obj_key.
            IF lwa_zmmt_ee_zgr-ch_referencia IS NOT INITIAL.
              SELECT SINGLE * INTO @DATA(lwa_zsdt0001)
                FROM zsdt0001
                WHERE ch_referencia  = @lwa_zmmt_ee_zgr-ch_referencia.
              IF sy-subrc = 0.
                SELECT SINGLE * INTO e_zlest0041
                  FROM zlest0041
                 WHERE centro_comprador EQ lwa_zsdt0001-branch
                   AND nr_nf            EQ lwa_zsdt0001-nfnum
                   AND cod_cliente      EQ lwa_zsdt0001-parid
                   AND serie            EQ lwa_zsdt0001-series.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          SELECT SINGLE * INTO e_zlest0041
            FROM zlest0041
           WHERE centro_comprador EQ p_j_1bnfdoc-branch
             AND nr_nf            IN r_nr_nf
             AND cod_cliente      EQ p_j_1bnfdoc-parid
             AND serie            IN r_serie.
        ENDIF.

        CASE sy-subrc.
          WHEN 0.
            IF e_cte_distr-tp_processo_cte IS INITIAL.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 006
                  p_message_v1   = me->ds_tipo_08
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_distr-tp_processo_cte = me->tipo_08.
            ENDIF.
          WHEN OTHERS.
            CLEAR: e_zlest0041.
            IF e_cte_distr-tp_processo_cte IS INITIAL.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 006
                  p_message_v1   = me->ds_tipo_05
                CHANGING
                  p_lc_sequencia = lc_sequencia.

              e_cte_distr-tp_processo_cte = me->tipo_05.
            ENDIF.
        ENDCASE.

        IF e_cte_n55-vbeln_re IS INITIAL.
          e_cte_n55-vbeln_re  = wa_lin-refkey(10).
          e_cte_n01-vbeln_re  = wa_lin-refkey(10).
          e_cte_n55-gjahr_re  = wa_lin-refkey+10(4).
          e_cte_n01-gjahr_re  = wa_lin-refkey+10(4).
        ENDIF.

      ENDIF.


      IF ( e_cte_distr-cd_modal EQ '01' OR e_cte_distr-cd_modal EQ '03' ) AND ( e_cte_n55-tknum IS NOT INITIAL ). "Rodoviário e Aquaviário
        CASE e_cte_distr-tp_processo_cte.
          WHEN tipo_01 OR tipo_05 OR tipo_06 OR tipo_07.
            CALL METHOD me->busca_peso_rodo_terceiro_vt
              EXPORTING
                p_tknum     = e_cte_n55-tknum
              CHANGING
                e_cte_distr = e_cte_distr
                e_cte_n55   = e_cte_n55
                e_cte_n01   = e_cte_n01
                e_cte_nit   = e_cte_nit
              EXCEPTIONS
                nao_achou   = 1
                OTHERS      = 2.

            IF sy-subrc IS NOT INITIAL.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'E'
                  p_id           = sy-msgid
                  p_num          = sy-msgno
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ELSE.
              sy-msgv1 = e_cte_n55-tknum.

              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = e_cte_distr-cd_chave_cte
                  p_type         = 'S'
                  p_num          = 073
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_LOG_PROC_CTE.


    DATA: IT_LOG TYPE TABLE OF ZIB_CTE_DIST_LOG,
          WA_LOG TYPE ZIB_CTE_DIST_LOG,
          WA_ALV TYPE ZDE_CTE_DIST_LOG_ALV.

    CLEAR: E_LOGS.

    SELECT *
      INTO TABLE IT_LOG
      FROM ZIB_CTE_DIST_LOG
      WHERE CD_CHAVE_CTE EQ P_CHAVE
      ORDER BY NR_SEQUENCIA.

    LOOP AT IT_LOG INTO WA_LOG.

      "Ctg.mens.: S sucesso, E erro, W aviso, I inform., A cancel.

      CASE WA_LOG-TYPE.
        WHEN 'S'.
          WA_ALV-IC_MESSAGE = ICON_LED_GREEN.
        WHEN 'E'.
          WA_ALV-IC_MESSAGE = ICON_LED_RED.
        WHEN 'W'.
          WA_ALV-IC_MESSAGE = ICON_LED_YELLOW.
        WHEN 'I'.
          WA_ALV-IC_MESSAGE = ICON_MESSAGE_INFORMATION_SMALL.
        WHEN 'A'.
          WA_ALV-IC_MESSAGE = ICON_INCOMPLETE.
      ENDCASE.

      IF WA_LOG-CK_ESTRATEGIA EQ ABAP_TRUE.
        CASE WA_LOG-TYPE.
          WHEN 'S'.
            WA_ALV-IC_TEXTO = ICON_ALLOW.
          WHEN 'E'.
            WA_ALV-IC_TEXTO = ICON_REJECT.
        ENDCASE.
      ENDIF.

      MOVE-CORRESPONDING WA_LOG TO WA_ALV.
      APPEND WA_ALV TO E_LOGS.

    ENDLOOP.

  ENDMETHOD.


  METHOD BUSCA_MOTORISTAS.


    SELECT * INTO TABLE E_MOT_T
      FROM ZIB_CTE_DIST_MOT
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_NOTAS_C57.


    SELECT * INTO TABLE E_C57_T
      FROM ZIB_CTE_DIST_C57
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_NOTAS_ITENS.


    SELECT * INTO TABLE E_NIT_T
      FROM ZIB_CTE_DIST_NIT
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_NOTAS_N01.


    SELECT * INTO TABLE E_N01_T
      FROM ZIB_CTE_DIST_N01
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_NOTAS_N55.


    SELECT * INTO TABLE E_N55_T
      FROM ZIB_CTE_DIST_N55
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_PESO_AUAVIARIO.


    DATA: WA_ZLEST0061 TYPE ZLEST0061.

    IF P_DOCNUM IS NOT INITIAL.

      SELECT SINGLE * INTO WA_ZLEST0061
        FROM ZLEST0061
       WHERE DOCNUM     EQ P_DOCNUM
         AND CK_ANULADO EQ ABAP_FALSE.

      IF ( SY-SUBRC IS INITIAL ).
        E_CTE-PESO_ORIGEM     = WA_ZLEST0061-PESO_VINCULADO.
        IF ( WA_ZLEST0061-DT_CHEGADA IS NOT INITIAL ).
          E_CTE-PESO_CHEGADA    = WA_ZLEST0061-PESO_CHEGADA.
          E_CTE-DT_CHEGADA      = WA_ZLEST0061-DT_CHEGADA.
          E_CTE-CK_PESO_CHEGADA = TRUE.
        ENDIF.
      ELSE.
        MESSAGE E063 RAISING NAO_ACHOU.
      ENDIF.

      CALL METHOD ME->BUSCA_PESO_RODO_TERCEIRO
        EXPORTING
          P_DOCNUM  = P_DOCNUM
        CHANGING
          E_CTE     = E_CTE
        EXCEPTIONS
          NAO_ACHOU = 1
          OTHERS    = 2.

    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_PESO_RODO_TERCEIRO.


    DATA: WA_ZLEST0034 TYPE ZLEST0034,
          IT_ZLEST0034 TYPE TABLE OF ZLEST0034.

    "As informações de peso/perda/quebra/valor icms/valor pis/valor cofins/liquido a pagar são por Doc. de Transporte
    "Deve ser buscado esta informação por VT vinculada ao CT-e. Ex.: Docnum 6317854 Mod 57

    IF P_DOCNUM IS NOT INITIAL.

      SELECT * INTO TABLE IT_ZLEST0034
        FROM ZLEST0034
       WHERE EN_DOCNUM EQ P_DOCNUM.

      IF SY-SUBRC IS INITIAL.

        E_CTE-PESO_ORIGEM     = 0.
        E_CTE-PESO_CHEGADA    = 0.
        E_CTE-ZPESO_DIFERENCA = 0.
        E_CTE-ZQUEBRA         = 0.
        E_CTE-ZPERDA          = 0.
        E_CTE-ZVLR_QUEBRA     = 0.
        E_CTE-ZVLR_PERDA      = 0.
        E_CTE-ZVLR_LIQ_PAGAR  = 0.
        E_CTE-ZBASE_ICMS      = 0.
        E_CTE-ZBASE_PIS       = 0.
        E_CTE-ZBASE_COFINS    = 0.
        E_CTE-ZVALOR_ICMS     = 0.
        E_CTE-ZVALOR_PIS      = 0.
        E_CTE-ZVALOR_COFINS   = 0.

        READ TABLE IT_ZLEST0034 INTO WA_ZLEST0034 INDEX 1.
        E_CTE-DT_CHEGADA      = WA_ZLEST0034-ZDT_CHEGADA.
        E_CTE-ZDT_MOV         = WA_ZLEST0034-ZDT_MOV.
        E_CTE-ZDT_VENCTO      = WA_ZLEST0034-ZDT_VENCTO.
        E_CTE-MWSKZ           = WA_ZLEST0034-IVA.
        E_CTE-ZBVTYP          = WA_ZLEST0034-BVTYP.
        E_CTE-MATNS           = WA_ZLEST0034-MATNS.
        E_CTE-ZRATE_ICMS      = WA_ZLEST0034-RATE_ICMS.
        E_CTE-ZRATE_PIS       = WA_ZLEST0034-RATE_PIS.
        E_CTE-ZRATE_COFINS    = WA_ZLEST0034-RATE_COFINS.
        E_CTE-ZVALOR_PEDAGIO  = WA_ZLEST0034-VALOR_PEDAGIO.

        E_CTE-CK_PESO_CHEGADA = TRUE.

        LOOP AT IT_ZLEST0034 INTO WA_ZLEST0034.
          ADD WA_ZLEST0034-ZPESO_ORIGEM    TO E_CTE-PESO_ORIGEM.
          ADD WA_ZLEST0034-ZPESO_DESTINO   TO E_CTE-PESO_CHEGADA.
          ADD WA_ZLEST0034-ZPESO_DIFERENCA TO E_CTE-ZPESO_DIFERENCA.
          ADD WA_ZLEST0034-ZQUEBRA         TO E_CTE-ZQUEBRA.
          ADD WA_ZLEST0034-ZPERDA          TO E_CTE-ZPERDA.
          ADD WA_ZLEST0034-ZVLR_QUEBRA     TO E_CTE-ZVLR_QUEBRA.
          ADD WA_ZLEST0034-ZVLR_PERDA      TO E_CTE-ZVLR_PERDA.
          ADD WA_ZLEST0034-ZVLR_LIQ_PAGAR  TO E_CTE-ZVLR_LIQ_PAGAR.
          ADD WA_ZLEST0034-BASE_ICMS       TO E_CTE-ZBASE_ICMS.
          ADD WA_ZLEST0034-BASE_PIS        TO E_CTE-ZBASE_PIS.
          ADD WA_ZLEST0034-BASE_COFINS     TO E_CTE-ZBASE_COFINS.
          ADD WA_ZLEST0034-VALOR_ICMS      TO E_CTE-ZVALOR_ICMS.
          ADD WA_ZLEST0034-VALOR_PIS       TO E_CTE-ZVALOR_PIS.
          ADD WA_ZLEST0034-VALOR_COFINS    TO E_CTE-ZVALOR_COFINS.
        ENDLOOP.
      ELSE.
        MESSAGE E070 RAISING NAO_ACHOU.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_PESO_RODO_TERCEIRO_VT.


    "Recuperar Informações digitadas anterior.

    DATA: WA_ZLEST0034 TYPE ZLEST0034,
          WA_ZLEST0032 TYPE ZLEST0032,
          IT_VFKP      TYPE TABLE OF VFKP,
          WA_VFKP      TYPE VFKP,
          WA_J_1BNFDOC TYPE J_1BNFDOC.

    FIELD-SYMBOLS: <NIT> TYPE ZIB_CTE_DIST_NIT.

    CHECK P_TKNUM IS NOT INITIAL.

    SELECT SINGLE * INTO WA_ZLEST0034
      FROM ZLEST0034
     WHERE TKNUM EQ P_TKNUM.

    "Incluido para Verificar a Numeração da CT-E
    IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZLEST0034-EN_DOCNUM IS NOT INITIAL ).
      SELECT SINGLE * INTO WA_J_1BNFDOC
        FROM J_1BNFDOC
       WHERE DOCNUM EQ WA_ZLEST0034-EN_DOCNUM.
    ENDIF.

    IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZLEST0034-EN_DOCNUM IS NOT INITIAL ) AND ( WA_J_1BNFDOC-NFENUM EQ E_CTE_DISTR-NUMR_CTE ).
      "CT-e
*---> 07/06/2023 - Migração S4 - JS
*      E_CTE_DISTR-ZVLR_VI         = WA_ZLEST0034-DMBTR.
       E_CTE_DISTR-ZVLR_VI = CONV #( WA_ZLEST0034-DMBTR ).
*<--- 07/06/2023 - Migração S4 - JS

      E_CTE_DISTR-DOCNUM_CTE      = WA_ZLEST0034-EN_DOCNUM.
      E_CTE_DISTR-MWSKZ           = WA_ZLEST0034-IVA.
      E_CTE_DISTR-BELNR           = WA_ZLEST0034-RE_BELNR.
      E_CTE_DISTR-GJAHR           = WA_ZLEST0034-RE_GJAHR.
*---> 07/06/2023 - Migração S4 - JS
*            E_CTE_DISTR-MATNS           = WA_ZLEST0034-MATNS.
      E_CTE_DISTR-MATNS = CONV #( WA_ZLEST0034-MATNS ).
*<--- 07/06/2023 - Migração S4 - JS
      E_CTE_DISTR-CK_PESO_CHEGADA = TRUE.

      "Notas Fiscais

*---> 07/06/2023 - Migração S4 - JS
*            E_CTE_N55-ZVLR_VI           = WA_ZLEST0034-DMBTR.
*            E_CTE_N55-ZVLR_FRETE        = WA_ZLEST0034-DMBTR_DOC.
      E_CTE_N55-ZVLR_VI     = CONV #( WA_ZLEST0034-DMBTR ).
      E_CTE_N55-ZVLR_FRETE = CONV #( WA_ZLEST0034-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS

      E_CTE_N55-ZVLR_MERCADORIA   = WA_ZLEST0034-VALOR_MERCADORIA.

*---> 07/06/2023 - Migração S4 - JS
*            E_CTE_N55-ZVLR_QUEBRA       = WA_ZLEST0034-ZVLR_QUEBRA.
*            E_CTE_N55-ZVLR_PERDA        = WA_ZLEST0034-ZVLR_PERDA.
      E_CTE_N55-ZVLR_QUEBRA = CONV #( WA_ZLEST0034-ZVLR_QUEBRA ).
      E_CTE_N55-ZVLR_PERDA  = CONV #( WA_ZLEST0034-ZVLR_PERDA ).
*<--- 07/06/2023 - Migração S4 - JS

      E_CTE_N55-ZVLR_LIQ_PAGAR    = WA_ZLEST0034-ZVLR_LIQ_PAGAR.

*---> 07/06/2023 - Migração S4 - JS
*            E_CTE_N55-BELNR             = WA_ZLEST0034-RE_BELNR.
*            E_CTE_N55-GJAHR             = WA_ZLEST0034-RE_GJAHR.
      E_CTE_N55-BELNR = CONV #( WA_ZLEST0034-RE_BELNR ).
      E_CTE_N55-GJAHR = CONV #( WA_ZLEST0034-RE_GJAHR ).
*<--- 07/06/2023 - Migração S4 - JS
      E_CTE_N55-CK_PESO_DIGITADO  = TRUE.

*---> 07/06/2023 - Migração S4 - JS
*            E_CTE_N01-ZVLR_VI           = WA_ZLEST0034-DMBTR.
*            E_CTE_N01-ZVLR_FRETE        = WA_ZLEST0034-DMBTR_DOC.
      E_CTE_N01-ZVLR_VI       = CONV #( WA_ZLEST0034-DMBTR ).
      E_CTE_N01-ZVLR_FRETE    = CONV #( WA_ZLEST0034-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS
      E_CTE_N01-ZVLR_MERCADORIA   = WA_ZLEST0034-VALOR_MERCADORIA.
      E_CTE_N01-ZVLR_QUEBRA       = WA_ZLEST0034-ZVLR_QUEBRA.
      E_CTE_N01-ZVLR_PERDA        = WA_ZLEST0034-ZVLR_PERDA.
      E_CTE_N01-ZVLR_LIQ_PAGAR    = WA_ZLEST0034-ZVLR_LIQ_PAGAR.
      E_CTE_N01-BELNR             = WA_ZLEST0034-RE_BELNR.
      E_CTE_N01-GJAHR             = WA_ZLEST0034-RE_GJAHR.
      E_CTE_N01-CK_PESO_DIGITADO  = TRUE.

      "Antes não era informado os valores por item, com isso está sendo atribuido somente co primeiro.
      READ TABLE E_CTE_NIT ASSIGNING <NIT> INDEX 1.
*---> 07/06/2023 - Migração S4 - JS
*            <NIT>-ZVLR_VI          = WA_ZLEST0034-DMBTR.
*            <NIT>-ZVLR_FRETE       = WA_ZLEST0034-DMBTR_DOC.
      <NIT>-ZVLR_VI    = CONV #( WA_ZLEST0034-DMBTR ).
      <NIT>-ZVLR_FRETE = CONV #( WA_ZLEST0034-DMBTR_DOC ).
*<--- 07/06/2023 - Migração S4 - JS

      <NIT>-ZVLR_MERCADORIA  = WA_ZLEST0034-VALOR_MERCADORIA.
      <NIT>-PESO_ORIGEM      = WA_ZLEST0034-ZPESO_ORIGEM.
      <NIT>-PESO_CHEGADA     = WA_ZLEST0034-ZPESO_DESTINO.
      <NIT>-ZPESO_DIFERENCA  = WA_ZLEST0034-ZPESO_DIFERENCA.
      <NIT>-ZQUEBRA          = WA_ZLEST0034-ZQUEBRA.
      <NIT>-ZPERDA           = WA_ZLEST0034-ZPERDA.
      <NIT>-ZVLR_QUEBRA      = WA_ZLEST0034-ZVLR_QUEBRA.
      <NIT>-ZVLR_PERDA       = WA_ZLEST0034-ZVLR_PERDA.
      <NIT>-ZVLR_LIQ_PAGAR   = WA_ZLEST0034-ZVLR_LIQ_PAGAR.
      IF WA_ZLEST0034-ZPESO_ORIGEM NE 0.
        <NIT>-ZVLR_KG_TRANSP   = WA_ZLEST0034-DMBTR / WA_ZLEST0034-ZPESO_ORIGEM.
        <NIT>-ZVLR_KG_MERCAD   = WA_ZLEST0034-VALOR_MERCADORIA / WA_ZLEST0034-ZPESO_ORIGEM.
      ELSE.
        <NIT>-ZVLR_KG_TRANSP   = 0.
        <NIT>-ZVLR_KG_MERCAD   = 0.
      ENDIF.

      LOOP AT E_CTE_NIT ASSIGNING <NIT>.
        <NIT>-CK_PESO_DIGITADO = TRUE.
      ENDLOOP.

    ELSE.

      SY-SUBRC = 0.

      IF WA_ZLEST0034-FKNUM IS NOT INITIAL.
        SELECT * INTO TABLE IT_VFKP
          FROM VFKP
         WHERE FKNUM EQ WA_ZLEST0034-FKNUM.
      ENDIF.

      IF ( WA_ZLEST0034-FKNUM IS INITIAL ) OR ( SY-SUBRC IS NOT INITIAL ).
        SELECT SINGLE * INTO WA_ZLEST0032
          FROM ZLEST0032
         WHERE TKNUM EQ P_TKNUM.

        IF ( SY-SUBRC IS INITIAL ) AND ( WA_ZLEST0032-FKNUM IS NOT INITIAL ).
          WA_ZLEST0034-FKNUM = WA_ZLEST0032-FKNUM.

          SELECT * INTO TABLE IT_VFKP
            FROM VFKP
           WHERE FKNUM EQ WA_ZLEST0034-FKNUM.
        ENDIF.
      ENDIF.

      IF WA_ZLEST0034-FKNUM IS NOT INITIAL AND SY-SUBRC IS INITIAL.

        READ TABLE E_CTE_NIT ASSIGNING <NIT> INDEX 1.

        E_CTE_DISTR-ZVLR_FRETE = E_CTE_DISTR-VALOR_RECEBER.
        E_CTE_N55-ZVLR_FRETE   = E_CTE_DISTR-VALOR_RECEBER.
        E_CTE_N01-ZVLR_FRETE   = E_CTE_DISTR-VALOR_RECEBER.
        <NIT>-ZVLR_FRETE       = E_CTE_DISTR-VALOR_RECEBER.

        E_CTE_DISTR-ZVLR_VI    = 0.
        E_CTE_N55-ZVLR_VI      = 0.
        E_CTE_N01-ZVLR_VI      = 0.
        <NIT>-ZVLR_VI          = 0.

        LOOP AT IT_VFKP INTO WA_VFKP.
          ADD WA_VFKP-NETWR TO E_CTE_N55-ZVLR_VI.
          ADD WA_VFKP-NETWR TO E_CTE_N01-ZVLR_VI.
          ADD WA_VFKP-NETWR TO <NIT>-ZVLR_VI.
          ADD WA_VFKP-NETWR TO E_CTE_DISTR-ZVLR_VI.
        ENDLOOP.
      ELSE.
        IF WA_ZLEST0034-FKNUM IS INITIAL.
          MESSAGE E072 WITH P_TKNUM RAISING NAO_ACHOU.
        ELSE.
          MESSAGE E164 WITH WA_ZLEST0034-FKNUM P_TKNUM RAISING NAO_ACHOU.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_PESO_TIP_FRETE.


    DATA: WA_TIP_FRETE TYPE ZPFE_LOTE_ITEM,
          IT_TIP_FRETE TYPE TABLE OF ZPFE_LOTE_ITEM.

    IF P_DOCNUM IS NOT INITIAL.

      SELECT * INTO TABLE IT_TIP_FRETE
        FROM ZPFE_LOTE_ITEM
       WHERE DOCNUM  EQ P_DOCNUM
         AND CHVID   EQ '2'
         AND STATUS  EQ 'C'.

      E_CTE-PESO_ORIGEM  = 0.
      E_CTE-PESO_CHEGADA = 0.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_TIP_FRETE INTO WA_TIP_FRETE INDEX 1.
        E_CTE-DT_CHEGADA      = WA_TIP_FRETE-DT_CHEGADA.
        E_CTE-CK_PESO_CHEGADA = TRUE.
        LOOP AT IT_TIP_FRETE INTO WA_TIP_FRETE.
          ADD WA_TIP_FRETE-PESO_ORIGEM  TO E_CTE-PESO_ORIGEM.
          ADD WA_TIP_FRETE-PESO_CHEGADA TO E_CTE-PESO_CHEGADA.
        ENDLOOP.
      ELSE.
        MESSAGE E063 RAISING NAO_ACHOU.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_PESO_TIP_FRETE_VT.


    "As informações de peso/perda/quebra/valor icms/valor pis/valor cofins/liquido a pagar são por Doc. de Transporte
    "Deve ser buscado esta informação por VT vinculada ao CT-e. Ex.: Docnum 6317854 Mod 57

    DATA: WA_TIP_FRETE TYPE ZPFE_LOTE_ITEM.

    FIELD-SYMBOLS: <NIT> TYPE ZIB_CTE_DIST_NIT.

    CHECK P_TKNUM IS NOT INITIAL.

    SELECT SINGLE * INTO WA_TIP_FRETE
      FROM ZPFE_LOTE_ITEM
     WHERE CHVID   EQ '2'
       AND STATUS  EQ 'C'
       AND TKNUM EQ P_TKNUM.

    IF SY-SUBRC IS INITIAL.

      E_CTE_N55-ZVLR_FRETE       = WA_TIP_FRETE-VL_TRANSACAO.
      "E_CTE_N55-ZVLR_MERCADORIA  = 0.
      "E_CTE_N55-ZVLR_QUEBRA      = 0.
      "E_CTE_N55-ZVLR_PERDA       = 0.
      E_CTE_N55-ZVLR_LIQ_PAGAR   = WA_TIP_FRETE-VL_TRANSACAO.
      E_CTE_N55-CK_PESO_DIGITADO = TRUE.

      E_CTE_N01-ZVLR_FRETE       = WA_TIP_FRETE-VL_TRANSACAO.
      "E_CTE_N01-ZVLR_MERCADORIA  = 0.
      "E_CTE_N01-ZVLR_QUEBRA      = 0.
      "E_CTE_N01-ZVLR_PERDA       = 0.
      E_CTE_N01-ZVLR_LIQ_PAGAR   = WA_TIP_FRETE-VL_TRANSACAO.
      E_CTE_N01-CK_PESO_DIGITADO = TRUE.

      "Antes não era informado os valores por item, com isso está sendo atribuido somente co primeiro.
      READ TABLE E_CTE_NIT ASSIGNING <NIT> INDEX 1.

      <NIT>-ZVLR_FRETE       = WA_TIP_FRETE-VL_TRANSACAO.
      <NIT>-ZVLR_MERCADORIA  = 0.
      <NIT>-PESO_ORIGEM      = WA_TIP_FRETE-PESO_ORIGEM.
      <NIT>-PESO_CHEGADA     = WA_TIP_FRETE-PESO_CHEGADA.
      <NIT>-ZPESO_DIFERENCA  = 0.
      <NIT>-ZQUEBRA          = 0.
      <NIT>-ZPERDA           = 0.
      <NIT>-ZVLR_QUEBRA      = 0.
      <NIT>-ZVLR_PERDA       = 0.
      <NIT>-ZVLR_LIQ_PAGAR   = WA_TIP_FRETE-VL_TRANSACAO.
      <NIT>-ZVLR_KG_TRANSP   = 0.
      <NIT>-ZVLR_KG_MERCAD   = 0.

      LOOP AT E_CTE_NIT ASSIGNING <NIT>.
        <NIT>-CK_PESO_DIGITADO = TRUE.
      ENDLOOP.

    ELSE.
      MESSAGE E072 WITH P_TKNUM RAISING NAO_ACHOU.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_PROXIMO_VENC_FATURA.


    DATA: E_DATA_FINAL TYPE DATUM,
          IT_DATAS     TYPE TABLE OF ISCAL_DAY,
          WA_DATAS     TYPE ISCAL_DAY,
          VDIAS        TYPE I.

    E_DATA_VENCIMENTO = SY-DATUM - 1.
    VDIAS             = 0.

    "72 Horas úteis (3 dias)
    WHILE VDIAS <= 3.

      CLEAR: IT_DATAS[], IT_DATAS.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = E_DATA_VENCIMENTO
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = E_DATA_VENCIMENTO.

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          HOLIDAY_CALENDAR = 'MG'
          FACTORY_CALENDAR = 'ZT'
          DATE_FROM        = E_DATA_VENCIMENTO
          DATE_TO          = E_DATA_VENCIMENTO
        TABLES
          HOLIDAYS         = IT_DATAS
        EXCEPTIONS
          OTHERS           = 1.

      IF IT_DATAS[] IS INITIAL.
        ADD 1 TO VDIAS.
      ELSE.
        CLEAR: IT_DATAS[].
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD BUSCA_TEXTO_CIDADE.


    DATA: WA_J_1BTXJURT TYPE J_1BTXJURT.

    CHECK ( P_UF IS NOT INITIAL ) AND ( P_IBGE IS NOT INITIAL ).

    CLEAR: E_NOME_CIDADE.

    CONCATENATE P_UF P_IBGE INTO WA_J_1BTXJURT-TAXJURCODE SEPARATED BY SPACE.

    SELECT SINGLE * INTO WA_J_1BTXJURT
      FROM J_1BTXJURT
     WHERE SPRAS      EQ SY-LANGU
       AND COUNTRY    EQ P_COUNTRY
       AND TAXJURCODE EQ WA_J_1BTXJURT-TAXJURCODE.

    IF SY-SUBRC IS INITIAL.
      E_NOME_CIDADE = WA_J_1BTXJURT-TEXT.
    ELSE.
      MESSAGE E074 WITH SY-LANGU P_COUNTRY WA_J_1BTXJURT-TAXJURCODE RAISING NAO_ACHOU.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_TOLERANCIA.


    DATA: WA_A912   TYPE A912,
          WA_KONP   TYPE KONP,
          VG_MATNR2 TYPE ZMATNR.

    E_TOLERANCIA = 0.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_COD_MERCADORIA
      IMPORTING
        OUTPUT = VG_MATNR2.

    SELECT SINGLE * INTO WA_A912 FROM A912 WHERE KSCHL EQ 'ZMRG' AND MATNR EQ VG_MATNR2.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE * INTO WA_KONP FROM KONP WHERE KNUMH EQ WA_A912-KNUMH.
      IF SY-SUBRC IS INITIAL.
        E_TOLERANCIA = WA_KONP-KBETR / 10.
      ELSE.
        MESSAGE E076 WITH P_COD_MERCADORIA RAISING NAO_ACHOU.
      ENDIF.
    ELSE.
      MESSAGE E076 WITH P_COD_MERCADORIA RAISING NAO_ACHOU.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_TOMADOR_SERVICO.


    DATA: WA_LFA1 TYPE LFA1.

    CASE P_CTE_DIST-CD_TOMADOR.
      WHEN '0'. "Remetente  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        MOVE: P_CTE_DIST-REME_CNPJ TO E_J_1BBRANCH-STCD1,
              P_CTE_DIST-REME_IE   TO E_J_1BBRANCH-STATE_INSC.
      WHEN '1'. "Expedidor ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        MOVE: P_CTE_DIST-EXPED_CNPJ TO E_J_1BBRANCH-STCD1,
              P_CTE_DIST-EXPED_IE   TO E_J_1BBRANCH-STATE_INSC.
      WHEN '2'. "Receberdor ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        MOVE: P_CTE_DIST-RECEB_CNPJ TO E_J_1BBRANCH-STCD1,
              P_CTE_DIST-RECEB_IE   TO E_J_1BBRANCH-STATE_INSC.
      WHEN '3'. "Destinatário ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        MOVE: P_CTE_DIST-DEST_CNPJ TO E_J_1BBRANCH-STCD1,
              P_CTE_DIST-DEST_IE   TO E_J_1BBRANCH-STATE_INSC.
      WHEN '4'. "Outros    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        MOVE: P_CTE_DIST-TOMA4_CNPJ TO E_J_1BBRANCH-STCD1,
              P_CTE_DIST-TOMA4_IE   TO E_J_1BBRANCH-STATE_INSC.
    ENDCASE.

    IF E_J_1BBRANCH-STATE_INSC IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = E_J_1BBRANCH-STATE_INSC
        IMPORTING
          OUTPUT = E_J_1BBRANCH-STATE_INSC.

      CONCATENATE '%' E_J_1BBRANCH-STATE_INSC INTO E_J_1BBRANCH-STATE_INSC.
    ENDIF.

    SELECT SINGLE * INTO E_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE STCD1      EQ   E_J_1BBRANCH-STCD1
       AND STATE_INSC LIKE E_J_1BBRANCH-STATE_INSC
       AND BRANCH     NE   '0001'. "Local de Negócio identificar CNPJ matriz para FI

    IF SY-SUBRC IS INITIAL.
      MESSAGE S016 WITH E_J_1BBRANCH-STCD1 E_J_1BBRANCH-STATE_INSC.
    ELSE.
      WA_LFA1-STCD1 = E_J_1BBRANCH-STCD1.
      WA_LFA1-STCD3 = E_J_1BBRANCH-STATE_INSC.

      SELECT SINGLE * INTO WA_LFA1
        FROM LFA1
       WHERE STCD1 EQ   WA_LFA1-STCD1
         AND STCD3 LIKE WA_LFA1-STCD3.

      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_LFA1-LIFNR
          IMPORTING
            OUTPUT = WA_LFA1-LIFNR.

        E_J_1BBRANCH-BRANCH = WA_LFA1-LIFNR+6(4).

        SELECT SINGLE * INTO E_J_1BBRANCH
          FROM J_1BBRANCH
         WHERE BRANCH EQ E_J_1BBRANCH-BRANCH.

        IF SY-SUBRC IS INITIAL.
          MESSAGE S016 WITH E_J_1BBRANCH-STCD1 E_J_1BBRANCH-STATE_INSC.
        ELSE.
          MESSAGE E015 WITH E_J_1BBRANCH-STCD1 E_J_1BBRANCH-STATE_INSC RAISING NAO_ACHOU_PARCEIRO.
        ENDIF.
      ELSE.
        MESSAGE E015 WITH E_J_1BBRANCH-STCD1 E_J_1BBRANCH-STATE_INSC RAISING NAO_ACHOU_PARCEIRO.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_TROCA_NF_43.


    DATA: E_DOCNUM  TYPE J_1BDOCNUM,
          IT_0032   TYPE TABLE OF ZLEST0032,
          IT_VTTP   TYPE TABLE OF VTTP,
          IT_VBFA   TYPE TABLE OF VBFA,
          WA_VBFA   TYPE VBFA,
          IT_BNFLIN TYPE TABLE OF J_1BNFLIN,
          WA_BNFLIN TYPE J_1BNFLIN,
          IT_ACTIVE TYPE TABLE OF J_1BNFE_ACTIVE,
          WA_ACTIVE TYPE J_1BNFE_ACTIVE,
          WA_N55    TYPE ZIB_CTE_DIST_N55.

    CALL METHOD ME->BUSCA_DOCNUM_CHAVE
      EXPORTING
        P_CHAVE     = CD_CHAVE_CTE
        P_PSQ_CHAVE = TRUE
        P_FORM      = FALSE
      CHANGING
        E_DOCNUM    = E_DOCNUM
      EXCEPTIONS
        NAO_ACHOU   = 1
        OTHERS      = 2.

    IF E_DOCNUM IS NOT INITIAL.

      SELECT * INTO TABLE IT_0032
        FROM ZLEST0032
       WHERE DOCNUM EQ E_DOCNUM.

      CHECK SY-SUBRC IS INITIAL.

      SELECT * INTO TABLE IT_VTTP
        FROM VTTP
         FOR ALL ENTRIES IN IT_0032
       WHERE TKNUM EQ IT_0032-TKNUM.

      CHECK SY-SUBRC IS INITIAL.

      SELECT * INTO TABLE IT_VBFA
        FROM VBFA
         FOR ALL ENTRIES IN IT_VTTP
       WHERE VBELV   EQ IT_VTTP-VBELN
         AND VBTYP_N EQ 'M'
         AND VBTYP_V EQ 'J'.

      CHECK SY-SUBRC IS INITIAL.

      LOOP AT IT_VBFA INTO WA_VBFA.
        CLEAR: WA_BNFLIN.
        WA_BNFLIN-REFTYP = 'BI'.
        WA_BNFLIN-REFKEY = WA_VBFA-VBELN.
        WA_BNFLIN-REFITM = WA_VBFA-POSNN.
        APPEND WA_BNFLIN TO IT_BNFLIN.
      ENDLOOP.

      SELECT * INTO TABLE IT_BNFLIN
        FROM J_1BNFLIN
         FOR ALL ENTRIES IN IT_BNFLIN
       WHERE REFTYP EQ IT_BNFLIN-REFTYP
         AND REFKEY EQ IT_BNFLIN-REFKEY
         AND REFITM EQ IT_BNFLIN-REFITM.

      CHECK SY-SUBRC IS INITIAL.

      SELECT * INTO TABLE IT_ACTIVE
        FROM J_1BNFE_ACTIVE
         FOR ALL ENTRIES IN IT_BNFLIN
       WHERE DOCNUM EQ IT_BNFLIN-DOCNUM.

      CHECK SY-SUBRC IS INITIAL.

      LOOP AT IT_ACTIVE INTO WA_ACTIVE.
        WA_N55-CD_CHAVE_CTE   = CD_CHAVE_CTE.
        WA_N55-DOCNUM_NFE     = WA_ACTIVE-DOCNUM.
        WA_N55-N55_STAT_SEFAZ = WA_ACTIVE-CODE.
        CONCATENATE WA_ACTIVE-REGIO
                    WA_ACTIVE-NFYEAR
                    WA_ACTIVE-NFMONTH
                    WA_ACTIVE-STCD1
                    WA_ACTIVE-MODEL
                    WA_ACTIVE-SERIE
                    WA_ACTIVE-NFNUM9
                    WA_ACTIVE-DOCNUM9
                    WA_ACTIVE-CDV INTO WA_N55-N55_CHAVE_ACESSO.
        APPEND WA_N55 TO E_N55.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD BUSCA_VEICULOS.


    SELECT * INTO TABLE E_VEI_T
      FROM ZIB_CTE_DIST_VEI
     WHERE CD_CHAVE_CTE EQ P_CHAVE_CTE.

  ENDMETHOD.


  METHOD BUSCA_VOLUME_UTILIZADO_FERRO.


    DATA: WA_ZLEST0119              TYPE ZLEST0119,
          IT_ZLEST0120              TYPE TABLE OF ZLEST0120,
          WA_ZLEST0120              TYPE ZLEST0120,
          WA_LFA1                   TYPE LFA1,
          IT_ZLEST0044              TYPE TABLE OF ZLEST0044,
          WA_ZLEST0044              TYPE ZLEST0044,
          IT_ZLEST0128              TYPE TABLE OF ZLEST0128,
          WA_ZLEST0128              TYPE ZLEST0128,
          WA_QTD_UTILIZADA_EMP      TYPE ZDE_SALDO_UTIL_EMP,
          WA_QTD_UTILIZADA_PRD      TYPE ZDE_SALDO_UTIL_PRO,
          WA_QTD_UTILIZADA_EMP_PROD TYPE ZDE_SALDO_UTIL_EP.

    DATA: LC_CIDADE_ORIGEM  TYPE ZLEST0044-CIDADE_ORIGEM,
          LC_UF_ORIGEM      TYPE ZLEST0044-UF_ORIGEM,
          LC_CIDADE_DESTINO TYPE ZLEST0044-CIDADE_DESTINO,
          LC_UF_DESTINO     TYPE ZLEST0044-UF_DESTINO,
          LC_TARIFA	        TYPE ZLEST0044-TARIFA.

    E_QTD_UTILIZADA = 0.

    SELECT SINGLE * INTO WA_ZLEST0119
      FROM ZLEST0119
     WHERE CD_SEQ_LANC EQ I_CD_SEQ_LANC.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE IT_ZLEST0120
      FROM ZLEST0120
     WHERE CD_SEQ_LANC EQ I_CD_SEQ_LANC.

    SELECT * INTO TABLE IT_ZLEST0128
      FROM ZLEST0128
     WHERE CD_SEQ_LANC EQ I_CD_SEQ_LANC.

    SELECT SINGLE * INTO WA_LFA1
      FROM LFA1
     WHERE LIFNR EQ WA_ZLEST0119-LIFNR.

    CHECK SY-SUBRC IS INITIAL.

    SPLIT WA_ZLEST0119-DOMICILIO_ORIGEM AT ' ' INTO LC_UF_ORIGEM LC_CIDADE_ORIGEM.
    SPLIT WA_ZLEST0119-DOMICILIO_DESTIN AT ' ' INTO LC_UF_DESTINO LC_CIDADE_DESTINO.

    LC_TARIFA = WA_ZLEST0119-PRECO.

*---- Modificação 09.12.2016 - Início

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_ZLEST0044
      FROM ZLEST0044
      "INNER JOIN ZIB_CTE_DIST_NIT
      "ON ( ZLEST0044~CHAVE_CTE = ZIB_CTE_DIST_NIT~CD_CHAVE_CTE )
    WHERE ZLEST0044~CNPJ_EMITENTE        EQ WA_LFA1-STCD1
       AND ZLEST0044~IE_EMITENTE         EQ WA_LFA1-STCD3
       AND ZLEST0044~DT_REFERENCIA       GE WA_ZLEST0119-DT_INICIO
       AND ZLEST0044~DT_REFERENCIA       LE WA_ZLEST0119-DT_FIM
       AND ZLEST0044~CIDADE_ORIGEM       EQ LC_CIDADE_ORIGEM
       AND ZLEST0044~UF_ORIGEM           EQ LC_UF_ORIGEM
       AND ZLEST0044~CIDADE_DESTINO      EQ LC_CIDADE_DESTINO
       AND ZLEST0044~UF_DESTINO          EQ LC_UF_DESTINO
       AND ZLEST0044~TARIFA              EQ LC_TARIFA
       AND ZLEST0044~NR_TRANS            NE SPACE
       AND ZLEST0044~NR_FRETE            NE SPACE.

*        !E_QTD_UTILIZADA_EMP      TYPE ZDE_SALDO_UTIL_EMP_T
*        !E_QTD_UTILIZADA_PRD      TYPE ZDE_SALDO_UTIL_PRO_T
*        !E_QTD_UTILIZADA_EMP_PROD TYPE ZDE_SALDO_UTIL_EP_T.

    CLEAR: E_QTD_UTILIZADA_EMP,
           E_QTD_UTILIZADA_PRD,
           E_QTD_UTILIZADA_EMP_PROD.

    "Percorrendo Faturas
    LOOP AT IT_ZLEST0044 INTO WA_ZLEST0044.

      "Procura Empresa no TOP
      READ TABLE IT_ZLEST0120 INTO WA_ZLEST0120 WITH KEY BUKRS = WA_ZLEST0044-BUKRS.
      IF SY-SUBRC IS INITIAL.
        "Procura Material no TOP
        READ TABLE IT_ZLEST0128 INTO WA_ZLEST0128 WITH KEY MATNR = WA_ZLEST0044-MATNR_FATURADO.
        IF SY-SUBRC IS INITIAL.
          ADD WA_ZLEST0044-PESO_BRUTO TO E_QTD_UTILIZADA .

          "Somando Empresa
          READ TABLE E_QTD_UTILIZADA_EMP WITH KEY BUKRS = WA_ZLEST0044-BUKRS ASSIGNING FIELD-SYMBOL(<LS_QTD_UTIL_EMP>).
          IF SY-SUBRC IS INITIAL.
            ADD WA_ZLEST0044-PESO_BRUTO TO <LS_QTD_UTIL_EMP>-PESO_BRUTO.
          ELSE.
            WA_QTD_UTILIZADA_EMP-BUKRS      = WA_ZLEST0044-BUKRS.
            WA_QTD_UTILIZADA_EMP-PESO_BRUTO = WA_ZLEST0044-PESO_BRUTO.
            APPEND WA_QTD_UTILIZADA_EMP TO E_QTD_UTILIZADA_EMP.
          ENDIF.

          "Somando Material
          READ TABLE E_QTD_UTILIZADA_PRD WITH KEY MATNR = WA_ZLEST0044-MATNR_FATURADO ASSIGNING FIELD-SYMBOL(<LS_QTD_UTIL_PRD>).
          IF SY-SUBRC IS INITIAL.
            ADD WA_ZLEST0044-PESO_BRUTO TO <LS_QTD_UTIL_PRD>-PESO_BRUTO.
          ELSE.
            WA_QTD_UTILIZADA_PRD-MATNR      = WA_ZLEST0044-MATNR_FATURADO.
            WA_QTD_UTILIZADA_PRD-PESO_BRUTO = WA_ZLEST0044-PESO_BRUTO.
            APPEND WA_QTD_UTILIZADA_PRD TO E_QTD_UTILIZADA_PRD.
          ENDIF.

          "Somando Material
          READ TABLE E_QTD_UTILIZADA_EMP_PROD WITH KEY BUKRS = WA_ZLEST0044-BUKRS MATNR = WA_ZLEST0044-MATNR_FATURADO ASSIGNING FIELD-SYMBOL(<LS_QTD_UTIL_EMP_PRD>).
          IF SY-SUBRC IS INITIAL.
            ADD WA_ZLEST0044-PESO_BRUTO TO <LS_QTD_UTIL_EMP_PRD>-PESO_BRUTO.
          ELSE.
            WA_QTD_UTILIZADA_EMP_PROD-BUKRS      = WA_ZLEST0044-BUKRS.
            WA_QTD_UTILIZADA_EMP_PROD-MATNR      = WA_ZLEST0044-MATNR_FATURADO.
            WA_QTD_UTILIZADA_EMP_PROD-PESO_BRUTO = WA_ZLEST0044-PESO_BRUTO.
            APPEND WA_QTD_UTILIZADA_EMP_PROD TO E_QTD_UTILIZADA_EMP_PROD.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    IF WA_ZLEST0119-UND_NEGOCIADO EQ 'TO'.
      DIVIDE E_QTD_UTILIZADA BY 1000.
    ENDIF.

  ENDMETHOD.


  METHOD CALCULA_QUEBRA_PERDA.


    "Diferença de Peso
    E_PESO_DIFERENCA = P_PESO_ORIGEM - P_PESO_DESTINO.
    E_VLR_LIQ_PAGAR  = 0.
    E_PC_QUEBRA      = 0.
    E_PC_TOLERANCIA  = 0.

    IF P_PESO_ORIGEM GT P_PESO_DESTINO.
      E_PC_QUEBRA = ( E_PESO_DIFERENCA * 100 ) / P_PESO_ORIGEM.

      CALL METHOD ZCL_CTE_DIST_G=>BUSCA_TOLERANCIA
        EXPORTING
          P_COD_MERCADORIA = P_COD_MERCADORIA
        IMPORTING
          E_TOLERANCIA     = E_PC_TOLERANCIA
        EXCEPTIONS
          NAO_ACHOU        = 1
          OTHERS           = 2.

      IF E_PC_QUEBRA > E_PC_TOLERANCIA.
        "Quebra e Perda
        E_PESO_PERDA   = ( ( E_PC_QUEBRA - E_PC_TOLERANCIA ) * P_PESO_ORIGEM ) / 100.
        E_VLR_PERDA    = E_PESO_PERDA * P_VLR_KG_MERCADORIA.
        E_PESO_QUEBRA  = E_PESO_DIFERENCA.
        E_VLR_QUEBRA   = E_PESO_DIFERENCA * P_VLR_KG_TRASPORT.
      ELSE.
        "Somente Quebra
        E_PESO_PERDA   = 0.
        E_VLR_PERDA    = 0.
        E_PESO_QUEBRA  = E_PESO_DIFERENCA.
        E_VLR_QUEBRA   = E_PESO_DIFERENCA * P_VLR_KG_TRASPORT.
      ENDIF.

    ELSEIF P_PESO_ORIGEM LE P_PESO_DESTINO.
      "Somente Quebra
      E_PESO_PERDA   = 0.
      E_VLR_PERDA    = 0.
      E_PESO_QUEBRA  = E_PESO_DIFERENCA.
      E_VLR_QUEBRA   = E_PESO_DIFERENCA * P_VLR_KG_TRASPORT.
    ELSE.
      CLEAR: E_PESO_QUEBRA, E_PESO_PERDA, E_VLR_QUEBRA, E_VLR_PERDA.
    ENDIF.

    IF E_VLR_QUEBRA LT 0.
      E_VLR_LIQ_PAGAR  = P_VLR_FRETE - E_VLR_PERDA.
    ELSE.
      E_VLR_LIQ_PAGAR  = P_VLR_FRETE - E_VLR_QUEBRA - E_VLR_PERDA.
    ENDIF.

  ENDMETHOD.


  METHOD DACTE.

    TRY.
      ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~IMPRIMIR_DOCUMENTO_AUX(
        EXPORTING
          I_CHAVE            =  CONV #( I_CTE ) ).
    CATCH ZCX_DOC_ELETRONICO.
      RAISE EXCEPTION TYPE ZCX_DOC_ELETRONICO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_DOC_ELETRONICO=>ZCX_DOC_CABE_NAO_ENC-MSGID MSGNO = ZCX_DOC_ELETRONICO=>ZCX_DOC_CABE_NAO_ENC-MSGNO ATTR1 = CONV #( I_CTE ) )
          MSGID  = ZCX_DOC_ELETRONICO=>ZCX_DOC_CABE_NAO_ENC-MSGID
          MSGNO  = ZCX_DOC_ELETRONICO=>ZCX_DOC_CABE_NAO_ENC-MSGNO
          MSGV1  = CONV #( I_CTE )
          MSGTY  = 'E'.
    ENDTRY.

*    DATA: LC_NODE_DATA TYPE BXMNODES-URL.
*
*    SELECT SINGLE * INTO @DATA(WA_CONHECIMENTO)
*      FROM ZIB_CTE_DIST_TER
*     WHERE CD_CHAVE_CTE EQ @I_CTE.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_CTE_INBOUND
*        EXPORTING
*          TEXTID = VALUE #(  MSGNO = ZCX_CTE_INBOUND=>ZCX_CHAVE_NAO_ENCONTRADA-MSGNO
*                             MSGID = ZCX_CTE_INBOUND=>ZCX_CHAVE_NAO_ENCONTRADA-MSGID
*                             ATTR1 = CONV #( I_CTE ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_CTE_INBOUND=>ZCX_CHAVE_NAO_ENCONTRADA-MSGNO
*          MSGID  = ZCX_CTE_INBOUND=>ZCX_CHAVE_NAO_ENCONTRADA-MSGID
*          MSGV1  = CONV #( I_CTE ).
*    ENDIF.
*
*    IF WA_CONHECIMENTO-ID_SIMETRYA IS INITIAL.
*      RAISE EXCEPTION TYPE ZCX_CTE_INBOUND
*        EXPORTING
*          TEXTID = VALUE #(  MSGNO = ZCX_CTE_INBOUND=>ZCX_SEM_ID_SIMETRYA-MSGNO
*                             MSGID = ZCX_CTE_INBOUND=>ZCX_SEM_ID_SIMETRYA-MSGID
*                             ATTR1 = CONV #( I_CTE ) )
*          MSGTY  = 'E'
*          MSGNO  = ZCX_CTE_INBOUND=>ZCX_SEM_ID_SIMETRYA-MSGNO
*          MSGID  = ZCX_CTE_INBOUND=>ZCX_SEM_ID_SIMETRYA-MSGID
*          MSGV1  = CONV #( I_CTE ).
*    ELSE.
*
*      DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
*      CREATE OBJECT OB_WEB_SERVICE.
*
*      TRY .
*          OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'YC' ).
*          OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'Y' ).
*          "http://simetrya.grupomaggi.com.br:8080/cte/cteTerceirosDactePdf?numrCteSeqc=
*          DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
*
*        CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
*          RAISE EXCEPTION TYPE ZCX_CTE_INBOUND
*            EXPORTING
*              TEXTID = VALUE #(  MSGNO = ZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGNO
*                                 MSGID = ZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGID )
*              MSGTY  = 'E'
*              MSGNO  = ZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGNO
*              MSGID  = ZCX_CTE_INBOUND=>ZCX_SEM_WEBSERVICE_DACTE-MSGID.
*      ENDTRY.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          INPUT  = WA_CONHECIMENTO-ID_SIMETRYA
*        IMPORTING
*          OUTPUT = WA_CONHECIMENTO-ID_SIMETRYA.
*
*      LC_NODE_DATA = LC_URI && WA_CONHECIMENTO-ID_SIMETRYA.
*
*      CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
*        EXPORTING
*          NODE_DATA = LC_NODE_DATA.
*
*    ENDIF.

  ENDMETHOD.


  METHOD ESTORNAR_DOC_CUSTO.


    DATA: WA_VFKP   TYPE VFKP,
          IT_SHDB	  TYPE BDCDATA_TAB,
          IT_MSG    TYPE TABLE OF BDCMSGCOLL,
          WA_MSG    TYPE BDCMSGCOLL,
          WA_RETURN TYPE BAPIRET2,
          LC_MODE   TYPE C LENGTH 1.

    CLEAR: IT_SHDB.

    SELECT SINGLE * FROM VFKP INTO WA_VFKP WHERE FKNUM = I_FKNUM.

    CHECK SY-SUBRC IS INITIAL.

    "Não tem folha de aceite
    IF WA_VFKP-LBLNI IS INITIAL.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0020'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '=UEBP'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'VFKK-FKNUM'
          P_VALUE    = I_FKNUM
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0030'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '/ELOES'
        CHANGING
          E_SHDB     = IT_SHDB.

      LC_MODE = 'N'.
      CALL TRANSACTION 'VI02' USING IT_SHDB MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MSG.
      COMMIT WORK AND WAIT.

    ELSE.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0020'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '=UEBP'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'VFKK-FKNUM'
          P_VALUE    = I_FKNUM
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0030'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_CURSOR'
          P_VALUE    = 'VFKP-FKPOS(01)'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '=PDET'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0040'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '=PABR'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'VFKP-POSTX'
          P_VALUE    = WA_VFKP-POSTX
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = TRUE
          P_NAME     = 'SAPMV54A'
          P_VALUE    = '0040'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'BDC_OKCODE'
          P_VALUE    = '=SICH'
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'VFKP-POSTX'
          P_VALUE    = WA_VFKP-POSTX
        CHANGING
          E_SHDB     = IT_SHDB.

      CALL METHOD ME->MONTA_SHDB
        EXPORTING
          P_DYNBEGIN = FALSE
          P_NAME     = 'VFKPD-SLSTOR'
          P_VALUE    = TRUE
        CHANGING
          E_SHDB     = IT_SHDB.

      LC_MODE = 'N'.
      CALL TRANSACTION 'VI02' USING IT_SHDB MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MSG.
      COMMIT WORK AND WAIT.
    ENDIF.

    LOOP AT IT_MSG INTO WA_MSG.
      MOVE: WA_MSG-MSGTYP TO WA_RETURN-TYPE,
            WA_MSG-MSGID  TO WA_RETURN-ID,
            WA_MSG-MSGNR  TO WA_RETURN-NUMBER,
            WA_MSG-MSGV1  TO WA_RETURN-MESSAGE_V1,
            WA_MSG-MSGV2  TO WA_RETURN-MESSAGE_V2,
            WA_MSG-MSGV3  TO WA_RETURN-MESSAGE_V3,
            WA_MSG-MSGV4  TO WA_RETURN-MESSAGE_V4.

      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = P_CHAVE_CTE
          P_TYPE         = WA_RETURN-TYPE
          P_ID           = WA_RETURN-ID
          P_NUM          = WA_RETURN-NUMBER
          P_MESSAGE_V1   = WA_RETURN-MESSAGE_V1
          P_MESSAGE_V2   = WA_RETURN-MESSAGE_V2
          P_MESSAGE_V3   = WA_RETURN-MESSAGE_V3
          P_MESSAGE_V4   = WA_RETURN-MESSAGE_V4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.
    ENDLOOP.

  ENDMETHOD.


  METHOD ESTORNAR_DOC_TRANSPORTE.


    DATA: WA_HEADERDATA       TYPE BAPISHIPMENTHEADER,
          WA_HEADERDATAACTION TYPE BAPISHIPMENTHEADERACTION,
          WA_ITEMDATA         TYPE BAPISHIPMENTITEM,
          IT_ITEMDATA         TYPE TABLE OF BAPISHIPMENTITEM,
          WA_VTTP             TYPE VTTP,
          WA_ITEMDATAACTION   TYPE BAPISHIPMENTITEMACTION,
          IT_ITEMDATAACTION   TYPE TABLE OF BAPISHIPMENTITEMACTION,
          IT_RETURN           TYPE TABLE OF BAPIRET2,
          WA_RETURN           TYPE BAPIRET2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_TKNUM
      IMPORTING
        OUTPUT = WA_HEADERDATA-SHIPMENT_NUM.

    WA_HEADERDATAACTION-SHIPMENT_NUM     = 'D'.
    WA_HEADERDATAACTION-SERVICE_AGENT_ID = 'D'.

    SELECT * INTO WA_VTTP
      FROM VTTP
     WHERE TKNUM EQ I_TKNUM.

      WA_ITEMDATA-DELIVERY  = WA_VTTP-VBELN.
      WA_ITEMDATA-ITENERARY = WA_VTTP-TPNUM.
      APPEND WA_ITEMDATA TO IT_ITEMDATA.

      WA_ITEMDATAACTION-DELIVERY  = 'D'.
      WA_ITEMDATAACTION-ITENERARY = 'D'.
      APPEND WA_ITEMDATAACTION TO IT_ITEMDATAACTION.
    ENDSELECT.

    CLEAR: IT_RETURN, IT_RETURN[].

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        HEADERDATA       = WA_HEADERDATA
        HEADERDATAACTION = WA_HEADERDATAACTION
      TABLES
        ITEMDATA         = IT_ITEMDATA
        ITEMDATAACTION   = IT_ITEMDATAACTION
        RETURN           = IT_RETURN.

    LOOP AT IT_RETURN INTO WA_RETURN.
      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = P_CHAVE_CTE
          P_TYPE         = WA_RETURN-TYPE
          P_ID           = WA_RETURN-ID
          P_NUM          = WA_RETURN-NUMBER
          P_MESSAGE_V1   = WA_RETURN-MESSAGE_V1
          P_MESSAGE_V2   = WA_RETURN-MESSAGE_V2
          P_MESSAGE_V3   = WA_RETURN-MESSAGE_V3
          P_MESSAGE_V4   = WA_RETURN-MESSAGE_V4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.
    ENDLOOP.

  ENDMETHOD.


  METHOD GERAR_COMPENSACAO.


    DATA: WA_BKPF TYPE BKPF.

    CALL FUNCTION 'Z_FI_GL_COMPENSA_ESTORNO_MIRO'
      EXPORTING
        E_BUKRS       = P_CTE-E_TOMADORA
        E_LIFNR       = P_CTE-P_EMISSOR
        E_INVOICE_IN  = E_INVOICE_IN
        E_YEAR_IN     = E_YEAR_IN
        E_INVOICE_OUT = E_INVOICE_OUT
        E_YEAR_OUT    = E_YEAR_OUT
      IMPORTING
        I_BKPF        = WA_BKPF
      EXCEPTIONS
        NAO_COMPENSOU = 1
        ERRO_BLOQUEIO = 2
        SEM_ACESSO    = 3
        OTHERS        = 4.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING OUTROS.
    ELSE.
      SY-MSGTY = 'S'.
      SY-MSGID = 'F5'.
      SY-MSGNO = '312'.
      SY-MSGV1 = WA_BKPF-BELNR.
      SY-MSGV2 = WA_BKPF-GJAHR.
    ENDIF.

  ENDMETHOD.


  METHOD GERAR_DOC_CUSTO.


    DATA: IT_SHDB	  TYPE BDCDATA_TAB,
          LC_DATA   TYPE C LENGTH 10,
          WA_MSG    TYPE BDCMSGCOLL,
          IT_MSG    TYPE TABLE OF BDCMSGCOLL,
          WA_RETURN TYPE BAPIRET2,
          LC_MODE   TYPE C LENGTH 1,
          WA_VFKP   TYPE VFKP.

    CLEAR: IT_SHDB.

    CONCATENATE  P_DATA+6(2) '.'  P_DATA+4(2) '.'  P_DATA(4) INTO  LC_DATA.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = 'X'
        P_NAME     = 'SAPMV54A'
        P_VALUE    = '0010'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_CURSOR'
        P_VALUE    = 'VTTK-TKNUM'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_OKCODE'
        P_VALUE    = '=UEBP'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'VTTK-TKNUM'
        P_VALUE    = P_TKNUM
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_CURSOR'
        P_VALUE    = 'VFKK-PRSDT'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_OKCODE'
        P_VALUE    = '=UEBP'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'VFKK-PRSDT'
        P_VALUE    = LC_DATA
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = 'X'
        P_NAME     = 'SAPMV54A'
        P_VALUE    = '0030'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_CURSOR'
        P_VALUE    = 'VFKK-FKNUM'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_OKCODE'
        P_VALUE    = '=SICH'
      CHANGING
        E_SHDB     = IT_SHDB.

    CALL METHOD ME->MONTA_SHDB
      EXPORTING
        P_DYNBEGIN = ' '
        P_NAME     = 'BDC_SUBSCR'
        P_VALUE    = 'SAPMV54A'
      CHANGING
        E_SHDB     = IT_SHDB.

    LC_MODE = 'N'.

    CALL TRANSACTION 'VI01' USING IT_SHDB MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MSG.
    COMMIT WORK AND WAIT.

    SELECT SINGLE * INTO WA_VFKP
      FROM VFKP
     WHERE REBEL = P_TKNUM.

    IF SY-SUBRC IS INITIAL.
      E_FKNUM = WA_VFKP-FKNUM.
    ELSE.
      LOOP AT IT_MSG INTO WA_MSG.

        MOVE: WA_MSG-MSGTYP TO WA_RETURN-TYPE,
              WA_MSG-MSGID  TO WA_RETURN-ID,
              WA_MSG-MSGNR  TO WA_RETURN-NUMBER,
              WA_MSG-MSGV1  TO WA_RETURN-MESSAGE_V1,
              WA_MSG-MSGV2  TO WA_RETURN-MESSAGE_V2,
              WA_MSG-MSGV3  TO WA_RETURN-MESSAGE_V3,
              WA_MSG-MSGV4  TO WA_RETURN-MESSAGE_V4.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CHAVE_CTE
            P_TYPE         = WA_RETURN-TYPE
            P_ID           = WA_RETURN-ID
            P_NUM          = WA_RETURN-NUMBER
            P_MESSAGE_V1   = WA_RETURN-MESSAGE_V1
            P_MESSAGE_V2   = WA_RETURN-MESSAGE_V2
            P_MESSAGE_V3   = WA_RETURN-MESSAGE_V3
            P_MESSAGE_V4   = WA_RETURN-MESSAGE_V4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD gerar_doc_transporte.

    DATA: p_org_point	       TYPE knota,
          p_org_cust         TYPE kunna,
          p_org_suppl	       TYPE lifna,
          p_dest_point       TYPE knotz,
          p_dest_cust	       TYPE kunnz,
          p_dest_suppl       TYPE lifnz,
          p_zlest0044        TYPE zlest0044,
          p_zib_cte_dist_n55 TYPE zib_cte_dist_n55,
          p_tipo_contrato    TYPE sdabw,
          p_notas_transporte TYPE zib_cte_dist_n55_t,
          p_matnr_faturado   TYPE matnr.

    DATA: wa_headerdata       TYPE bapishipmentheader,
          wa_stagedata        TYPE bapishipmentstage,
          it_stagedata        TYPE TABLE OF bapishipmentstage,
          wa_itemdata         TYPE bapishipmentitem,
          it_itemdata         TYPE TABLE OF bapishipmentitem,
          wa_headerdataaction TYPE bapishipmentheaderaction,
          nr_numr_cte         TYPE j_1bnfnum9,
          nr_serie_cte        TYPE zib_cte_dist_ter-numr_serie,
          vnumr_cte(11)       TYPE c,
          vserie_cte          TYPE zib_cte_dist_ter-numr_serie,
          wa_n55              TYPE zib_cte_dist_n55,
          it_return           TYPE TABLE OF bapiret2,
          wa_return           TYPE bapiret2,
          lc_itenerary        TYPE tprfo,
          wa_vfkp             TYPE vfkp,
          wa_vttk             TYPE vttk,
          p_cte               TYPE zib_cte_dist_ter.

    "CLEAR: e_tknum.
"********************************************************************** Start - 178025 CS2023000574 Job dinâmico PSA
      CLEAR: p_org_point, p_org_cust, p_org_suppl, p_dest_point, p_dest_cust, p_dest_suppl,
         p_zlest0044, p_zib_cte_dist_n55, p_tipo_contrato, p_notas_transporte, p_matnr_faturado,
         wa_headerdata, wa_stagedata, it_stagedata, wa_itemdata, it_itemdata, wa_headerdataaction,
         nr_numr_cte, nr_serie_cte, vnumr_cte, vserie_cte, wa_n55, it_return, wa_return,
         lc_itenerary, wa_vfkp, wa_vttk, p_cte.

  REFRESH: it_stagedata, it_itemdata, it_return.
"********************************************************************** Finish
    SELECT SINGLE * INTO p_cte
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte EQ p_cte_chave.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e001 WITH p_cte_chave RAISING doc_transp.
    ENDIF.

    IF p_cte-ck_finalizado EQ true.
      MESSAGE w041 WITH p_cte_chave.
    ENDIF.

    CHECK p_cte-ck_finalizado EQ false.

    SELECT * INTO TABLE it_n55
      FROM zib_cte_dist_n55
     WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

    LOOP AT it_n55 INTO wa_n55.

      IF wa_n55-docnum_nfe IS INITIAL.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_itens)
        FROM j_1bnflin
       WHERE docnum EQ @wa_n55-docnum_nfe.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
      SELECT SINGLE * INTO @DATA(wa_zlest0154)
        FROM zlest0154
       WHERE bukrs EQ @p_cte-e_tomadora
         AND lifnr EQ @p_cte-p_emissor
         AND matkl EQ @wa_itens-matkl.

      IF sy-subrc IS INITIAL.
        DATA(e_tipo_contrato) = '0002'.
      ENDIF.

    ENDLOOP.

    IF p_cte-cd_modal NE '04' AND
       p_cte-cd_modal NE '03' AND
       NOT ( e_tipo_contrato EQ '0002' AND p_cte-cd_modal EQ '06' ) .
      MESSAGE e130 WITH p_cte-cd_modal RAISING doc_transp.
    ENDIF.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto

    IF p_cte-cd_tipo_cte NE '0' AND p_cte-cd_tipo_cte NE '3'.
      MESSAGE e141 RAISING doc_transp.
    ENDIF.

    CALL METHOD me->inicia_sequencia
      EXPORTING
        p_cd_chave_cte = p_cte-cd_chave_cte
      IMPORTING
        p_lc_sequencia = lc_sequencia.

    CASE p_cte-cd_modal .
      WHEN '04' OR '06'.

        SELECT SINGLE * INTO p_zlest0044
          FROM zlest0044
         WHERE chave_cte EQ p_cte-cd_chave_cte.

        IF p_zlest0044-nr_trans IS INITIAL AND p_zlest0044-nr_frete IS INITIAL.

          CALL METHOD me->buscar_info_ferroviario
            EXPORTING
              p_cte            = p_cte
              p_gravar         = 'X'
            IMPORTING
              e_remessas       = p_notas_transporte
              e_org_point      = p_org_point
              e_org_cust       = p_org_cust
              e_org_suppl      = p_org_suppl
              e_dest_point     = p_dest_point
              e_dest_cust      = p_dest_cust
              e_dest_suppl     = p_dest_suppl
              e_zlest0044      = p_zlest0044
              e_tipo_contrato  = p_tipo_contrato
              e_matnr_faturado = p_matnr_faturado
            EXCEPTIONS
              nao_achou        = 1
              OTHERS           = 2.

          IF sy-subrc IS NOT INITIAL.
            wa_return-type       = sy-msgty.
            wa_return-id         = sy-msgid.
            wa_return-number     = sy-msgno.
            wa_return-message_v1 = sy-msgv1.
            wa_return-message_v2 = sy-msgv2.
            wa_return-message_v3 = sy-msgv3.
            wa_return-message_v4 = sy-msgv4.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

            MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4 RAISING doc_transp.
          ENDIF.

        ENDIF.

        "Criar Documentos """"""""""""""""""""""""""""""""""""""""""""""""""""
        IF p_zlest0044-nr_trans IS INITIAL AND p_zlest0044-nr_frete IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_cte-numr_cte
            IMPORTING
              output = nr_numr_cte.

          CLEAR wa_headerdata.

          CASE p_cte-cd_modal.
            WHEN '01'.  "Rodoviário
              wa_headerdata-shipping_type = '01'.
            WHEN '02'.  "Aéreo
              wa_headerdata-shipping_type = '05'.
            WHEN '03'.  "Aquaviário
              wa_headerdata-shipping_type = '03'.
            WHEN '04'.  "Ferroviário
              wa_headerdata-shipping_type = '02'.
              wa_headerdata-shipment_type = p_zlest0044-shtyp.
            WHEN '05'.  "Dutoviário.
              wa_headerdata-shipping_type = ''.
            WHEN '06'.  "Ferroviário - multimodal
              wa_headerdata-shipping_type = '02'.
              wa_headerdata-shipment_type = p_zlest0044-shtyp.
          ENDCASE.

          wa_headerdata-service_agent_id     = p_cte-p_emissor.
          wa_headerdata-trans_plan_pt        = p_cte-f_tomadora.
          wa_headerdata-service_level        = '1'.
          wa_headerdata-external_id_1        = nr_numr_cte."wa_zlest0006-nr_fatura.
          wa_headerdata-status_plan          = true.
          wa_headerdata-status_checkin       = true.
          wa_headerdata-status_load_start    = true.
          wa_headerdata-time_travel          = '30'.
          wa_headerdata-time_total           = '1'.
          wa_headerdata-time_unit            = 'H'.
          wa_headerdata-special_procedure_id = p_tipo_contrato.
          wa_headerdata-shpmnt_cost_rel      = true.

          IF ( p_org_point IS NOT INITIAL ) OR ( p_org_cust IS NOT INITIAL ) OR ( p_org_suppl IS NOT INITIAL ).
            "1  Transporte
            "2  Terminal
            "3  Fronteira
            wa_stagedata-stage_cat  = '1'.
            wa_stagedata-stage_seq  = '0001'.
            wa_stagedata-org_point  = p_org_point.
            wa_stagedata-org_cust   = p_org_cust.
            wa_stagedata-org_suppl  = p_org_suppl.
            wa_stagedata-dest_point = p_dest_point.
            wa_stagedata-dest_cust  = p_dest_cust.
            wa_stagedata-dest_suppl = p_dest_suppl.
            APPEND wa_stagedata TO it_stagedata.
          ENDIF.

          lc_itenerary = 1.

          SELECT * INTO TABLE @DATA(it_zlest0045)
            FROM zlest0045
           WHERE chave_cte EQ @p_cte-cd_chave_cte.

          LOOP AT p_notas_transporte INTO wa_n55.
            IF wa_n55-vbeln_vl IS NOT INITIAL.

              "Verificar se a NF-e Possui Volume Vinculado
              IF it_zlest0045[] IS NOT INITIAL.
                READ TABLE it_zlest0045 INTO DATA(wa_zlest0045) WITH KEY chave = wa_n55-n55_chave_acesso.
                IF sy-subrc IS INITIAL AND
                   wa_zlest0045-peso_rateado IS INITIAL AND
                   e_tipo_contrato NE '0002'.
                  CONTINUE.
                ENDIF.
              ENDIF.

              wa_itemdata-delivery  = wa_n55-vbeln_vl.
              wa_itemdata-itenerary = lc_itenerary.
              APPEND wa_itemdata TO it_itemdata.
              ADD 1 TO lc_itenerary.
            ENDIF.
          ENDLOOP.

          CLEAR: it_return, it_return[].

          CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              headerdata = wa_headerdata
            IMPORTING
              transport  = e_tknum
            TABLES
              itemdata   = it_itemdata
              stagedata  = it_stagedata
              return     = it_return.

          IF e_tknum IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = true.
          ENDIF.

          WAIT UP TO 1 SECONDS.

          LOOP AT it_return INTO wa_return.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = wa_return-type
                p_id           = wa_return-id
                p_num          = wa_return-number
                p_message_v1   = wa_return-message_v1
                p_message_v2   = wa_return-message_v2
                p_message_v3   = wa_return-message_v3
                p_message_v4   = wa_return-message_v4
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDLOOP.

          CHECK e_tknum IS NOT INITIAL.

          CLEAR: wa_headerdata.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = e_tknum
            IMPORTING
              output = wa_headerdata-shipment_num.

          wa_headerdata-status_load_end           = true.
          wa_headerdata-status_compl              = true.
          wa_headerdata-status_shpmnt_start       = true.
          wa_headerdata-status_shpmnt_end         = true.

          wa_headerdataaction-status_load_end     = 'C'.
          wa_headerdataaction-status_compl        = 'C'.
          wa_headerdataaction-status_shpmnt_start = 'C'.
          wa_headerdataaction-status_shpmnt_end   = 'C'.

          CLEAR: it_return, it_return[].

          CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
            EXPORTING
              headerdata       = wa_headerdata
              headerdataaction = wa_headerdataaction
            TABLES
              return           = it_return.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          WAIT UP TO 2 SECONDS.

          LOOP AT it_return INTO wa_return.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = wa_return-type
                p_id           = wa_return-id
                p_num          = wa_return-number
                p_message_v1   = wa_return-message_v1
                p_message_v2   = wa_return-message_v2
                p_message_v3   = wa_return-message_v3
                p_message_v4   = wa_return-message_v4
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDLOOP.

          DELETE it_return WHERE type NE 'E'.

          CALL METHOD me->gerar_doc_custo
            EXPORTING
              p_chave_cte = p_cte-cd_chave_cte
              p_tknum     = e_tknum
              p_data      = p_zlest0044-dt_referencia
            IMPORTING
              e_fknum     = e_fknum.

          WAIT UP TO 1 SECONDS.

          IF e_fknum IS NOT INITIAL AND e_tknum IS NOT INITIAL.
            p_zlest0044-nr_trans = e_tknum.
            p_zlest0044-nr_frete = e_fknum.
            p_zlest0044-status   = 'B'.
            p_zlest0044-matnr_faturado = p_matnr_faturado.
            MODIFY zlest0044 FROM p_zlest0044.
            COMMIT WORK.
          ELSE.
            IF e_tknum IS NOT INITIAL.

              p_zlest0044-nr_trans = e_tknum.

              CALL METHOD me->estornar_doc_transporte
                EXPORTING
                  p_chave_cte = p_cte-cd_chave_cte
                  i_tknum     = e_tknum.

              SELECT SINGLE * INTO wa_vttk
                FROM vttk
               WHERE tknum EQ e_tknum.

              IF sy-subrc IS NOT INITIAL.
                CLEAR: p_zlest0044-nr_trans.
              ENDIF.

            ENDIF.

            MODIFY zlest0044 FROM p_zlest0044.
            COMMIT WORK.
          ENDIF.

        ELSE.
          IF p_zlest0044-nr_frete IS NOT INITIAL.
            CALL METHOD me->estornar_doc_custo
              EXPORTING
                p_chave_cte = p_cte-cd_chave_cte
                i_fknum     = p_zlest0044-nr_frete.
          ENDIF.

          IF p_zlest0044-nr_trans IS NOT INITIAL.
            SELECT SINGLE * INTO wa_vfkp
              FROM vfkp
             WHERE rebel = p_zlest0044-nr_trans.

            IF sy-subrc IS NOT INITIAL.
              "Limpa Doc. Custo
              CLEAR: p_zlest0044-nr_frete.

              CALL METHOD me->estornar_doc_transporte
                EXPORTING
                  p_chave_cte = p_cte-cd_chave_cte
                  i_tknum     = p_zlest0044-nr_trans.

              SELECT SINGLE * INTO wa_vttk
                FROM vttk
               WHERE tknum EQ p_zlest0044-nr_trans.

              IF sy-subrc IS NOT INITIAL.
                "Limpa Doc. Transporte
                CLEAR: p_zlest0044-nr_trans.
              ENDIF.
            ENDIF.
          ENDIF.

          IF p_zlest0044-nr_trans IS INITIAL.
            p_zlest0044-status = 'L'.
          ENDIF.

          IF p_tipo_contrato EQ '0002'.
            p_zlest0044-tarifa = p_cte-valor_receber.
          ENDIF.

          MODIFY zlest0044 FROM p_zlest0044.
          COMMIT WORK.
        ENDIF.

      WHEN '03'.

        TRY .
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = p_cte-p_emissor
              )->ck_parceiro_local_negocio(
              ).

          CATCH zcx_parceiros INTO DATA(ex_parceiros).
            DATA(n_local_negocio) = abap_true.
            "EX_PARCEIROS->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            "MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING DOC_TRANSP.
        ENDTRY.


        IF n_local_negocio =  abap_false.

          CALL METHOD me->busca_docnum_chave
            EXPORTING
              p_chave            = p_cte-cd_chave_cte
              p_form             = abap_true
            CHANGING
              e_docnum           = p_cte-docnum_cte_sub
            EXCEPTIONS
              nao_achou          = 1
              nao_achou_parceiro = 2
              OTHERS             = 3.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING doc_transp.
          ENDIF.

          SELECT SINGLE * INTO @DATA(wa_zlest0061)
            FROM zlest0061
           WHERE docnum EQ @p_cte-docnum_cte_sub.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE e200 RAISING doc_transp.
          ENDIF.

          DATA: number           TYPE tbtcjob-jobcount,
                name             TYPE tbtcjob-jobname,
                print_parameters TYPE pri_params.

          IF wa_zlest0061-tknum IS NOT INITIAL.
            DATA(pestorn) = abap_true.
          ELSE.
            pestorn = abap_false.
          ENDIF.

          IF 1 EQ 2.

            SUBMIT zlesr0077
              WITH pdocnum EQ p_cte-docnum_cte_sub
              WITH pestorn EQ pestorn
               AND RETURN.

          ELSE.

            TRY .
                DATA(lc_user_job) = zcl_job=>get_user_job( ).
              CATCH zcx_job INTO DATA(ex_job).
                ex_job->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
                MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING doc_transp.
            ENDTRY.

            CONCATENATE 'VT_CTE_AQUA' p_cte-docnum_cte_sub INTO name SEPARATED BY '_'.

            CALL FUNCTION 'JOB_OPEN'
              EXPORTING
                jobname          = name
              IMPORTING
                jobcount         = number
              EXCEPTIONS
                cant_create_job  = 1
                invalid_job_data = 2
                jobname_missing  = 3
                OTHERS           = 4.

            IF sy-subrc = 0.

              SUBMIT zlesr0077 TO SAP-SPOOL SPOOL PARAMETERS print_parameters WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                WITH pdocnum EQ p_cte-docnum_cte_sub
                WITH pestorn EQ pestorn
                USER lc_user_job

                 AND RETURN.

              IF sy-subrc = 0.
                CALL FUNCTION 'JOB_CLOSE'
                  EXPORTING
                    jobcount             = number
                    jobname              = name
                    strtimmed            = 'X'
                  EXCEPTIONS
                    cant_start_immediate = 1
                    invalid_startdate    = 2
                    jobname_missing      = 3
                    job_close_failed     = 4
                    job_nosteps          = 5
                    job_notex            = 6
                    lock_failed          = 7
                    OTHERS               = 8.
              ENDIF.
            ENDIF.

            TRY.

                CASE pestorn.
                  WHEN abap_true.
                    DATA(lc_texto) = 'Aguardar Estornar Documento de Transporte e Doc.Custo'.
                  WHEN abap_false.
                    lc_texto = 'Aguardar Gerar Documento de Transporte e Doc.Custo'.
                ENDCASE.

                DATA(obj_job) = zcl_job=>get_instance( ).
                obj_job->set_key_job( i_jobname  = name
                                      i_jobcount = number
                                                   )->get_wait_job_exec( i_text_wait = CONV #( lc_texto )
                                                   ).
              CATCH zcx_job.
            ENDTRY.

            TRY.
                obj_job->get_log_job( IMPORTING e_logs = DATA(e_logs) ).
              CATCH zcx_job.
            ENDTRY.

            CLEAR: obj_job.

            LOOP AT e_logs INTO DATA(wa_logs).
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = wa_logs-msgtype
                  p_id           = wa_logs-msgid
                  p_num          = wa_logs-msgno
                  p_message_v1   = wa_logs-msgv1
                  p_message_v2   = wa_logs-msgv2
                  p_message_v3   = wa_logs-msgv3
                  p_message_v4   = wa_logs-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDLOOP.
          ENDIF.

          SELECT SINGLE * INTO wa_zlest0061
            FROM zlest0061
           WHERE docnum EQ p_cte-docnum_cte_sub.

          CASE pestorn.
            WHEN abap_true.
              "201  Não Estornado Documento de Custo &1!
              "202  Não Estornado Documento de Transporte &1!
              "203  Não foram estornados os documento de transporte e custo, verificar logs!
              IF wa_zlest0061-fknum IS NOT INITIAL AND wa_zlest0061-tknum IS NOT INITIAL.
                MESSAGE e203 RAISING doc_transp.
              ELSEIF wa_zlest0061-fknum IS NOT INITIAL.
                MESSAGE e201 RAISING doc_transp.
              ELSEIF wa_zlest0061-tknum IS NOT INITIAL.
                MESSAGE e202 RAISING doc_transp.
              ELSE.
                MESSAGE s204.
              ENDIF.

            WHEN abap_false.
              IF wa_zlest0061-fknum IS NOT INITIAL.
                MESSAGE s197 WITH wa_zlest0061-fknum.
              ELSEIF wa_zlest0061-tknum IS NOT INITIAL.
                MESSAGE s198 WITH wa_zlest0061-tknum.
              ELSE.
                MESSAGE e199 RAISING doc_transp.
              ENDIF.
          ENDCASE.

        ELSE.

          READ TABLE it_n55 INTO wa_n55 INDEX 1.

          IF wa_n55-tknum IS INITIAL  AND wa_n55-fknum IS INITIAL.

            SELECT  * FROM vbpa INTO TABLE @DATA(it_vbpa)
              WHERE vbeln EQ @wa_n55-vbeln_vl
              AND   parvw IN ('LF','LR').


            LOOP AT it_vbpa INTO DATA(wa_vbpa).
              IF wa_vbpa-parvw EQ 'LF'.
                p_org_suppl = wa_vbpa-lifnr.

              ELSEIF wa_vbpa-parvw EQ 'LR'.
                p_dest_cust  = wa_vbpa-kunnr.
              ENDIF.
            ENDLOOP.

            nr_numr_cte    = |{ p_cte-numr_cte ALPHA = IN }|.
            vserie_cte   = |{ p_cte-numr_serie ALPHA = OUT }|.

            CONCATENATE nr_numr_cte '-' vserie_cte INTO  vnumr_cte.

            CLEAR wa_headerdata.

            CASE p_cte-cd_modal.
              WHEN '01'.  "Rodoviário
                wa_headerdata-shipping_type = '01'.
              WHEN '02'.  "Aéreo
                wa_headerdata-shipping_type = '05'.
              WHEN '03'.  "Aquaviário
                wa_headerdata-shipping_type = '03'.
                wa_headerdata-shipment_type = 'Z027'.
              WHEN '04'.  "Ferroviário
                wa_headerdata-shipping_type = '02'.
                wa_headerdata-shipment_type = 'Z027'.
              WHEN '05'.  "Dutoviário.
                wa_headerdata-shipping_type = ''.
              WHEN '06'.  "Ferroviário - multimodal
                wa_headerdata-shipping_type = '02'.
                wa_headerdata-shipment_type = p_zlest0044-shtyp.
            ENDCASE.

            wa_headerdata-service_agent_id     = p_cte-p_emissor.
            wa_headerdata-trans_plan_pt        = p_cte-f_tomadora.
            wa_headerdata-service_level        = '1'.
            wa_headerdata-external_id_1        = vnumr_cte.
            wa_headerdata-status_plan          = true.
            wa_headerdata-status_checkin       = true.
            wa_headerdata-status_load_start    = true.
            wa_headerdata-time_travel          = '30'.
            wa_headerdata-time_total           = '1'.
            wa_headerdata-time_unit            = 'H'.
            wa_headerdata-special_procedure_id = '0001'.
            wa_headerdata-shpmnt_cost_rel      = true.

            wa_stagedata-stage_cat  = '1'.
            wa_stagedata-stage_seq  = '0001'.
            "WA_STAGEDATA-ORG_POINT  = P_ORG_POINT.
            "WA_STAGEDATA-ORG_CUST   = P_ORG_CUST.
            wa_stagedata-org_suppl  = p_org_suppl.
            "WA_STAGEDATA-DEST_POINT = P_DEST_POINT.
            wa_stagedata-dest_cust  = p_dest_cust.
            "WA_STAGEDATA-DEST_SUPPL = P_DEST_SUPPL.
            APPEND wa_stagedata TO it_stagedata.

            lc_itenerary = 1.

            IF sy-subrc = 0.

              CASE p_cte-tp_processo_cte.
                WHEN '06'.

                  LOOP AT it_n55 INTO wa_n55.
                    wa_itemdata-delivery  = wa_n55-vbeln_vl.
                    wa_itemdata-itenerary = lc_itenerary.
                    APPEND wa_itemdata TO it_itemdata.
                    ADD 1 TO lc_itenerary.

                    CLEAR: wa_n55.
                  ENDLOOP.

                WHEN OTHERS.
              ENDCASE.
            ENDIF.


            CLEAR: it_return, it_return[].

            CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                headerdata = wa_headerdata
              IMPORTING
                transport  = e_tknum
              TABLES
                itemdata   = it_itemdata
                stagedata  = it_stagedata
                return     = it_return.

            IF e_tknum IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = true.
            ENDIF.

            LOOP AT it_return INTO wa_return.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = wa_return-type
                  p_id           = wa_return-id
                  p_num          = wa_return-number
                  p_message_v1   = wa_return-message_v1
                  p_message_v2   = wa_return-message_v2
                  p_message_v3   = wa_return-message_v3
                  p_message_v4   = wa_return-message_v4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDLOOP.

            CHECK e_tknum IS NOT INITIAL.

            CLEAR: wa_headerdata.

            wa_headerdata-shipment_num = |{ e_tknum ALPHA = IN }|.

            wa_headerdata-status_load_end           = true.
            wa_headerdata-status_compl              = true.
            wa_headerdata-status_shpmnt_start       = true.
            wa_headerdata-status_shpmnt_end         = true.

            wa_headerdataaction-status_load_end     = 'C'.
            wa_headerdataaction-status_compl        = 'C'.
            wa_headerdataaction-status_shpmnt_start = 'C'.
            wa_headerdataaction-status_shpmnt_end   = 'C'.

            CLEAR: it_return, it_return[].

            CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
              EXPORTING
                headerdata       = wa_headerdata
                headerdataaction = wa_headerdataaction
              TABLES
                return           = it_return.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            LOOP AT it_return INTO wa_return.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = wa_return-type
                  p_id           = wa_return-id
                  p_num          = wa_return-number
                  p_message_v1   = wa_return-message_v1
                  p_message_v2   = wa_return-message_v2
                  p_message_v3   = wa_return-message_v3
                  p_message_v4   = wa_return-message_v4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDLOOP.

            DELETE it_return WHERE type NE 'E'.

            CALL METHOD me->gerar_doc_custo
              EXPORTING
                p_chave_cte = p_cte-cd_chave_cte
                p_tknum     = e_tknum
                p_data      = sy-datum
              IMPORTING
                e_fknum     = e_fknum.

          ELSE.

            IF wa_n55-fknum IS NOT INITIAL.
              DO 2 TIMES.

                CALL METHOD me->estornar_doc_custo
                  EXPORTING
                    p_chave_cte = p_cte-cd_chave_cte
                    i_fknum     = wa_n55-fknum.
              ENDDO.
            ENDIF.

            IF wa_n55-fknum IS NOT INITIAL.

              SELECT SINGLE * INTO wa_vfkp
                FROM vfkp
               WHERE fknum EQ wa_n55-fknum.

              IF sy-subrc IS NOT INITIAL.

                CLEAR: wa_n55-fknum.

                CALL METHOD me->estornar_doc_transporte
                  EXPORTING
                    p_chave_cte = p_cte-cd_chave_cte
                    i_tknum     = wa_n55-tknum.

                SELECT SINGLE * INTO wa_vttk
                  FROM vttk
                 WHERE tknum EQ wa_n55-tknum.

                IF sy-subrc IS NOT INITIAL.
                  CLEAR: wa_n55-tknum.
                ENDIF.
              ENDIF.
            ENDIF.

            "            MODIFY ZIB_CTE_DIST_N55 FROM WA_N55.
            "            COMMIT WORK.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD gerar_escrit_entrada.


    DATA: wa_obj_header     TYPE bapi_j_1bnfdoc,
          wa_obj_partner    TYPE bapi_j_1bnfnad,
          wa_obj_item       TYPE bapi_j_1bnflin,
          wa_obj_item_tax   TYPE bapi_j_1bnfstx,
          wa_nfcheck        TYPE bapi_j_1bnfcheck,
          wa_impostos       TYPE bapi_j_1bnfstx,
          it_obj_partner    TYPE TABLE OF bapi_j_1bnfnad,
          it_obj_item       TYPE TABLE OF bapi_j_1bnflin,
          it_obj_item_tax   TYPE TABLE OF bapi_j_1bnfstx,
          wa_nfe_forn       TYPE zib_nfe_forn,
          wa_j_1baa         TYPE j_1baa,
          p_data_ent        TYPE datum,
          p_data_val        TYPE datum,
          wa_mara           TYPE mara,
          wa_makt           TYPE makt,
          wa_zlest0040      TYPE zlest0040,
          wa_j_1bbranch     TYPE j_1bbranch,
          lc_dstcat	        TYPE j_1bdstcat,
          it_return	        TYPE TABLE OF	bapiret2,
          wa_return	        TYPE bapiret2,
          wa_j_1bnfe_active TYPE j_1bnfe_active,
          wa_j_1bnfdoc      TYPE j_1bnfdoc,
          lc_taxgrp	        TYPE j_1btaxgrp,
          wa_j_1batl1       TYPE j_1batl1, "Situação Tributária do ICMS
          wa_j_1batl2       TYPE j_1batl2, "Situação Tributária do IPI
          wa_j_1batl4a      TYPE j_1batl4a, "Situação Tributária do COFINS
          wa_j_1batl5       TYPE j_1batl5, "Situação Tributária do PIS
          wa_c57            TYPE zib_cte_dist_c57,
          lc_emissor_uf	    TYPE regio,
          lc_tomador_uf	    TYPE regio,
          wa_lfa1           TYPE lfa1,
          wa_kna1           TYPE kna1,
          lc_modal          TYPE i,
          lc_operacao       TYPE zlest0061-operacao. "*-CS2024000597-14.10.2024-#146076-JT-inicio

    IF p_chave_cte IS NOT INITIAL.
      SELECT SINGLE * INTO p_cte
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte EQ p_chave_cte.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e001 WITH p_chave_cte RAISING nao_enc_frete.
      ENDIF.
    ENDIF.

    CALL METHOD me->busca_docnum_chave
      EXPORTING
        p_chave            = p_cte-cd_chave_cte
        p_psq_chave        = abap_true
      CHANGING
        e_docnum           = p_cte-docnum_cte
      EXCEPTIONS
        nao_achou          = 1
        nao_achou_parceiro = 2
        OTHERS             = 3.

    IF sy-subrc IS INITIAL AND p_cte-docnum_cte IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
        FROM j_1bnflin
       WHERE docnum EQ @p_cte-docnum_cte.

      UPDATE zib_cte_dist_ter
         SET docnum_cte    = p_cte-docnum_cte
             ck_finalizado = true
             matns         = wa_j_1bnflin-matnr
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      COMMIT WORK.
    ENDIF.

    CHECK p_cte-docnum_cte IS INITIAL.

    SELECT SINGLE * INTO wa_nfe_forn
      FROM zib_nfe_forn
     WHERE nu_chave EQ p_cte-cd_chave_cte.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e080 WITH p_chave_cte RAISING nao_cte_forn.
    ENDIF.

*01	Rodoviário
*02	Aéreo
*03	Aquaviário
*04	Ferroviário
*05	Dutoviário
    wa_obj_header-nftype  = me->get_nftype_entrada( ). "'C2'. "Tipo de Nota Fiscal ( Modelo 57 de Entrada de Fornecedor )
    wa_obj_header-xmlvers = wa_nfe_forn-xmlvers.
    wa_obj_header-code    = wa_nfe_forn-nu_code.

    SELECT SINGLE * INTO wa_j_1baa
      FROM j_1baa
     WHERE nftype EQ wa_obj_header-nftype.

    MOVE-CORRESPONDING wa_j_1baa TO wa_obj_header.

    IF p_cte-cd_tipo_cte EQ '1'. "CT-e de Complemento de Valores

      SELECT SINGLE * INTO wa_c57
        FROM zib_cte_dist_c57
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

*  092  CT-e de Complemento e não encontrado CT-e Complementada!
*  093  CT-e Complementada não foi escriturada!

      IF ( sy-subrc IS NOT INITIAL ).
        MESSAGE e092 RAISING nao_cte_forn.
      ENDIF.

      IF ( wa_c57-docnum_cte IS INITIAL ).
        MESSAGE e093 RAISING nao_cte_forn.
      ENDIF.

      wa_obj_header-doctyp  = '4'.
      wa_obj_header-direct  = '1'.
      wa_obj_header-docref  = wa_c57-docnum_cte.
    ENDIF.

    wa_obj_header-docstat    = wa_nfe_forn-st_nota.
    wa_obj_header-parid      = p_cte-p_emissor.
    wa_obj_header-access_key = p_cte-cd_chave_cte.
    wa_obj_header-docdat     = p_cte-dt_emissao.

    p_data_ent = p_cte-zdt_mov.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = p_data_ent
        p_bukrs        = p_cte-e_tomadora
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      IMPORTING
        p_data_val     = p_data_val
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
    ENDIF.

    wa_obj_header-pstdat      = p_data_val.
    wa_obj_header-bukrs       = p_cte-e_tomadora.
    wa_obj_header-branch      = p_cte-f_tomadora.
    wa_obj_header-waerk       = 'BRL'.
    wa_obj_header-series      = p_cte-numr_serie.
    wa_obj_header-nfenum      = p_cte-numr_cte.
    wa_obj_header-manual      = true.

    "Item da parte fiscal
    wa_obj_item-itmnum    = 10.
    wa_obj_item-bwkey     = p_cte-f_tomadora.
    wa_obj_item-werks     = p_cte-f_tomadora.
    wa_obj_item-reftyp    = 'LI'.
    wa_obj_item-refitm    = 1.
    CONCATENATE p_cte-belnr p_cte-gjahr INTO wa_obj_item-refkey.

*** Inicio CS2019001610 - 01/09/2020 - Pedro Leite

    DATA: vl_matkl TYPE zlest0037-matkl.

    CALL METHOD me->busca_grp_mercadoria
      EXPORTING
        p_chave_cte = p_cte-cd_chave_cte
      CHANGING
        p_matkl     = vl_matkl.

*-CS2024000597-14.10.2024-#146076-JT-inicio
    lc_operacao = p_zlest0061-operacao.

    IF p_cte-cd_modal <> '03'.
      CLEAR lc_operacao.
    ENDIF.
*-CS2024000597-14.10.2024-#146076-JT-fim
    IF NOT vl_matkl IS INITIAL.
      SELECT SINGLE matnr INTO wa_obj_item-matnr "#EC CI_FLDEXT_OK[2215424]
        FROM zlest0037
       WHERE bukrs      EQ p_cte-e_tomadora
         AND cd_modal   EQ p_cte-cd_modal
         AND lifnr      EQ p_cte-p_emissor
         AND ck_servico EQ false
         AND matkl      EQ vl_matkl
         AND operacao   EQ lc_operacao.  "*-CS2024000597-14.10.2024-#146076-JT-inicio
    ELSE.
      sy-subrc = 1.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
*** Fim CS2019001610 - 01/09/2020 - Pedro Leite
      SELECT SINGLE matnr INTO wa_obj_item-matnr "#EC CI_FLDEXT_OK[2215424]
        FROM zlest0037
       WHERE bukrs      EQ p_cte-e_tomadora
         AND cd_modal   EQ p_cte-cd_modal
         AND lifnr      EQ p_cte-p_emissor
         AND ck_servico EQ false
         AND matkl      EQ space
         AND operacao   EQ lc_operacao.  "*-CS2024000597-14.10.2024-#146076-JT-inicio
*** Inicio CS2019001610 - 01/09/2020 - Pedro Leite
    ENDIF.
*** Fim CS2019001610 - 01/09/2020 - Pedro Leite

    lc_modal = p_cte-cd_modal.
    MOVE lc_modal TO wa_obj_header-transp_mode.

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE matnr INTO wa_obj_item-matnr "#EC CI_FLDEXT_OK[2215424]
        FROM zlest0037
       WHERE bukrs      EQ p_cte-e_tomadora
         AND cd_modal   EQ p_cte-cd_modal
         AND lifnr      EQ space
         AND ck_servico EQ false
         AND matkl      EQ vl_matkl
         AND operacao   EQ lc_operacao.  "*-CS2024000597-14.10.2024-#146076-JT-inicio

      IF sy-subrc IS NOT INITIAL.
        SELECT SINGLE matnr INTO wa_obj_item-matnr "#EC CI_FLDEXT_OK[2215424]
          FROM zlest0037
         WHERE bukrs      EQ p_cte-e_tomadora
           AND cd_modal   EQ p_cte-cd_modal
           AND lifnr      EQ space
           AND ck_servico EQ false
           AND matkl      EQ space
           AND operacao   EQ lc_operacao.  "*-CS2024000597-14.10.2024-#146076-JT-inicio
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e103 WITH p_cte-cd_modal p_cte-e_tomadora p_cte-p_emissor RAISING nao_servico_param.
      ENDIF.

    ENDIF.

    wa_obj_item-itmtyp    = 'ZH'.
    wa_obj_item-menge     = 1.
*---> 07/06/2023 - Migração S4 - JS
*     wa_obj_item-netoth    = p_cte-zvalor_pedagio.
    wa_obj_item-netoth = CONV #( p_cte-zvalor_pedagio ).
*<--- 07/06/2023 - Migração S4 - JS

    wa_obj_item-netpr     = p_cte-zvlr_frete - ( p_cte-zvalor_pedagio ) - ( p_cte-zvalor_icms + p_cte-zvalor_pis + p_cte-zvalor_cofins ).
    wa_obj_item-netwr     = p_cte-zvlr_frete - ( p_cte-zvalor_pedagio ) - ( p_cte-zvalor_icms + p_cte-zvalor_pis + p_cte-zvalor_cofins ).

    CALL FUNCTION 'J_1B_MATERIAL_READ'
      EXPORTING
        matnr                = wa_obj_item-matnr "#EC CI_FLDEXT_OK[2215424]
        val_area             = wa_obj_item-werks
        val_type             = space
        language             = sy-langu
        i_werks              = wa_obj_item-werks
      IMPORTING
        nbm                  = wa_obj_item-nbm
        matuse               = wa_obj_item-matuse
        matorg               = wa_obj_item-matorg
        material_record      = wa_mara
        material_text_record = wa_makt
        e_matkl              = wa_obj_item-matkl
      EXCEPTIONS
        material_not_found   = 1
        valuation_not_found  = 2
        OTHERS               = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
    ENDIF.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs  EQ wa_obj_header-bukrs
       AND branch EQ wa_obj_header-branch.

    wa_obj_item-maktx = wa_makt-maktx.
    wa_obj_item-matkl = wa_mara-matkl.
    wa_obj_item-meins = wa_mara-meins.

    SELECT SINGLE * INTO wa_lfa1
      FROM lfa1
     WHERE lifnr EQ wa_obj_header-parid.

    wa_kna1-kunnr = wa_obj_header-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_kna1-kunnr
      IMPORTING
        output = wa_kna1-kunnr.

    SELECT SINGLE * INTO wa_kna1
      FROM kna1
     WHERE kunnr EQ wa_kna1-kunnr.

    lc_emissor_uf = wa_lfa1-regio.
    lc_tomador_uf = wa_kna1-regio.

*147  UF de Emissor &1 não determinado!
*148  UF de Tomador &1 não determinado!

    IF lc_emissor_uf IS INITIAL.
      MESSAGE e147 WITH wa_obj_header-parid RAISING nao_param_cfop.
    ENDIF.

    IF lc_tomador_uf IS INITIAL.
      MESSAGE e148 WITH wa_kna1-kunnr RAISING nao_param_cfop.
    ENDIF.

    IF lc_emissor_uf EQ lc_tomador_uf.
      lc_dstcat = '0'.
    ELSE.
      lc_dstcat = '1'.
    ENDIF.

    SELECT SINGLE cfop
      INTO wa_obj_item-cfop_10
      FROM zlest0030
     WHERE direct     EQ '1'
       AND dstcat     EQ lc_dstcat
       AND industry   EQ wa_j_1bbranch-industry
       AND tpparceiro EQ '1'
       AND tdlnr      EQ p_cte-p_emissor
       AND bukrs      EQ p_cte-e_tomadora.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE cfop
        INTO wa_obj_item-cfop_10
        FROM zlest0030
       WHERE direct     EQ '1'
         AND dstcat     EQ lc_dstcat
         AND industry   EQ wa_j_1bbranch-industry
         AND tpparceiro EQ '1'.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e082 WITH '1' lc_dstcat wa_j_1bbranch-industry '1' RAISING nao_param_cfop.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO wa_zlest0040
      FROM zlest0040
     WHERE iva    EQ p_cte-mwskz
       AND fatura EQ 'T'.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e081 WITH p_cte-mwskz RAISING nao_param_iva.
    ENDIF.

    wa_obj_item-taxlw1 = wa_zlest0040-icms.
    wa_obj_item-taxlw2 = wa_zlest0040-ipi.
    wa_obj_item-taxlw4 = wa_zlest0040-cofins.
    wa_obj_item-taxlw5 = wa_zlest0040-pis.

    "Determinação do Código da Situação Tributária do ICMS - CST
    IF wa_obj_item-taxlw1 IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1batl1
        FROM j_1batl1
       WHERE taxlaw = wa_obj_item-taxlw1.

      wa_obj_item-taxsit = wa_j_1batl1-taxsit.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = p_cte-zbase_icms.
      wa_impostos-rate   = p_cte-zrate_icms.
      wa_impostos-taxval = p_cte-zvalor_icms.

      IF wa_impostos-taxval GT 0.
        wa_impostos-excbas = 0.
*---> 07/06/2023 - Migração S4 - JS
*            wa_impostos-othbas = p_cte-zvalor_pedagio.
        wa_impostos-othbas = CONV #( p_cte-zvalor_pedagio ).
*<--- 07/06/2023 - Migração S4 - JS

      ELSE.
        wa_impostos-excbas = p_cte-zvlr_frete - p_cte-zvalor_pedagio.
*---> 07/06/2023 - Migração S4 - JS
*            wa_impostos-othbas = p_cte-zvalor_pedagio.
        wa_impostos-othbas = CONV #( p_cte-zvalor_pedagio ).
*<--- 07/06/2023 - Migração S4 - JS
      ENDIF.

      lc_taxgrp = 'ICMS'.

      CALL METHOD me->buscar_tipo_imposto
        EXPORTING
          i_taxgrp = lc_taxgrp
        IMPORTING
          e_kschl  = wa_impostos-taxtyp
        EXCEPTIONS
          erro     = 1
          OTHERS   = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

      APPEND wa_impostos TO it_obj_item_tax.
    ENDIF.

    "Determinação do Código da Situação Tributária do IPI - CST
    IF wa_obj_item-taxlw2 IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1batl2 FROM j_1batl2
       WHERE taxlaw = wa_obj_item-taxlw2.

      wa_obj_item-taxsi2 = wa_j_1batl2-taxsit.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = 0."P_CTE-ZBASE_IPI.
      wa_impostos-rate   = 0."P_CTE-ZRATE_IPI.
      wa_impostos-taxval = 0."P_CTE-ZVALOR_IPI.

      "IF WA_IMPOSTOS-TAXVAL GT 0.
      "  WA_IMPOSTOS-EXCBAS = 0.
      "  WA_IMPOSTOS-OTHBAS = P_CTE-ZVALOR_PEDAGIO.
      "ELSE.
      "  WA_IMPOSTOS-EXCBAS = P_CTE-ZVLR_FRETE - P_CTE-ZVALOR_PEDAGIO.
      "  WA_IMPOSTOS-OTHBAS = P_CTE-ZVALOR_PEDAGIO.
      "ENDIF.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = p_cte-zvlr_frete.

      lc_taxgrp = 'IPI'.

      CALL METHOD me->buscar_tipo_imposto
        EXPORTING
          i_taxgrp = lc_taxgrp
        IMPORTING
          e_kschl  = wa_impostos-taxtyp
        EXCEPTIONS
          erro     = 1
          OTHERS   = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

      APPEND wa_impostos TO it_obj_item_tax.
    ENDIF.

    "Determinação do Código da Situação Tributária do COFINS - CST
    IF wa_obj_item-taxlw4 IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1batl4a FROM j_1batl4a
       WHERE taxlaw = wa_obj_item-taxlw4.

      wa_obj_item-taxsi4 = wa_j_1batl4a-taxsit.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = p_cte-zbase_cofins.
      wa_impostos-rate   = p_cte-zrate_cofins.
      wa_impostos-taxval = p_cte-zvalor_cofins.

      IF wa_impostos-taxval GT 0.
        wa_impostos-excbas = 0.
*---> 07/06/2023 - Migração S4 - JS
*            wa_impostos-othbas = p_cte-zvalor_pedagio.
        wa_impostos-othbas = CONV #( p_cte-zvalor_pedagio ).
*<--- 07/06/2023 - Migração S4 - JS

      ELSE.
        wa_impostos-excbas = 0.
        wa_impostos-othbas = p_cte-zvlr_frete.
      ENDIF.

      lc_taxgrp = 'COFI'.

      CALL METHOD me->buscar_tipo_imposto
        EXPORTING
          i_taxgrp = lc_taxgrp
        IMPORTING
          e_kschl  = wa_impostos-taxtyp
        EXCEPTIONS
          erro     = 1
          OTHERS   = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

      APPEND wa_impostos TO it_obj_item_tax.
    ENDIF.

    "Determinação do Código da Situação Tributária do PIS - CST
    IF wa_obj_item-taxlw5 IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1batl5 FROM j_1batl5
       WHERE taxlaw = wa_obj_item-taxlw5.

      wa_obj_item-taxsi5 = wa_j_1batl5-taxsit.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = p_cte-zbase_pis.
      wa_impostos-rate   = p_cte-zrate_pis.
      wa_impostos-taxval = p_cte-zvalor_pis.

      IF wa_impostos-taxval GT 0.
        wa_impostos-excbas = 0.
*---> 07/06/2023 - Migração S4 - JS
*       wa_impostos-othbas = p_cte-zvalor_pedagio.
        wa_impostos-othbas = CONV #( p_cte-zvalor_pedagio ).
*<--- 07/06/2023 - Migração S4 - JS
      ELSE.
        wa_impostos-excbas = 0.
        wa_impostos-othbas = p_cte-zvlr_frete.
      ENDIF.

      lc_taxgrp = 'PIS'.

      CALL METHOD me->buscar_tipo_imposto
        EXPORTING
          i_taxgrp = lc_taxgrp
        IMPORTING
          e_kschl  = wa_impostos-taxtyp
        EXCEPTIONS
          erro     = 1
          OTHERS   = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

      APPEND wa_impostos TO it_obj_item_tax.
    ENDIF.

    APPEND wa_obj_item TO it_obj_item.
    wa_nfcheck-chekcon = true.

    "Alimentar a estrutura do parceiros da nota (somente o parceiro LF é necessário)
    wa_obj_partner-mandt  = sy-mandt.
    wa_obj_partner-parvw  = 'LF'.
    wa_obj_partner-parid  = p_cte-p_emissor.
    wa_obj_partner-partyp = 'V'.
    APPEND wa_obj_partner TO it_obj_partner.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_obj_header-nfenum
      IMPORTING
        output = wa_obj_header-nfenum.

    p_cte-matns  = wa_obj_item-matnr.

    CONCATENATE p_cte-inicio_uf  p_cte-inicio_ibge  INTO wa_obj_header-cte_strt_lct SEPARATED BY space.
    CONCATENATE p_cte-termino_uf p_cte-termino_ibge INTO wa_obj_header-cte_end_lct  SEPARATED BY space.

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header   = wa_obj_header
        nfcheck      = wa_nfcheck
      IMPORTING
        e_docnum     = p_cte-docnum_cte
      TABLES
        obj_partner  = it_obj_partner
        obj_item     = it_obj_item
        obj_item_tax = it_obj_item_tax
        return       = it_return.

    IF p_cte-docnum_cte IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = true.

      SELECT SINGLE * INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum EQ p_cte-docnum_cte.

      SELECT SINGLE * INTO wa_j_1bnfe_active
        FROM j_1bnfe_active
       WHERE docnum EQ p_cte-docnum_cte.

      wa_j_1bnfdoc-authcod      = wa_nfe_forn-nu_protocolo.
      wa_j_1bnfdoc-docstat      = '1'.
      wa_j_1bnfe_active-authcod = wa_nfe_forn-nu_protocolo.
      wa_j_1bnfe_active-docnum9 = wa_nfe_forn-nu_chave_aleator.
      wa_j_1bnfe_active-docsta  = '1'.
      wa_j_1bnfe_active-cdv     = wa_nfe_forn-nu_chave_dv.
      wa_j_1bnfe_active-regio   = wa_nfe_forn-nu_chave_regiao.

      CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
        EXPORTING
          i_doc     = wa_j_1bnfdoc
          i_acttab  = wa_j_1bnfe_active
          i_updmode = 'U'.

      UPDATE zib_cte_dist_ter
         SET docnum_cte    = p_cte-docnum_cte
             ck_finalizado = true
             matns         = p_cte-matns
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      COMMIT WORK.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    SELECT MAX( nr_sequencia ) INTO lc_sequencia
      FROM zib_cte_dist_log
     WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

    IF lc_sequencia IS INITIAL.
      lc_sequencia = 1.
    ELSE.
      ADD 1 TO lc_sequencia.
    ENDIF.

    LOOP AT it_return INTO wa_return.
      CALL METHOD me->add_log_cte_dist
        EXPORTING
          p_cd_chave_cte = p_cte-cd_chave_cte
          p_type         = wa_return-type
          p_id           = wa_return-id
          p_num          = wa_return-number
          p_message_v1   = wa_return-message_v1
          p_message_v2   = wa_return-message_v2
          p_message_v3   = wa_return-message_v3
          p_message_v4   = wa_return-message_v4
        CHANGING
          p_lc_sequencia = lc_sequencia.
    ENDLOOP.

  ENDMETHOD.


  METHOD gerar_fatura_frete.


    DATA: e_rate_icms	  TYPE j_1btxrate,
          e_rate_pis    TYPE j_1btxrate,
          e_rate_cofins	TYPE j_1btxrate.

    DATA: wa_headerdata       TYPE bapi_incinv_create_header,
          wa_itemdata         TYPE bapi_incinv_create_item,
          it_itemdata         TYPE TABLE OF bapi_incinv_create_item,
          wa_contas           TYPE bapi_incinv_create_gl_account,
          it_contas           TYPE TABLE OF bapi_incinv_create_gl_account,
          e_contas            TYPE zbapi_incinv_gl_account_t,
          vg_rblgp            TYPE rblgp,
          lva_lifnr_toma      TYPE lfa1-lifnr,
          it_n55              TYPE TABLE OF zib_cte_dist_n55,
          it_n55_aux1         TYPE TABLE OF zib_cte_dist_n55,
          it_n55_aux2         TYPE TABLE OF zib_cte_dist_n55,
          it_n55_outra        TYPE TABLE OF zib_cte_dist_n55,
          it_n01              TYPE TABLE OF zib_cte_dist_n01,
          it_n01_2            TYPE TABLE OF zib_cte_dist_n01,
          wa_ter_cmpl         TYPE zib_cte_dist_ter,
          wa_n55              TYPE zib_cte_dist_n55,
          e_tipo_contrato	    TYPE sdabw,
          wa_n01              TYPE zib_cte_dist_n01,
          wa_c57              TYPE zib_cte_dist_c57,
          it_essr             TYPE TABLE OF essr,
          wa_essr             TYPE essr,
          p_rate_icms	        TYPE j_1btxrate,
          p_rate_pis          TYPE j_1btxrate,
          p_rate_cofins	      TYPE j_1btxrate,
          invoicedocnumber    TYPE re_belnr,
          fiscalyear          TYPE gjahr,
          it_return	          TYPE TABLE OF	bapiret2,
          wa_return	          TYPE bapiret2,
          it_vttk             TYPE TABLE OF vttk,
          wa_vttk             TYPE vttk,
          lc_zvalor_icms      TYPE j_1btaxval,
          lc_zvalor_pis       TYPE j_1btaxval,
          lc_zvalor_cofins    TYPE j_1btaxval,
          e_docnum            TYPE j_1bdocnum,
          lva_vlr_vi_brl      TYPE zib_cte_dist_n55-zvlr_vi,
          wa_j_1bnfdoc        TYPE j_1bnfdoc,
          wa_rbkp             TYPE rbkp,
          vg_augdt            TYPE augdt,
          vg_augbl            TYPE augbl,
          lc_cd_chave_cte     TYPE zde_chave_doc_e,
          wa_header           TYPE thead,
          wa_lines            TYPE tline,
          lc_matnr            TYPE matnr,
          it_lines            TYPE STANDARD TABLE OF tline,
          lc_zvalor_ft_peso	  TYPE zde_valor_frete_ton,
          lc_zvalor_vi_peso	  TYPE zde_valor_vi_ton,
          lc_zvalor_df_peso   TYPE zde_valor_vi_ton,
          lc_zvalor_df_perm   TYPE zde_valor_vi_ton,
          wa_material_nf      TYPE mara,
          wa_lfa1             TYPE lfa1,
          lc_zvlr_vi          TYPE zde_vlr_vi,
          ca_brtwr(16),
          moeda_condicao_zfre TYPE waerk.

    DATA: wa_zib_cte_dist_eap TYPE zib_cte_dist_eap,
          ck_liberado         TYPE c LENGTH 1,
          zlr_frete_2         TYPE zde_vlr_frete,
          zlr_perda_1         TYPE zib_cte_dist_n55-zvlr_perda,
          zlr_quebra_1        TYPE zib_cte_dist_n55-zvlr_quebra,
          zvlr_vi_1           TYPE zib_cte_dist_n55-zvlr_vi,
          wa_zlest0061        TYPE zlest0061.    ""*-CS2024000597-14.10.2024-#146076-JT-inicio


    IF p_chave_cte IS NOT INITIAL.
      SELECT SINGLE * INTO p_cte
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte EQ p_chave_cte.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e001 WITH p_chave_cte RAISING nao_enc_frete.
      ENDIF.
    ENDIF.

    CASE p_cte-tp_processo_cte.
      WHEN tipo_02.
        MESSAGE e090 WITH ds_tipo_02 RAISING fatura.
      WHEN tipo_03.
        IF p_cte-cd_modal NE '03'.
          MESSAGE e090 WITH ds_tipo_03 RAISING fatura.
        ENDIF.
      WHEN tipo_04.
        MESSAGE e090 WITH ds_tipo_04 RAISING fatura.
    ENDCASE.

    IF p_cte-cd_tipo_cte EQ '1'. "CT-e de Complemento de Valores

      SELECT SINGLE * INTO wa_c57
        FROM zib_cte_dist_c57
       WHERE cd_chave_cte EQ p_chave_cte.

*  092  CT-e de Complemento e não encontrado CT-e Complementada!
*  093  CT-e Complementada não foi escriturada!

      IF ( sy-subrc IS NOT INITIAL ).
        MESSAGE e092 RAISING fatura.
      ENDIF.

      SELECT SINGLE * INTO wa_ter_cmpl
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte EQ wa_c57-c57_chave_acesso.

      IF ( wa_ter_cmpl-docnum_cte IS INITIAL ).

        "Verifica se Existe Outro CT-e que pagou a mesma nota fiscal do CT-e complementado
        "CT-e que Complementado que foi pago
        SELECT t~cd_chave_cte INTO TABLE @DATA(it_ctes)
          FROM zib_cte_dist_c57 AS o
         INNER JOIN zib_cte_dist_n55 AS n ON n~cd_chave_cte     EQ o~c57_chave_acesso
         INNER JOIN zib_cte_dist_n55 AS m ON m~n55_chave_acesso EQ n~n55_chave_acesso
         INNER JOIN zib_cte_dist_ter AS t ON t~cd_chave_cte     EQ m~cd_chave_cte
         WHERE o~cd_chave_cte   EQ @p_chave_cte
           AND t~p_emissor      EQ @p_cte-p_emissor
           AND t~inicio_ibge    EQ @p_cte-inicio_ibge
           AND t~termino_ibge	  EQ @p_cte-termino_ibge
           AND t~emit_cnpj      EQ @p_cte-emit_cnpj
           AND t~emit_cpf       EQ @p_cte-emit_cpf
           AND t~emit_ie        EQ @p_cte-emit_ie
           AND t~reme_cnpj      EQ @p_cte-reme_cnpj
           AND t~reme_cpf       EQ @p_cte-reme_cpf
           AND t~reme_ie        EQ @p_cte-reme_ie
           AND t~dest_cnpj      EQ @p_cte-dest_cnpj
           AND t~dest_cpf       EQ @p_cte-dest_cpf
           AND t~dest_ie        EQ @p_cte-dest_ie
           AND t~ck_finalizado  EQ @abap_true
           AND t~cd_tipo_cte    EQ '0'.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE e093 RAISING fatura.
        ELSE.
          READ TABLE it_ctes INDEX 1 INTO wa_c57-c57_chave_acesso.
        ENDIF.

        SELECT SINGLE * INTO wa_ter_cmpl
          FROM zib_cte_dist_ter
         WHERE cd_chave_cte EQ wa_c57-c57_chave_acesso.

      ENDIF.

    ENDIF.

    CALL METHOD me->inicia_sequencia
      EXPORTING
        p_cd_chave_cte = p_cte-cd_chave_cte
      IMPORTING
        p_lc_sequencia = lc_sequencia.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" ESTORNO DE MIRO E DOC FISCAL """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF p_estornar IS NOT INITIAL AND ( ( p_cte-belnr IS NOT INITIAL AND p_cte-gjahr IS NOT INITIAL )  OR ( p_cte-docnum_cte IS NOT INITIAL ) ).

      IF ( p_cte-belnr IS NOT INITIAL AND p_cte-gjahr IS NOT INITIAL ).
        CALL FUNCTION 'Z_VERIFICA_MIRO_PAGA'
          EXPORTING
            belnr = p_cte-belnr
            gjahr = p_cte-gjahr
          CHANGING
            augdt = vg_augdt
            augbl = vg_augbl.

        "Miro está baixada/compensada
        IF NOT vg_augdt IS INITIAL.
          MESSAGE e083 WITH p_cte-belnr p_cte-gjahr vg_augbl vg_augdt RAISING miro_compensada.
        ENDIF.
      ENDIF.

      IF p_cte-docnum_cte IS NOT INITIAL.

        SELECT SINGLE * INTO wa_j_1bnfdoc FROM j_1bnfdoc WHERE docnum EQ p_cte-docnum_cte.

        IF wa_j_1bnfdoc-cancel EQ abap_false.

          DATA(lc_docnum_cte_estornado)    = p_cte-docnum_cte.
          DATA(lc_matnr_estornado)         = p_cte-matns.
          DATA(lc_ck_finalizado_estornado) = p_cte-ck_finalizado.

          CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
            EXPORTING
              doc_number               = p_cte-docnum_cte
              ref_type                 = space
              ref_key                  = space
            IMPORTING
              doc_number               = e_docnum
            EXCEPTIONS
              document_not_found       = 1
              cancel_not_possible      = 2
              nf_cancel_type_not_found = 3
              database_problem         = 4
              docum_lock               = 5
              nfe_cancel_simulation    = 6
              OTHERS                   = 7.

          IF e_docnum IS NOT INITIAL.

            sy-msgty = 'S'.
            sy-msgid = '8B'.
            sy-msgno = '191'.
            sy-msgv1 = p_cte-docnum_cte.
            sy-msgv2 = e_docnum.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
              CHANGING
                p_lc_sequencia = lc_sequencia.

            CLEAR: p_cte-docnum_cte, p_cte-matns.
            p_cte-ck_finalizado = false.

          ELSE.
            IF sy-msgty IS NOT INITIAL.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = sy-msgty
                  p_id           = sy-msgid
                  p_num          = sy-msgno
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDIF.
          ENDIF.
        ELSE.
          sy-msgty = 'W'.
          sy-msgid = 'ZCTE_DISTRI'.
          sy-msgno = '184'.
          sy-msgv1 = p_cte-docnum_cte.

          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
            CHANGING
              p_lc_sequencia = lc_sequencia.

          CLEAR: p_cte-docnum_cte, p_cte-matns.
          p_cte-ck_finalizado = false.
        ENDIF.
      ENDIF.

      IF ( p_cte-belnr IS NOT INITIAL AND p_cte-gjahr IS NOT INITIAL ).

        SELECT SINGLE * INTO wa_rbkp FROM rbkp WHERE belnr EQ p_cte-belnr AND gjahr EQ p_cte-gjahr.
        IF wa_rbkp-stblg IS INITIAL.

          CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
            EXPORTING
              invoicedocnumber          = p_cte-belnr
              fiscalyear                = p_cte-gjahr
              reasonreversal            = 'Z1' "Estorno no período atual
            IMPORTING
              invoicedocnumber_reversal = invoicedocnumber
              fiscalyear_reversal       = fiscalyear
            TABLES
              return                    = it_return.

          LOOP AT it_return INTO wa_return.
            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = wa_return-type
                p_id           = wa_return-id
                p_num          = wa_return-number
                p_message_v1   = wa_return-message_v1
                p_message_v2   = wa_return-message_v2
                p_message_v3   = wa_return-message_v3
                p_message_v4   = wa_return-message_v4
              CHANGING
                p_lc_sequencia = lc_sequencia.
          ENDLOOP.

          IF invoicedocnumber IS NOT INITIAL.

            sy-msgty = 'S'.
            sy-msgid = 'ZCTE_DISTRI'.
            sy-msgno = '084'.
            sy-msgv1 = p_cte-belnr.
            sy-msgv2 = p_cte-gjahr.
            sy-msgv3 = invoicedocnumber.
            sy-msgv4 = fiscalyear.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = true.

            CALL METHOD me->gerar_compensacao
              EXPORTING
                p_cte         = p_cte
                e_invoice_in  = p_cte-belnr
                e_year_in     = p_cte-gjahr
                e_invoice_out = invoicedocnumber
                e_year_out    = fiscalyear
              EXCEPTIONS
                outros        = 1
                OTHERS        = 2.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_sequencia.

            CLEAR: p_cte-belnr,
                   p_cte-gjahr.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            IF lc_docnum_cte_estornado IS NOT INITIAL.
              p_cte-docnum_cte    = lc_docnum_cte_estornado.
              p_cte-matns         = lc_matnr_estornado.
              p_cte-ck_finalizado = lc_ck_finalizado_estornado.
            ENDIF.
          ENDIF.
        ELSE.

          sy-msgty = 'W'.
          sy-msgid = 'ZCTE_DISTRI'.
          sy-msgno = '185'.
          sy-msgv1 = p_cte-belnr.
          sy-msgv2 = p_cte-gjahr.
          sy-msgv3 = wa_rbkp-stblg.
          sy-msgv4 = wa_rbkp-stjah.

          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.

          CLEAR: p_cte-belnr,
                 p_cte-gjahr.
        ENDIF.
      ENDIF.

      UPDATE zib_cte_dist_ter
         SET belnr         = p_cte-belnr
             gjahr         = p_cte-gjahr
             docnum_cte    = p_cte-docnum_cte
             ck_finalizado = p_cte-ck_finalizado
             matns         = p_cte-matns
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      UPDATE zib_cte_dist_n55
         SET belnr = p_cte-belnr
             gjahr = p_cte-gjahr
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      UPDATE zib_cte_dist_n01
         SET belnr = p_cte-belnr
             gjahr = p_cte-gjahr
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

      UPDATE zlest0044
         SET dt_venc = p_cte-zdt_vencto
             belnr   = p_cte-belnr
             gjahr   = p_cte-gjahr
       WHERE chave_cte EQ p_cte-cd_chave_cte.

      COMMIT WORK.

      CALL METHOD me->atribui_dados_vt
        EXPORTING
          p_chave_cte = p_cte-cd_chave_cte
        EXCEPTIONS
          erro        = 1
          OTHERS      = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CHECK p_estornar IS INITIAL.

    SELECT SINGLE * INTO wa_zib_cte_dist_eap
      FROM zib_cte_dist_eap
     WHERE cd_chave_cte  EQ p_cte-cd_chave_cte
       AND tp_aprovacao  EQ '05'
       AND tp_autorizado EQ '01'
       AND ck_ultimo     EQ abap_true.

    CASE sy-subrc.
      WHEN 0.
        ck_liberado = abap_true.
      WHEN OTHERS.
        ck_liberado = abap_false.
    ENDCASE.

    "Cancelado
    IF p_cte-cd_status_sefaz EQ '101' AND ck_liberado NE abap_true.
      MESSAGE e091 WITH p_cte-numr_cte p_cte-numr_serie RAISING fatura.
    ELSEIF p_cte-cd_status_sefaz EQ '101' AND ck_liberado EQ abap_true.
      MESSAGE w091 WITH p_cte-numr_cte p_cte-numr_serie.
    ENDIF.

    IF p_cte-ck_peso_chegada EQ false.
      MESSAGE e087 WITH p_cte-numr_cte p_cte-numr_serie RAISING peso_chegada.
    ENDIF.

    IF p_cte-cd_tipo_cte NE '1' AND p_cte-peso_chegada IS INITIAL.
      MESSAGE e087 WITH p_cte-numr_cte p_cte-numr_serie RAISING peso_chegada.
    ENDIF.

    IF p_cte-cd_tipo_cte NE '1' AND p_cte-dt_chegada IS INITIAL.
      MESSAGE e087 WITH p_cte-numr_cte p_cte-numr_serie RAISING peso_chegada.
    ENDIF.

    IF p_cte-ebeln IS INITIAL OR p_cte-ebelp IS INITIAL.
      MESSAGE e068 RAISING pedido.
    ENDIF.

    IF p_cte-mwskz IS INITIAL.
      MESSAGE e069 RAISING cod_iva.
    ENDIF.

    IF p_cte-zbvtyp IS INITIAL.
      MESSAGE e078 WITH p_cte-p_emissor RAISING banco_parceiro.
    ENDIF.

    IF p_cte-zdt_mov LT sy-datum AND p_cte-belnr IS INITIAL AND p_cte-gjahr IS INITIAL.
      MESSAGE e101 RAISING fatura.
    ENDIF.

    IF p_cte-zquebra GT 0 AND p_cte-zvlr_quebra LE 0 AND p_cte-cd_tipo_cte NE '1'.
      "172  Existe quantidade de quebra e não foi calculado valor de quebra!
      MESSAGE e172 RAISING fatura.
    ENDIF.

    IF p_cte-zperda GT 0 AND p_cte-zvlr_perda LE 0 AND p_cte-cd_tipo_cte NE '1'.
      "173  Existe quantidade de perda e não foi calculado valor de perda!
      MESSAGE e173 RAISING fatura.
    ELSEIF p_cte-zperda GT 0 AND p_cte-cd_tipo_cte NE '1' AND p_cte-zquebra LE 0.
      "173  Existe quantidade de perda e não foi calculado quantidade de quebra!
      MESSAGE e174 RAISING fatura.
    ENDIF.

    IF ( p_cte-cd_modal EQ '04' OR p_cte-cd_modal EQ '06' ) AND p_cte-cd_tipo_cte NE '1'.

      "Buscar Frete Lotação
      SELECT *
        INTO TABLE it_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ p_cte-cd_chave_cte
         AND tknum        NE false
         AND docnum_nfe   NE false.

      e_tipo_contrato = '0001'.

      LOOP AT it_n55 INTO wa_n55.
        IF wa_n55-docnum_nfe IS INITIAL.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * INTO @DATA(wa_itens)
          FROM j_1bnflin
         WHERE docnum EQ @wa_n55-docnum_nfe.

        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
        SELECT SINGLE * INTO @DATA(wa_zlest0154)
          FROM zlest0154
         WHERE bukrs EQ @p_cte-e_tomadora
           AND lifnr EQ @p_cte-p_emissor
           AND matkl EQ @wa_itens-matkl.

        IF sy-subrc IS INITIAL.
          e_tipo_contrato = '0002'.
        ENDIF.
      ENDLOOP.

      IF e_tipo_contrato EQ '0001'.
        IF p_cte-zvlr_vi NE p_cte-zvlr_frete.
          lc_zvalor_ft_peso = p_cte-zvlr_frete / ( p_cte-qt_carga_cte / 1000 ). "Peso do Transportador com Valor do Transportador
          lc_zvalor_vi_peso	= p_cte-zvlr_vi / ( p_cte-peso_origem / 1000 ).     "Peso da Nota Fiscal com VI
          "Se diferênça do valor frete tonelada for maior que R$ 1,00
          IF lc_zvalor_ft_peso NE lc_zvalor_vi_peso AND ck_liberado NE abap_true.
            MESSAGE e102 RAISING fatura.
          ELSEIF lc_zvalor_ft_peso NE lc_zvalor_vi_peso AND ck_liberado EQ abap_true.
            MESSAGE w102.
          ENDIF.
        ENDIF.
      ELSEIF e_tipo_contrato EQ '0002'.
        IF p_cte-zvlr_frete NE p_cte-zvlr_vi.
          lc_zvalor_df_peso = abs( p_cte-zvlr_vi - p_cte-zvlr_frete ).
          lc_zvalor_df_perm = 1.
          "Se diferênça do valor frete tonelada for maior que 0,50 centavos bloquear
          IF lc_zvalor_df_peso GT lc_zvalor_df_perm AND ck_liberado NE abap_true.
            MESSAGE e102 RAISING fatura.
          ELSEIF lc_zvalor_df_peso GT lc_zvalor_df_perm AND ck_liberado EQ abap_true.
            MESSAGE w102.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF p_cte-cd_modal EQ '01' OR p_cte-cd_modal EQ '03'.

      IF p_cte-cd_tipo_cte NE '1'. "Não CT-e de Complemento de Valores/Rodoviário
        IF p_cte-vl_total_merc NE p_cte-zvlr_mercadoria.
          MESSAGE w104 WITH p_cte-vl_total_merc p_cte-zvlr_mercadoria.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDIF.
      ENDIF.

      CASE p_cte-waerk_vi.
        WHEN 'BRL' OR space.
          lc_zvlr_vi = p_cte-zvlr_vi.
        WHEN OTHERS.
          lc_zvlr_vi = p_cte-zvlr_vi * p_cte-kursk_vi.
      ENDCASE.

      "Nos casos de algodão não verificar o valor do frete, verificar o preço tonelada.
      IF lc_zvlr_vi NE p_cte-zvlr_frete AND p_cte-cd_tipo_cte NE '1'.

        lc_zvalor_ft_peso = 0.
        lc_zvalor_vi_peso	= 1.

        "Verifica Nota Fiscal de Algodão """""""""""""""""""""""""""""""""""""""
        lc_cd_chave_cte = p_cte-cd_chave_cte.

        SELECT *
          INTO TABLE it_n55
          FROM zib_cte_dist_n55
         WHERE cd_chave_cte EQ lc_cd_chave_cte
           AND tknum        NE false
           AND docnum_nfe   NE false.

        IF sy-subrc IS INITIAL.
          READ TABLE it_n55 INTO wa_n55 INDEX 1.
          CLEAR: wa_material_nf.
          SELECT SINGLE matnr INTO wa_material_nf-matnr FROM j_1bnflin WHERE docnum EQ wa_n55-docnum_nfe.

          IF wa_material_nf-matnr IS NOT INITIAL.

            SELECT SINGLE * INTO wa_material_nf FROM mara AS m
             WHERE m~matnr EQ wa_material_nf-matnr
               AND EXISTS ( SELECT * FROM zib_cte_dist_gm AS g WHERE g~matkl EQ m~matkl ).

            IF sy-subrc IS INITIAL AND p_cte-cd_modal NE '04' AND p_cte-cd_modal NE '03' AND e_tipo_contrato NE '0002'.
              "Calcula Valor Frete Peso""""""""""""""""""""""""""""">
              IF ( p_cte-cd_tipo_cte EQ '0' OR p_cte-cd_tipo_cte EQ '3' ) AND ( p_cte-qt_carga_cte NE 0 ) AND ( p_cte-zvlr_frete NE 0 ).
                lc_zvalor_ft_peso = p_cte-zvlr_frete / ( p_cte-qt_carga_cte / 1000 ). "Peso do Transportador com Valor do Transportador
              ENDIF.
              """"""""""""""""""""""""""""""""""""""""""""""""""""""<

              "Calcula Valor Frete Peso VT """"""""""""""""""""""""">
              IF ( p_cte-cd_tipo_cte EQ '0' OR p_cte-cd_tipo_cte EQ '3' ) AND ( p_cte-peso_origem NE 0 ) AND ( p_cte-zvlr_vi NE 0 ).
                lc_zvalor_vi_peso	= lc_zvlr_vi / ( p_cte-peso_origem / 1000 ).     "Peso da Nota Fiscal com VI
              ENDIF.
              """"""""""""""""""""""""""""""""""""""""""""""""""""""<
            ELSE.
              lc_zvalor_ft_peso = p_cte-zvlr_frete.
              lc_zvalor_vi_peso = lc_zvlr_vi.
            ENDIF.
          ENDIF.
        ELSE.
          SELECT *
               INTO TABLE it_n01_2
               FROM zib_cte_dist_n01
              WHERE cd_chave_cte EQ lc_cd_chave_cte
                AND tknum        NE false
                AND docnum_nf    NE false.
          IF sy-subrc IS INITIAL.
            READ TABLE it_n01_2 INTO wa_n01 INDEX 1.
            CLEAR: wa_material_nf.
            SELECT SINGLE matnr INTO wa_material_nf-matnr FROM j_1bnflin WHERE docnum EQ wa_n01-docnum_nf.

            IF wa_material_nf-matnr IS NOT INITIAL.

              SELECT SINGLE * INTO wa_material_nf FROM mara AS m
               WHERE m~matnr EQ wa_material_nf-matnr
                 AND EXISTS ( SELECT * FROM zib_cte_dist_gm AS g WHERE g~matkl EQ m~matkl ).

              IF sy-subrc IS INITIAL AND p_cte-cd_modal NE '04' AND p_cte-cd_modal NE '03' AND e_tipo_contrato NE '0002'.
                "Calcula Valor Frete Peso""""""""""""""""""""""""""""">
                IF ( p_cte-cd_tipo_cte EQ '0' OR p_cte-cd_tipo_cte EQ '3' ) AND ( p_cte-qt_carga_cte NE 0 ) AND ( p_cte-zvlr_frete NE 0 ).
                  lc_zvalor_ft_peso = p_cte-zvlr_frete / ( p_cte-qt_carga_cte / 1000 ). "Peso do Transportador com Valor do Transportador
                ENDIF.
                """"""""""""""""""""""""""""""""""""""""""""""""""""""<

                "Calcula Valor Frete Peso VT """"""""""""""""""""""""">
                IF ( p_cte-cd_tipo_cte EQ '0' OR p_cte-cd_tipo_cte EQ '3' ) AND ( p_cte-peso_origem NE 0 ) AND ( p_cte-zvlr_vi NE 0 ).
                  lc_zvalor_vi_peso	= lc_zvlr_vi / ( p_cte-peso_origem / 1000 ).     "Peso da Nota Fiscal com VI
                ENDIF.
                """"""""""""""""""""""""""""""""""""""""""""""""""""""<
              ELSE.
                lc_zvalor_ft_peso = p_cte-zvlr_frete.
                lc_zvalor_vi_peso = lc_zvlr_vi.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        lc_zvalor_df_peso = lc_zvalor_ft_peso - lc_zvalor_vi_peso.
        lc_zvalor_df_peso = abs( lc_zvalor_df_peso ).
        CASE p_cte-cd_modal.
          WHEN 03.

            moeda_condicao_zfre = 'BRL'.

            SELECT * INTO TABLE @DATA(it_itens)
              FROM zib_cte_dist_n55
              WHERE cd_chave_cte EQ @p_cte-cd_chave_cte
                AND fknum        NE @space.

            IF sy-subrc IS INITIAL.
              SELECT * INTO TABLE @DATA(it_vfkp)
                FROM vfkp
                FOR ALL ENTRIES IN @it_itens
               WHERE fknum EQ @it_itens-fknum.

              IF sy-subrc IS INITIAL.
                SELECT * INTO TABLE @DATA(it_konv)
                  FROM konv
                   FOR ALL ENTRIES IN @it_vfkp
                 WHERE knumv EQ @it_vfkp-knumv
                   AND kschl EQ 'ZFRE'.

                IF sy-subrc IS INITIAL.
                  READ TABLE it_konv INDEX 1 INTO DATA(wa_konv).
                  moeda_condicao_zfre = wa_konv-waers.
                ENDIF.
              ENDIF.
            ELSE.
              SELECT * INTO TABLE @DATA(it_itens2)
               FROM zib_cte_dist_n01
               WHERE cd_chave_cte EQ @p_cte-cd_chave_cte
                 AND fknum        NE @space.

              IF sy-subrc IS INITIAL.
                SELECT * INTO TABLE @DATA(it_vfkp2)
                  FROM vfkp
                  FOR ALL ENTRIES IN @it_itens2
                 WHERE fknum EQ @it_itens2-fknum.

                IF sy-subrc IS INITIAL.
                  SELECT * INTO TABLE @DATA(it_konv2)
                    FROM konv
                     FOR ALL ENTRIES IN @it_vfkp2
                   WHERE knumv EQ @it_vfkp2-knumv
                     AND kschl EQ 'ZFRE'.

                  IF sy-subrc IS INITIAL.
                    READ TABLE it_konv2 INDEX 1 INTO DATA(wa_konv2).
                    moeda_condicao_zfre = wa_konv2-waers.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDIF.

            IF p_cte-waerk_vi EQ 'BRL' AND moeda_condicao_zfre NE 'BRL'.
              lc_zvalor_df_perm = 5.
            ELSE.
              lc_zvalor_df_perm = 1 / 2.
            ENDIF.
          WHEN OTHERS.
            lc_zvalor_df_perm = 1 / 2.
        ENDCASE.

        "Se diferênça do valor frete tonelada for maior que 0,50 centavos bloquear
        IF lc_zvalor_df_peso GT lc_zvalor_df_perm AND ck_liberado NE abap_true.
          MESSAGE e102 RAISING fatura.
        ELSEIF lc_zvalor_df_peso GT lc_zvalor_df_perm AND ck_liberado EQ abap_true.
          MESSAGE w102.
        ENDIF.

        IF lc_zvalor_df_peso GT 0.
          MESSAGE w118 WITH lc_zvalor_ft_peso lc_zvalor_vi_peso.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ( p_cte-belnr IS NOT INITIAL AND p_cte-gjahr IS NOT INITIAL ) AND ( p_cte-docnum_cte IS INITIAL ).

*      SELECT SINGLE MATNR INTO LC_MATNR
*        FROM ZLEST0037
*       WHERE BUKRS      EQ P_CTE-E_TOMADORA
*         AND CD_MODAL   EQ P_CTE-CD_MODAL
*         AND LIFNR      EQ P_CTE-P_EMISSOR
*         AND CK_SERVICO EQ FALSE.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        SELECT SINGLE MATNR INTO LC_MATNR
*          FROM ZLEST0037
*         WHERE BUKRS      EQ P_CTE-E_TOMADORA
*           AND CD_MODAL   EQ P_CTE-CD_MODAL
*           AND LIFNR      EQ SPACE
*           AND CK_SERVICO EQ FALSE.
*
*        IF SY-SUBRC IS NOT INITIAL.
*          MESSAGE E103 WITH P_CTE-CD_MODAL P_CTE-E_TOMADORA P_CTE-P_EMISSOR RAISING FATURA.
*        ENDIF.
*      ENDIF.

*-CS2024000597-14.10.2024-#146076-JT-inicio
      IF p_cte-docnum_cte_sub IS NOT INITIAL AND p_cte-cd_modal EQ '03'.
        SELECT SINGLE * INTO @wa_zlest0061 "@DATA(wa_zlest0061) "*-CS2024000597-14.10.2024-#146076-JT-inicio
          FROM zlest0061
         WHERE docnum     EQ @p_cte-docnum_cte_sub
           AND ck_anulado EQ @abap_false.
      ENDIF.
*-CS2024000597-14.10.2024-#146076-JT-fim

      CALL METHOD me->gerar_escrit_entrada
        EXPORTING
          p_zlest0061       = wa_zlest0061   "*-CS2024000597-14.10.2024-#146076-JT-inicio
        CHANGING
          p_cte             = p_cte
        EXCEPTIONS
          nao_enc_frete     = 1
          nao_cte_forn      = 2
          nao_param_iva     = 3
          nao_servico_param = 4
          nao_param_cfop    = 5
          erro              = 6
          OTHERS            = 7.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.

      CALL METHOD me->atribui_dados_vt
        EXPORTING
          p_chave_cte = p_cte-cd_chave_cte
        EXCEPTIONS
          erro        = 1
          OTHERS      = 2.

      IF sy-subrc IS NOT INITIAL.
        CALL METHOD me->add_log_cte_dist
          EXPORTING
            p_cd_chave_cte = p_cte-cd_chave_cte
            p_type         = sy-msgty
            p_id           = sy-msgid
            p_num          = sy-msgno
            p_message_v1   = sy-msgv1
            p_message_v2   = sy-msgv2
            p_message_v3   = sy-msgv3
            p_message_v4   = sy-msgv4
          CHANGING
            p_lc_sequencia = lc_sequencia.
      ENDIF.
      COMMIT WORK.

    ELSEIF ( p_cte-belnr  IS INITIAL ) AND
       ( p_cte-ebeln  IS NOT INITIAL ) AND
       ( p_cte-ebelp  IS NOT INITIAL ) AND
       ( p_cte-mwskz  IS NOT INITIAL ) AND
       ( p_cte-zbvtyp IS NOT INITIAL ).

      IF p_cte-cd_tipo_cte EQ '1'. "CT-e de Complemento de Valores
        lc_cd_chave_cte = wa_ter_cmpl-cd_chave_cte.
      ELSE.
        lc_cd_chave_cte = p_cte-cd_chave_cte.
      ENDIF.

      SELECT *
        INTO TABLE it_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ lc_cd_chave_cte
         AND tknum        NE false.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "" Verificar se Chaves foram pagas por outra CT-e """""""""""""""""""""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF it_n55 IS NOT INITIAL AND ( p_cte-cd_modal NE '04' AND p_cte-cd_modal NE '03' AND e_tipo_contrato NE '0002' ).
        SELECT * INTO TABLE it_n55_outra
          FROM zib_cte_dist_n55 AS n
           FOR ALL ENTRIES IN it_n55
         WHERE n~n55_chave_acesso EQ it_n55-n55_chave_acesso
           AND n~cd_chave_cte     NE it_n55-cd_chave_cte
           AND EXISTS ( SELECT * FROM zib_cte_dist_ter AS t
                         WHERE t~cd_chave_cte  EQ n~cd_chave_cte
                           AND t~ck_finalizado EQ 'X'
                           AND t~cd_modal      EQ p_cte-cd_modal ).

        IF it_n55_outra IS NOT INITIAL.
          READ TABLE it_n55_outra INDEX 1 INTO wa_n55.
          MESSAGE e099 WITH wa_n55-cd_chave_cte+25(9) wa_n55-n55_chave_acesso+25(9) RAISING fatura.
        ENDIF.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      SELECT *
        INTO TABLE it_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ lc_cd_chave_cte
         AND tknum        NE false.

      IF it_n55 IS NOT INITIAL.
        SELECT *
          INTO TABLE it_essr
          FROM essr
           FOR ALL ENTRIES IN it_n55
         WHERE lblni EQ it_n55-lblni.
      ENDIF.

      IF it_n01 IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_essr
          FROM essr
           FOR ALL ENTRIES IN it_n01
         WHERE lblni EQ it_n01-lblni.
      ENDIF.

      SORT it_essr BY lblni.

      CASE p_cte-waerk_vi.
        WHEN 'BRL' OR space.

          CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
            EXPORTING
              p_bukrs           = p_cte-e_tomadora
              p_lifnr           = p_cte-p_emissor
              p_valor           = p_cte-valor_prestacao
              p_bvtyp           = p_cte-zbvtyp  "Tipo de banco do parceiro do Fornecedor do Frete
            IMPORTING
              p_forma_pagamento = wa_headerdata-pymt_meth
              p_princ_bnc_emp   = wa_headerdata-housebankid
            EXCEPTIONS
              nao_fornecedor    = 1
              fornecedor_conta  = 2
              fornecedor_banco  = 3
              faixa_valor       = 4
              banco_empresa     = 5
              OTHERS            = 6.

        WHEN OTHERS.

          DATA: p_valor TYPE netwr_fp.

          p_valor = p_cte-zvlr_vi.

          CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
            EXPORTING
              p_bukrs           = p_cte-e_tomadora
              p_lifnr           = p_cte-p_emissor
              p_valor           = p_valor
              p_cotacao         = p_cte-kursk_vi
              p_bvtyp           = p_cte-zbvtyp  "Tipo de banco do parceiro do Fornecedor do Frete
            IMPORTING
              p_forma_pagamento = wa_headerdata-pymt_meth
              p_princ_bnc_emp   = wa_headerdata-housebankid
            EXCEPTIONS
              nao_fornecedor    = 1
              fornecedor_conta  = 2
              fornecedor_banco  = 3
              faixa_valor       = 4
              banco_empresa     = 5
              OTHERS            = 6.

      ENDCASE.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING banco_empresa.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_cte-numr_cte
        IMPORTING
          output = p_cte-numr_cte.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_cte-numr_serie
        IMPORTING
          output = p_cte-numr_serie.

      LOOP AT it_n55 INTO wa_n55.

        IF wa_n55-docnum_nfe IS INITIAL.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * INTO wa_itens
          FROM j_1bnflin
         WHERE docnum EQ wa_n55-docnum_nfe.
      ENDLOOP.

      CALL METHOD me->busca_impostos_taxas
        EXPORTING
          p_iva            = p_cte-mwskz
          p_data_documento = p_cte-dt_emissao
          p_shipfrom       = p_cte-inicio_uf
          p_shipto         = p_cte-termino_uf
          e_tomadora       = p_cte-e_tomadora
          f_tomadora       = p_cte-f_tomadora
          p_emissora       = p_cte-p_emissor
          p_matnr          = wa_itens-matnr
        IMPORTING
          e_rate_icms      = e_rate_icms
          e_rate_pis       = e_rate_pis
          e_rate_cofins    = e_rate_cofins
        EXCEPTIONS
          sem_iva          = 1
          OTHERS           = 2.

      wa_headerdata-invoice_ind    = true.
      wa_headerdata-doc_type       = 'FT'.
      wa_headerdata-doc_date       = p_cte-dt_emissao.
      wa_headerdata-pstng_date     = p_cte-zdt_mov.
      wa_headerdata-bline_date     = p_cte-zdt_vencto.
      wa_headerdata-comp_code      = p_cte-e_tomadora.
      wa_headerdata-diff_inv       = p_cte-p_emissor.
      wa_headerdata-currency       = 'BRL'.
      wa_headerdata-header_txt     = 'Frete Terceiro'.
      "Retirado bloqueio de pagamento CS2018002738
      "WA_HEADERDATA-PMNT_BLOCK     = 'A'.

      "Colocar Bloqueio para pagamento ferroviário e multimodal
      "CS2018002868
      "01	Rodoviário
      "02	Aéreo
      "03	Aquaviário
      "04	Ferroviário
      "05	Dutoviário
      "06	Multimodal
      IF p_cte-cd_modal NE '01' AND p_cte-cd_modal NE '04' AND p_cte-cd_modal NE '06'.
        wa_headerdata-pmnt_block = 'A'.

        "=============================USER STORY 9516  /  Anderson Oenning
      ELSEIF p_cte-cd_tipo_cte EQ '1'. "Tipo de CTE for complemetar tipo 1, criar a MIRO desbloqueada.
        wa_headerdata-pmnt_block = space.
      ENDIF.
      "=============================USER STORY 9516  /  Anderson Oenning



      IF p_cte-docnum_cte_sub IS NOT INITIAL AND p_cte-cd_modal EQ '03'.
        SELECT SINGLE * INTO @wa_zlest0061 "@DATA(wa_zlest0061) "*-CS2024000597-14.10.2024-#146076-JT-inicio
          FROM zlest0061
         WHERE docnum     EQ @p_cte-docnum_cte_sub
           AND ck_anulado EQ @abap_false.

        IF sy-subrc IS INITIAL.
          p_cte-zterm            = wa_zlest0061-zterm.
          wa_headerdata-pmnttrms = wa_zlest0061-zterm.
          TRY .
              wa_headerdata-bline_date =
              zcl_miro=>get_data_vencimento_cond_pag(
                    i_zterm         = wa_zlest0061-zterm
                    i_dt_documento  = p_cte-dt_emissao
                    i_dt_lancamento = p_cte-zdt_mov
                    i_dt_contabil   = p_cte-zdt_mov ).
            CATCH zcx_miro_exception INTO DATA(ex_miro).
              ex_miro->published_erro( EXPORTING i_msgty ='S' i_msgty_display ='S' ).
              MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING fatura.
          ENDTRY.
          p_cte-zdt_vencto = wa_headerdata-bline_date.
        ELSE.
          MESSAGE e210 RAISING sem_vt.
        ENDIF.
      ELSE.
        p_cte-zterm            = '0004'.
        wa_headerdata-pmnttrms = '0004'.
      ENDIF.

      wa_headerdata-del_costs_taxc = p_cte-mwskz.
      IF p_cte-zvlr_quebra LT 0.
        wa_headerdata-gross_amount   = p_cte-zvlr_frete - p_cte-zvlr_perda.
      ELSE.
        wa_headerdata-gross_amount   = p_cte-zvlr_frete - p_cte-zvlr_perda - p_cte-zvlr_quebra.
      ENDIF.
      wa_headerdata-alloc_nmbr     = p_cte-ebeln. "Pedido de Compra de Frete
      wa_headerdata-bus_area       = p_cte-f_tomadora.
      wa_headerdata-calc_tax_ind   = true.
      wa_headerdata-goods_affected = true.
      wa_headerdata-partner_bk     = p_cte-zbvtyp.

      CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
        EXPORTING
          series     = p_cte-numr_serie
          nf_number9 = p_cte-numr_cte
        IMPORTING
          ref_number = wa_headerdata-ref_doc_no.

      "VL_RETIDO = 0.

      LOOP AT it_n01 INTO wa_n01.
        MOVE-CORRESPONDING wa_n01 TO wa_n55.
        APPEND wa_n55 TO it_n55.
      ENDLOOP.

*      "========================================================================== BUG 59796 - AOENNING.
      "Pagamento por Doc. de Transporte
      SORT it_n55 BY tknum.
      IF p_cte-cd_modal EQ '01'.
        it_n55_aux1[] = it_n55[].
        it_n55_aux2[] = it_n55[].

        DELETE ADJACENT DUPLICATES FROM it_n55_aux1 COMPARING tknum.
        REFRESH it_n55[].

        LOOP AT it_n55_aux1 INTO DATA(ws_n55_aux1).
          CLEAR: zlr_frete_2, zlr_perda_1, zlr_quebra_1, zvlr_vi_1.
          LOOP AT it_n55_aux2 INTO DATA(ws_n55_aux2) WHERE tknum EQ ws_n55_aux1-tknum.
            ADD ws_n55_aux2-zvlr_frete  TO zlr_frete_2.

            ADD ws_n55_aux2-zvlr_perda  TO zlr_perda_1.
            ADD ws_n55_aux2-zvlr_quebra TO zlr_quebra_1.
            ADD ws_n55_aux2-zvlr_vi     TO zvlr_vi_1.

            MOVE-CORRESPONDING ws_n55_aux2 TO wa_n55.
          ENDLOOP.
          wa_n55-zvlr_perda  = zlr_perda_1.
          wa_n55-zvlr_quebra = zlr_quebra_1.
          wa_n55-zvlr_vi     = zvlr_vi_1.

          wa_n55-zvlr_frete = zlr_frete_2.
          APPEND wa_n55 TO it_n55.
        ENDLOOP.
      ELSE.
        DELETE ADJACENT DUPLICATES FROM it_n55 COMPARING tknum.
      ENDIF.
*      "==========================================================================



      "Verificar Local de Negócio da VT
      "Encontrar Tipo de transporte
      "Verificar Emissor do documento
      IF it_n55 IS NOT INITIAL.
        SELECT * INTO TABLE it_vttk
          FROM vttk
           FOR ALL ENTRIES IN it_n55
         WHERE tknum EQ it_n55-tknum.

        LOOP AT it_vttk INTO wa_vttk.
          "Nº do agente de frete
          IF wa_vttk-tdlnr NE p_cte-p_emissor.
            MESSAGE e089 WITH wa_vttk-tknum wa_vttk-tdlnr p_cte-p_emissor RAISING sem_vt.
          ENDIF.

          DATA: lc_centro_real TYPE  werks_d.

          CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
            EXPORTING
              centro               = wa_vttk-tplst
            IMPORTING
              centro_real          = lc_centro_real
            EXCEPTIONS
              informar_centro      = 1
              nao_centro_r_virtual = 2
              informar_centro_out  = 3
              informar_centro_v    = 4
              OTHERS               = 5.

          "Local de organização de transportes
          IF lc_centro_real NE p_cte-f_tomadora.
            MESSAGE e096 WITH wa_vttk-tknum wa_vttk-tplst p_cte-f_tomadora RAISING sem_vt.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF wa_vttk-shtyp IS INITIAL.
        MESSAGE e079 RAISING sem_vt.
      ENDIF.

      CLEAR: it_contas.
      vg_rblgp  = 1.

      SELECT SINGLE * INTO wa_lfa1
        FROM lfa1
       WHERE lifnr EQ p_cte-p_emissor.

      lva_lifnr_toma = p_cte-f_tomadora.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lva_lifnr_toma
        IMPORTING
          output = lva_lifnr_toma.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_toma)
       WHERE lifnr = @lva_lifnr_toma.
      IF sy-subrc NE 0 OR lva_lifnr_toma IS INITIAL.
        CLEAR: lwa_lfa1_toma.
      ENDIF.

      IF p_cte-cd_tipo_cte EQ '1'. "CT-e de Complemento de Valores

        READ TABLE it_essr INTO wa_essr INDEX 1.
        "Foi retirado pelo fato da BSIK pegar a data de vencimento do documento original, e não acatar a da MIRO
        "WA_HEADERDATA-INV_REF_NO = WA_TER_CMPL-BELNR.
        "WA_HEADERDATA-INV_YEAR   = WA_TER_CMPL-GJAHR.

        IF e_rate_icms GT 0.
          lc_zvalor_icms = p_cte-zvlr_frete * ( e_rate_icms / 100 ).
        ELSE.
          lc_zvalor_icms = 0.
        ENDIF.

        DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( p_cte-zvlr_frete )
                                                                               i_valor_icms  =   CONV #( lc_zvalor_icms ) ).

        IF e_rate_pis GT 0.
          lc_zvalor_pis = lva_base_calc_pis_cofins * ( e_rate_pis / 100 ).
        ELSE.
          lc_zvalor_pis = 0.
        ENDIF.

        IF e_rate_cofins GT 0.
          lc_zvalor_cofins = lva_base_calc_pis_cofins * ( e_rate_cofins / 100 ).
        ELSE.
          lc_zvalor_cofins = 0.
        ENDIF.

        CLEAR: wa_itemdata.
        wa_itemdata-invoice_doc_item = vg_rblgp.
        wa_itemdata-po_number        = p_cte-ebeln.
        wa_itemdata-po_item          = p_cte-ebelp.
        wa_itemdata-tax_code         = p_cte-mwskz.

        IF wa_lfa1-txjcd(2) NE p_cte-termino_uf(2).
          CONCATENATE p_cte-termino_uf p_cte-termino_ibge INTO wa_itemdata-taxjurcode SEPARATED BY space.
        ELSE.
          wa_itemdata-taxjurcode = lwa_lfa1_toma-txjcd.
        ENDIF.

        wa_itemdata-ref_doc          = wa_essr-lblni.
        wa_itemdata-ref_doc_year     = wa_essr-budat(4).

*         Quando transporte rodoviário (ZIB_CTE_DIST_TER-CD_MODAL = ‘1’) e frete complementar (ZIB_CTE_DIST_TER-CD_TIPO_CTE = ‘1’ ) vamos gerar uma miro de débito posterior
*         Como é debito posterior não precisamos passar a folha de serviço. - PBI 26775 - BG

        IF ( p_cte-cd_modal EQ '01' or
             p_cte-cd_modal EQ '03' or " Rubenilson Pereira - 02.05.24 - US84542
             p_cte-cd_modal EQ '04' or " Rubenilson Pereira - 02.05.24 - US84542
             p_cte-cd_modal EQ '06' )  " Rubenilson Pereira - 02.05.24 - US84542
         AND p_cte-cd_tipo_cte EQ '1'.
          wa_itemdata-de_cre_ind = 'X'.
          "wa_itemdata-sheet_no = ' '.
          "ELSE.

        ENDIF.

        wa_itemdata-sheet_no         = wa_essr-lblni.
        "Valor do Servço menos os impostos
        wa_itemdata-item_amount      = p_cte-zvlr_frete - ( lc_zvalor_icms + lc_zvalor_pis + lc_zvalor_cofins ).
        APPEND wa_itemdata TO it_itemdata.

        CLEAR: e_contas.
        IF  wa_itemdata-de_cre_ind <> 'X'.
          CALL METHOD me->buscar_contas_miro
            EXPORTING
              i_cte           = p_cte
              i_shtyp         = wa_vttk-shtyp
              i_vsart         = wa_vttk-vsart
              i_item_invoice  = vg_rblgp
              i_valor_item    = wa_itemdata-item_amount
              i_valor_perda   = p_cte-zvlr_perda
              i_valor_quebra  = p_cte-zvlr_quebra
              i_valor_vi      = p_cte-zvlr_frete
              i_dt_referencia = wa_essr-budat
            IMPORTING
              e_contas        = e_contas
            EXCEPTIONS
              param_ctb       = 1
              OTHERS          = 2.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING param_ctb.
          ELSE.
            LOOP AT e_contas INTO wa_contas.
              APPEND wa_contas TO it_contas.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        LOOP AT it_n55 INTO wa_n55.

          READ TABLE it_essr INTO wa_essr WITH KEY lblni = wa_n55-lblni BINARY SEARCH.

          IF e_rate_icms GT 0.
            lc_zvalor_icms = wa_n55-zvlr_frete * ( e_rate_icms / 100 ).
          ELSE.
            lc_zvalor_icms = 0.
          ENDIF.

          lva_base_calc_pis_cofins = zcl_cte_dist_g=>get_base_pis_cofins(  i_valor_frete =   CONV #( wa_n55-zvlr_frete )
                                                                           i_valor_icms  =   CONV #( lc_zvalor_icms ) ).

          IF e_rate_pis GT 0.
            lc_zvalor_pis = lva_base_calc_pis_cofins * ( e_rate_pis / 100 ).
          ELSE.
            lc_zvalor_pis = 0.
          ENDIF.

          IF e_rate_cofins GT 0.
            lc_zvalor_cofins = lva_base_calc_pis_cofins * ( e_rate_cofins / 100 ).
          ELSE.
            lc_zvalor_cofins = 0.
          ENDIF.

          CLEAR: wa_itemdata.
          wa_itemdata-invoice_doc_item = vg_rblgp.
          wa_itemdata-po_number        = wa_n55-ebeln.
          wa_itemdata-po_item          = wa_n55-ebelp.
          wa_itemdata-tax_code         = p_cte-mwskz.

          IF wa_lfa1-txjcd(2) NE p_cte-termino_uf(2).
            CONCATENATE p_cte-termino_uf p_cte-termino_ibge INTO wa_itemdata-taxjurcode SEPARATED BY space.
          ELSE.
            wa_itemdata-taxjurcode = lwa_lfa1_toma-txjcd.
          ENDIF.

          wa_itemdata-ref_doc          = wa_essr-lblni.
          wa_itemdata-ref_doc_year     = wa_essr-budat(4).
          wa_itemdata-sheet_no         = wa_essr-lblni.
          "Valor do Servço menos os impostos
          wa_itemdata-item_amount      = wa_n55-zvlr_frete - ( lc_zvalor_icms + lc_zvalor_pis + lc_zvalor_cofins ).
          APPEND wa_itemdata TO it_itemdata.

          CLEAR: e_contas.
          IF  wa_itemdata-de_cre_ind <> 'X'.
            IF wa_n55-waerk_vi EQ 'BRL'.
              lva_vlr_vi_brl = wa_n55-zvlr_vi.
            ELSE.
              lva_vlr_vi_brl = wa_n55-zvlr_vi * wa_n55-kursk_vi.
            ENDIF.

            CALL METHOD me->buscar_contas_miro
              EXPORTING
                i_cte           = p_cte
                i_shtyp         = wa_vttk-shtyp
                i_vsart         = wa_vttk-vsart
                i_item_invoice  = vg_rblgp
                i_valor_item    = wa_itemdata-item_amount
                i_valor_perda   = wa_n55-zvlr_perda
                i_valor_quebra  = wa_n55-zvlr_quebra
                i_valor_vi      = lva_vlr_vi_brl
                i_dt_referencia = wa_essr-budat
              IMPORTING
                e_contas        = e_contas
              EXCEPTIONS
                param_ctb       = 1
                OTHERS          = 2.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING param_ctb.
            ELSE.
              LOOP AT e_contas INTO wa_contas.
                APPEND wa_contas TO it_contas.
              ENDLOOP.
            ENDIF.
          ENDIF.
          ADD 1 TO vg_rblgp.
        ENDLOOP.
      ENDIF.

      CLEAR: invoicedocnumber, fiscalyear.

      CALL METHOD me->autorizacao_verificar
        EXPORTING
          i_cte     = p_cte
        EXCEPTIONS
          bloqueado = 1
          OTHERS    = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING nao_autorizado.
      ENDIF.

      CASE p_cte-cd_modal.
        WHEN '03'.

          CALL METHOD zcl_cte_dist_g=>verifica_vencimento_fatura
            EXPORTING
              i_data_vencimento = wa_headerdata-bline_date
              i_valida_dia_util = abap_false
            EXCEPTIONS
              nao_valida        = 1
              OTHERS            = 2.

        WHEN OTHERS.
          "IF sy-cprog NE 'ZMMR183'.
          CALL METHOD zcl_cte_dist_g=>verifica_vencimento_fatura
            EXPORTING
              i_data_vencimento = wa_headerdata-bline_date
              i_valida_dia_util = abap_true
            EXCEPTIONS
              nao_valida        = 1
              OTHERS            = 2.
          "ENDIF.
      ENDCASE.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING fatura.
      ENDIF.

      IF p_cte-waerk_vi NE 'BRL' AND p_cte-waerk_vi NE space.

        wa_headerdata-currency  = p_cte-waerk_vi.
        wa_headerdata-exch_rate = p_cte-kursk_vi.
        wa_headerdata-gross_amount = wa_headerdata-gross_amount / p_cte-kursk_vi.

        LOOP AT it_itemdata ASSIGNING FIELD-SYMBOL(<fs_item>).
          <fs_item>-item_amount = <fs_item>-item_amount / p_cte-kursk_vi.
        ENDLOOP.

        LOOP AT it_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
          <fs_conta>-item_amount = <fs_conta>-item_amount / p_cte-kursk_vi.
        ENDLOOP.

      ENDIF.

      TRY .

          "Procurar MIRO já criada
          DATA(r_rbkp) = zcl_cte_dist_g=>get_miro_frete( EXPORTING i_cte = p_cte ).
          invoicedocnumber = r_rbkp-belnr.
          fiscalyear       = r_rbkp-gjahr.
          p_cte-belnr = invoicedocnumber.
          p_cte-gjahr = fiscalyear.

        CATCH zcx_cte_dist_g INTO DATA(ex_erro).

          CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
            EXPORTING
              p_lifnr  = wa_headerdata-diff_inv
              p_nftype = me->get_nftype_entrada( )
              p_xblnr  = wa_headerdata-ref_doc_no
              p_data   = p_cte-dt_emissao
              p_werks  = wa_headerdata-bus_area
            EXCEPTIONS
              error    = 1
              OTHERS   = 2.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING fatura.
          ENDIF.

          DATA: clear_memory TYPE c.
          clear_memory = abap_true.
          EXPORT clear_memory FROM clear_memory TO MEMORY ID 'CLEAR_MEMORY_EKBE_MIRO'.

          CALL FUNCTION 'ME_REFRESH_GOODS_RECEIPT'. "Limpar Tabelas Buffer

          CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              headerdata       = wa_headerdata
            IMPORTING
              invoicedocnumber = invoicedocnumber
              fiscalyear       = fiscalyear
            TABLES
              itemdata         = it_itemdata
              glaccountdata    = it_contas
              return           = it_return.

          IF ( invoicedocnumber IS NOT INITIAL ) AND ( fiscalyear IS NOT INITIAL ).

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = true.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            "" Gravar Texto de Invoice """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            wa_header-tdobject = 'RBKP'.
            CONCATENATE invoicedocnumber fiscalyear INTO wa_header-tdname.
            wa_header-tdid    = '0001'.
            wa_header-tdspras = sy-langu.

            wa_lines-tdformat = '*'.
            wa_lines-tdline   = 'MONITOR DE PAGAMENTO DE FRETE DE TERCEIRO'.
            APPEND wa_lines TO it_lines.

            CONCATENATE 'CT-e' p_cte-cd_chave_cte INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvlr_frete TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor Frete:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvlr_perda TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor Perda:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvlr_quebra TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor Quebra:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvalor_pedagio TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor Pedágio:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvalor_icms TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor ICMS:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvalor_pis TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor PIS:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            WRITE p_cte-zvalor_cofins TO ca_brtwr.
            CONDENSE ca_brtwr NO-GAPS.
            CONCATENATE 'Valor COFINS:' ca_brtwr INTO wa_lines-tdline SEPARATED BY space.
            APPEND wa_lines TO it_lines.

            CALL FUNCTION 'SAVE_TEXT'
              EXPORTING
                header          = wa_header
                savemode_direct = true
              TABLES
                lines           = it_lines
              EXCEPTIONS
                id              = 1
                language        = 2
                name            = 3
                object          = 4
                OTHERS          = 5.

            IF sy-subrc <> 0.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = sy-msgty
                  p_id           = sy-msgid
                  p_num          = sy-msgno
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDIF.

            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            wa_return-type       = 'S'.
            wa_return-id         = 'M8'.
            wa_return-number     = 060.
            wa_return-message_v1 = invoicedocnumber.

            CALL METHOD me->add_log_cte_dist
              EXPORTING
                p_cd_chave_cte = p_cte-cd_chave_cte
                p_type         = wa_return-type
                p_id           = wa_return-id
                p_num          = wa_return-number
                p_message_v1   = wa_return-message_v1
              CHANGING
                p_lc_sequencia = lc_sequencia.

            p_cte-belnr = invoicedocnumber.
            p_cte-gjahr = fiscalyear.

          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            LOOP AT it_return INTO wa_return.
              CALL METHOD me->add_log_cte_dist
                EXPORTING
                  p_cd_chave_cte = p_cte-cd_chave_cte
                  p_type         = wa_return-type
                  p_id           = wa_return-id
                  p_num          = wa_return-number
                  p_message_v1   = wa_return-message_v1
                  p_message_v2   = wa_return-message_v2
                  p_message_v3   = wa_return-message_v3
                  p_message_v4   = wa_return-message_v4
                CHANGING
                  p_lc_sequencia = lc_sequencia.
            ENDLOOP.
            COMMIT WORK.

          ENDIF.

      ENDTRY.

      IF ( invoicedocnumber IS NOT INITIAL ) AND ( fiscalyear IS NOT INITIAL ).

        UPDATE zib_cte_dist_ter
           SET belnr = invoicedocnumber
               gjahr = fiscalyear
         WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

        UPDATE zib_cte_dist_n55
           SET belnr = invoicedocnumber
               gjahr = fiscalyear
         WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

        UPDATE zib_cte_dist_n01
           SET belnr = invoicedocnumber
               gjahr = fiscalyear
         WHERE cd_chave_cte EQ p_cte-cd_chave_cte.

        UPDATE zlest0044
           SET belnr   = invoicedocnumber
               gjahr   = fiscalyear
               dt_venc = p_cte-zdt_vencto
         WHERE chave_cte EQ p_cte-cd_chave_cte.

        COMMIT WORK.

        CALL METHOD me->gerar_escrit_entrada
          EXPORTING
            p_zlest0061       = wa_zlest0061   "*-CS2024000597-14.10.2024-#146076-JT-inicio
          CHANGING
            p_cte             = p_cte
          EXCEPTIONS
            nao_enc_frete     = 1
            nao_cte_forn      = 2
            nao_param_iva     = 3
            nao_servico_param = 4
            nao_param_cfop    = 5
            erro              = 6
            OTHERS            = 7.

        IF sy-subrc IS NOT INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDIF.

        CALL METHOD me->atribui_dados_vt
          EXPORTING
            p_chave_cte = p_cte-cd_chave_cte
          EXCEPTIONS
            erro        = 1
            OTHERS      = 2.

        IF sy-subrc IS NOT INITIAL.
          CALL METHOD me->add_log_cte_dist
            EXPORTING
              p_cd_chave_cte = p_cte-cd_chave_cte
              p_type         = sy-msgty
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_sequencia.
        ENDIF.

      ENDIF.
    ELSE.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e000 RAISING nao_enc_frete.
      ENDIF.
      IF ( p_cte-belnr IS NOT INITIAL ).
        MESSAGE e067 RAISING fatura.
      ENDIF.
      IF ( p_cte-ebeln IS INITIAL ) OR ( p_cte-ebelp IS INITIAL ).
        MESSAGE e068 RAISING pedido.
      ENDIF.
      IF ( p_cte-mwskz IS INITIAL ).
        MESSAGE e069 RAISING cod_iva.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GERAR_PAGAMENTO_AUTOMATICO.

    TYPES: R_E_TOMADORA TYPE RANGE OF ZIB_CTE_DIST_TER-E_TOMADORA.

    DATA: LC_CTE TYPE REF TO ZCL_CTE_DIST_G.

    DATA: WA_LINHA            TYPE TY_CTE_AUTO,
          IT_LINHA            TYPE TABLE OF TY_CTE_AUTO,
          DT_CHEGADA          TYPE SY-DATUM,
          DT_CORTE            TYPE SY-DATUM,
          LC_ZIB_CTE_DIST_TER TYPE ZIB_CTE_DIST_TER.

    TYPES BEGIN OF TY_CHAVE.
    TYPES: CHAVE  TYPE ZDE_CHAVE_DOC_E,
           DOCNUM TYPE J_1BDOCNUM.
    TYPES END OF TY_CHAVE.

    DATA: IT_CD_CHAVE_NFE	TYPE TABLE OF TY_CHAVE,
          WA_CD_CHAVE_NFE TYPE TY_CHAVE.

    DT_CHEGADA = SY-DATUM - 30.
    DT_CORTE   = SY-DATUM - 2.

    SELECT A~DOCNUM,
           A~PESOTRANSB  AS PESO_CHEGADA,
           A~DATATRANSB  AS DT_CHEGADA
      INTO TABLE @DATA(IT_DOCUMENTOS)
      FROM ZLEST0039 AS A
     WHERE A~DATATRANSB GE @DT_CHEGADA.

    SELECT A~DOCNUM,
           A~PESOCHEGADA AS PESO_CHEGADA,
           A~DATACHEGADA AS DT_CHEGADA
 APPENDING TABLE @IT_DOCUMENTOS
      FROM ZLEST0039 AS A
     WHERE DATATRANSB  EQ '00000000'
       AND DATACHEGADA GE @DT_CHEGADA.

    CHECK IT_DOCUMENTOS[] IS NOT INITIAL.

    SELECT *
      INTO TABLE @DATA(IT_J_1BNFE_ACTIVE)
      FROM J_1BNFE_ACTIVE
       FOR ALL ENTRIES IN @IT_DOCUMENTOS
     WHERE DOCNUM EQ @IT_DOCUMENTOS-DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    LOOP AT IT_J_1BNFE_ACTIVE INTO DATA(WA_J_1BNFE_ACTIVE).
      WA_CD_CHAVE_NFE-CHAVE = WA_J_1BNFE_ACTIVE-REGIO &&
                       WA_J_1BNFE_ACTIVE-NFYEAR &&
                       WA_J_1BNFE_ACTIVE-NFMONTH &&
                       WA_J_1BNFE_ACTIVE-STCD1 &&
                       WA_J_1BNFE_ACTIVE-MODEL &&
                       WA_J_1BNFE_ACTIVE-SERIE &&
                       WA_J_1BNFE_ACTIVE-NFNUM9 &&
                       WA_J_1BNFE_ACTIVE-DOCNUM9 &&
                       WA_J_1BNFE_ACTIVE-CDV.
      WA_CD_CHAVE_NFE-DOCNUM = WA_J_1BNFE_ACTIVE-DOCNUM.
      APPEND WA_CD_CHAVE_NFE TO IT_CD_CHAVE_NFE.
    ENDLOOP.

    "Busca Notas dos CT-es
    SELECT *
       INTO TABLE @DATA(IT_ZIB_CTE_DIST_N55)
       FROM ZIB_CTE_DIST_N55
        FOR ALL ENTRIES IN @IT_CD_CHAVE_NFE
      WHERE N55_CHAVE_ACESSO EQ @IT_CD_CHAVE_NFE-CHAVE.

    CHECK SY-SUBRC IS INITIAL.

    "Define Empresas Tomadoras
    DATA(R_E_TOMADORA) = VALUE R_E_TOMADORA( ( SIGN = 'I' OPTION = 'EQ' LOW = '0001' )
                                             ( SIGN = 'I' OPTION = 'EQ' LOW = '0015' )
                                             ( SIGN = 'I' OPTION = 'EQ' LOW = '0035' ) ).
    SELECT *
      INTO TABLE @DATA(IT_ZIB_CTE_DIST_TER)
      FROM ZIB_CTE_DIST_TER AS TER
       FOR ALL ENTRIES IN @IT_ZIB_CTE_DIST_N55
     WHERE CD_CHAVE_CTE   EQ @IT_ZIB_CTE_DIST_N55-CD_CHAVE_CTE
       AND CK_FINALIZADO  EQ @ABAP_FALSE
       AND DT_EMISSAO     LE @DT_CORTE
       AND CD_MODAL       EQ '01'
       AND CANCEL         EQ @ABAP_FALSE
       AND MWSKZ          NE @ABAP_FALSE
       AND P_EMISSOR      NE @ABAP_FALSE
       AND E_TOMADORA     NE @ABAP_FALSE
       AND F_TOMADORA     NE @ABAP_FALSE
       AND CD_TIPO_CTE    EQ '0'
       AND E_TOMADORA     IN @R_E_TOMADORA
       AND NOT EXISTS ( SELECT * FROM LFA1 AS L WHERE L~STCD1 EQ TER~EMIT_CNPJ AND L~KTOKK = 'ZFIC' )
       AND NOT EXISTS ( SELECT * FROM ZIB_CTE_DIST_EAP AS TR WHERE TR~CD_CHAVE_CTE EQ TER~CD_CHAVE_CTE AND TR~CK_ULTIMO EQ 'X' AND TR~TP_APROVACAO EQ '03' AND TR~TP_AUTORIZADO EQ '01' ).

    CHECK SY-SUBRC IS INITIAL.

    LOOP AT IT_ZIB_CTE_DIST_TER INTO DATA(WA_ZIB_CTE_DIST_TER).
      CLEAR: WA_LINHA.
      WA_LINHA-CD_CHAVE_CTE = WA_ZIB_CTE_DIST_TER-CD_CHAVE_CTE.
      WA_LINHA-ZBVTYP       = WA_ZIB_CTE_DIST_TER-ZBVTYP.
      WA_LINHA-ZDT_VENCTO   = WA_ZIB_CTE_DIST_TER-ZDT_VENCTO.

      "NF-e da CT-e
      READ TABLE IT_ZIB_CTE_DIST_N55 WITH KEY CD_CHAVE_CTE = WA_ZIB_CTE_DIST_TER-CD_CHAVE_CTE INTO DATA(WA_ZIB_CTE_DIST_N55).
      IF SY-SUBRC IS INITIAL.
        "Docnum da Chave de NF-e da CT-e
        READ TABLE IT_CD_CHAVE_NFE WITH KEY CHAVE = WA_ZIB_CTE_DIST_N55-N55_CHAVE_ACESSO INTO WA_CD_CHAVE_NFE.
        IF SY-SUBRC IS INITIAL.
          "Pedo de Chegada e Data de Chegada da NF-e da CT-e
          READ TABLE IT_DOCUMENTOS WITH KEY DOCNUM = WA_CD_CHAVE_NFE-DOCNUM INTO DATA(WA_DOCUMENTOS).
          WA_LINHA-PESO_CHEGADA = WA_DOCUMENTOS-PESO_CHEGADA.
          WA_LINHA-DT_CHEGADA   = WA_DOCUMENTOS-DT_CHEGADA.
          APPEND WA_LINHA TO IT_LINHA.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CREATE OBJECT LC_CTE.
    LOOP AT IT_LINHA INTO WA_LINHA.

      "Inicializa Documento
      LC_CTE->LER_DADOS_XI( EXPORTING P_CHAVE_CTE = WA_LINHA-CD_CHAVE_CTE
                            EXCEPTIONS
                              FOREIGN_LOCK = 1
                              OTHERS       = 2 ).
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CALL METHOD ZCL_CTE_PAGAMENTO=>SET_INFO_FATURAR_CTE
        EXPORTING
          I_CD_CHAVE_CTE    = WA_LINHA-CD_CHAVE_CTE
          I_INDEX_ULTIMO    = 0
          I_NAO_CHAMAR_TELA = ABAP_TRUE
        RECEIVING
          R_CTE             = DATA(CTE)
        EXCEPTIONS
          BLOQUEADO_USUARIO = 1
          OTHERS            = 2.

      IF SY-SUBRC IS NOT INITIAL OR CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_TRUE.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      CTE->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = LC_ZIB_CTE_DIST_TER ).
      CTE->SET_ZDT_MOV( EXPORTING I_ZDT_MOV = SY-DATUM ).

      SELECT SINGLE * INTO @DATA(E_LFBK)
        FROM LFBK
       WHERE LIFNR EQ @LC_ZIB_CTE_DIST_TER-P_EMISSOR
         AND BVTYP EQ '0001'.

      IF SY-SUBRC IS NOT INITIAL.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      CTE->SET_ZBVTYP( EXPORTING I_ZBVTYP = E_LFBK-BVTYP ).

      ZCL_MIRO=>GET_PROXIMO_VENC_FATURA(
        IMPORTING
          E_DATA_VENCIMENTO = LC_ZIB_CTE_DIST_TER-ZDT_VENCTO    " Data
        EXCEPTIONS
          ERRO              = 1
          OTHERS            = 2 ).

      IF SY-SUBRC IS NOT INITIAL.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      CTE->SET_ZDT_VENCTO( EXPORTING I_ZDT_VENCTO = LC_ZIB_CTE_DIST_TER-ZDT_VENCTO ).
      CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = WA_LINHA-DT_CHEGADA ).

      DATA(IT_05_VT)  = CTE->GET_IT_VT( ).
      DELETE IT_05_VT WHERE TKNUM EQ SPACE.
      READ TABLE IT_05_VT INDEX 1 INTO DATA(WA_05_VT).

      IF SY-SUBRC IS NOT INITIAL.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      CTE->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = LC_ZIB_CTE_DIST_TER ).

      IF LC_ZIB_CTE_DIST_TER-ZQUEBRA GT 0 AND LC_ZIB_CTE_DIST_TER-ZVLR_QUEBRA EQ 0.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      IF LC_ZIB_CTE_DIST_TER-ZDT_VENCTO LT SY-DATUM.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

*      IF LC_ZIB_CTE_DIST_TER-PESO_CHEGADA IS INITIAL AND CTE->GET_CK_ENTRADA_MANUAL_PESO( ) EQ ABAP_FALSE.
*        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
*          EXPORTING
*            CHAVE = WA_LINHA-CD_CHAVE_CTE.
*        CONTINUE.
*      ENDIF.

      IF LC_ZIB_CTE_DIST_TER-MWSKZ IS INITIAL.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      "Salvar Registro quando Existir Somente uma VT
      IF CTE->ZIF_CADASTRO~GRAVAR_REGISTRO( ) NE ABAP_TRUE.
        CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
          EXPORTING
            CHAVE = WA_LINHA-CD_CHAVE_CTE.
        CONTINUE.
      ENDIF.

      LC_CTE->GERAR_FATURA_FRETE(
        EXPORTING
          P_CHAVE_CTE         = LC_ZIB_CTE_DIST_TER-CD_CHAVE_CTE    " Vorschlagswerte bei Anlage von Debitoren aus Workbench
        CHANGING
          P_CTE               = LC_ZIB_CTE_DIST_TER    " Chave de Documento Fiscal Eletrônico
        EXCEPTIONS
          NAO_ENC_FRETE       = 1
          FATURA              = 2
          PEDIDO              = 3
          COD_IVA             = 4
          BANCO_PARCEIRO      = 5
          PARAM_CTB           = 6
          BANCO_EMPRESA       = 7
          SEM_VT              = 8
          ERRO_ENTRADA_FISCAL = 9
          MIRO_COMPENSADA     = 10
          PESO_CHEGADA        = 11
          NAO_AUTORIZADO      = 12
          OTHERS              = 13 ).

      CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
        EXPORTING
          CHAVE = WA_LINHA-CD_CHAVE_CTE.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_miro_frete.

    DATA(lc_xblnr) = zcl_miro=>get_chave_referencia( i_series = i_cte-numr_serie i_nf_number9 = i_cte-numr_cte ).

    SELECT SINGLE * INTO @r_rbkp
      FROM rbkp
     WHERE xblnr EQ @lc_xblnr
       AND lifnr EQ @i_cte-p_emissor
       AND bldat EQ @i_cte-dt_emissao
       AND blart EQ 'FT'
       AND stblg EQ @space.

*---> IR121605 / CS1049449
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE * INTO @r_rbkp
         FROM rbkp
        WHERE xblnr EQ @lc_xblnr
          AND lifnr EQ @i_cte-p_emissor
          AND bldat EQ @i_cte-dt_emissao
          AND blart EQ 'RE'
          AND stblg EQ @space.
    ENDIF.
*<--- IR121605 / CS1049449

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE zcx_cte_dist_g
      EXPORTING
        textid = VALUE #( msgid = zcx_cte_dist_g=>zcx_miro_nao_encontrada-msgid
                          msgno = zcx_cte_dist_g=>zcx_miro_nao_encontrada-msgno
                          attr1 = CONV #( lc_xblnr )
                          attr2 = CONV #( i_cte-p_emissor ) )
        msgid  = zcx_cte_dist_g=>zcx_miro_nao_encontrada-msgid
        msgno  = zcx_cte_dist_g=>zcx_miro_nao_encontrada-msgno
        msgty  = 'E'
        msgv1  = CONV #( lc_xblnr )
        msgv2  = CONV #( i_cte-p_emissor ).

  ENDMETHOD.


  METHOD get_nftype_entrada.

*---> IR121605 / CS1049449
    IF sy-tcode EQ 'ZMM0079'.

      SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_setleaf_nftype)
       WHERE setname EQ 'ZMM0079'.

       IF sy-subrc NE 0.
         e_nftype = 'C2'.
       ELSE.
         e_nftype = _WL_SETLEAF_NFTYPE-VALFROM.
       endif.
     ELSE.
       e_nftype = 'C2'.
     ENDIF.
*<--- IR121605 / CS1049449

    ENDMETHOD.


  METHOD GET_VOLUME_VALOR_NOTA.

    "Buscar Notas Fiscais """""""""""""""""""""
    IF I_N55T[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_J_1BNFLIN)
        FROM J_1BNFLIN
         FOR ALL ENTRIES IN @I_N55T
       WHERE DOCNUM EQ @I_N55T-DOCNUM_NFE.
    ENDIF.

    IF I_N01T[] IS NOT INITIAL.
      SELECT * APPENDING TABLE @IT_J_1BNFLIN
        FROM J_1BNFLIN
         FOR ALL ENTRIES IN @I_N01T
       WHERE DOCNUM EQ @I_N01T-DOCNUM_NF.
    ENDIF.

    CLEAR: E_J_1BNFLIN_TAB.

    "Somar Peso Bruto Remessa/Aviso Recebimento """"""""""""""""""""""""""""""""
    " Somente KG
    DATA(IT_N55_AUX) = I_N55T[].
    DATA(IT_N01_AUX) = I_N01T[].
    DELETE IT_N55_AUX WHERE VBELN_VL IS INITIAL.
    DELETE IT_N01_AUX WHERE VBELN_VL IS INITIAL.
    IF IT_N55_AUX[] IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(IT_LIPS)
        FROM LIPS
         FOR ALL ENTRIES IN @IT_N55_AUX
       WHERE VBELN EQ @IT_N55_AUX-VBELN_VL
         AND GEWEI EQ 'KG'.
    ENDIF.
    IF IT_N01_AUX[] IS NOT INITIAL.
      SELECT * APPENDING TABLE @IT_LIPS
        FROM LIPS
         FOR ALL ENTRIES IN @IT_N01_AUX
       WHERE VBELN EQ @IT_N01_AUX-VBELN_VL
         AND GEWEI EQ 'KG'.
    ENDIF.

    LOOP AT IT_J_1BNFLIN ASSIGNING FIELD-SYMBOL(<FS_J_1BNFLIN>).

      DATA(IT_LIPS_AUX) = IT_LIPS[].

      READ TABLE I_N55T INTO DATA(WA_N55) WITH KEY DOCNUM_NFE = <FS_J_1BNFLIN>-DOCNUM.
      IF SY-SUBRC IS INITIAL AND WA_N55-VBELN_VL IS NOT INITIAL.
        DELETE IT_LIPS_AUX WHERE VBELN NE WA_N55-VBELN_VL.
        DESCRIBE TABLE IT_LIPS_AUX LINES DATA(QTD_ITENS_REMESSAS).

        IF QTD_ITENS_REMESSAS EQ 1.
          READ TABLE IT_LIPS WITH KEY VBELN = WA_N55-VBELN_VL INTO DATA(WA_LIPS).
          IF SY-SUBRC IS INITIAL.
            "Ajusta Peso para Peso Bruto da Remessa/Aviso de Recebimento
            <FS_J_1BNFLIN>-MENGE = WA_LIPS-BRGEW.

            IF ( WA_N55-chave_rom_saida IS NOT INITIAL ). "Nota Fiscal com mais de uma Remessa (Faturamento Agrupado Remessa - Filiais Sul)
              "Ajustar valor proporcional NF
              <FS_J_1BNFLIN>-NETWR = WA_LIPS-NTGEW *  <FS_J_1BNFLIN>-NETPR.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE I_N01T INTO DATA(WA_N01) WITH KEY DOCNUM_NF = <FS_J_1BNFLIN>-DOCNUM.
        IF SY-SUBRC IS INITIAL AND WA_N01-VBELN_VL IS NOT INITIAL.
          DELETE IT_LIPS_AUX WHERE VBELN NE WA_N01-VBELN_VL.
          DESCRIBE TABLE IT_LIPS_AUX LINES QTD_ITENS_REMESSAS.

          IF QTD_ITENS_REMESSAS EQ 1.
            READ TABLE IT_LIPS WITH KEY VBELN = WA_N01-VBELN_VL INTO WA_LIPS.
            IF SY-SUBRC IS INITIAL.
              "Ajusta Peso para Peso Bruto da Remessa/Aviso de Recebimento
              <FS_J_1BNFLIN>-MENGE = WA_LIPS-BRGEW.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    E_J_1BNFLIN_TAB = IT_J_1BNFLIN[].

  ENDMETHOD.


  METHOD INFORMAR_DADOS_FATURAMENTO.


    DATA: CK_SAIR          TYPE C LENGTH 1,
          LC_INDEX         TYPE I,
          LC_QTD_REG       TYPE I,
          LC_CTE           TYPE ZIB_CTE_DIST_TER,
          LC_DT_VENCIMENTO TYPE	ZDT_VENCTO,
          LC_TOTAL         TYPE I.

    LC_INDEX = 1.

    DESCRIBE TABLE I_CTE LINES LC_QTD_REG.

    WHILE CK_SAIR EQ ABAP_FALSE.

      READ TABLE I_CTE INDEX LC_INDEX INTO LC_CTE.

      IF SY-SUBRC IS INITIAL.

        LC_TOTAL = 0.

        SELECT COUNT(*) INTO LC_TOTAL
          FROM ZIB_CTE_DIST_GMI
         WHERE CD_CHAVE_CTE EQ LC_CTE-CD_CHAVE_CTE.

        IF SY-SUBRC IS INITIAL.
          LC_TOTAL = 0.
        ENDIF.

        IF LC_CTE-CD_MODAL EQ '01' AND LC_TOTAL EQ 0. "Rodoviário
          CALL METHOD ZCL_CTE_PAGAMENTO=>SET_INFO_FATURAR_CTE
            EXPORTING
              I_CD_CHAVE_CTE    = LC_CTE-CD_CHAVE_CTE
              I_INDEX_ULTIMO    = LC_QTD_REG
            IMPORTING
              E_COMANDO         = CK_SAIR
            CHANGING
              I_INDEX           = LC_INDEX
              I_DT_VENCIMENTO   = LC_DT_VENCIMENTO
            EXCEPTIONS
              BLOQUEADO_USUARIO = 1
              OTHERS            = 2.
        ELSEIF LC_CTE-CD_MODAL EQ '03'. "Aquaviário
          CALL METHOD ZCL_CTE_PAGAMENTO=>SET_INFO_FATURAR_CTE
            EXPORTING
              I_CD_CHAVE_CTE    = LC_CTE-CD_CHAVE_CTE
              I_INDEX_ULTIMO    = LC_QTD_REG
            IMPORTING
              E_COMANDO         = CK_SAIR
            CHANGING
              I_INDEX           = LC_INDEX
              I_DT_VENCIMENTO   = LC_DT_VENCIMENTO
            EXCEPTIONS
              BLOQUEADO_USUARIO = 1
              OTHERS            = 2.
        ELSE.
          CALL FUNCTION 'ZCTE_DIST_FATURAMENTO'
            EXPORTING
              I_CTE             = LC_CTE
              I_INDEX_ULTIMO    = LC_QTD_REG
            IMPORTING
              E_ORDEM           = CK_SAIR
            CHANGING
              I_INDEX           = LC_INDEX
              I_DT_VENCIMENTO   = LC_DT_VENCIMENTO
            EXCEPTIONS
              BLOQUEADO_USUARIO = 1
              OTHERS            = 2.
        ENDIF.
      ELSE.
        CK_SAIR = ABAP_TRUE.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD INICIA_SEQUENCIA.


    SELECT MAX( NR_SEQUENCIA ) INTO P_LC_SEQUENCIA
    FROM ZIB_CTE_DIST_LOG
    WHERE CD_CHAVE_CTE EQ P_CD_CHAVE_CTE.

    IF P_LC_SEQUENCIA IS INITIAL.
      P_LC_SEQUENCIA = 1.
    ELSE.
      ADD 1 TO P_LC_SEQUENCIA.
    ENDIF.

  ENDMETHOD.


  METHOD LER_DADOS_FERROVIARIO.


    DATA: WA_ZLEST0044     TYPE ZLEST0044,
          IT_ZLEST0045     TYPE TABLE OF ZLEST0045,
          WA_ZLEST0045     TYPE ZLEST0045,
          IT_ZLEST0041     TYPE TABLE OF ZLEST0041,
          WA_ZLEST0041     TYPE ZLEST0041,
          "IT_VGA           TYPE TABLE OF ZIB_CTE_DIST_VGA,
          "WA_VGA           TYPE ZIB_CTE_DIST_VGA,
          IT_D55           TYPE TABLE OF ZIB_CTE_DIST_D55,
          WA_D55           TYPE ZIB_CTE_DIST_D55,
          IT_CVL           TYPE TABLE OF ZIB_CTE_DIST_CVL,
          WA_CVL           TYPE ZIB_CTE_DIST_CVL,
          WA_N55           TYPE ZIB_CTE_DIST_N55,
          IT_CPL           TYPE TABLE OF ZIB_CTE_DIST_CPL,
          WA_CPL           TYPE ZIB_CTE_DIST_CPL,
          WA_PESO_LIBERADO TYPE ZIB_CTE_DIST_LBP.

    FIELD-SYMBOLS: <FS_VGA> TYPE ZIB_CTE_DIST_VGA,
                   <FS_D55> TYPE ZIB_CTE_DIST_D55,
                   <FS_D01> TYPE ZIB_CTE_DIST_D01.

    SELECT SINGLE * INTO WA_ZLEST0044
      FROM ZLEST0044
     WHERE CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

    IF SY-SUBRC IS NOT INITIAL.

      WA_ZLEST0044-CHAVE_CTE        = P_CTE-CD_CHAVE_CTE.
      WA_ZLEST0044-NR_CTE           = P_CTE-NUMR_CTE.
      WA_ZLEST0044-SERIE            = P_CTE-NUMR_SERIE.
      WA_ZLEST0044-DATA_EMISAO      = P_CTE-DT_EMISSAO.
      WA_ZLEST0044-NR_PROTOCOLO     = P_CTE-NR_PROTOCOLO.
      WA_ZLEST0044-CFOP             = P_CTE-CODG_CFOP.
      WA_ZLEST0044-MODELO           = P_CTE-MODELO.
      WA_ZLEST0044-FORMA_PGTO       = P_CTE-CD_FPAGAMENTO.
      WA_ZLEST0044-TP_CTE           = P_CTE-CD_TIPO_CTE.
      WA_ZLEST0044-TP_SERVICO       = P_CTE-CD_TIPO_SERVICO.
      WA_ZLEST0044-CIDADE_ORIGEM    = P_CTE-INICIO_IBGE.
      WA_ZLEST0044-UF_ORIGEM        = P_CTE-INICIO_UF.
      WA_ZLEST0044-CIDADE_DESTINO   = P_CTE-TERMINO_IBGE.
      WA_ZLEST0044-UF_DESTINO       = P_CTE-TERMINO_UF.

      "1  CNPJ
      "2  CPF
      IF P_CTE-TOMA4_TP_DOC IS NOT INITIAL.
        CASE P_CTE-TOMA4_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_TOMADOR = P_CTE-TOMA4_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_TOMADOR = P_CTE-TOMA4_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_TOMADOR = P_CTE-TOMA4_IE.
        WA_ZLEST0044-TOMADOR    = P_CTE-TOMA4_RSOCIAL.
      ENDIF.

      IF P_CTE-EMIT_TP_DOC IS NOT INITIAL.
        CASE P_CTE-EMIT_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_EMITENTE = P_CTE-EMIT_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_EMITENTE = P_CTE-EMIT_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_EMITENTE = P_CTE-EMIT_IE.
        WA_ZLEST0044-EMITENTE    = P_CTE-EMIT_RSOCIAL.
      ENDIF.

      IF P_CTE-REME_TP_DOC IS NOT INITIAL.
        CASE P_CTE-REME_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_REMETENTE = P_CTE-REME_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_REMETENTE = P_CTE-REME_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_REMETENTE = P_CTE-REME_IE.
        WA_ZLEST0044-REMETENTE    = P_CTE-REME_RSOCIAL.
      ENDIF.

      IF P_CTE-DEST_TP_DOC IS NOT INITIAL.
        CASE P_CTE-DEST_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_DESTINATARI = P_CTE-DEST_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_DESTINATARI = P_CTE-DEST_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_DESTINATARIO = P_CTE-DEST_IE.
        WA_ZLEST0044-DESTINATARIO    = P_CTE-DEST_RSOCIAL.
      ENDIF.

      IF P_CTE-EXPED_TP_DOC IS NOT INITIAL.
        CASE P_CTE-EXPED_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_EXPEDIDOR = P_CTE-EXPED_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_EXPEDIDOR = P_CTE-EXPED_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_EXPEDIDOR = P_CTE-EXPED_IE.
        WA_ZLEST0044-EXPEDIDOR    = P_CTE-EXPED_RSOCIAL.
      ENDIF.


      IF P_CTE-RECEB_TP_DOC IS NOT INITIAL.
        CASE P_CTE-RECEB_TP_DOC.
          WHEN 1.
            WA_ZLEST0044-CNPJ_RECEBEDOR = P_CTE-RECEB_CNPJ.
          WHEN 2.
            WA_ZLEST0044-CNPJ_RECEBEDOR = P_CTE-RECEB_CPF.
        ENDCASE.
        WA_ZLEST0044-IE_RECEBEDOR = P_CTE-RECEB_IE.
        WA_ZLEST0044-RECEBEDOR    = P_CTE-RECEB_RSOCIAL.
      ENDIF.

      WA_ZLEST0044-PESO_BRUTO    = P_CTE-QT_CARGA_CTE.
      WA_ZLEST0044-PRODUTO       = P_CTE-DS_PROD_PRED.
      WA_ZLEST0044-VLR_MERC      = P_CTE-VL_TOTAL_MERC.
      WA_ZLEST0044-VLR_SERV      = P_CTE-VALOR_PRESTACAO.
      WA_ZLEST0044-VLR_REC       = P_CTE-VALOR_RECEBER.
      WA_ZLEST0044-CANCELADO     = P_CTE-CANCEL.
      WA_ZLEST0044-SIT_TRIB      = P_CTE-CST_ICMS.
      WA_ZLEST0044-DATA          = SY-DATUM.
      WA_ZLEST0044-HORA          = SY-UZEIT.
      WA_ZLEST0044-USUARIO       = SY-UNAME.
      WA_ZLEST0044-BUKRS         = P_CTE-E_TOMADORA.
      WA_ZLEST0044-BRANCH        = P_CTE-F_TOMADORA.

      SELECT * INTO TABLE IT_CPL
        FROM ZIB_CTE_DIST_CPL
       WHERE CD_CHAVE_CTE  EQ P_CTE-CD_CHAVE_CTE
         AND TP_IDENTIFICA EQ '1'.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_CPL INTO WA_CPL WITH KEY DS_CAMPO = 'DATA:'.
        IF SY-SUBRC IS INITIAL.
          CONCATENATE WA_CPL-DS_TEXTO+6(4) WA_CPL-DS_TEXTO+3(2) WA_CPL-DS_TEXTO(2) INTO WA_ZLEST0044-DT_REFERENCIA.
        ELSE.
          WA_ZLEST0044-DT_REFERENCIA = P_CTE-DT_EMISSAO.
        ENDIF.

        READ TABLE IT_CPL INTO WA_CPL WITH KEY DS_CAMPO = 'VENCIMENTO:'.
        IF SY-SUBRC IS INITIAL.
          CONCATENATE WA_CPL-DS_TEXTO+6(4) WA_CPL-DS_TEXTO+3(2) WA_CPL-DS_TEXTO(2) INTO WA_ZLEST0044-DT_VENC.
          P_CTE-ZDT_VENCTO = WA_ZLEST0044-DT_VENC.
        ENDIF.
      ELSE.
        WA_ZLEST0044-DT_REFERENCIA  = P_CTE-DT_EMISSAO.
      ENDIF.
    ENDIF.

    IF WA_ZLEST0044-NR_TRANS IS INITIAL.

      "Busca Tarifa do Frete """"""""""""""""""""""""""""""""""""""""""""""""""""""" >>>>
      CLEAR: IT_CVL.
      SELECT * INTO TABLE IT_CVL
        FROM ZIB_CTE_DIST_CVL
       WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      WA_ZLEST0044-TARIFA = 0.

      LOOP AT IT_CVL INTO WA_CVL.
        TRANSLATE WA_CVL-NOME_COMPONENTE TO UPPER CASE.
        IF WA_CVL-NOME_COMPONENTE = 'TARIFA'.
          WA_ZLEST0044-TARIFA = WA_CVL-VALR_COMPONENTE.
        ENDIF.
      ENDLOOP.

      IF ( WA_ZLEST0044-TARIFA IS INITIAL ) AND ( P_CTE-VALOR_RECEBER IS NOT INITIAL ) AND ( P_CTE-QT_CARGA_CTE IS NOT INITIAL ).
        WA_ZLEST0044-TARIFA = ( P_CTE-VALOR_RECEBER / P_CTE-QT_CARGA_CTE ) * 1000.
      ENDIF.
      "Busca Tarifa do Frete """"""""""""""""""""""""""""""""""""""""""""""""""""""" <<<<

      MODIFY ZLEST0044 FROM WA_ZLEST0044.
      COMMIT WORK.
    ENDIF.

    CALL METHOD ME->LER_DADOS_RODOVIARIO
      EXPORTING
        P_TIPO_CONTRATO = P_TIPO_CONTRATO
      CHANGING
        P_CTE           = P_CTE.

    "Liberação de Diferênça de Peso """""""""""""""""""""""""""""""""""""""""""""" >>>>>>
    SELECT SINGLE * INTO WA_PESO_LIBERADO
      FROM ZIB_CTE_DIST_LBP
     WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

    IF SY-SUBRC IS INITIAL.
      MOVE: P_CTE-QT_CARGA_CTE TO SY-MSGV1,
            WA_PESO_LIBERADO-QT_DIFERENCA TO SY-MSGV2,
            WA_PESO_LIBERADO-DS_NAME_USUARIO TO SY-MSGV3.

      CONDENSE: SY-MSGV1, SY-MSGV2.

      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
          P_TYPE         = 'W'
          P_NUM          = 150
          P_MESSAGE_V1   = SY-MSGV1
          P_MESSAGE_V2   = SY-MSGV2
          P_MESSAGE_V3   = SY-MSGV3
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.
    ENDIF.
    "Liberação de Diferênça de Peso """""""""""""""""""""""""""""""""""""""""""""" <<<<<<

    IF P_CTE-CD_TIPO_CTE NE '1'.
      LOOP AT IT_N55 INTO WA_N55.
        IF WA_N55-EBELN IS NOT INITIAL AND WA_N55-EBELP IS NOT INITIAL AND P_CTE-EBELN IS INITIAL.
          P_CTE-EBELN = WA_N55-EBELN.
          P_CTE-EBELP = WA_N55-EBELP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF WA_ZLEST0044-NR_TRANS IS INITIAL.

      "Distribui quantidades do vagão """""""""""""""""""""""""""""""""""""""""""""" >>>>
      CLEAR:
      "IT_VGA,
      IT_ZLEST0045,
      IT_D55.

      "Vagões
      "SELECT * INTO TABLE IT_VGA
      "  FROM ZIB_CTE_DIST_VGA
      " WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      "LOOP AT IT_VGA ASSIGNING <FS_VGA>.
      "  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      "    EXPORTING
      "      INPUT  = <FS_VGA>-NUMR_IDENT_VAGAO
      "    IMPORTING
      "      OUTPUT = <FS_VGA>-NUMR_IDENT_VAGAO.
      "ENDLOOP.

      "Rateio
      SELECT * INTO TABLE IT_D55
        FROM ZIB_CTE_DIST_D55
       WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      LOOP AT IT_D55 ASSIGNING <FS_D55>.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = <FS_D55>-ID_UNID_TRANSP
          IMPORTING
            OUTPUT = <FS_D55>-ID_UNID_TRANSP.
      ENDLOOP.

      IF P_CTE-CD_TIPO_CTE NE '1'.
        CLEAR: P_CTE-EBELN, P_CTE-EBELP.
      ENDIF.

*  "Rateio
*  SELECT * INTO TABLE IT_D01
*    FROM ZIB_CTE_DIST_D01
*   WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.
*
*  LOOP AT IT_D01 ASSIGNING <FS_D01>.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = <FS_D01>-ID_UNID_TRANSP
*      IMPORTING
*        OUTPUT = <FS_D01>-ID_UNID_TRANSP.
*  ENDLOOP.

      DELETE FROM ZLEST0045 WHERE CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      SELECT *
        INTO TABLE IT_ZLEST0041
        FROM ZLEST0041
         FOR ALL ENTRIES IN IT_N55
       WHERE DOCNUM EQ IT_N55-DOCNUM_NFE.

      "Multimodal - Frete Lotação
      IF P_CTE-CD_MODAL EQ '06' AND P_TIPO_CONTRATO EQ '0002'.

        SELECT * INTO TABLE @DATA(IT_001)
          FROM ZIB_CTE_DIST_001
         WHERE CD_CHAVE_CTE EQ @P_CTE-CD_CHAVE_CTE.

        LOOP AT IT_N55 INTO WA_N55 WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.
          CLEAR: WA_ZLEST0045.
          WA_ZLEST0045-CHAVE         = WA_N55-N55_CHAVE_ACESSO.
          WA_ZLEST0045-CHAVE_CTE     = WA_N55-CD_CHAVE_CTE.
          WA_ZLEST0045-CNPJ_EMITENTE = WA_N55-N55_CHAVE_ACESSO+6(14).

          "Some quantidade de nota fiscal
          WA_ZLEST0045-PESO_REAL     = 0.
          WA_ZLEST0045-PESO_RATEADO  = 0.
          LOOP AT IT_001 INTO DATA(WA_001) WHERE N55_CHAVE_ACESSO EQ WA_N55-N55_CHAVE_ACESSO.
            ADD WA_001-NM_QUANTIDADE TO WA_ZLEST0045-PESO_REAL.
            ADD WA_001-NM_QUANTIDADE TO WA_ZLEST0045-PESO_RATEADO.
          ENDLOOP.

          WA_ZLEST0045-BUKRS         = WA_N55-BUKRS.
          WA_ZLEST0045-BRANCH        = WA_N55-BRANCH.
          LOOP AT IT_ZLEST0041 INTO WA_ZLEST0041.
            IF WA_N55-N55_CHAVE_ACESSO+25(9) = WA_ZLEST0041-NR_NF AND
               WA_N55-N55_CHAVE_ACESSO+22(3) = WA_ZLEST0041-SERIE.
              WA_ZLEST0045-DOCNUM = WA_ZLEST0041-DOCNUM.
            ENDIF.
          ENDLOOP.

          IF WA_ZLEST0045-DOCNUM IS INITIAL.
            WA_ZLEST0045-DOCNUM  = WA_N55-DOCNUM_NFE.
          ENDIF.

          IF WA_ZLEST0045-DOCNUM IS NOT INITIAL.
            SELECT SINGLE MATNR INTO WA_ZLEST0045-MATNR_FATURADO
              FROM J_1BNFLIN
             WHERE DOCNUM EQ WA_ZLEST0045-DOCNUM.
          ENDIF.

          APPEND WA_ZLEST0045 TO IT_ZLEST0045.

        ENDLOOP.
      ELSE.
        "LOOP AT IT_VGA INTO WA_VGA.
        LOOP AT IT_D55 INTO WA_D55. "WHERE ID_UNID_TRANSP EQ WA_VGA-NUMR_IDENT_VAGAO.
          LOOP AT IT_N55 INTO WA_N55 WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE
                                       AND N55_CHAVE_ACESSO EQ WA_D55-N55_CHAVE_ACESSO.
            CLEAR: WA_ZLEST0045.
            WA_ZLEST0045-CHAVE         = WA_D55-N55_CHAVE_ACESSO.
            WA_ZLEST0045-CHAVE_CTE     = WA_D55-CD_CHAVE_CTE.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_D55-ID_UNID_TRANSP
              IMPORTING
                OUTPUT = WA_D55-ID_UNID_TRANSP.

            MOVE WA_D55-ID_UNID_TRANSP TO WA_ZLEST0045-NR_VAGAO.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_ZLEST0045-NR_VAGAO
              IMPORTING
                OUTPUT = WA_ZLEST0045-NR_VAGAO.

            "WA_ZLEST0045-TP_VAGAO      = WA_VGA-TIPO_VAGAO.
            WA_ZLEST0045-CNPJ_EMITENTE = WA_D55-N55_CHAVE_ACESSO+6(14).
            WA_ZLEST0045-PESO_REAL     = WA_D55-VALR_PESO_RATE * 1000.
            WA_ZLEST0045-PESO_RATEADO  = WA_D55-VALR_PESO_RATE * 1000.
            WA_ZLEST0045-BUKRS         = WA_N55-BUKRS.
            WA_ZLEST0045-BRANCH        = WA_N55-BRANCH.

            LOOP AT IT_ZLEST0041 INTO WA_ZLEST0041.
              IF WA_D55-N55_CHAVE_ACESSO+25(9) = WA_ZLEST0041-NR_NF AND
                 WA_D55-N55_CHAVE_ACESSO+22(3) = WA_ZLEST0041-SERIE.
                WA_ZLEST0045-DOCNUM = WA_ZLEST0041-DOCNUM.
              ENDIF.
            ENDLOOP.
            IF WA_ZLEST0045-DOCNUM IS INITIAL.
              WA_ZLEST0045-DOCNUM  = WA_N55-DOCNUM_NFE.
            ENDIF.

            IF WA_ZLEST0045-DOCNUM IS NOT INITIAL.
              SELECT SINGLE MATNR INTO WA_ZLEST0045-MATNR_FATURADO
                FROM J_1BNFLIN
               WHERE DOCNUM EQ WA_ZLEST0045-DOCNUM.
            ENDIF.

            APPEND WA_ZLEST0045 TO IT_ZLEST0045.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
*    LOOP AT IT_D01 INTO WA_D01 WHERE ID_UNID_TRANSP EQ WA_VGA-NUMR_IDENT_VAGAO.
*      LOOP AT IT_N01 INTO WA_N01 WHERE CD_CHAVE_CTE    EQ P_CTE-CD_CHAVE_CTE
*                                  AND N55_CHAVE_ACESSO EQ WA_D55-N55_CHAVE_ACESSO.
*        WA_ZLEST0045-CHAVE         = WA_D55-N55_CHAVE_ACESSO.
*        WA_ZLEST0045-CHAVE_CTE     = WA_D55-CD_CHAVE_CTE.
*        WA_ZLEST0045-NR_VAGAO      = WA_VGA-NUMR_IDENT_VAGAO.
*        WA_ZLEST0045-TP_VAGAO      = WA_VGA-TIPO_VAGAO.
*        WA_ZLEST0045-CNPJ_EMITENTE = WA_D55-N55_CHAVE_ACESSO+6(14).
*        WA_ZLEST0045-PESO_REAL     = WA_VGA-VALR_PESO_REAL.
*        WA_ZLEST0045-PESO_RATEADO  = WA_D55-VALR_PESO_RATE.
*        WA_ZLEST0045-DOCNUM        = WA_N55-DOCNUM_NFE.
*        WA_ZLEST0045-BUKRS         = WA_N55-BUKRS.
*        WA_ZLEST0045-BRANCH        = WA_N55-BRANCH.
*        APPEND WA_ZLEST0045 TO IT_ZLEST0045.
*      ENDLOOP.
*    ENDLOOP.

      "ENDLOOP.
      "Distribui quantidades do vagão """""""""""""""""""""""""""""""""""""""""""""" <<<<

      IF IT_ZLEST0045 IS NOT INITIAL.
        MODIFY ZLEST0045 FROM TABLE IT_ZLEST0045.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD LER_DADOS_RODOVIARIO.


    DATA: IT_N55_AUX    TYPE TABLE OF ZIB_CTE_DIST_N55,
          WA_NIT        TYPE ZIB_CTE_DIST_NIT,
          WA_TIPOS      TYPE ZIB_CTE_DIST_FLG,
          E_DOCNUM      TYPE J_1BDOCNUM,
          E_CHAVE_ROM   TYPE ZSDT0001-CH_REFERENCIA,
          WA_ACTIVE     TYPE J_1BNFE_ACTIVE,
          WA_J_1BBRANCH TYPE J_1BBRANCH.

    DATA: IT_N01_T TYPE ZIB_CTE_DIST_N01_T,
          IT_N55_T TYPE ZIB_CTE_DIST_N55_T,
          IT_C57_T TYPE ZIB_CTE_DIST_C57_T,
          IT_NIT_T TYPE ZIB_CTE_DIST_NIT_T,
          WA_NIT_T TYPE ZIB_CTE_DIST_NIT,
          WA_N55   TYPE ZIB_CTE_DIST_N55.

    FIELD-SYMBOLS: <FS_N55> TYPE ZIB_CTE_DIST_N55,
                   <FS_N01> TYPE ZIB_CTE_DIST_N01,
                   <FS_NIT> TYPE ZIB_CTE_DIST_NIT,
                   <FS_C57> TYPE ZIB_CTE_DIST_C57,
                   <FS_ANT> TYPE ZIB_CTE_DIST_ANT.

    SORT IT_TIPOS BY TP_PROCESSO_CTE.

    "Busca Nota Fiscal própria que acobeta o pagamento do frete
    CLEAR: IT_N01_T, IT_N55_T.

    CALL METHOD ME->BUSCA_TROCA_NF_43
      EXPORTING
        CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
      CHANGING
        E_N55        = IT_N55_T
        E_N01        = IT_N01_T.

    LOOP AT IT_N55_T ASSIGNING <FS_N55>.
      APPEND <FS_N55> TO IT_N55.
    ENDLOOP.

    LOOP AT IT_N01_T ASSIGNING <FS_N01>.
      APPEND <FS_N01> TO IT_N01.
    ENDLOOP.

    CASE P_CTE-CD_TIPO_CTE.
      WHEN '0' OR '3'. "CT-e Normal ou CT-e Substituto

        LOOP AT IT_N01 ASSIGNING <FS_N01> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE AND CK_NFE_CTA_ORDEM EQ FALSE.

          CLEAR: IT_NIT_T.

          IF <FS_N01>-DOCNUM_NF IS NOT INITIAL.
            LOOP AT IT_NIT INTO WA_NIT WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF.
              APPEND WA_NIT TO IT_NIT_T.
            ENDLOOP.
          ENDIF.

          CALL METHOD ME->ATRIBUI_DADOS_NOTA
            IMPORTING
              E_DOCNUM    = E_DOCNUM
              E_CHAVE_ROM = E_CHAVE_ROM
            CHANGING
              P_CTE_DIST = P_CTE
              P_NF01     = <FS_N01>
              P_NIT_T    = IT_NIT_T
            EXCEPTIONS
              ERRO       = 1
              OTHERS     = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = SY-MSGTY
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            LOOP AT IT_NIT_T INTO WA_NIT_T.
              READ TABLE IT_NIT ASSIGNING <FS_NIT> WITH KEY CD_CHAVE_CTE = WA_NIT_T-CD_CHAVE_CTE
                                                            DOCNUM       = WA_NIT_T-DOCNUM
                                                            ITMNUM       = WA_NIT_T-ITMNUM.
              IF SY-SUBRC IS INITIAL.
                MOVE-CORRESPONDING WA_NIT_T TO <FS_NIT>.
              ELSE.
                APPEND WA_NIT_T TO IT_NIT.
              ENDIF.
            ENDLOOP.

            IF E_DOCNUM IS NOT INITIAL.
              SELECT SINGLE * INTO WA_ACTIVE
                FROM J_1BNFE_ACTIVE
               WHERE DOCNUM EQ E_DOCNUM.
              IF SY-SUBRC IS INITIAL.
                WA_N55-CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE.
                WA_N55-CHAVE_ROM_SAIDA = E_CHAVE_ROM.

                CONCATENATE WA_ACTIVE-REGIO WA_ACTIVE-NFYEAR WA_ACTIVE-NFMONTH WA_ACTIVE-STCD1 WA_ACTIVE-MODEL WA_ACTIVE-SERIE
                            WA_ACTIVE-NFNUM9 WA_ACTIVE-DOCNUM9 WA_ACTIVE-CDV INTO WA_N55-N55_CHAVE_ACESSO.
                APPEND WA_N55 TO IT_N55_AUX.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_N55 ASSIGNING <FS_N55> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE AND CK_NFE_CTA_ORDEM EQ FALSE.

          IF <FS_N55>-DOCNUM_NFE IS NOT INITIAL.
            LOOP AT IT_NIT INTO WA_NIT WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
              APPEND WA_NIT TO IT_NIT_T.
            ENDLOOP.
          ENDIF.

          CALL METHOD ME->ATRIBUI_DADOS_NOTA
            EXPORTING
              P_TIPO_CONTRATO = P_TIPO_CONTRATO
            IMPORTING
              E_DOCNUM        = E_DOCNUM
              E_CHAVE_ROM     = E_CHAVE_ROM
            CHANGING
              P_CTE_DIST      = P_CTE
              P_NF55          = <FS_N55>
              P_NIT_T         = IT_NIT_T
            EXCEPTIONS
              ERRO            = 1
              OTHERS          = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = SY-MSGTY
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            LOOP AT IT_NIT_T INTO WA_NIT_T.
              READ TABLE IT_NIT ASSIGNING <FS_NIT> WITH KEY CD_CHAVE_CTE = WA_NIT_T-CD_CHAVE_CTE
                                                            DOCNUM       = WA_NIT_T-DOCNUM
                                                            ITMNUM       = WA_NIT_T-ITMNUM.
              IF SY-SUBRC IS INITIAL.
                MOVE-CORRESPONDING WA_NIT_T TO <FS_NIT>.
              ELSE.
                APPEND WA_NIT_T TO IT_NIT.
              ENDIF.
            ENDLOOP.

            IF E_DOCNUM IS NOT INITIAL.
              SELECT SINGLE * INTO WA_ACTIVE
                FROM J_1BNFE_ACTIVE
               WHERE DOCNUM EQ E_DOCNUM.
              IF SY-SUBRC IS INITIAL.
                WA_N55-CD_CHAVE_CTE    = P_CTE-CD_CHAVE_CTE.
                WA_N55-DOCNUM_NFE      = WA_ACTIVE-DOCNUM.
                WA_N55-CHAVE_ROM_SAIDA = E_CHAVE_ROM.
                CONCATENATE WA_ACTIVE-REGIO WA_ACTIVE-NFYEAR WA_ACTIVE-NFMONTH WA_ACTIVE-STCD1 WA_ACTIVE-MODEL WA_ACTIVE-SERIE
                            WA_ACTIVE-NFNUM9 WA_ACTIVE-DOCNUM9 WA_ACTIVE-CDV INTO WA_N55-N55_CHAVE_ACESSO.
                APPEND WA_N55 TO IT_N55_AUX.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        "Incluir notas de Remessa por conta e ordem de terceiro
        LOOP AT IT_N55_AUX INTO WA_N55.
          READ TABLE IT_N55 ASSIGNING <FS_N55> WITH KEY CD_CHAVE_CTE     = WA_N55-CD_CHAVE_CTE
                                                        N55_CHAVE_ACESSO = WA_N55-N55_CHAVE_ACESSO.
          IF SY-SUBRC IS NOT INITIAL.
            CLEAR: IT_NIT_T.
            CALL METHOD ME->ATRIBUI_DADOS_NOTA
              CHANGING
                P_CTE_DIST = P_CTE
                P_NF55     = WA_N55
                P_NIT_T    = IT_NIT_T
              EXCEPTIONS
                ERRO       = 1
                OTHERS     = 2.

            IF SY-SUBRC IS NOT INITIAL.
              CALL METHOD ME->ADD_LOG_CTE_DIST
                EXPORTING
                  P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                  P_TYPE         = SY-MSGTY
                  P_ID           = SY-MSGID
                  P_NUM          = SY-MSGNO
                  P_MESSAGE_V1   = SY-MSGV1
                  P_MESSAGE_V2   = SY-MSGV2
                  P_MESSAGE_V3   = SY-MSGV3
                  P_MESSAGE_V4   = SY-MSGV4
                CHANGING
                  P_LC_SEQUENCIA = LC_SEQUENCIA.
            ELSE.
              WA_N55-CK_NFE_CTA_ORDEM = TRUE.
              APPEND WA_N55 TO IT_N55.
              LOOP AT IT_NIT_T INTO WA_NIT_T.
                READ TABLE IT_NIT ASSIGNING <FS_NIT> WITH KEY CD_CHAVE_CTE = WA_NIT_T-CD_CHAVE_CTE
                                                              DOCNUM       = WA_NIT_T-DOCNUM
                                                              ITMNUM       = WA_NIT_T-ITMNUM.
                IF SY-SUBRC IS INITIAL.
                  MOVE-CORRESPONDING WA_NIT_T TO <FS_NIT>.
                ELSE.
                  APPEND WA_NIT_T TO IT_NIT.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ELSE.
            IF <FS_N55>-DOCNUM_NFE IS NOT INITIAL.
              LOOP AT IT_NIT INTO WA_NIT WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
                APPEND WA_NIT TO IT_NIT_T.
              ENDLOOP.
            ENDIF.

            CALL METHOD ME->ATRIBUI_DADOS_NOTA
              IMPORTING
                E_DOCNUM   = E_DOCNUM
              CHANGING
                P_CTE_DIST = P_CTE
                P_NF55     = <FS_N55>
                P_NIT_T    = IT_NIT_T
              EXCEPTIONS
                ERRO       = 1
                OTHERS     = 2.

            IF SY-SUBRC IS NOT INITIAL.
              CALL METHOD ME->ADD_LOG_CTE_DIST
                EXPORTING
                  P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                  P_TYPE         = SY-MSGTY
                  P_ID           = SY-MSGID
                  P_NUM          = SY-MSGNO
                  P_MESSAGE_V1   = SY-MSGV1
                  P_MESSAGE_V2   = SY-MSGV2
                  P_MESSAGE_V3   = SY-MSGV3
                  P_MESSAGE_V4   = SY-MSGV4
                CHANGING
                  P_LC_SEQUENCIA = LC_SEQUENCIA.
            ELSE.
              LOOP AT IT_NIT_T INTO WA_NIT_T.
                READ TABLE IT_NIT ASSIGNING <FS_NIT> WITH KEY CD_CHAVE_CTE = WA_NIT_T-CD_CHAVE_CTE
                                                              DOCNUM       = WA_NIT_T-DOCNUM
                                                              ITMNUM       = WA_NIT_T-ITMNUM.
                IF SY-SUBRC IS INITIAL.
                  MOVE-CORRESPONDING WA_NIT_T TO <FS_NIT>.
                ELSE.
                  APPEND WA_NIT_T TO IT_NIT.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_C57 ASSIGNING <FS_C57> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

          CALL METHOD ME->BUSCA_CTE_DADOS_ANTERIOR
            CHANGING
              E_CTE_DISTR     = P_CTE
              E_CTE_C57       = <FS_C57>
            EXCEPTIONS
              NAO_ACHOU_CTE   = 1
              NAO_ESCRITURADO = 2
              OTHERS          = 3.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = SY-MSGTY
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            SY-MSGV1 = <FS_C57>-C57_CHAVE_ACESSO+25(9).
            SY-MSGV2 = <FS_C57>-DOCNUM_CTE.

            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 098
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_ANT ASSIGNING <FS_ANT> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

        ENDLOOP.

      WHEN '1'. "CT-e de Complemento de Valores

        LOOP AT IT_C57 ASSIGNING <FS_C57> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

          CALL METHOD ME->BUSCA_CTE_DADOS_ANTERIOR
            CHANGING
              E_CTE_DISTR     = P_CTE
              E_CTE_C57       = <FS_C57>
            EXCEPTIONS
              NAO_ACHOU_CTE   = 1
              NAO_ESCRITURADO = 2
              OTHERS          = 3.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = SY-MSGTY
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            SY-MSGV1 = <FS_C57>-C57_CHAVE_ACESSO+25(9).
            SY-MSGV2 = <FS_C57>-DOCNUM_CTE.

            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 098
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.

        ENDLOOP.

      WHEN '2'. "CT-e de Anulação de Valores

        LOOP AT IT_C57 ASSIGNING <FS_C57> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

          CALL METHOD ME->BUSCA_CTE_DADOS_ANTERIOR
            CHANGING
              E_CTE_DISTR     = P_CTE
              E_CTE_C57       = <FS_C57>
            EXCEPTIONS
              NAO_ACHOU_CTE   = 1
              NAO_ESCRITURADO = 2
              OTHERS          = 3.

          IF SY-SUBRC IS NOT INITIAL.
            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = SY-MSGTY
                P_ID           = SY-MSGID
                P_NUM          = SY-MSGNO
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
                P_MESSAGE_V3   = SY-MSGV3
                P_MESSAGE_V4   = SY-MSGV4
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ELSE.
            SY-MSGV1 = <FS_C57>-C57_CHAVE_ACESSO+25(9).
            SY-MSGV2 = <FS_C57>-DOCNUM_CTE.

            CALL METHOD ME->ADD_LOG_CTE_DIST
              EXPORTING
                P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                P_TYPE         = 'S'
                P_NUM          = 098
                P_MESSAGE_V1   = SY-MSGV1
                P_MESSAGE_V2   = SY-MSGV2
              CHANGING
                P_LC_SEQUENCIA = LC_SEQUENCIA.
          ENDIF.

        ENDLOOP.

    ENDCASE.

    IF P_CTE-TP_PROCESSO_CTE IS INITIAL.
      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
          P_TYPE         = 'A'
          P_NUM          = 019
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

      CHECK 1 = 2.
    ENDIF.

    CLEAR: IT_N01_T, IT_N55_T, IT_C57_T.

    LOOP AT IT_N01 ASSIGNING <FS_N01> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.
      APPEND <FS_N01> TO IT_N01_T.
    ENDLOOP.

    LOOP AT IT_N55 ASSIGNING <FS_N55> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.
      APPEND <FS_N55> TO IT_N55_T.
    ENDLOOP.

    LOOP AT IT_C57 ASSIGNING <FS_C57> WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.
      APPEND <FS_C57> TO IT_C57_T.
    ENDLOOP.

    CALL METHOD ME->BUSCA_ENTRADA_DO_FRETE
      EXPORTING
        P_N01_T         = IT_N01_T
        P_N55_T         = IT_N55_T
        P_C57_T         = IT_C57_T
      CHANGING
        E_CTE_DISTR     = P_CTE
      EXCEPTIONS
        ERRO_PEDIDO     = 1
        ERRO_MIRO_FRETE = 2
        NAO_ACHOU       = 3
        SEM_IVA         = 4
        OTHERS          = 5.

    IF SY-SUBRC IS NOT INITIAL.
      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
          P_TYPE         = SY-MSGTY
          P_ID           = SY-MSGID
          P_NUM          = SY-MSGNO
          P_MESSAGE_V1   = SY-MSGV1
          P_MESSAGE_V2   = SY-MSGV2
          P_MESSAGE_V3   = SY-MSGV3
          P_MESSAGE_V4   = SY-MSGV4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.
    ENDIF.

    CALL METHOD ME->BUSCA_TEXTO_CIDADE
      EXPORTING
        P_UF          = P_CTE-INICIO_UF
        P_IBGE        = P_CTE-INICIO_IBGE
      IMPORTING
        E_NOME_CIDADE = P_CTE-INICIO_MUNI
      EXCEPTIONS
        NAO_ACHOU     = 1
        OTHERS        = 2.

    CALL METHOD ME->BUSCA_TEXTO_CIDADE
      EXPORTING
        P_UF          = P_CTE-TERMINO_UF
        P_IBGE        = P_CTE-TERMINO_IBGE
      IMPORTING
        E_NOME_CIDADE = P_CTE-TERMINO_MUNI
      EXCEPTIONS
        NAO_ACHOU     = 1
        OTHERS        = 2.

    READ TABLE IT_TIPOS INTO WA_TIPOS WITH KEY TP_PROCESSO_CTE = P_CTE-TP_PROCESSO_CTE BINARY SEARCH.

    CALL METHOD ME->VALIDA_ETAPAS_CTE
      EXPORTING
        P_TIPO          = WA_TIPOS
        P_CTE           = P_CTE
        P_CTE_N01       = IT_N01_T
        P_CTE_N55       = IT_N55_T
        P_CTE_C57       = IT_C57_T
        P_GRAVAR_ETAPAS = TRUE.

  ENDMETHOD.


  METHOD LER_DADOS_XI.

    DATA: WA_CTE           TYPE ZIB_CTE_DIST_TER,
          WA_J_1BBRANCH    TYPE J_1BBRANCH,
          WA_J_1BNFDOC     TYPE J_1BNFDOC,
          LC_TIMESTAMP     TYPE TIMESTAMP,
          LC_DT_EMISSAO	   TYPE J_1BDOCDAT,
          LC_QTD_LINA      TYPE I,
          LC_NAO_ATUALIZAR TYPE C LENGTH 1,
          WA_RBKP          TYPE RBKP,
          WA_N01           TYPE ZIB_CTE_DIST_N01,
          WA_N55           TYPE ZIB_CTE_DIST_N55,
          IT_N01_AUX       TYPE ZIB_CTE_DIST_N01_T,
          IT_N55_AUX       TYPE ZIB_CTE_DIST_N55_T.

    FIELD-SYMBOLS: <FS_TER> TYPE ZIB_CTE_DIST_TER.
*                 <FS_N55> TYPE ZIB_CTE_DIST_N55,
*                 <FS_N01> TYPE ZIB_CTE_DIST_N01,
*                 <FS_NIT> TYPE ZIB_CTE_DIST_NIT.

    DATA: WA_CHAVES TYPE ZCL_CHAVE_RANGES,
          IT_CHAVES TYPE TABLE OF ZCL_CHAVE_RANGES.

    IF P_CHAVE_CTE IS NOT INITIAL.

      WA_CHAVES-SIGN   = 'I'.
      WA_CHAVES-OPTION = 'EQ'.
      WA_CHAVES-LOW    = P_CHAVE_CTE.
      WA_CHAVES-HIGH   = P_CHAVE_CTE.
      APPEND WA_CHAVES TO IT_CHAVES.

      SELECT * INTO TABLE IT_CTE
        FROM ZIB_CTE_DIST_TER
       WHERE CD_CHAVE_CTE IN IT_CHAVES.

      READ TABLE IT_CTE INTO WA_CTE INDEX 1.

      IF WA_CTE-BELNR IS NOT INITIAL.
        SELECT SINGLE * INTO WA_RBKP
          FROM RBKP
         WHERE BELNR = WA_CTE-BELNR
           AND GJAHR = WA_CTE-GJAHR.

        "Miro Estornada
        IF WA_RBKP-STBLG IS NOT INITIAL.
          CLEAR: WA_CTE-DOCNUM_CTE,
                 WA_CTE-CK_FINALIZADO,
                 WA_CTE-TP_PROCESSO_CTE,
                 WA_CTE-BELNR,
                 WA_CTE-GJAHR.
          MODIFY IT_CTE INDEX 1 FROM WA_CTE TRANSPORTING DOCNUM_CTE CK_FINALIZADO TP_PROCESSO_CTE BELNR GJAHR.
        ENDIF.
      ENDIF.

      IF WA_CTE-DOCNUM_CTE IS NOT INITIAL.
        SELECT SINGLE * INTO WA_J_1BNFDOC FROM J_1BNFDOC WHERE DOCNUM EQ WA_CTE-DOCNUM_CTE.
        IF WA_J_1BNFDOC-CANCEL EQ ABAP_TRUE.
          CLEAR: WA_CTE-DOCNUM_CTE.
          WA_CTE-CK_FINALIZADO = FALSE.
          MODIFY IT_CTE INDEX 1 FROM WA_CTE TRANSPORTING DOCNUM_CTE CK_FINALIZADO.
        ENDIF.
      ENDIF.

      IF WA_CTE-CK_FINALIZADO EQ TRUE.
        MESSAGE W041 WITH P_CHAVE_CTE.
      ENDIF.

      CHECK WA_CTE-CK_FINALIZADO EQ FALSE.

    ELSEIF P_PENDENTES EQ TRUE.

      CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
        EXPORTING
          I_DATE   = SY-DATUM
          I_TIME   = SY-UZEIT
        IMPORTING
          E_TSTAMP = LC_TIMESTAMP.

      CALL FUNCTION 'IAM_TIMESTAMP_CALC'
        EXPORTING
          IV_REFDATE = LC_TIMESTAMP
          IV_DAYS    = -1
        IMPORTING
          EV_DATE    = LC_TIMESTAMP.

      LC_DT_EMISSAO  = SY-DATUM.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DT_EMISSAO
          DAYS      = 0
          MONTHS    = 1
          YEARS     = 0
          SIGNUM    = '-'
        IMPORTING
          CALC_DATE = LC_DT_EMISSAO.

      SELECT * INTO TABLE IT_CTE
        FROM ZIB_CTE_DIST_TER
       WHERE CK_FINALIZADO EQ FALSE
         AND DT_EMISSAO    GE LC_DT_EMISSAO
         AND TIMESTAMP     LE LC_TIMESTAMP
       ORDER BY DT_EMISSAO.

      DESCRIBE TABLE IT_CTE LINES LC_QTD_LINA.
      IF LC_QTD_LINA GT 200.
        LOOP AT IT_CTE INTO WA_CTE.
          IF SY-TABIX GT 200.
            DELETE IT_CTE INDEX SY-TABIX.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      SELECT * INTO TABLE IT_CTE
        FROM ZIB_CTE_DIST_TER
       WHERE RG_LIDO_PAG_FRET EQ FALSE.
    ENDIF.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE IT_N01
      FROM ZIB_CTE_DIST_N01
       FOR ALL ENTRIES IN IT_CTE
     WHERE CD_CHAVE_CTE EQ IT_CTE-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_N55
      FROM ZIB_CTE_DIST_N55
       FOR ALL ENTRIES IN IT_CTE
     WHERE CD_CHAVE_CTE EQ IT_CTE-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_NIT
      FROM ZIB_CTE_DIST_NIT
       FOR ALL ENTRIES IN IT_CTE
     WHERE CD_CHAVE_CTE EQ IT_CTE-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_C57
      FROM ZIB_CTE_DIST_C57
       FOR ALL ENTRIES IN IT_CTE
     WHERE CD_CHAVE_CTE EQ IT_CTE-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_ANT
      FROM ZIB_CTE_DIST_ANT
       FOR ALL ENTRIES IN IT_CTE
     WHERE CD_CHAVE_CTE EQ IT_CTE-CD_CHAVE_CTE.

    SELECT * INTO TABLE IT_TIPOS
      FROM ZIB_CTE_DIST_FLG.

    SORT IT_TIPOS BY TP_PROCESSO_CTE.

    LC_NAO_ATUALIZAR = TRUE.

    LOOP AT IT_CTE ASSIGNING <FS_TER>.

      CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
        EXPORTING
          CHAVE          = <FS_TER>-CD_CHAVE_CTE
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC IS NOT INITIAL.
        IF P_CHAVE_CTE IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING FOREIGN_LOCK.
          LC_NAO_ATUALIZAR = FALSE.
        ENDIF.
        CONTINUE.
      ENDIF.

      LC_SEQUENCIA = 1.

      CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
        EXPORTING
          I_DATE   = SY-DATUM
          I_TIME   = SY-UZEIT
        IMPORTING
          E_TSTAMP = <FS_TER>-TIMESTAMP.

      DELETE FROM ZIB_CTE_DIST_LOG WHERE CD_CHAVE_CTE EQ <FS_TER>-CD_CHAVE_CTE.

      CLEAR: <FS_TER>-E_TOMADORA,
             <FS_TER>-F_TOMADORA,
             <FS_TER>-P_EMISSOR,
             <FS_TER>-E_EMISSOR,
             <FS_TER>-F_EMISSOR,
             <FS_TER>-DOCNUM_CTE_C,
             <FS_TER>-DOCNUM_CTE_A,
             <FS_TER>-DOCNUM_CTE_S,
             <FS_TER>-EBELN,
             <FS_TER>-EBELP,
             <FS_TER>-BELNR,
             <FS_TER>-GJAHR,
             <FS_TER>-DOCNUM_CTE_SUB,
             <FS_TER>-TP_PROCESSO_CTE,
             <FS_TER>-DOCNUM_CTE_P.

      <FS_TER>-RG_LIDO_PAG_FRET = ME->TRUE.
      <FS_TER>-ZVLR_FRETE       = <FS_TER>-VALOR_RECEBER.

      CALL METHOD ME->BUSCA_TOMADOR_SERVICO
        EXPORTING
          P_CTE_DIST         = <FS_TER>
        IMPORTING
          E_J_1BBRANCH       = WA_J_1BBRANCH
        EXCEPTIONS
          NAO_ACHOU_PARCEIRO = 1
          OTHERS             = 2.

      IF SY-SUBRC IS NOT INITIAL.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
            P_TYPE         = SY-MSGTY
            P_ID           = SY-MSGID
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
        CONTINUE.
      ENDIF.

      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
          P_TYPE         = SY-MSGTY
          P_ID           = SY-MSGID
          P_NUM          = SY-MSGNO
          P_MESSAGE_V1   = SY-MSGV1
          P_MESSAGE_V2   = SY-MSGV2
          P_MESSAGE_V3   = SY-MSGV3
          P_MESSAGE_V4   = SY-MSGV4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

      <FS_TER>-E_TOMADORA = WA_J_1BBRANCH-BUKRS.
      <FS_TER>-F_TOMADORA = WA_J_1BBRANCH-BRANCH.

      CALL METHOD ME->BUSCA_FORN_EMPRESA
        CHANGING
          P_CTE_DIST           = <FS_TER>
        EXCEPTIONS
          NAO_ACHOU_FORNECEDOR = 1
          OTHERS               = 2.

      IF SY-SUBRC IS NOT INITIAL.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
            P_TYPE         = SY-MSGTY
            P_ID           = SY-MSGID
            P_NUM          = SY-MSGNO
            P_MESSAGE_V1   = SY-MSGV1
            P_MESSAGE_V2   = SY-MSGV2
            P_MESSAGE_V3   = SY-MSGV3
            P_MESSAGE_V4   = SY-MSGV4
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.

        CONTINUE.
      ENDIF.

      CALL METHOD ME->ADD_LOG_CTE_DIST
        EXPORTING
          P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
          P_TYPE         = SY-MSGTY
          P_ID           = SY-MSGID
          P_NUM          = SY-MSGNO
          P_MESSAGE_V1   = SY-MSGV1
          P_MESSAGE_V2   = SY-MSGV2
          P_MESSAGE_V3   = SY-MSGV3
          P_MESSAGE_V4   = SY-MSGV4
        CHANGING
          P_LC_SEQUENCIA = LC_SEQUENCIA.

      IF <FS_TER>-E_EMISSOR IS NOT INITIAL.
        IF <FS_TER>-E_EMISSOR EQ <FS_TER>-E_TOMADORA.
          "Frete Próprio
          <FS_TER>-TP_PROCESSO_CTE = ME->TIPO_02.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 006
              P_MESSAGE_V1   = ME->DS_TIPO_02
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ELSE.
          "Frete Próprio - Intercompany
          <FS_TER>-TP_PROCESSO_CTE = ME->TIPO_03.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 006
              P_MESSAGE_V1   = ME->DS_TIPO_03
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.

      ENDIF.

      "Se CNPJ do Tomador for diferente do Remetente, diferente do Destinatário
      "CNPJ tomador não for nulo
      "Empresa Emissora não é uma empresa do Grupo
      "Empresa Tomadora for uma empresa do grupo
      IF ( <FS_TER>-REME_CNPJ <> <FS_TER>-TOMA4_CNPJ ) AND
         ( <FS_TER>-DEST_CNPJ <> <FS_TER>-TOMA4_CNPJ ) AND
         ( <FS_TER>-TOMA4_CNPJ IS NOT INITIAL ) AND
         ( <FS_TER>-E_EMISSOR  IS INITIAL     ) AND
         ( <FS_TER>-E_TOMADORA IS NOT INITIAL ).
        "Frete Próprio - Subcontratado
        <FS_TER>-TP_PROCESSO_CTE = ME->TIPO_04.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_NUM          = 006
            P_MESSAGE_V1   = ME->DS_TIPO_04
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      IF ( <FS_TER>-TP_PROCESSO_CTE EQ ME->TIPO_02 ) OR
         ( <FS_TER>-TP_PROCESSO_CTE EQ ME->TIPO_03 ).

        CALL METHOD ME->BUSCA_DOCNUM_CHAVE
          EXPORTING
            P_CHAVE     = <FS_TER>-CD_CHAVE_CTE
            P_PSQ_CHAVE = TRUE
            P_FORM      = TRUE
          CHANGING
            E_DOCNUM    = <FS_TER>-DOCNUM_CTE_SUB
          EXCEPTIONS
            NAO_ACHOU   = 1
            OTHERS      = 2.

        IF SY-SUBRC IS INITIAL.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = SY-MSGTY
              P_ID           = SY-MSGID
              P_NUM          = SY-MSGNO
              P_MESSAGE_V1   = SY-MSGV1
              P_MESSAGE_V2   = SY-MSGV2
              P_MESSAGE_V3   = SY-MSGV3
              P_MESSAGE_V4   = SY-MSGV4
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ELSE.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = SY-MSGTY
              P_ID           = SY-MSGID
              P_NUM          = SY-MSGNO
              P_MESSAGE_V1   = SY-MSGV1
              P_MESSAGE_V2   = SY-MSGV2
              P_MESSAGE_V3   = SY-MSGV3
              P_MESSAGE_V4   = SY-MSGV4
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
          CONTINUE.
        ENDIF.
      ENDIF.

      CASE <FS_TER>-CD_TIPO_CTE.
        WHEN '0'.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 003
              P_MESSAGE_V1   = 'Normal'
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        WHEN '1'.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 003
              P_MESSAGE_V1   = 'CT-e de Complemento de Valores'
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.

          <FS_TER>-ZVLR_VI = <FS_TER>-VALOR_PRESTACAO.

        WHEN '2'.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 003
              P_MESSAGE_V1   = 'CT-e de Anulação de Valores'
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        WHEN '3'.
          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
              P_TYPE         = 'S'
              P_NUM          = 003
              P_MESSAGE_V1   = 'CT-e Substituto'
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDCASE.

      "Busca CT-e de Saída Próprio """""""""""""""""""""""""
      CALL METHOD ME->BUSCA_DOCNUM_CHAVE
        EXPORTING
          P_CHAVE            = <FS_TER>-CD_CHAVE_CTE
          P_PSQ_CHAVE        = ABAP_TRUE
          P_FORM             = ABAP_TRUE
        CHANGING
          E_DOCNUM           = <FS_TER>-DOCNUM_CTE_P
        EXCEPTIONS
          NAO_ACHOU          = 1
          NAO_ACHOU_PARCEIRO = 2
          OTHERS             = 3.

      IF <FS_TER>-DOCNUM_CTE_P IS NOT INITIAL.
        SY-MSGV1 = <FS_TER>-DOCNUM_CTE_P.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = <FS_TER>-CD_CHAVE_CTE
            P_TYPE         = 'S'
            P_NUM          = 212
            P_MESSAGE_V1   = SY-MSGV1
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      CASE <FS_TER>-CD_MODAL.
        WHEN '01'.  "Rodoviário
          ME->LER_DADOS_RODOVIARIO( CHANGING P_CTE = <FS_TER> ).
          ME->BUSCA_INFO_ALGODAO( CHANGING P_CTE_DIST = <FS_TER> ).
        WHEN '02'.  "Aéreo

        WHEN '03'.  "Aquaviário
          ME->LER_DADOS_RODOVIARIO( CHANGING P_CTE = <FS_TER> ).
        WHEN '04'.  "Ferroviário

          LOOP AT IT_N55 INTO WA_N55.

            IF WA_N55-DOCNUM_NFE IS INITIAL.
              CONTINUE.
            ENDIF.

            SELECT SINGLE * INTO @DATA(WA_ITENS)
              FROM J_1BNFLIN
             WHERE DOCNUM EQ @WA_N55-DOCNUM_NFE.

            IF SY-SUBRC IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
            SELECT SINGLE * INTO @DATA(WA_ZLEST0154)
              FROM ZLEST0154
             WHERE BUKRS EQ @<FS_TER>-E_TOMADORA
               AND LIFNR EQ @<FS_TER>-P_EMISSOR
               AND MATKL EQ @WA_ITENS-MATKL.

            IF SY-SUBRC IS INITIAL.
              DATA(E_TIPO_CONTRATO) = '0002'.
            ENDIF.

          ENDLOOP.

          CALL METHOD ME->LER_DADOS_FERROVIARIO
            EXPORTING
              P_TIPO_CONTRATO = E_TIPO_CONTRATO
            CHANGING
              P_CTE           = <FS_TER>.
        WHEN '05'.  "Dutoviário.
        WHEN '06'.  "Multimodal

          LOOP AT IT_N55 INTO WA_N55.

            IF WA_N55-DOCNUM_NFE IS INITIAL.
              CONTINUE.
            ENDIF.

            SELECT SINGLE * INTO WA_ITENS
              FROM J_1BNFLIN
             WHERE DOCNUM EQ WA_N55-DOCNUM_NFE.

            IF SY-SUBRC IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            "Verificar se a Empresa Emissora / Tomador / Grupo de Mercadoria está parametrizado por frete lotação
            SELECT SINGLE * INTO WA_ZLEST0154
              FROM ZLEST0154
             WHERE BUKRS EQ <FS_TER>-E_TOMADORA
               AND LIFNR EQ <FS_TER>-P_EMISSOR
               AND MATKL EQ WA_ITENS-MATKL.

            IF SY-SUBRC IS INITIAL.
              E_TIPO_CONTRATO = '0002'.
            ENDIF.

          ENDLOOP.

          IF E_TIPO_CONTRATO EQ '0002'.
            CALL METHOD ME->LER_DADOS_FERROVIARIO
              EXPORTING
                P_TIPO_CONTRATO = E_TIPO_CONTRATO
              CHANGING
                P_CTE           = <FS_TER>.
          ELSE.
            CALL METHOD ME->LER_DADOS_RODOVIARIO
              CHANGING
                P_CTE = <FS_TER>.
          ENDIF.

      ENDCASE.

      "Pegar informações de Valor de VI das entradas de Digitação
      IF <FS_TER>-CK_PESO_CHEGADA IS NOT INITIAL.

        IT_N01_AUX = IT_N01.
        IT_N55_AUX = IT_N55.

        <FS_TER>-ZVLR_VI = 0.
        SORT IT_N01_AUX BY TKNUM.
        DELETE ADJACENT DUPLICATES FROM IT_N01_AUX COMPARING TKNUM.
        LOOP AT IT_N01_AUX INTO WA_N01 WHERE CD_CHAVE_CTE EQ <FS_TER>-CD_CHAVE_CTE.
          IF WA_N01-TKNUM IS NOT INITIAL.
            ADD WA_N01-ZVLR_VI TO <FS_TER>-ZVLR_VI.
          ENDIF.
        ENDLOOP.
        SORT IT_N55_AUX BY TKNUM.
        DELETE ADJACENT DUPLICATES FROM IT_N55_AUX COMPARING TKNUM.
        LOOP AT IT_N55_AUX INTO WA_N55 WHERE CD_CHAVE_CTE EQ <FS_TER>-CD_CHAVE_CTE.
          IF WA_N55-TKNUM IS NOT INITIAL.
            ADD WA_N55-ZVLR_VI TO <FS_TER>-ZVLR_VI.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL METHOD ME->AUTORIZACAO_LOG
        CHANGING
          P_CTE = <FS_TER>.

      CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
        EXPORTING
          CHAVE = <FS_TER>-CD_CHAVE_CTE.

    ENDLOOP.

    IF LC_NAO_ATUALIZAR EQ TRUE.
      "Grava valores de processamento da CT-e
      MODIFY ZIB_CTE_DIST_TER FROM TABLE IT_CTE.
      MODIFY ZIB_CTE_DIST_N55 FROM TABLE IT_N55.
      MODIFY ZIB_CTE_DIST_N01 FROM TABLE IT_N01.
      MODIFY ZIB_CTE_DIST_NIT FROM TABLE IT_NIT.
      MODIFY ZIB_CTE_DIST_C57 FROM TABLE IT_C57.
      MODIFY ZIB_CTE_DIST_ANT FROM TABLE IT_ANT.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD MONTA_SHDB.


    DATA: WA_BDC TYPE BDCDATA.

    IF P_DYNBEGIN EQ TRUE.
      WA_BDC-PROGRAM  = P_NAME.
      WA_BDC-DYNPRO   = P_VALUE.
      WA_BDC-DYNBEGIN = P_DYNBEGIN.
    ELSE.
      WA_BDC-FNAM = P_NAME.
      WA_BDC-FVAL = P_VALUE.
    ENDIF.

    APPEND WA_BDC TO E_SHDB.

  ENDMETHOD.


  METHOD prepara_reg_alv_saida.


    DATA: it_doc_n55 TYPE TABLE OF zib_cte_dist_n55,
          it_doc_n01 TYPE TABLE OF zib_cte_dist_n01,
          it_doc_nit TYPE TABLE OF zib_cte_dist_nit,
          it_doc_c57 TYPE TABLE OF zib_cte_dist_c57,
          it_doc_eap TYPE TABLE OF zib_cte_dist_eap,
          it_doc_ter TYPE TABLE OF zib_cte_dist_ter,
          it_mara    TYPE TABLE OF mara,
          it_makt    TYPE TABLE OF makt,
          it_t023t   TYPE TABLE OF t023t,
          it_lfa1    TYPE TABLE OF lfa1,
          it_aux     TYPE zib_cte_dist_ter_t.

    DATA: wa_doc_n55        TYPE zib_cte_dist_n55,
          wa_doc_n01        TYPE zib_cte_dist_n01,
          wa_doc_nit        TYPE zib_cte_dist_nit,
          wa_doc_c57        TYPE zib_cte_dist_c57,
          wa_doc_ter        TYPE zib_cte_dist_ter,
          wa_zlest0039      TYPE zlest0039,
          wa_makt           TYPE makt,
          wa_mara           TYPE mara,
          wa_t023t          TYPE t023t,
          wa_doc_eap        TYPE zib_cte_dist_eap,
          wa_j_1bnfe_active TYPE j_1bnfe_active,
          wa_j_1bnflin      TYPE j_1bnflin.

    DATA: it_n01_t TYPE zib_cte_dist_n01_t,
          it_n55_t TYPE zib_cte_dist_n55_t,
          it_c57_t TYPE zib_cte_dist_c57_t.

    DATA: it_tipos TYPE TABLE OF zib_cte_dist_flg,
          wa_tipos TYPE zib_cte_dist_flg.

    DATA: it_cte_n55 TYPE TABLE OF zib_cte_dist_n55,
          wa_cte_n55 TYPE zib_cte_dist_n55.

    DATA: it_cte TYPE TABLE OF zib_cte_dist_ter,
          wa_cte TYPE zib_cte_dist_ter.

    DATA: it_nfe TYPE TABLE OF zib_nfe_dist_ter,
          wa_nfe TYPE zib_nfe_dist_ter.

    DATA: wa_cte_dist	TYPE zib_cte_dist_ter,
          wa_alv      TYPE zde_cte_dist_alv,
          wa_lfa1     TYPE lfa1.

    DATA: fg_status(1),
          fg_peso_chegada(1).

    DATA: lc_xblnr TYPE xblnr1,
          l_serie  TYPE c LENGTH 3.

*    CLEAR: R_MIRO.


    CLEAR: e_alv.

    it_aux = p_cte_dist.

    DELETE it_aux WHERE p_emissor EQ space.

    IF it_aux IS NOT INITIAL.
      SELECT * INTO TABLE it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN it_aux
       WHERE lifnr EQ it_aux-p_emissor.

      SORT it_lfa1 BY lifnr.
    ENDIF.

    SELECT * INTO TABLE it_tipos
      FROM zib_cte_dist_flg.

    SORT it_tipos BY tp_processo_cte.

    SELECT * INTO TABLE it_doc_n55
      FROM zib_cte_dist_n55
       FOR ALL ENTRIES IN p_cte_dist
     WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte.

    SELECT * INTO TABLE it_doc_n01
      FROM zib_cte_dist_n01
       FOR ALL ENTRIES IN p_cte_dist
     WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte.

    SELECT * INTO TABLE it_doc_c57
      FROM zib_cte_dist_c57
       FOR ALL ENTRIES IN p_cte_dist
     WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte.

    SORT it_doc_c57 BY cd_chave_cte.

    SELECT * INTO TABLE it_doc_nit
      FROM zib_cte_dist_nit
       FOR ALL ENTRIES IN p_cte_dist
     WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte.

    IF it_doc_c57[] IS NOT INITIAL.

      SELECT * INTO TABLE it_doc_ter
        FROM zib_cte_dist_ter
         FOR ALL ENTRIES IN it_doc_c57
       WHERE cd_chave_cte EQ it_doc_c57-c57_chave_acesso.

      SORT it_doc_ter BY cd_chave_cte.

      SELECT * APPENDING TABLE it_doc_nit
        FROM zib_cte_dist_nit
         FOR ALL ENTRIES IN it_doc_c57
       WHERE cd_chave_cte EQ it_doc_c57-c57_chave_acesso.
    ENDIF.
    "============================================================
    IF it_doc_nit[] IS NOT INITIAL.

      SELECT * INTO TABLE it_mara
        FROM mara
         FOR ALL ENTRIES IN it_doc_nit
       WHERE matnr EQ it_doc_nit-zmatnr_merc.

      SORT it_mara BY matnr.

      SELECT * INTO TABLE it_makt
        FROM makt
         FOR ALL ENTRIES IN it_mara
       WHERE matnr EQ it_mara-matnr
         AND spras EQ sy-langu.

      SORT it_makt BY matnr.

      IF it_makt[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_t023t
          FROM t023t
           FOR ALL ENTRIES IN it_mara
         WHERE spras EQ sy-langu
           AND matkl EQ it_mara-matkl.

        SORT it_t023t BY matkl.
      ENDIF.

    ENDIF.

    SORT it_doc_nit BY cd_chave_cte.

    SELECT * INTO TABLE it_doc_eap
      FROM zib_cte_dist_eap
       FOR ALL ENTRIES IN p_cte_dist
     WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte
       AND ck_ultimo    EQ abap_true.

    SORT it_doc_eap BY cd_chave_cte tp_aprovacao.

*    FREE: it_cte_n55, it_nfe.
    IF p_cte_dist IS NOT INITIAL.
      SELECT * FROM zib_cte_dist_n55 INTO TABLE it_cte_n55
        FOR ALL ENTRIES IN p_cte_dist
          WHERE cd_chave_cte EQ p_cte_dist-cd_chave_cte.

      IF sy-subrc EQ 0.
        "============================================================
        " Inicio CS2019001142 - Anderson Oenning.
        FREE: it_nfe.
        SELECT * FROM zib_nfe_dist_ter
          INTO TABLE it_nfe
          FOR ALL ENTRIES IN it_cte_n55
          WHERE chave_nfe EQ it_cte_n55-n55_chave_acesso.
      ENDIF.
    ENDIF.

    LOOP AT p_cte_dist INTO wa_cte_dist.

      CLEAR: wa_alv.

      MOVE-CORRESPONDING wa_cte_dist TO wa_alv.

      "Calcula Valor Frete Peso""""""""""""""""""""""""""""">
      IF ( wa_alv-cd_tipo_cte EQ '0' OR wa_alv-cd_tipo_cte EQ '3' ) AND ( wa_alv-qt_carga_cte NE 0 ) AND ( wa_alv-zvlr_frete NE 0 ).
        wa_alv-zvalor_ft_peso = wa_alv-zvlr_frete / ( wa_alv-qt_carga_cte / 1000 ). "Peso do Transportador com Valor do Transportador
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""<

      "Calcula Valor Frete Peso VT """"""""""""""""""""""""">
      IF ( wa_alv-cd_tipo_cte EQ '0' OR wa_alv-cd_tipo_cte EQ '3' ) AND ( wa_alv-peso_origem NE 0 ) AND ( wa_alv-zvlr_vi NE 0 ).
        wa_alv-zvalor_vi_peso	= wa_alv-zvlr_vi / ( wa_alv-peso_origem / 1000 ).     "Peso da Nota Fiscal com VI
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""<

      CLEAR: wa_alv-p_emissor_n.

      IF wa_cte_dist-p_emissor IS NOT INITIAL.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_cte_dist-p_emissor BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_alv-p_emissor_n = wa_lfa1-name1.
        ENDIF.
      ENDIF.

      IF wa_cte_dist-ck_finalizado EQ true.
        wa_alv-status = icon_complete.
      ELSEIF wa_cte_dist-tp_processo_cte IS INITIAL.
        wa_alv-status = icon_initial.
      ELSE.
        "*  TP_PROCESSO_CTE: Processo mapeado;
        READ TABLE it_tipos INTO wa_tipos WITH KEY tp_processo_cte = wa_cte_dist-tp_processo_cte BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          CLEAR: it_n01_t, it_n55_t, it_c57_t.

          LOOP AT it_doc_n01 INTO wa_doc_n01 WHERE cd_chave_cte EQ wa_cte_dist-cd_chave_cte.
            APPEND wa_doc_n01 TO it_n01_t.
          ENDLOOP.

          LOOP AT it_doc_n55 INTO wa_doc_n55 WHERE cd_chave_cte EQ wa_cte_dist-cd_chave_cte.
            APPEND wa_doc_n55 TO it_n55_t.
          ENDLOOP.

          LOOP AT it_doc_c57 INTO wa_doc_c57 WHERE cd_chave_cte EQ wa_cte_dist-cd_chave_cte.
            APPEND wa_doc_c57 TO it_c57_t.
          ENDLOOP.

          CALL METHOD me->valida_etapas_cte
            EXPORTING
              p_tipo          = wa_tipos
              p_cte           = wa_cte_dist
              p_cte_n01       = it_n01_t
              p_cte_n55       = it_n55_t
              p_cte_c57       = it_c57_t
              p_gravar_etapas = false
            CHANGING
              p_concluido     = fg_status
              p_peso_chegada  = fg_peso_chegada.

          CASE fg_peso_chegada.
            WHEN true.
              CASE fg_status.
                WHEN true.
                  wa_alv-status = icon_release.
                WHEN false.
                  wa_alv-status = icon_defect.
              ENDCASE.
            WHEN false.
              wa_alv-status = icon_delivery_no_confirmation.
          ENDCASE.

        ELSE.
          wa_alv-status = icon_alert.
        ENDIF.

        "Verifica Trava de Pagamento
        READ TABLE it_doc_eap INTO wa_doc_eap WITH KEY cd_chave_cte = wa_cte_dist-cd_chave_cte tp_aprovacao = '03' BINARY SEARCH.
        IF sy-subrc IS INITIAL AND wa_doc_eap-tp_autorizado EQ '01'. "Travado o Pagamento
          wa_alv-status = icon_locked.
        ELSE.
          "Verificar Aprovação de Pagamento de Complemento
          IF wa_alv-cd_tipo_cte EQ '1'.
            READ TABLE it_doc_eap INTO wa_doc_eap WITH KEY cd_chave_cte = wa_cte_dist-cd_chave_cte tp_aprovacao = '02' BINARY SEARCH.
            IF sy-subrc IS INITIAL AND wa_doc_eap-tp_autorizado EQ '01'. "Autorizado o Pagamento
              wa_alv-status = icon_unlocked.
            ELSEIF sy-subrc IS INITIAL AND wa_doc_eap-tp_autorizado EQ '02'. "Não Autorizado o Pagamento
              wa_alv-status = icon_locked.
            ELSEIF  sy-subrc IS NOT INITIAL.
              wa_alv-status = icon_locked.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE it_doc_eap INTO wa_doc_eap WITH KEY cd_chave_cte = wa_cte_dist-cd_chave_cte tp_aprovacao = '01' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF wa_doc_eap-tp_autorizado EQ '01'.
          wa_alv-ck_chegada_doc = abap_true.
        ELSE.
          wa_alv-ck_chegada_doc = abap_false.
        ENDIF.
      ELSE.
        wa_alv-ck_chegada_doc = abap_false.
      ENDIF.

      CLEAR: wa_doc_nit-cd_chave_cte.

      IF wa_cte_dist-cd_tipo_cte EQ '1'.
        READ TABLE it_doc_c57 INTO wa_doc_c57 WITH KEY cd_chave_cte = wa_cte_dist-cd_chave_cte.
        IF sy-subrc IS INITIAL.
          wa_doc_nit-cd_chave_cte = wa_doc_c57-c57_chave_acesso.
          READ TABLE it_doc_ter INTO wa_doc_ter WITH KEY cd_chave_cte = wa_doc_c57-c57_chave_acesso.
          IF sy-subrc IS INITIAL.
            wa_alv-ds_prod_pred = wa_doc_ter-ds_prod_pred.
          ENDIF.
        ENDIF.
      ELSE.
        wa_doc_nit-cd_chave_cte = wa_cte_dist-cd_chave_cte.
      ENDIF.

      IF wa_doc_nit-cd_chave_cte IS NOT INITIAL.
        "Preenche Material/Grupo de Mercadoria da Nota Fiscal
        READ TABLE it_doc_nit INTO wa_doc_nit WITH KEY cd_chave_cte = wa_doc_nit-cd_chave_cte BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_alv-cd_matnr_nota      = wa_doc_nit-zmatnr_merc.
          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_doc_nit-zmatnr_merc BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            "Texto do Material
            READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_mara-matnr BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wa_alv-tx_prod_nota = wa_makt-maktx.
            ENDIF.

            "Texto do Grupo do Material
            READ TABLE it_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wa_alv-tx_grupo_merc_nota = wa_t023t-wgbez.
            ENDIF.

          ENDIF.
        ELSE.
          CLEAR: wa_doc_n55.
          LOOP AT it_doc_n55 INTO wa_doc_n55 WHERE cd_chave_cte EQ wa_doc_nit-cd_chave_cte.
            IF wa_alv-cd_matnr_nota IS NOT INITIAL.
              CONTINUE.
            ENDIF.
            SELECT SINGLE * INTO wa_j_1bnfe_active
              FROM j_1bnfe_active
             WHERE regio    EQ wa_doc_n55-n55_chave_acesso(2)
               AND nfyear   EQ wa_doc_n55-n55_chave_acesso+2(2)
               AND nfmonth  EQ wa_doc_n55-n55_chave_acesso+4(2)
               AND stcd1    EQ wa_doc_n55-n55_chave_acesso+6(14)
               AND model    EQ wa_doc_n55-n55_chave_acesso+20(2)
               AND serie    EQ wa_doc_n55-n55_chave_acesso+22(3)
               AND nfnum9   EQ wa_doc_n55-n55_chave_acesso+25(9)
               AND docnum9  EQ wa_doc_n55-n55_chave_acesso+34(9)
               AND cdv      EQ wa_doc_n55-n55_chave_acesso+43(1).

            IF sy-subrc IS INITIAL.
              SELECT SINGLE * INTO wa_j_1bnflin FROM j_1bnflin WHERE docnum EQ wa_j_1bnfe_active-docnum.
              IF sy-subrc IS INITIAL.
                wa_alv-cd_matnr_nota = wa_j_1bnflin-matnr.
                SELECT SINGLE maktx INTO wa_alv-tx_prod_nota FROM makt WHERE matnr = wa_j_1bnflin-matnr AND spras EQ sy-langu.
                SELECT SINGLE wgbez INTO wa_alv-tx_grupo_merc_nota FROM t023t WHERE spras EQ sy-langu AND matkl EQ wa_j_1bnflin-matkl.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      "============================================================
      " Inicio CS2019001142 - Anderson Oenning.
      "Verificar se a chave vinculada a CTE ja tem MIRO gerada.
*      IF wa_alv-status NE icon_complete.
*        IF wa_alv-cd_modal EQ '01'.
*          READ TABLE it_cte_n55 INTO wa_cte_n55 WITH KEY cd_chave_cte = wa_alv-cd_chave_cte.
*          IF sy-subrc EQ 0.
*            READ TABLE it_nfe INTO wa_nfe WITH KEY chave_nfe = wa_cte_n55-n55_chave_acesso.
*            IF sy-subrc EQ 0.
*              CLEAR: lc_xblnr, l_serie.
*              lc_xblnr = |{ wa_nfe-numero ALPHA = OUT }|.
*              l_serie = |{ wa_nfe-serie ALPHA = OUT }|.
*
*              CONCATENATE lc_xblnr l_serie INTO lc_xblnr SEPARATED BY '-'.
*              SELECT SINGLE * INTO @DATA(ws_miro)
*              FROM rbkp
*              WHERE xblnr  EQ @lc_xblnr
*              AND lifnr  EQ @wa_nfe-p_emissor
*              AND bukrs  EQ @wa_nfe-e_tomadora
*              AND rbstat NE '2'
*              AND stblg  EQ @space
*              AND bldat  GE @wa_nfe-dt_emissao.
*              IF ws_miro-belnr IS NOT INITIAL.
*                wa_alv-status = icon_message_error_small.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.

      "============================================================
      " Fim CS2019001142 - Anderson Oenning.


      APPEND wa_alv TO e_alv.
      CLEAR: wa_nfe, wa_cte_n55, wa_doc_ter.
*      ws_miro.
    ENDLOOP.

  ENDMETHOD.


  METHOD PSQ_NFE.


  ENDMETHOD.


  METHOD VALIDA_ETAPAS_CTE.


    DATA: WA_DOC_N01       TYPE ZIB_CTE_DIST_N01,
          WA_DOC_N55       TYPE ZIB_CTE_DIST_N55,
          WA_DOC_C57       TYPE ZIB_CTE_DIST_C57,
          P_TYPE           TYPE BAPI_MTYPE,
          WA_ZLEST0041     TYPE ZLEST0041,
          IT_DOC_N01X      TYPE TABLE OF ZIB_CTE_DIST_N01,
          IT_DOC_N55X      TYPE TABLE OF ZIB_CTE_DIST_N55,
          IT_J_1BNFLIN     TYPE TABLE OF J_1BNFLIN,
          IT_              TYPE TABLE OF ZIB_CTE_DIST_N55,
          IT_ZLEST0039     TYPE TABLE OF ZLEST0039,
          IT_MARA          TYPE TABLE OF MARA,
          WA_ZLEST0039     TYPE ZLEST0039,
          CK_PESO_APROVADO TYPE C LENGTH 1.

    P_CONCLUIDO = TRUE.

    "Verificar Peso Chegada """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    MOVE P_CTE_N01 TO IT_DOC_N01X[].
    MOVE P_CTE_N55 TO IT_DOC_N55X[].
    DELETE IT_DOC_N01X WHERE DOCNUM_NF  EQ SPACE.
    DELETE IT_DOC_N55X WHERE DOCNUM_NFE EQ SPACE.

    IF IT_DOC_N01X IS NOT INITIAL.
      SELECT * INTO TABLE IT_ZLEST0039
        FROM ZLEST0039
         FOR ALL ENTRIES IN IT_DOC_N01X
       WHERE DOCNUM EQ IT_DOC_N01X-DOCNUM_NF.

      SELECT * INTO TABLE IT_J_1BNFLIN
        FROM J_1BNFLIN
         FOR ALL ENTRIES IN IT_DOC_N01X
       WHERE DOCNUM EQ IT_DOC_N01X-DOCNUM_NF.
    ENDIF.

    IF IT_DOC_N55X IS NOT INITIAL.
      SELECT * APPENDING TABLE IT_ZLEST0039
        FROM ZLEST0039
         FOR ALL ENTRIES IN IT_DOC_N55X
       WHERE DOCNUM EQ IT_DOC_N55X-DOCNUM_NFE.

      SELECT * APPENDING TABLE IT_J_1BNFLIN
        FROM J_1BNFLIN
         FOR ALL ENTRIES IN IT_DOC_N55X
       WHERE DOCNUM EQ IT_DOC_N55X-DOCNUM_NFE.

    ENDIF.

    "Cadastro de Grupos de Mercadoria (validação frete peso TO)
    CK_PESO_APROVADO = ABAP_FALSE.
    IF IT_J_1BNFLIN[] IS NOT INITIAL.
      SELECT * INTO TABLE IT_MARA
        FROM MARA AS A
         FOR ALL ENTRIES IN IT_J_1BNFLIN
       WHERE A~MATNR EQ IT_J_1BNFLIN-MATNR
      AND EXISTS ( SELECT * FROM ZIB_CTE_DIST_GM AS G WHERE G~MATKL EQ A~MATKL ).

      IF SY-SUBRC IS INITIAL.
        CK_PESO_APROVADO = ABAP_TRUE.
      ENDIF.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "CK_FORNECEDOR: Validar Prestador de Serviço;
    IF ( P_TIPO-CK_FORNECEDOR EQ TRUE ).
      IF ( P_CTE-P_EMISSOR IS INITIAL ).
        P_CONCLUIDO = FALSE.
        P_TYPE = ERRO.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.

      IF P_GRAVAR_ETAPAS EQ TRUE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 042
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "	CK_TOMADOR: Validar Tomador do Serviço;
    IF ( P_TIPO-CK_TOMADOR EQ TRUE ).
      IF ( P_CTE-E_TOMADORA IS INITIAL OR P_CTE-F_TOMADORA IS INITIAL ).
        P_CONCLUIDO = FALSE.
        P_TYPE = ERRO.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 043
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "CK_CT_TRANSP_SUB: Valida Doc. de Transporte Subcontratado;
    IF ( P_TIPO-CK_CT_TRANSP_SUB EQ TRUE ).
      IF ( P_CTE-DOCNUM_CTE_SUB IS INITIAL ).
        P_CONCLUIDO = FALSE.
        P_TYPE = ERRO.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 066
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    LOOP AT P_CTE_N55 INTO WA_DOC_N55 WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      SELECT SINGLE * INTO WA_ZLEST0041 FROM ZLEST0041 WHERE DOCNUM EQ WA_DOC_N55-DOCNUM_NFE.
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      "	CK_NF_TRANSP: Validar Nota Fiscal da Prestação do Serviço;
      "	CK_FATURA_VF: Validar Fatura da Nota Fiscal da Prestação do Serviço;
      "	CK_REMESSA_VL: Validar Remessa da Nota Fiscal da Prestação do Serviço;
      "	CK_DOC_TRANS_VT: Validar Doc. Transporte da Remessa;
      "	CK_DOC_CUSTO_VI: Validar Doc. De Custo do Doc. De Transporte;
      "	CK_FOLHA_PAG: Validar Folha de Serviço do Pedido;
      "	CK_MIRO_MERC: Validar Miro de Entrada da Mercadoria;

      IF P_TIPO-CK_NF_TRANSP EQ TRUE.
        IF WA_DOC_N55-DOCNUM_NFE IS INITIAL.
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 045
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_FATURA_VF EQ TRUE ) .
        IF ( WA_DOC_N55-VBELN_VF IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 047
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_REMESSA_VL EQ TRUE ).
        IF ( WA_DOC_N55-VBELN_VL IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 048
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_DOC_TRANS_VT EQ TRUE ).
        IF ( WA_DOC_N55-TKNUM IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 049
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_DOC_CUSTO_VI EQ TRUE ) .
        IF ( WA_DOC_N55-FKNUM IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 050
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_FOLHA_PAG EQ TRUE ).
        IF ( WA_DOC_N55-LBLNI IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 052
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_MIRO_MERC EQ TRUE ).
        IF ( WA_DOC_N55-VBELN_RE IS INITIAL OR WA_DOC_N55-GJAHR_RE IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE      = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 053
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

    ENDLOOP.

    LOOP AT P_CTE_N01 INTO WA_DOC_N01 WHERE CD_CHAVE_CTE EQ P_CTE-CD_CHAVE_CTE.

      SELECT SINGLE * INTO WA_ZLEST0041 FROM ZLEST0041 WHERE DOCNUM EQ WA_DOC_N01-DOCNUM_NF.
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      "	CK_NF_TRANSP: Validar Nota Fiscal da Prestação do Serviço;
      "	CK_FATURA_VF: Validar Fatura da Nota Fiscal da Prestação do Serviço;
      "	CK_REMESSA_VL: Validar Remessa da Nota Fiscal da Prestação do Serviço;
      "	CK_DOC_TRANS_VT: Validar Doc. Transporte da Remessa;
      "	CK_DOC_CUSTO_VI: Validar Doc. De Custo do Doc. De Transporte;
      "	CK_FOLHA_PAG: Validar Folha de Serviço do Pedido;
      "	CK_MIRO_MERC: Validar Miro de Entrada da Mercadoria;

      IF P_TIPO-CK_NF_TRANSP EQ TRUE.
        IF WA_DOC_N01-DOCNUM_NF IS INITIAL.
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 045
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_FATURA_VF EQ TRUE ).
        IF ( WA_DOC_N01-VBELN_VF IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 047
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_REMESSA_VL EQ TRUE ).
        IF ( WA_DOC_N01-VBELN_VL IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 048
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_DOC_TRANS_VT EQ TRUE ).
        IF ( WA_DOC_N01-TKNUM IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 049
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_DOC_CUSTO_VI EQ TRUE ).
        IF ( WA_DOC_N01-FKNUM IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 050
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_FOLHA_PAG EQ TRUE ).
        IF ( WA_DOC_N01-LBLNI IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 052
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

      IF ( P_TIPO-CK_MIRO_MERC EQ TRUE ).
        IF ( WA_DOC_N01-VBELN_RE IS INITIAL OR WA_DOC_N01-GJAHR_RE IS INITIAL ).
          P_CONCLUIDO = FALSE.
          P_TYPE = ERRO.
        ELSE.
          P_TYPE = SUCESSO.
        ENDIF.
        IF P_GRAVAR_ETAPAS EQ TRUE.

          CALL METHOD ME->ADD_LOG_CTE_DIST
            EXPORTING
              P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
              P_TYPE         = P_TYPE
              P_NUM          = 053
            CHANGING
              P_LC_SEQUENCIA = LC_SEQUENCIA.
        ENDIF.
      ENDIF.

    ENDLOOP.

    "	CK_CT_TRANSP: Validar Conhecimento de Transporte da Prestação do Serviço;
    "IF ( P_TIPO-CK_CT_TRANSP EQ TRUE ) AND ( P_CTE-DOCNUM_CTE IS INITIAL ).
    "  P_CONCLUIDO = FALSE.
    "ENDIF.

    "	CK_PEDIDO_VI: Validar Pedido de Compra de Frete;
    IF ( P_TIPO-CK_PEDIDO_VI EQ TRUE ).
      IF ( P_CTE-EBELN IS INITIAL OR P_CTE-EBELP IS INITIAL ).
        P_CONCLUIDO = FALSE.
        P_TYPE      = ERRO.
      ELSE.
        P_TYPE      = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 051
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      IF ( P_CTE-MWSKZ IS INITIAL ).
        P_CONCLUIDO = FALSE.
        P_TYPE      = ERRO.
      ELSE.
        P_TYPE      = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 040
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "	CK_MIRO_FRETE: Validar Miro do Frete.
    IF ( P_TIPO-CK_MIRO_FRETE EQ TRUE ).
      IF ( P_CTE-BELNR IS INITIAL OR P_CTE-GJAHR IS INITIAL ).
        P_TYPE = INFORMA.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 054
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "CK_ENT_FIS_CTE: Verificar se o Frete Foi escriturado na filial tomadora;
    IF ( P_TIPO-CK_ENT_FIS_CTE EQ TRUE ).
      IF ( P_CTE-DOCNUM_CTE IS INITIAL ).
        P_TYPE = INFORMA.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.

        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 044
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "CK_PESO_ORIGEM: Validar Peso de Origem;
    IF ( P_TIPO-CK_PESO_ORIGEM EQ TRUE ).
      IF ( P_CTE-PESO_ORIGEM IS INITIAL ) AND ( P_CTE-CK_PESO_CHEGADA EQ FALSE ).
        P_TYPE = INFORMA.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.


        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 061
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.
    ENDIF.

    "CK_PESO_CHEGADA: validar Peso de Chegada.
    P_PESO_CHEGADA = TRUE.

    IF ( P_TIPO-CK_PESO_CHEGADA EQ TRUE ).

      IF ( ( P_CTE-PESO_CHEGADA IS INITIAL ) OR ( P_CTE-DT_CHEGADA IS INITIAL ) ) AND ( P_CTE-CK_PESO_CHEGADA EQ FALSE ).
        P_TYPE = INFORMA.
      ELSE.
        P_TYPE = SUCESSO.
      ENDIF.
      IF P_GRAVAR_ETAPAS EQ TRUE.
        CALL METHOD ME->ADD_LOG_CTE_DIST
          EXPORTING
            P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
            P_TYPE         = P_TYPE
            P_NUM          = 062
          CHANGING
            P_LC_SEQUENCIA = LC_SEQUENCIA.
      ENDIF.

      IF CK_PESO_APROVADO NE ABAP_TRUE.
        LOOP AT IT_DOC_N01X INTO WA_DOC_N01.
          IF P_CTE-TP_PROCESSO_CTE EQ '01' AND P_CTE-CD_MODAL EQ '01' AND P_CTE-CD_TIPO_CTE NE '1' AND ( WA_DOC_N01-AUART_VA EQ 'ZRFL' OR "Remessa Form. Lote.
                                                                                                      WA_DOC_N01-AUART_VA EQ 'ZRDC' ).   "Rem Form Lote DCO ).
            READ TABLE IT_ZLEST0039 INTO WA_ZLEST0039 WITH KEY DOCNUM = WA_DOC_N01-DOCNUM_NF.
            IF SY-SUBRC IS INITIAL.
              IF WA_ZLEST0039-PONTOTRANSB IS INITIAL.
                IF ( WA_ZLEST0039-PESOCHEGADA IS NOT INITIAL ) AND ( WA_ZLEST0039-DATACHEGADA IS NOT INITIAL ).
                  P_PESO_CHEGADA = TRUE.
                  IF P_GRAVAR_ETAPAS EQ TRUE.
                    SY-MSGV1 = WA_DOC_N01-DOCNUM_NF.
                    CALL METHOD ME->ADD_LOG_CTE_DIST
                      EXPORTING
                        P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                        P_TYPE         = P_TYPE
                        P_NUM          = 170
                        P_MESSAGE_V1   = SY-MSGV1
                      CHANGING
                        P_LC_SEQUENCIA = LC_SEQUENCIA.
                  ENDIF.
                ELSE.
                  P_PESO_CHEGADA = FALSE.
                ENDIF.
              ELSE.
                IF ( WA_ZLEST0039-PESOTRANSB IS NOT INITIAL ) AND ( WA_ZLEST0039-DATATRANSB IS NOT INITIAL ).
                  P_PESO_CHEGADA = TRUE.
                  IF P_GRAVAR_ETAPAS EQ TRUE.
                    SY-MSGV1 = WA_DOC_N01-DOCNUM_NF.
                    CALL METHOD ME->ADD_LOG_CTE_DIST
                      EXPORTING
                        P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                        P_TYPE         = P_TYPE
                        P_NUM          = 170
                        P_MESSAGE_V1   = SY-MSGV1
                      CHANGING
                        P_LC_SEQUENCIA = LC_SEQUENCIA.
                  ENDIF.
                ELSE.
                  P_PESO_CHEGADA = FALSE.
                ENDIF.
              ENDIF.
            ELSE.
              P_PESO_CHEGADA = FALSE.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_DOC_N55X INTO WA_DOC_N55.
          IF P_CTE-TP_PROCESSO_CTE EQ '01' AND P_CTE-CD_MODAL EQ '01' AND P_CTE-CD_TIPO_CTE NE '1' AND ( WA_DOC_N55-AUART_VA EQ 'ZRFL' OR "Remessa Form. Lote.
                                                                                                      WA_DOC_N55-AUART_VA EQ 'ZRDC' ).   "Rem Form Lote DCO ).
            READ TABLE IT_ZLEST0039 INTO WA_ZLEST0039 WITH KEY DOCNUM = WA_DOC_N55-DOCNUM_NFE.
            IF SY-SUBRC IS INITIAL.
              IF WA_ZLEST0039-PONTOTRANSB IS INITIAL.
                IF ( WA_ZLEST0039-PESOCHEGADA IS NOT INITIAL ) AND ( WA_ZLEST0039-DATACHEGADA IS NOT INITIAL ).
                  P_PESO_CHEGADA = TRUE.
                  IF P_GRAVAR_ETAPAS EQ TRUE.
                    SY-MSGV1 = WA_DOC_N55-DOCNUM_NFE.
                    CALL METHOD ME->ADD_LOG_CTE_DIST
                      EXPORTING
                        P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                        P_TYPE         = P_TYPE
                        P_NUM          = 170
                        P_MESSAGE_V1   = SY-MSGV1
                      CHANGING
                        P_LC_SEQUENCIA = LC_SEQUENCIA.
                  ENDIF.
                ELSE.
                  P_PESO_CHEGADA = FALSE.
                ENDIF.
              ELSE.
                IF ( WA_ZLEST0039-PESOTRANSB IS NOT INITIAL ) AND ( WA_ZLEST0039-DATATRANSB IS NOT INITIAL ).
                  P_PESO_CHEGADA = TRUE.
                  IF P_GRAVAR_ETAPAS EQ TRUE.
                    SY-MSGV1 = WA_DOC_N55-DOCNUM_NFE.
                    CALL METHOD ME->ADD_LOG_CTE_DIST
                      EXPORTING
                        P_CD_CHAVE_CTE = P_CTE-CD_CHAVE_CTE
                        P_TYPE         = P_TYPE
                        P_NUM          = 170
                        P_MESSAGE_V1   = SY-MSGV1
                      CHANGING
                        P_LC_SEQUENCIA = LC_SEQUENCIA.
                  ENDIF.
                ELSE.
                  P_PESO_CHEGADA = FALSE.
                ENDIF.
              ENDIF.
            ELSE.
              P_PESO_CHEGADA = FALSE.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICA_VENCIMENTO_FATURA.


    DATA: LC_PROXIMO_VENCIMENTO TYPE DATUM,
          IT_DATAS              TYPE TABLE OF ISCAL_DAY,
          WA_DATAS              TYPE ISCAL_DAY.


    CALL METHOD ZCL_CTE_DIST_G=>BUSCA_PROXIMO_VENC_FATURA
      IMPORTING
        E_DATA_VENCIMENTO = LC_PROXIMO_VENCIMENTO
      EXCEPTIONS
        ERRO              = 1
        OTHERS            = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NAO_VALIDA.
    ENDIF.

    IF I_DATA_VENCIMENTO LT LC_PROXIMO_VENCIMENTO.
      MESSAGE E166 WITH LC_PROXIMO_VENCIMENTO RAISING NAO_VALIDA.
    ENDIF.

    CHECK I_VALIDA_DIA_UTIL EQ ABAP_TRUE.

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        HOLIDAY_CALENDAR = 'MG'
        FACTORY_CALENDAR = 'ZT'
        DATE_FROM        = I_DATA_VENCIMENTO
        DATE_TO          = I_DATA_VENCIMENTO
      TABLES
        HOLIDAYS         = IT_DATAS
      EXCEPTIONS
        OTHERS           = 1.

    READ TABLE IT_DATAS INTO WA_DATAS WITH KEY DATE = I_DATA_VENCIMENTO.
    IF SY-SUBRC IS INITIAL.
      CASE WA_DATAS-HOLIDAY.
        WHEN ABAP_FALSE.
          MESSAGE E167 WITH I_DATA_VENCIMENTO RAISING NAO_VALIDA.
        WHEN ABAP_TRUE.
          MESSAGE E168 WITH I_DATA_VENCIMENTO WA_DATAS-TXT_LONG RAISING NAO_VALIDA.
      ENDCASE.
    ENDIF.

    "167  Dia &1 é final de semana!
    "168  Dia &1 é feriado de &2!


  ENDMETHOD.


  METHOD VERIFICA_VOLUME_PRECO_FERRO.


    DATA: WA_ZLEST0118        TYPE ZLEST0118,
          IT_ZLEST0119        TYPE TABLE OF ZLEST0119,
          IT_ZLEST0119_AUX    TYPE TABLE OF ZLEST0119,
          WA_ZLEST0119        TYPE ZLEST0119,
          IT_ZLEST0120        TYPE TABLE OF ZLEST0120,
          WA_ZLEST0120        TYPE ZLEST0120,
          IT_ZLEST0128        TYPE TABLE OF ZLEST0128,
          WA_ZLEST0128        TYPE ZLEST0128,
          IT_ZLEST0044        TYPE TABLE OF ZLEST0044,
          WA_ZLEST0044        TYPE ZLEST0044,
          WA_LFA1             TYPE LFA1,
          LC_TOTAL_NEGOCIADO  TYPE BRGEW_15,
          LC_TOTAL_FATURADO   TYPE BRGEW_15,
          LC_TOTAL_DISPONIVEL TYPE BRGEW_15.

    DATA: LC_CIDADE_ORIGEM  TYPE ZLEST0044-CIDADE_ORIGEM,
          LC_UF_ORIGEM      TYPE ZLEST0044-UF_ORIGEM,
          LC_CIDADE_DESTINO TYPE ZLEST0044-CIDADE_DESTINO,
          LC_UF_DESTINO     TYPE ZLEST0044-UF_DESTINO,
          LC_TARIFA	        TYPE ZLEST0044-TARIFA.

    CLEAR: IT_ZLEST0119[],
           IT_ZLEST0119_AUX[],
           IT_ZLEST0120[].
*---
    IF I_MATERIAL IS INITIAL.
      MESSAGE E176 RAISING SEM_MATERIAL.
    ENDIF.
*---
    SELECT SINGLE * INTO WA_ZLEST0118
      FROM ZLEST0118
     WHERE LIFNR EQ I_FORNECEDOR
       AND PAIS EQ I_PAIS
       AND DOMICILIO_ORIGEM EQ I_INICIO
       AND DOMICILIO_DESTIN EQ I_FINAL.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E119 WITH I_FORNECEDOR I_INICIO I_FINAL RAISING SEM_ITINERARIO.
    ENDIF.

    CASE I_TIPO_CONTRATO.
      WHEN '0001'.

        SELECT * INTO TABLE IT_ZLEST0119
          FROM ZLEST0119
         WHERE TP_PRECO EQ SPACE
           AND TP_MODAL EQ '4'
           AND LIFNR EQ WA_ZLEST0118-LIFNR
           AND PAIS  EQ WA_ZLEST0118-PAIS
           AND DOMICILIO_ORIGEM EQ WA_ZLEST0118-DOMICILIO_ORIGEM
           AND DOMICILIO_DESTIN EQ WA_ZLEST0118-DOMICILIO_DESTIN
           AND WAERK EQ I_MOEDA_TARIFA
           AND PRECO EQ I_VLR_TARIFA
           AND UND_PRECO EQ I_UND_TARIFA
           AND DT_INICIO LE I_DT_REFERENCIA
           AND DT_FIM GE I_DT_REFERENCIA
           AND CK_EXCLUIDO NE 'X'
           AND BUKRS EQ I_TOMADOR
           AND BRANCH EQ I_TOMADOR_CENTRO.

        IF SY-SUBRC IS NOT INITIAL.
          DATA(CK_FILIAL) = ABAP_FALSE.

          SELECT * INTO TABLE IT_ZLEST0119
            FROM ZLEST0119
           WHERE TP_PRECO EQ SPACE
             AND TP_MODAL EQ '4'
             AND LIFNR EQ WA_ZLEST0118-LIFNR
             AND PAIS  EQ WA_ZLEST0118-PAIS
             AND DOMICILIO_ORIGEM EQ WA_ZLEST0118-DOMICILIO_ORIGEM
             AND DOMICILIO_DESTIN EQ WA_ZLEST0118-DOMICILIO_DESTIN
             AND WAERK EQ I_MOEDA_TARIFA
             AND PRECO EQ I_VLR_TARIFA
             AND UND_PRECO EQ I_UND_TARIFA
             AND DT_INICIO LE I_DT_REFERENCIA
             AND DT_FIM GE I_DT_REFERENCIA
             AND CK_EXCLUIDO NE 'X'.
        ELSE.
          CK_FILIAL = ABAP_TRUE.
        ENDIF.

      WHEN '0002'.

        SELECT * INTO TABLE IT_ZLEST0119
          FROM ZLEST0119
         WHERE TP_PRECO EQ SPACE
           AND TP_MODAL EQ '4'
           AND LIFNR EQ WA_ZLEST0118-LIFNR
           AND PAIS  EQ WA_ZLEST0118-PAIS
           AND DOMICILIO_ORIGEM EQ WA_ZLEST0118-DOMICILIO_ORIGEM
           AND DOMICILIO_DESTIN EQ WA_ZLEST0118-DOMICILIO_DESTIN
           AND WAERK EQ I_MOEDA_TARIFA
           AND PRECO EQ I_VLR_TARIFA
           AND UND_PRECO EQ SPACE
           AND DT_INICIO LE I_DT_REFERENCIA
           AND DT_FIM GE I_DT_REFERENCIA
           AND CK_EXCLUIDO NE 'X'
           AND BUKRS EQ I_TOMADOR
           AND BRANCH EQ I_TOMADOR_CENTRO.

        IF SY-SUBRC IS NOT INITIAL.
          CK_FILIAL = ABAP_FALSE.
          SELECT * INTO TABLE IT_ZLEST0119
            FROM ZLEST0119
           WHERE TP_PRECO EQ SPACE
             AND TP_MODAL EQ '4'
             AND LIFNR EQ WA_ZLEST0118-LIFNR
             AND PAIS  EQ WA_ZLEST0118-PAIS
             AND DOMICILIO_ORIGEM EQ WA_ZLEST0118-DOMICILIO_ORIGEM
             AND DOMICILIO_DESTIN EQ WA_ZLEST0118-DOMICILIO_DESTIN
             AND WAERK EQ I_MOEDA_TARIFA
             AND PRECO EQ I_VLR_TARIFA
             AND UND_PRECO EQ SPACE
             AND DT_INICIO LE I_DT_REFERENCIA
             AND DT_FIM GE I_DT_REFERENCIA
             AND CK_EXCLUIDO NE 'X'.
        ELSE.
          CK_FILIAL = ABAP_TRUE.
        ENDIF.
    ENDCASE.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E120 WITH I_VLR_TARIFA I_DT_REFERENCIA RAISING SEM_VOLUME.
    ENDIF.

    IF CK_FILIAL EQ ABAP_FALSE.
      SELECT * INTO TABLE IT_ZLEST0120
        FROM ZLEST0120
         FOR ALL ENTRIES IN IT_ZLEST0119
       WHERE CD_SEQ_LANC EQ IT_ZLEST0119-CD_SEQ_LANC
         AND BUKRS EQ I_TOMADOR.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE E175 WITH I_VLR_TARIFA I_DT_REFERENCIA I_TOMADOR RAISING SEM_VOLUME_EMPRESA.
      ENDIF.
    ENDIF.

    SELECT * INTO TABLE IT_ZLEST0128
      FROM ZLEST0128
       FOR ALL ENTRIES IN IT_ZLEST0119
     WHERE CD_SEQ_LANC EQ IT_ZLEST0119-CD_SEQ_LANC
       AND MATNR EQ I_MATERIAL.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E180 WITH I_VLR_TARIFA I_DT_REFERENCIA I_MATERIAL RAISING SEM_MATERIAL.
    ENDIF.

    "Limpar e Deixar somente volumes que são permitidos para a empresa
    IF CK_FILIAL EQ ABAP_FALSE.
      LOOP AT IT_ZLEST0120 INTO WA_ZLEST0120.
        LOOP AT IT_ZLEST0128 INTO WA_ZLEST0128 WHERE CD_SEQ_LANC EQ WA_ZLEST0120-CD_SEQ_LANC.
          LOOP AT IT_ZLEST0119 INTO WA_ZLEST0119 WHERE CD_SEQ_LANC EQ WA_ZLEST0120-CD_SEQ_LANC.
            APPEND WA_ZLEST0119 TO IT_ZLEST0119_AUX.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      LOOP AT IT_ZLEST0128 INTO WA_ZLEST0128.
        LOOP AT IT_ZLEST0119 INTO WA_ZLEST0119 WHERE CD_SEQ_LANC EQ WA_ZLEST0128-CD_SEQ_LANC.
          APPEND WA_ZLEST0119 TO IT_ZLEST0119_AUX.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    CLEAR: IT_ZLEST0119[].
    MOVE IT_ZLEST0119_AUX[] TO IT_ZLEST0119[].
    CLEAR: IT_ZLEST0119_AUX[].

    SELECT SINGLE * INTO WA_LFA1
      FROM LFA1
     WHERE LIFNR EQ I_FORNECEDOR.

    SPLIT I_INICIO AT ' ' INTO LC_UF_ORIGEM LC_CIDADE_ORIGEM.
    SPLIT I_FINAL  AT ' ' INTO LC_UF_DESTINO LC_CIDADE_DESTINO.

    LC_TARIFA = I_VLR_TARIFA.

    SELECT * INTO TABLE IT_ZLEST0044
      FROM ZLEST0044
       FOR ALL ENTRIES IN IT_ZLEST0119
     WHERE CNPJ_EMITENTE  EQ WA_LFA1-STCD1
       AND IE_EMITENTE    EQ WA_LFA1-STCD3
       AND DT_REFERENCIA  GE IT_ZLEST0119-DT_INICIO
       AND DT_REFERENCIA  LE IT_ZLEST0119-DT_FIM
       AND CIDADE_ORIGEM  EQ LC_CIDADE_ORIGEM
       AND UF_ORIGEM      EQ LC_UF_ORIGEM
       AND CIDADE_DESTINO EQ LC_CIDADE_DESTINO
       AND UF_DESTINO     EQ LC_UF_DESTINO
       AND TARIFA         EQ LC_TARIFA
       AND MATNR_FATURADO EQ I_MATERIAL
       AND NR_TRANS       NE SPACE
       AND NR_FRETE       NE SPACE.

    LC_TOTAL_NEGOCIADO = 0.
    LOOP AT IT_ZLEST0119 INTO WA_ZLEST0119.
      IF WA_ZLEST0119-UND_NEGOCIADO EQ 'TO'.
        MULTIPLY WA_ZLEST0119-QTD_NEGOCIADO  BY 1000.
        MULTIPLY WA_ZLEST0119-QTD_TOLERANCIA BY 1000.
      ENDIF.
      ADD WA_ZLEST0119-QTD_NEGOCIADO  TO LC_TOTAL_NEGOCIADO.
      ADD WA_ZLEST0119-QTD_TOLERANCIA TO LC_TOTAL_NEGOCIADO.
      MOVE-CORRESPONDING WA_ZLEST0119 TO E_DISPONIVEL.
    ENDLOOP.

    LC_TOTAL_FATURADO = 0.
    LOOP AT IT_ZLEST0044 INTO WA_ZLEST0044.
      READ TABLE IT_ZLEST0120 INTO WA_ZLEST0120 WITH KEY BUKRS = WA_ZLEST0044-BUKRS.
      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_ZLEST0128 INTO WA_ZLEST0128 WITH KEY MATNR = WA_ZLEST0044-MATNR_FATURADO.
        IF SY-SUBRC IS INITIAL.
          CASE I_TIPO_CONTRATO.
            WHEN '0001'.
              ADD WA_ZLEST0044-PESO_BRUTO TO LC_TOTAL_FATURADO.
            WHEN '0002'.
              ADD 1 TO LC_TOTAL_FATURADO.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LC_TOTAL_DISPONIVEL = LC_TOTAL_NEGOCIADO - LC_TOTAL_FATURADO.

    IF LC_TOTAL_DISPONIVEL LT I_QTD_FATURAR.
      IF I_TIPO_CONTRATO EQ '0001'.
        "Saldo insuficiente! Faturado: &1 Disponível &2! (Tonelada)
        DIVIDE LC_TOTAL_FATURADO   BY 1000.
        DIVIDE LC_TOTAL_DISPONIVEL BY 1000.
      ENDIF.
      MESSAGE E123 WITH LC_TOTAL_FATURADO LC_TOTAL_DISPONIVEL RAISING SEM_VOLUME_DISPONIVEL.
    ENDIF.

    E_DISPONIVEL-QTD_UTILIZADA = LC_TOTAL_FATURADO.

  ENDMETHOD.


  METHOD add_log_cte_job.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
    DATA: wa_log     TYPE zib_cte_dist_log,
          lc_message TYPE bapi_msg.

    MOVE: p_message_v1 TO wa_log-message_v1,
          p_message_v2 TO wa_log-message_v2,
          p_message_v3 TO wa_log-message_v3,
          p_message_v4 TO wa_log-message_v4.

    wa_log-cd_chave_cte   = p_cd_chave_cte.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-nr_sequencia   = p_lc_sequencia.

    TRY .
        wa_log-type           = p_type.
        wa_log-id             = p_id.
        wa_log-num            = p_num.
        wa_log-message_v1     = p_message_v1.
        wa_log-message_v2     = p_message_v2.
        wa_log-message_v3     = p_message_v3.
        wa_log-message_v4     = p_message_v4.
        wa_log-bname          = sy-uname.
        wa_log-ck_estrategia  = p_estrategia.

        IF p_mensagem IS INITIAL.
          IF p_id IS NOT INITIAL AND p_type IS NOT INITIAL.
            MESSAGE ID p_id TYPE p_type NUMBER p_num INTO wa_log-message WITH wa_log-message_v1 wa_log-message_v2 wa_log-message_v3 wa_log-message_v4.
            MODIFY zib_cte_dist_log FROM wa_log.
          ELSE.
            CONCATENATE p_message_v1 p_message_v2 p_message_v3 p_message_v4 INTO wa_log-message SEPARATED BY space.
            MODIFY zib_cte_dist_log FROM wa_log.
          ENDIF.
        ELSE.
          wa_log-message = p_mensagem.
          MODIFY zib_cte_dist_log FROM wa_log.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    ADD 1 TO p_lc_sequencia.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  ENDMETHOD.


  METHOD get_base_pis_cofins.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
    CLEAR: r_base.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarv)
     WHERE name EQ 'EXC_ICMS_BASE_PIS_COFINS'.

    IF sy-subrc EQ 0 AND ( lwa_tvarv-low IS NOT INITIAL ).
      r_base = i_valor_frete - i_valor_icms.
    ELSE.
      r_base = i_valor_frete.
    ENDIF.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  ENDMETHOD.
ENDCLASS.
