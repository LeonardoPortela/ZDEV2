class ZCL_CTE_PAGAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .

  methods SET_MWSKZ
    importing
      !I_MWSKZ type MWSKZ .
  methods SET_DT_CHEGADA
    importing
      !I_DT_CHEGADA type ZDT_CHEGADA .
  methods SET_ZDT_MOV
    importing
      !I_ZDT_MOV type ZDT_MOV .
  methods SET_ZDT_VENCTO
    importing
      !I_ZDT_VENCTO type ZDT_VENCTO .
  methods SET_ZBVTYP
    importing
      !I_ZBVTYP type BVTYP .
  class-methods SET_INFO_FATURAR_CTE
    importing
      !I_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E
      !I_INDEX_ULTIMO type I
      !I_NAO_CHAMAR_TELA type CHAR01 default ' '
    exporting
      !E_COMANDO type CHAR01
    changing
      !I_INDEX type I optional
      !I_DT_VENCIMENTO type ZDT_VENCTO optional
    returning
      value(R_CTE) type ref to ZCL_CTE_PAGAMENTO
    exceptions
      BLOQUEADO_USUARIO .
  methods GET_QT_REG_NF
    importing
      !I_TKNUM type TKNUM
    returning
      value(E_QT_REG_NF) type I .
  methods GET_QT_REG_NI
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(E_QT_REG_NI) type I .
  methods GET_QT_REG_VT
    returning
      value(E_QT_REG_VT) type I .
  methods GET_IT_N55
    returning
      value(E_IT_N55) type ZIB_CTE_DIST_N55_T .
  methods GET_IT_N01
    returning
      value(E_IT_N01) type ZIB_CTE_DIST_N01_T .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_CADASTRO .
  methods GET_IT_NIT
    returning
      value(E_IT_NIT) type ZIB_CTE_DIST_NIT_T .
  methods GET_IT_DUP
    returning
      value(E_IT_DUP) type ZIB_CTE_DIST_DUP_T .
  methods GET_IT_VT
    returning
      value(E_IT_VT) type ZDE_CTE_DIST_VT_ALV_T .
  methods GET_CK_AUTORIZADO_PAGAMENTO
    returning
      value(E_CK_AUTORIZADO_PAGAMENTO) type CHAR01 .
  methods SET_PESO_CHEGADA_ITEM
    importing
      !I_NIT type ZIB_CTE_DIST_NIT .
  methods GET_CK_ENTRADA_MANUAL_PESO
    returning
      value(E_CK_ENTRADA_MANUAL_PESO) type CHAR01 .
  methods GET_CK_FATURA_PELA_VT
    returning
      value(E_CK_FATURA_PELA_VT) type CHAR01 .
  methods GET_CK_ALTERADO
    returning
      value(E_CK_ALTERADO) type CHAR01 .
  PROTECTED SECTION.
private section.

  data CD_CHAVE_CTE type ZDE_CHAVE_DOC_E .
  data DOCNUM_CTE type J_1BDOCNUM .
  data CK_FINALIZADO type ZDE_FAGL_FINALIZADO .
  data CD_STATUS_DIST type ZDE_CTE_DIST_ST .
  data DS_STATUS_DIST type TEXT60 .
  data CD_STATUS_SEFAZ type J_1BSTATUSCODE .
  data NUMR_CTE type J_1BNFNUM9 .
  data NUMR_SERIE type J_1BSERIES .
  data DT_EMISSAO type J_1BDOCDAT .
  data HR_EMISSAO type J_1BCRETIM .
  data VALOR_PRESTACAO type ZDE_VLR15_02 .
  data VALOR_RECEBER type ZDE_VLR15_02 .
  data VALOR_BASE_ICMS type ZDE_VLR15_02 .
  data VALOR_ICMS type ZDE_VLR15_02 .
  data CST_ICMS type ZDE_CST_ICMS .
  data CD_TOMADOR type ZCTE_TOMA .
  data DS_TOMADOR type TEXT60 .
  data EMIT_TP_DOC type ZDE_DOC_PARC .
  data EMIT_CNPJ type J_1BCGC .
  data EMIT_CPF type J_1BCPF .
  data EMIT_IE type J_1BSTAINS .
  data EMIT_RSOCIAL type TEXT60 .
  data EMIT_FANTASIA type TEXT60 .
  data REME_TP_DOC type ZDE_DOC_PARC .
  data REME_CNPJ type J_1BCGC .
  data REME_CPF type J_1BCPF .
  data REME_IE type J_1BSTAINS .
  data REME_RSOCIAL type TEXT60 .
  data REME_FANTASIA type TEXT60 .
  data EXPED_TP_DOC type ZDE_DOC_PARC .
  data EXPED_CNPJ type J_1BCGC .
  data EXPED_CPF type J_1BCPF .
  data EXPED_IE type J_1BSTAINS .
  data EXPED_RSOCIAL type TEXT60 .
  data EXPED_FANTASIA type TEXT60 .
  data RECEB_TP_DOC type ZDE_DOC_PARC .
  data RECEB_CNPJ type J_1BCGC .
  data RECEB_CPF type J_1BCPF .
  data RECEB_IE type J_1BSTAINS .
  data RECEB_RSOCIAL type TEXT60 .
  data RECEB_FANTASIA type TEXT60 .
  data DEST_TP_DOC type ZDE_DOC_PARC .
  data DEST_CNPJ type J_1BCGC .
  data DEST_CPF type J_1BCPF .
  data DEST_IE type J_1BSTAINS .
  data DEST_RSOCIAL type TEXT60 .
  data DEST_FANTASIA type TEXT60 .
  data TOMA4_TP_DOC type ZDE_DOC_PARC .
  data TOMA4_CNPJ type J_1BCGC .
  data TOMA4_CPF type J_1BCPF .
  data TOMA4_IE type J_1BSTAINS .
  data TOMA4_RSOCIAL type TEXT60 .
  data TOMA4_FANTASIA type TEXT60 .
  data MODELO type J_1BMODEL .
  data CD_MODAL type ZMODAL .
  data DS_MODAL type TEXT60 .
  data CD_TIPO_SERVICO type ZTPSERV .
  data DS_TIPO_SERVICO type TEXT60 .
  data CD_TIPO_CTE type ZTPCTE .
  data DS_TIPO_CTE type TEXT60 .
  data CD_FPAGAMENTO type ZFORPAG .
  data DS_FPAGAMENTO type TEXT60 .
  data CD_FEMISSAO type ZTFOREM .
  data DS_FEMISSAO type TEXT60 .
  data CD_APLICATIVO type ZDE_APLIC_EM .
  data DS_APLICATIVO type TEXT60 .
  data CODG_CFOP type ZDE_CFOP .
  data INICIO_IBGE type ZMUNIC_IBGE .
  data INICIO_MUNI type TEXT60 .
  data INICIO_UF type REGIO .
  data TERMINO_IBGE type ZMUNIC_IBGE .
  data TERMINO_MUNI type TEXT60 .
  data TERMINO_UF type REGIO .
  data NR_PROTOCOLO type J_1BNFEAUTHCODE .
  data DT_PROTOCOLO type J_1BDOCDAT .
  data HR_PROTOCOLO type J_1BCRETIM .
  data DOCSTA type J_1BNFEDOCSTATUS .
  data CANCEL type J_1BCANCEL .
  data REGIO type J_1BREGIO .
  data NFYEAR type J_1BYEAR .
  data NFMONTH type J_1BMONTH .
  data DOCNUM9 type J_1BDOCNUM9 .
  data CDV type J_1BCHECKDIGIT .
  data DS_PROD_PRED type ZDE_CTE_PROD_P .
  data QT_CARGA_CTE type ZDE_QT_CARGA_CTE .
  data VL_TOTAL_MERC type ZDE_VLR_TOTAL_MERC_CTE .
  data RG_LIDO_PAG_FRET type CHAR01 .
  data TP_PROCESSO_CTE type ZDE_PROCESSO_CTE .
  data E_TOMADORA type BUKRS .
  data F_TOMADORA type J_1BBRANC_ .
  data P_EMISSOR type LIFNR .
  data E_EMISSOR type BUKRS .
  data F_EMISSOR type J_1BBRANC_ .
  data DOCNUM_CTE_C type ZDE_DOCNUM_C .
  data DOCNUM_CTE_A type ZDE_DOCNUM_A .
  data DOCNUM_CTE_S type ZDE_DOCNUM_S .
  data DOCNUM_CTE_P type ZDE_DOCNUM_P .
  data EBELN type EBELN .
  data EBELP type EBELP .
  data MWSKZ type MWSKZ .
  data BELNR type RE_BELNR .
  data GJAHR type GJAHR .
  data DOCNUM_CTE_SUB type J_1BDOCNUM .
  data CK_MANUAL type CHAR01 .
  data WAERK_VI type WAERK .
  data KURSK_VI type KURSK .
  data ZVLR_VI type ZDE_VLR_VI .
  data ZVLR_FRETE type ZDE_VLR_FRETE .
  data ZVLR_MERCADORIA type ZDE_VLR_MERC .
  data PESO_ORIGEM type ZDE_PESO_ORIGEM .
  data PESO_CHEGADA type ZDE_PESO_CHEGADA .
  data DT_CHEGADA type ZDT_CHEGADA .
  data ZDT_MOV type ZDT_MOV .
  data ZDT_VENCTO type ZDT_VENCTO .
  data ZPESO_DIFERENCA type ZDE_PESO_DIF .
  data ZQUEBRA type ZDE_QUEBRA .
  data ZPERDA type ZDE_PERDA .
  data ZVLR_QUEBRA type ZVLR_QUEBRA .
  data ZVLR_PERDA type ZVLR_PERDA .
  data ZVLR_LIQ_PAGAR type ZVLR_LIQ_PAGAR .
  data ZBVTYP type BVTYP .
  data MATNS type MATNR .
  data ZBASE_ICMS type J_1BBASE .
  data ZBASE_PIS type J_1BBASE .
  data ZBASE_COFINS type J_1BBASE .
  data ZRATE_ICMS type J_1BTXRATE .
  data ZRATE_PIS type J_1BTXRATE .
  data ZRATE_COFINS type J_1BTXRATE .
  data ZVALOR_ICMS type ZDE_VALOR_ICMS .
  data ZVALOR_PIS type ZDE_VALOR_PIS .
  data ZVALOR_COFINS type ZDE_VALOR_COFINS .
  data ZVALOR_PEDAGIO type DMBTR .
  data CK_PESO_CHEGADA type CHAR01 .
  data TIMESTAMP type TIMESTAMP .
  data CK_AUTORIZADO type ZDE_CK_AUTORIZADO .
  data IT_N55 type ZIB_CTE_DIST_N55_T .
  data IT_N01 type ZIB_CTE_DIST_N01_T .
  data IT_NIT type ZIB_CTE_DIST_NIT_T .
  data IT_DUP type ZIB_CTE_DIST_DUP_T .
  data IT_VT type ZDE_CTE_DIST_VT_ALV_T .
  data CK_ALTERADO type CHAR01 .
  data QT_REG_VT type I .
  data QT_REG_NF type I .
  data QT_REG_NI type I .
  data CK_AUTORIZADO_PAGAMENTO type CHAR01 .
  data CK_ENTRADA_MANUAL_PESO type CHAR01 .
  data CK_FATURA_PELA_VT type CHAR01 .

  methods SET_CD_CHAVE_CTE
    importing
      !I_CD_CHAVE_CTE type ZDE_CHAVE_DOC_E .
  methods SET_DOCNUM_CTE
    importing
      !I_DOCNUM_CTE type J_1BDOCNUM .
  methods SET_CK_FINALIZADO
    importing
      !I_CK_FINALIZADO type ZDE_FAGL_FINALIZADO .
  methods SET_CD_STATUS_DIST
    importing
      !I_CD_STATUS_DIST type ZDE_CTE_DIST_ST .
  methods SET_DS_STATUS_DIST
    importing
      !I_DS_STATUS_DIST type TEXT60 .
  methods SET_CD_STATUS_SEFAZ
    importing
      !I_CD_STATUS_SEFAZ type J_1BSTATUSCODE .
  methods SET_NUMR_CTE
    importing
      !I_NUMR_CTE type J_1BNFNUM9 .
  methods SET_NUMR_SERIE
    importing
      !I_NUMR_SERIE type J_1BSERIES .
  methods SET_DT_EMISSAO
    importing
      !I_DT_EMISSAO type J_1BDOCDAT .
  methods SET_HR_EMISSAO
    importing
      !I_HR_EMISSAO type J_1BCRETIM .
  methods SET_VALOR_PRESTACAO
    importing
      !I_VALOR_PRESTACAO type ZDE_VLR15_02 .
  methods SET_VALOR_RECEBER
    importing
      !I_VALOR_RECEBER type ZDE_VLR15_02 .
  methods SET_VALOR_BASE_ICMS
    importing
      !I_VALOR_BASE_ICMS type ZDE_VLR15_02 .
  methods SET_VALOR_ICMS
    importing
      !I_VALOR_ICMS type ZDE_VLR15_02 .
  methods SET_CST_ICMS
    importing
      !I_CST_ICMS type ZDE_CST_ICMS .
  methods SET_CD_TOMADOR
    importing
      !I_CD_TOMADOR type ZCTE_TOMA .
  methods SET_DS_TOMADOR
    importing
      !I_DS_TOMADOR type TEXT60 .
  methods SET_EMIT_TP_DOC
    importing
      !I_EMIT_TP_DOC type ZDE_DOC_PARC .
  methods SET_EMIT_CNPJ
    importing
      !I_EMIT_CNPJ type J_1BCGC .
  methods SET_EMIT_CPF
    importing
      !I_EMIT_CPF type J_1BCPF .
  methods SET_EMIT_IE
    importing
      !I_EMIT_IE type J_1BSTAINS .
  methods SET_EMIT_RSOCIAL
    importing
      !I_EMIT_RSOCIAL type TEXT60 .
  methods SET_EMIT_FANTASIA
    importing
      !I_EMIT_FANTASIA type TEXT60 .
  methods SET_REME_TP_DOC
    importing
      !I_REME_TP_DOC type ZDE_DOC_PARC .
  methods SET_REME_CNPJ
    importing
      !I_REME_CNPJ type J_1BCGC .
  methods SET_REME_CPF
    importing
      !I_REME_CPF type J_1BCPF .
  methods SET_REME_IE
    importing
      !I_REME_IE type J_1BSTAINS .
  methods SET_REME_RSOCIAL
    importing
      !I_REME_RSOCIAL type TEXT60 .
  methods SET_REME_FANTASIA
    importing
      !I_REME_FANTASIA type TEXT60 .
  methods SET_EXPED_TP_DOC
    importing
      !I_EXPED_TP_DOC type ZDE_DOC_PARC .
  methods SET_EXPED_CNPJ
    importing
      !I_EXPED_CNPJ type J_1BCGC .
  methods SET_EXPED_CPF
    importing
      !I_EXPED_CPF type J_1BCPF .
  methods SET_EXPED_IE
    importing
      !I_EXPED_IE type J_1BSTAINS .
  methods SET_EXPED_RSOCIAL
    importing
      !I_EXPED_RSOCIAL type TEXT60 .
  methods SET_EXPED_FANTASIA
    importing
      !I_EXPED_FANTASIA type TEXT60 .
  methods SET_RECEB_TP_DOC
    importing
      !I_RECEB_TP_DOC type ZDE_DOC_PARC .
  methods SET_RECEB_CNPJ
    importing
      !I_RECEB_CNPJ type J_1BCGC .
  methods SET_RECEB_CPF
    importing
      !I_RECEB_CPF type J_1BCPF .
  methods SET_RECEB_IE
    importing
      !I_RECEB_IE type J_1BSTAINS .
  methods SET_RECEB_RSOCIAL
    importing
      !I_RECEB_RSOCIAL type TEXT60 .
  methods SET_RECEB_FANTASIA
    importing
      !I_RECEB_FANTASIA type TEXT60 .
  methods SET_DEST_TP_DOC
    importing
      !I_DEST_TP_DOC type ZDE_DOC_PARC .
  methods SET_DEST_CNPJ
    importing
      !I_DEST_CNPJ type J_1BCGC .
  methods SET_DEST_CPF
    importing
      !I_DEST_CPF type J_1BCPF .
  methods SET_DEST_IE
    importing
      !I_DEST_IE type J_1BSTAINS .
  methods SET_DEST_RSOCIAL
    importing
      !I_DEST_RSOCIAL type TEXT60 .
  methods SET_DEST_FANTASIA
    importing
      !I_DEST_FANTASIA type TEXT60 .
  methods SET_TOMA4_TP_DOC
    importing
      !I_TOMA4_TP_DOC type ZDE_DOC_PARC .
  methods SET_TOMA4_CNPJ
    importing
      !I_TOMA4_CNPJ type J_1BCGC .
  methods SET_TOMA4_CPF
    importing
      !I_TOMA4_CPF type J_1BCPF .
  methods SET_TOMA4_IE
    importing
      !I_TOMA4_IE type J_1BSTAINS .
  methods SET_TOMA4_RSOCIAL
    importing
      !I_TOMA4_RSOCIAL type TEXT60 .
  methods SET_TOMA4_FANTASIA
    importing
      !I_TOMA4_FANTASIA type TEXT60 .
  methods SET_MODELO
    importing
      !I_MODELO type J_1BMODEL .
  methods SET_CD_MODAL
    importing
      !I_CD_MODAL type ZMODAL .
  methods SET_DS_MODAL
    importing
      !I_DS_MODAL type TEXT60 .
  methods SET_CD_TIPO_SERVICO
    importing
      !I_CD_TIPO_SERVICO type ZTPSERV .
  methods SET_DS_TIPO_SERVICO
    importing
      !I_DS_TIPO_SERVICO type TEXT60 .
  methods SET_CD_TIPO_CTE
    importing
      !I_CD_TIPO_CTE type ZTPCTE .
  methods SET_DS_TIPO_CTE
    importing
      !I_DS_TIPO_CTE type TEXT60 .
  methods SET_CD_FPAGAMENTO
    importing
      !I_CD_FPAGAMENTO type ZFORPAG .
  methods SET_DS_FPAGAMENTO
    importing
      !I_DS_FPAGAMENTO type TEXT60 .
  methods SET_CD_FEMISSAO
    importing
      !I_CD_FEMISSAO type ZTFOREM .
  methods SET_DS_FEMISSAO
    importing
      !I_DS_FEMISSAO type TEXT60 .
  methods SET_CD_APLICATIVO
    importing
      !I_CD_APLICATIVO type ZDE_APLIC_EM .
  methods SET_DS_APLICATIVO
    importing
      !I_DS_APLICATIVO type TEXT60 .
  methods SET_CODG_CFOP
    importing
      !I_CODG_CFOP type ZDE_CFOP .
  methods SET_INICIO_IBGE
    importing
      !I_INICIO_IBGE type ZMUNIC_IBGE .
  methods SET_INICIO_MUNI
    importing
      !I_INICIO_MUNI type TEXT60 .
  methods SET_INICIO_UF
    importing
      !I_INICIO_UF type REGIO .
  methods SET_TERMINO_IBGE
    importing
      !I_TERMINO_IBGE type ZMUNIC_IBGE .
  methods SET_TERMINO_MUNI
    importing
      !I_TERMINO_MUNI type TEXT60 .
  methods SET_TERMINO_UF
    importing
      !I_TERMINO_UF type REGIO .
  methods SET_NR_PROTOCOLO
    importing
      !I_NR_PROTOCOLO type J_1BNFEAUTHCODE .
  methods SET_DT_PROTOCOLO
    importing
      !I_DT_PROTOCOLO type J_1BDOCDAT .
  methods SET_HR_PROTOCOLO
    importing
      !I_HR_PROTOCOLO type J_1BCRETIM .
  methods SET_DOCSTA
    importing
      !I_DOCSTA type J_1BNFEDOCSTATUS .
  methods SET_CANCEL
    importing
      !I_CANCEL type J_1BCANCEL .
  methods SET_REGIO
    importing
      !I_REGIO type J_1BREGIO .
  methods SET_NFYEAR
    importing
      !I_NFYEAR type J_1BYEAR .
  methods SET_NFMONTH
    importing
      !I_NFMONTH type J_1BMONTH .
  methods SET_DOCNUM9
    importing
      !I_DOCNUM9 type J_1BDOCNUM9 .
  methods SET_CDV
    importing
      !I_CDV type J_1BCHECKDIGIT .
  methods SET_DS_PROD_PRED
    importing
      !I_DS_PROD_PRED type ZDE_CTE_PROD_P .
  methods SET_QT_CARGA_CTE
    importing
      !I_QT_CARGA_CTE type ZDE_QT_CARGA_CTE .
  methods SET_VL_TOTAL_MERC
    importing
      !I_VL_TOTAL_MERC type ZDE_VLR_TOTAL_MERC_CTE .
  methods SET_RG_LIDO_PAG_FRET
    importing
      !I_RG_LIDO_PAG_FRET type CHAR01 .
  methods SET_TP_PROCESSO_CTE
    importing
      !I_TP_PROCESSO_CTE type ZDE_PROCESSO_CTE .
  methods SET_E_TOMADORA
    importing
      !I_E_TOMADORA type BUKRS .
  methods SET_F_TOMADORA
    importing
      !I_F_TOMADORA type J_1BBRANC_ .
  methods SET_P_EMISSOR
    importing
      !I_P_EMISSOR type LIFNR .
  methods SET_E_EMISSOR
    importing
      !I_E_EMISSOR type BUKRS .
  methods SET_F_EMISSOR
    importing
      !I_F_EMISSOR type J_1BBRANC_ .
  methods SET_DOCNUM_CTE_C
    importing
      !I_DOCNUM_CTE_C type ZDE_DOCNUM_C .
  methods SET_DOCNUM_CTE_A
    importing
      !I_DOCNUM_CTE_A type ZDE_DOCNUM_A .
  methods SET_DOCNUM_CTE_S
    importing
      !I_DOCNUM_CTE_S type ZDE_DOCNUM_S .
  methods SET_EBELN
    importing
      !I_EBELN type EBELN .
  methods SET_EBELP
    importing
      !I_EBELP type EBELP .
  methods SET_BELNR
    importing
      !I_BELNR type RE_BELNR .
  methods SET_GJAHR
    importing
      !I_GJAHR type GJAHR .
  methods SET_DOCNUM_CTE_SUB
    importing
      !I_DOCNUM_CTE_SUB type J_1BDOCNUM .
  methods SET_DOCNUM_CTE_P
    importing
      !I_DOCNUM_CTE_P type J_1BDOCNUM .
  methods SET_CK_MANUAL
    importing
      !I_CK_MANUAL type CHAR01 .
  methods SET_WAERK_VI
    importing
      !I_WAERK_VI type WAERK .
  methods SET_KURSK_VI
    importing
      !I_KURSK_VI type KURSK .
  methods SET_ZVLR_VI
    importing
      !I_ZVLR_VI type ZDE_VLR_VI .
  methods SET_ZVLR_FRETE
    importing
      !I_ZVLR_FRETE type ZDE_VLR_FRETE .
  methods SET_ZVLR_MERCADORIA
    importing
      !I_ZVLR_MERCADORIA type ZDE_VLR_MERC .
  methods SET_PESO_ORIGEM
    importing
      !I_PESO_ORIGEM type ZDE_PESO_ORIGEM .
  methods SET_PESO_CHEGADA
    importing
      !I_PESO_CHEGADA type ZDE_PESO_CHEGADA .
  methods SET_ZPESO_DIFERENCA
    importing
      !I_ZPESO_DIFERENCA type ZDE_PESO_DIF .
  methods SET_ZQUEBRA
    importing
      !I_ZQUEBRA type ZDE_QUEBRA .
  methods SET_ZPERDA
    importing
      !I_ZPERDA type ZDE_PERDA .
  methods SET_ZVLR_QUEBRA
    importing
      !I_ZVLR_QUEBRA type ZVLR_QUEBRA .
  methods SET_ZVLR_PERDA
    importing
      !I_ZVLR_PERDA type ZVLR_PERDA .
  methods SET_ZVLR_LIQ_PAGAR
    importing
      !I_ZVLR_LIQ_PAGAR type ZVLR_LIQ_PAGAR .
  methods SET_MATNS
    importing
      !I_MATNS type MATNR .
  methods SET_ZBASE_ICMS
    importing
      !I_ZBASE_ICMS type J_1BBASE .
  methods SET_ZBASE_PIS
    importing
      !I_ZBASE_PIS type J_1BBASE .
  methods SET_ZBASE_COFINS
    importing
      !I_ZBASE_COFINS type J_1BBASE .
  methods SET_ZRATE_ICMS
    importing
      !I_ZRATE_ICMS type J_1BTXRATE .
  methods SET_ZRATE_PIS
    importing
      !I_ZRATE_PIS type J_1BTXRATE .
  methods SET_ZRATE_COFINS
    importing
      !I_ZRATE_COFINS type J_1BTXRATE .
  methods SET_ZVALOR_ICMS
    importing
      !I_ZVALOR_ICMS type ZDE_VALOR_ICMS .
  methods SET_ZVALOR_PIS
    importing
      !I_ZVALOR_PIS type ZDE_VALOR_PIS .
  methods SET_ZVALOR_COFINS
    importing
      !I_ZVALOR_COFINS type ZDE_VALOR_COFINS .
  methods SET_ZVALOR_PEDAGIO
    importing
      !I_ZVALOR_PEDAGIO type DMBTR .
  methods SET_CK_PESO_CHEGADA
    importing
      !I_CK_PESO_CHEGADA type CHAR01 .
  methods SET_TIMESTAMP
    importing
      !I_TIMESTAMP type TIMESTAMP .
  methods SET_CK_AUTORIZADO
    importing
      !I_CK_AUTORIZADO type ZDE_CK_AUTORIZADO .
  methods SET_CK_AUTORIZADO_PAGAMENTO
    importing
      !I_CK_AUTORIZADO_PAGAMENTO type CHAR01 .
  methods SET_CK_ENTRADA_MANUAL_PESO
    importing
      !I_CK_ENTRADA_MANUAL_PESO type CHAR01 .
  methods SET_TOTALIZA_VTS .
  methods SET_CK_FATURA_PELA_VT
    importing
      !I_CK_FATURA_PELA_VT type CHAR01 .
ENDCLASS.



CLASS ZCL_CTE_PAGAMENTO IMPLEMENTATION.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_CADASTRO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_CADASTRO=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_CADASTRO=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_CADASTRO=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_CADASTRO=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_CK_ALTERADO.
    E_CK_ALTERADO = ME->CK_ALTERADO.
  ENDMETHOD.


  METHOD GET_CK_AUTORIZADO_PAGAMENTO.
    E_CK_AUTORIZADO_PAGAMENTO = ME->CK_AUTORIZADO_PAGAMENTO.
  ENDMETHOD.


  METHOD GET_CK_ENTRADA_MANUAL_PESO.
    E_CK_ENTRADA_MANUAL_PESO = ME->CK_ENTRADA_MANUAL_PESO.
  ENDMETHOD.


  METHOD GET_CK_FATURA_PELA_VT.
    E_CK_FATURA_PELA_VT = ME->CK_FATURA_PELA_VT.
  ENDMETHOD.


  METHOD GET_IT_DUP.
    E_IT_DUP = ME->IT_DUP.
  ENDMETHOD.


  METHOD GET_IT_N01.
    E_IT_N01 = ME->IT_N01.
  ENDMETHOD.


  METHOD GET_IT_N55.
    E_IT_N55 = ME->IT_N55.
  ENDMETHOD.


  METHOD GET_IT_NIT.
    E_IT_NIT = ME->IT_NIT.
  ENDMETHOD.


  METHOD GET_IT_VT.
    E_IT_VT = ME->IT_VT.
  ENDMETHOD.


  METHOD GET_QT_REG_NF.

    DATA: WA_N55 TYPE ZIB_CTE_DIST_N55,
          WA_N01 TYPE ZIB_CTE_DIST_N01.

    E_QT_REG_NF = 0.
    LOOP AT ME->IT_N55 INTO WA_N55 WHERE TKNUM EQ I_TKNUM.
      ADD 1 TO E_QT_REG_NF.
    ENDLOOP.
    LOOP AT ME->IT_N01 INTO WA_N01 WHERE TKNUM EQ I_TKNUM.
      ADD 1 TO E_QT_REG_NF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_QT_REG_NI.

    DATA: WA_NIT TYPE ZIB_CTE_DIST_NIT.

    E_QT_REG_NI = 0.
    LOOP AT ME->IT_NIT INTO WA_NIT WHERE DOCNUM EQ I_DOCNUM.
      ADD 1 TO E_QT_REG_NI.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_QT_REG_VT.
    E_QT_REG_VT = ME->QT_REG_VT.
  ENDMETHOD.


  METHOD SET_BELNR.
    ME->BELNR = I_BELNR.
  ENDMETHOD.


  METHOD SET_CANCEL.
    ME->CANCEL = I_CANCEL.
  ENDMETHOD.


  METHOD SET_CDV.
    ME->CDV = I_CDV.
  ENDMETHOD.


  METHOD SET_CD_APLICATIVO.
    ME->CD_APLICATIVO = I_CD_APLICATIVO.
  ENDMETHOD.


  METHOD SET_CD_CHAVE_CTE.
    ME->CD_CHAVE_CTE = I_CD_CHAVE_CTE.
  ENDMETHOD.


  METHOD SET_CD_FEMISSAO.
    ME->CD_FEMISSAO = I_CD_FEMISSAO.
  ENDMETHOD.


  METHOD SET_CD_FPAGAMENTO.
    ME->CD_FPAGAMENTO = I_CD_FPAGAMENTO.
  ENDMETHOD.


  METHOD SET_CD_MODAL.
    ME->CD_MODAL = I_CD_MODAL.
  ENDMETHOD.


  METHOD SET_CD_STATUS_DIST.
    ME->CD_STATUS_DIST = I_CD_STATUS_DIST.
  ENDMETHOD.


  METHOD SET_CD_STATUS_SEFAZ.
    ME->CD_STATUS_SEFAZ = I_CD_STATUS_SEFAZ.
  ENDMETHOD.


  METHOD SET_CD_TIPO_CTE.
    ME->CD_TIPO_CTE = I_CD_TIPO_CTE.
  ENDMETHOD.


  METHOD SET_CD_TIPO_SERVICO.
    ME->CD_TIPO_SERVICO = I_CD_TIPO_SERVICO.
  ENDMETHOD.


  METHOD SET_CD_TOMADOR.
    ME->CD_TOMADOR = I_CD_TOMADOR.
  ENDMETHOD.


  METHOD SET_CK_AUTORIZADO.
    ME->CK_AUTORIZADO = I_CK_AUTORIZADO.
  ENDMETHOD.


  METHOD SET_CK_AUTORIZADO_PAGAMENTO.
    ME->CK_AUTORIZADO_PAGAMENTO = I_CK_AUTORIZADO_PAGAMENTO.
  ENDMETHOD.


  METHOD SET_CK_ENTRADA_MANUAL_PESO.
    ME->CK_ENTRADA_MANUAL_PESO = I_CK_ENTRADA_MANUAL_PESO.
  ENDMETHOD.


  METHOD SET_CK_FATURA_PELA_VT.
    ME->CK_FATURA_PELA_VT = I_CK_FATURA_PELA_VT.
  ENDMETHOD.


  METHOD SET_CK_FINALIZADO.
    ME->CK_FINALIZADO = I_CK_FINALIZADO.
  ENDMETHOD.


  METHOD SET_CK_MANUAL.
    ME->CK_MANUAL = I_CK_MANUAL.
  ENDMETHOD.


  METHOD SET_CK_PESO_CHEGADA.
    ME->CK_PESO_CHEGADA = I_CK_PESO_CHEGADA.
  ENDMETHOD.


  METHOD SET_CODG_CFOP.
    ME->CODG_CFOP = I_CODG_CFOP.
  ENDMETHOD.


  METHOD SET_CST_ICMS.
    ME->CST_ICMS = I_CST_ICMS.
  ENDMETHOD.


  METHOD SET_DEST_CNPJ.
    ME->DEST_CNPJ = I_DEST_CNPJ.
  ENDMETHOD.


  METHOD SET_DEST_CPF.
    ME->DEST_CPF = I_DEST_CPF.
  ENDMETHOD.


  METHOD SET_DEST_FANTASIA.
    ME->DEST_FANTASIA = I_DEST_FANTASIA.
  ENDMETHOD.


  METHOD SET_DEST_IE.
    ME->DEST_IE = I_DEST_IE.
  ENDMETHOD.


  METHOD SET_DEST_RSOCIAL.
    ME->DEST_RSOCIAL = I_DEST_RSOCIAL.
  ENDMETHOD.


  METHOD SET_DEST_TP_DOC.
    ME->DEST_TP_DOC = I_DEST_TP_DOC.
  ENDMETHOD.


  METHOD SET_DOCNUM9.
    ME->DOCNUM9 = I_DOCNUM9.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE.
    ME->DOCNUM_CTE = I_DOCNUM_CTE.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE_A.
    ME->DOCNUM_CTE_A = I_DOCNUM_CTE_A.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE_C.
    ME->DOCNUM_CTE_C = I_DOCNUM_CTE_C.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE_P.
    ME->DOCNUM_CTE_P = I_DOCNUM_CTE_P.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE_S.
    ME->DOCNUM_CTE_S = I_DOCNUM_CTE_S.
  ENDMETHOD.


  METHOD SET_DOCNUM_CTE_SUB.
    ME->DOCNUM_CTE_SUB = I_DOCNUM_CTE_SUB.
  ENDMETHOD.


  METHOD SET_DOCSTA.
    ME->DOCSTA = I_DOCSTA.
  ENDMETHOD.


  METHOD SET_DS_APLICATIVO.
    ME->DS_APLICATIVO = I_DS_APLICATIVO.
  ENDMETHOD.


  METHOD SET_DS_FEMISSAO.
    ME->DS_FEMISSAO = I_DS_FEMISSAO.
  ENDMETHOD.


  METHOD SET_DS_FPAGAMENTO.
    ME->DS_FPAGAMENTO = I_DS_FPAGAMENTO.
  ENDMETHOD.


  METHOD SET_DS_MODAL.
    ME->DS_MODAL = I_DS_MODAL.
  ENDMETHOD.


  METHOD SET_DS_PROD_PRED.
    ME->DS_PROD_PRED  = I_DS_PROD_PRED .
  ENDMETHOD.


  METHOD SET_DS_STATUS_DIST.
    ME->DS_STATUS_DIST = I_DS_STATUS_DIST.
  ENDMETHOD.


  METHOD SET_DS_TIPO_CTE.
    ME->DS_TIPO_CTE = I_DS_TIPO_CTE.
  ENDMETHOD.


  METHOD SET_DS_TIPO_SERVICO.
    ME->DS_TIPO_SERVICO = I_DS_TIPO_SERVICO.
  ENDMETHOD.


  METHOD SET_DS_TOMADOR.
    ME->DS_TOMADOR = I_DS_TOMADOR.
  ENDMETHOD.


  METHOD SET_DT_CHEGADA.
    IF ME->DT_CHEGADA NE I_DT_CHEGADA.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.

    ME->DT_CHEGADA = I_DT_CHEGADA.
  ENDMETHOD.


  METHOD SET_DT_EMISSAO.
    ME->DT_EMISSAO  = I_DT_EMISSAO .
  ENDMETHOD.


  METHOD SET_DT_PROTOCOLO.
    ME->DT_PROTOCOLO = I_DT_PROTOCOLO.
  ENDMETHOD.


  METHOD SET_EBELN.
    ME->EBELN = I_EBELN.
  ENDMETHOD.


  METHOD SET_EBELP.
    ME->EBELP = I_EBELP.
  ENDMETHOD.


  METHOD SET_EMIT_CNPJ.
    ME->EMIT_CNPJ = I_EMIT_CNPJ.
  ENDMETHOD.


  METHOD SET_EMIT_CPF.
    ME->EMIT_CPF = I_EMIT_CPF.
  ENDMETHOD.


  METHOD SET_EMIT_FANTASIA.
    ME->EMIT_FANTASIA = I_EMIT_FANTASIA.
  ENDMETHOD.


  METHOD SET_EMIT_IE.
    ME->EMIT_IE = I_EMIT_IE.
  ENDMETHOD.


  METHOD SET_EMIT_RSOCIAL.
    ME->EMIT_RSOCIAL = I_EMIT_RSOCIAL.
  ENDMETHOD.


  METHOD SET_EMIT_TP_DOC.
    ME->EMIT_TP_DOC = I_EMIT_TP_DOC.
  ENDMETHOD.


  METHOD SET_EXPED_CNPJ.
    ME->EXPED_CNPJ = I_EXPED_CNPJ.
  ENDMETHOD.


  METHOD SET_EXPED_CPF.
    ME->EXPED_CPF = I_EXPED_CPF.
  ENDMETHOD.


  METHOD SET_EXPED_FANTASIA.
    ME->EXPED_FANTASIA = I_EXPED_FANTASIA.
  ENDMETHOD.


  METHOD SET_EXPED_IE.
    ME->EXPED_IE = I_EXPED_IE.
  ENDMETHOD.


  METHOD SET_EXPED_RSOCIAL.
    ME->EXPED_RSOCIAL = I_EXPED_RSOCIAL.
  ENDMETHOD.


  METHOD SET_EXPED_TP_DOC.
    ME->EXPED_TP_DOC  = I_EXPED_TP_DOC .
  ENDMETHOD.


  METHOD SET_E_EMISSOR.
    ME->E_EMISSOR = I_E_EMISSOR.
  ENDMETHOD.


  METHOD SET_E_TOMADORA.
    ME->E_TOMADORA = I_E_TOMADORA.
  ENDMETHOD.


  METHOD SET_F_EMISSOR.
    ME->F_EMISSOR = I_F_EMISSOR.
  ENDMETHOD.


  METHOD SET_F_TOMADORA.
    ME->F_TOMADORA = I_F_TOMADORA.
  ENDMETHOD.


  METHOD SET_GJAHR.
    ME->GJAHR = I_GJAHR.
  ENDMETHOD.


  METHOD SET_HR_EMISSAO.
    ME->HR_EMISSAO = I_HR_EMISSAO.
  ENDMETHOD.


  METHOD SET_HR_PROTOCOLO.
    ME->HR_PROTOCOLO = I_HR_PROTOCOLO.
  ENDMETHOD.


  METHOD SET_INFO_FATURAR_CTE.

    DATA: LC_VOLUME_NOTAS     TYPE J_1BNETQTY,
          LC_VALOR_NOTAS      TYPE J_1BNETVAL,
          LC_VOLUME_VT        TYPE J_1BNETQTY,
          LC_VALOR_NOTAS_VT   TYPE J_1BNETVAL,
          LC_VOLUME_NOTA      TYPE J_1BNETQTY,
          LC_VALOR_NOTA       TYPE J_1BNETVAL,
          LC_VOLUME_NOTA_ITEM TYPE J_1BNETQTY,
          LC_VALOR_NOTA_ITEM  TYPE J_1BNETVAL,
          LC_VALOR_VT         TYPE NETWR_P,
          WA_VFKP             TYPE VFKP,
          LC_VALOR_FRETE      TYPE ZDE_VLR15_02,
          QT_LINHAS_VT        TYPE I,
          WA_J_1BNFLIN        TYPE J_1BNFLIN,
          WA_DUP              TYPE ZIB_CTE_DIST_DUP,
          WA_N55              TYPE ZIB_CTE_DIST_N55,
          WA_N01              TYPE ZIB_CTE_DIST_N01,
          WA_NIT              TYPE ZIB_CTE_DIST_NIT,
          WA_VT               TYPE ZDE_CTE_DIST_VT_ALV,
          WA_0039             TYPE ZLEST0039.

    FIELD-SYMBOLS: <FS_VT>  TYPE ZDE_CTE_DIST_VT_ALV,
                   <FS_N55> TYPE ZIB_CTE_DIST_N55,
                   <FS_N01> TYPE ZIB_CTE_DIST_N01,
                   <FS_NIT> TYPE ZIB_CTE_DIST_NIT.

    DATA: OBJ_COTACAO TYPE REF TO ZCL_UTIL_SD,
          LC_DATA     TYPE GDATU_INV.


    CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
      EXPORTING
        CHAVE          = I_CD_CHAVE_CTE
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING BLOQUEADO_USUARIO.
    ENDIF.

    CREATE OBJECT R_CTE.
    R_CTE->ZIF_CADASTRO~SET_REGISTRO( EXPORTING I_ID_REGISTRO = I_CD_CHAVE_CTE ).

    IF R_CTE->CK_FINALIZADO EQ ABAP_FALSE.

      R_CTE->SET_ZDT_MOV( EXPORTING I_ZDT_MOV = SY-DATUM ).

      IF R_CTE->CD_MODAL = '04' AND R_CTE->DT_CHEGADA IS INITIAL.
        R_CTE->DT_CHEGADA = R_CTE->DT_EMISSAO.
      ELSEIF R_CTE->CD_MODAL = '03' AND R_CTE->DT_CHEGADA IS INITIAL AND R_CTE->TP_PROCESSO_CTE EQ ZCL_CTE_DIST_G=>TIPO_03.
        "Frete Aguaviário com Emissão Propria
        SELECT SINGLE * INTO @DATA(WA_ZLEST0061)
          FROM ZLEST0061
         WHERE DOCNUM EQ @R_CTE->DOCNUM_CTE_SUB.
        IF SY-SUBRC IS INITIAL.
          R_CTE->DT_CHEGADA = WA_ZLEST0061-DT_CHEGADA.
        ENDIF.
      ENDIF.

      IF R_CTE->ZDT_VENCTO IS INITIAL.
        READ TABLE R_CTE->IT_DUP INDEX 1 INTO WA_DUP.
        IF SY-SUBRC IS INITIAL.
          R_CTE->ZDT_VENCTO = WA_DUP-DT_VENCIMENTO.
        ENDIF.
      ENDIF.

      IF I_DT_VENCIMENTO IS NOT INITIAL.
        R_CTE->ZDT_VENCTO = I_DT_VENCIMENTO.
      ELSEIF I_DT_VENCIMENTO IS INITIAL AND R_CTE->ZDT_VENCTO IS INITIAL.
        CALL METHOD ZCL_CTE_DIST_G=>BUSCA_PROXIMO_VENC_FATURA
          IMPORTING
            E_DATA_VENCIMENTO = I_DT_VENCIMENTO.
        R_CTE->ZDT_VENCTO = I_DT_VENCIMENTO.
      ENDIF.

      IF R_CTE->ZBVTYP IS INITIAL.
        R_CTE->SET_ZBVTYP( EXPORTING I_ZBVTYP = '0001').
      ENDIF.

      "0  CT-e Normal
      "1  CT-e de Complemento de Valores
      "2  CT-e de Anulação de Valores
      "3  CT-e Substituto

      CASE R_CTE->CD_TIPO_CTE.
        WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

          CASE R_CTE->CD_MODAL.
            WHEN '01'. "Rodoviário

              ZCL_CTE_DIST_G=>GET_VOLUME_VALOR_NOTA(
                EXPORTING
                  I_N55T          = R_CTE->IT_N55    " Nº documento
                  I_N01T          = R_CTE->IT_N01    " Tabela de InBound de CT-e Distribuida - NF01
                IMPORTING
                  E_J_1BNFLIN_TAB = DATA(IT_J_1BNFLIN)    " Quantidade
              ).

*              "Buscar Notas Fiscais """""""""""""""""""""
*              IF R_CTE->IT_N55[] IS NOT INITIAL.
*                SELECT * INTO TABLE IT_J_1BNFLIN
*                  FROM J_1BNFLIN
*                   FOR ALL ENTRIES IN R_CTE->IT_N55
*                 WHERE DOCNUM EQ R_CTE->IT_N55-DOCNUM_NFE.
*              ENDIF.
*
*              IF R_CTE->IT_N01[] IS NOT INITIAL.
*                SELECT * APPENDING TABLE IT_J_1BNFLIN
*                  FROM J_1BNFLIN
*                   FOR ALL ENTRIES IN R_CTE->IT_N01
*                 WHERE DOCNUM EQ R_CTE->IT_N01-DOCNUM_NF.
*              ENDIF.

              LC_VOLUME_NOTAS = 0.
              LC_VALOR_NOTAS  = 0.

              "Somar Peso Bruto Remessa/Aviso Recebimento """"""""""""""""""""""""""""""""
              " Somente KG
*              DATA(IT_N55_AUX) = R_CTE->IT_N55[].
*              DATA(IT_N01_AUX) = R_CTE->IT_N01[].
*              DELETE IT_N55_AUX WHERE VBELN_VL IS INITIAL.
*              DELETE IT_N01_AUX WHERE VBELN_VL IS INITIAL.
*              IF IT_N55_AUX[] IS NOT INITIAL.
*                SELECT * INTO TABLE @DATA(IT_LIPS)
*                  FROM LIPS
*                   FOR ALL ENTRIES IN @IT_N55_AUX
*                 WHERE VBELN EQ @IT_N55_AUX-VBELN_VL
*                   AND GEWEI EQ 'KG'.
*              ENDIF.
*              IF IT_N01_AUX[] IS NOT INITIAL.
*                SELECT * APPENDING TABLE @IT_LIPS
*                  FROM LIPS
*                   FOR ALL ENTRIES IN @IT_N01_AUX
*                 WHERE VBELN EQ @IT_N01_AUX-VBELN_VL
*                   AND GEWEI EQ 'KG'.
*              ENDIF.
*
*              LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN.
*
*                DATA(IT_LIPS_AUX) = IT_LIPS[].
*
*                READ TABLE R_CTE->IT_N55 INTO WA_N55 WITH KEY DOCNUM_NFE = WA_J_1BNFLIN-DOCNUM.
*                IF SY-SUBRC IS INITIAL AND WA_N55-VBELN_VL IS NOT INITIAL.
*                  DELETE IT_LIPS_AUX WHERE VBELN NE WA_N55-VBELN_VL.
*                  DESCRIBE TABLE IT_LIPS_AUX LINES DATA(QTD_ITENS_REMESSAS).
*
*                  IF QTD_ITENS_REMESSAS EQ 1.
*                    READ TABLE IT_LIPS WITH KEY VBELN = WA_N55-VBELN_VL INTO DATA(WA_LIPS).
*                    IF SY-SUBRC IS INITIAL.
*                      ADD WA_LIPS-BRGEW TO LC_VOLUME_NOTAS.
*                    ELSE.
*                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
*                    ENDIF.
*                  ELSE.
*                    ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
*                  ENDIF.
*                ELSE.
*                  READ TABLE R_CTE->IT_N01 INTO WA_N01 WITH KEY DOCNUM_NF = WA_J_1BNFLIN-DOCNUM.
*                  IF SY-SUBRC IS INITIAL AND WA_N01-VBELN_VL IS NOT INITIAL.
*                    DELETE IT_LIPS_AUX WHERE VBELN NE WA_N01-VBELN_VL.
*                    DESCRIBE TABLE IT_LIPS_AUX LINES QTD_ITENS_REMESSAS.
*
*                    IF QTD_ITENS_REMESSAS EQ 1.
*                      READ TABLE IT_LIPS WITH KEY VBELN = WA_N01-VBELN_VL INTO WA_LIPS.
*                      IF SY-SUBRC IS INITIAL.
*                        ADD WA_LIPS-BRGEW TO LC_VOLUME_NOTAS.
*                      ELSE.
*                        ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
*                      ENDIF.
*                    ELSE.
*                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
*                    ENDIF.
*                  ELSE.
*                    ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
*                  ENDIF.
*                ENDIF.
*
*                ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS .
*
*              ENDLOOP.
              """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN.
                ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTAS.
                ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS .
              ENDLOOP.

              "Agrupar VT's """""""""""""""""""""""""""""
              LOOP AT R_CTE->IT_N55 INTO WA_N55.
                MOVE-CORRESPONDING WA_N55 TO WA_VT.
                APPEND WA_VT TO R_CTE->IT_VT.
              ENDLOOP.

              LOOP AT R_CTE->IT_N01 INTO WA_N01.
                MOVE-CORRESPONDING WA_N01 TO WA_VT.
                APPEND WA_VT TO R_CTE->IT_VT.
              ENDLOOP.

              SORT R_CTE->IT_VT BY TKNUM.
              DELETE ADJACENT DUPLICATES FROM R_CTE->IT_VT COMPARING TKNUM.
              IF R_CTE->CK_FATURA_PELA_VT EQ ABAP_FALSE.

                LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.
                  "Volume total da VT """"""""""""""""""""""""""""""""""""""""
                  LC_VOLUME_VT        = 0.
                  LC_VALOR_NOTAS_VT   = 0.
                  LC_VALOR_VT         = 0.
                  <FS_VT>-ZVLR_VI     = 0.
                  <FS_VT>-ZVLR_FRETE  = 0.
                  <FS_VT>-PESO_ORIGEM = 0.

                  IF <FS_VT>-FKNUM IS NOT INITIAL.

                    SELECT SINGLE * INTO WA_VFKP
                      FROM VFKP
                    WHERE FKNUM  EQ <FS_VT>-FKNUM
                       AND NETWR NE 0.

                    IF SY-SUBRC IS INITIAL.
                      SELECT SUM( NETWR ) INTO WA_VFKP-NETWR
                         FROM VFKP
                        WHERE FKNUM EQ <FS_VT>-FKNUM
                          AND NETWR NE 0.

                      R_CTE->WAERK_VI = WA_VFKP-WAERS.

                      CASE WA_VFKP-WAERS.
                        WHEN 'BRL'.
                          ADD WA_VFKP-NETWR  TO LC_VALOR_VT.

                        WHEN OTHERS.
                          TRY .
                              CREATE OBJECT OBJ_COTACAO.
                              LC_DATA = WA_VFKP-PRSDT.
                              IF LC_DATA IS NOT INITIAL.
                                OBJ_COTACAO->SET_DATA(  EXPORTING I_DATA = LC_DATA ).
                                OBJ_COTACAO->SET_KURST( EXPORTING I_KURST = WA_VFKP-KURST ).
                                OBJ_COTACAO->SET_WAERK( EXPORTING I_WAERK = WA_VFKP-WAERS ).
                                OBJ_COTACAO->SET_TCURR( EXPORTING I_TCURR = 'BRL' ).
                                DATA(LC_UKURS) = OBJ_COTACAO->TAXA_CAMBIO( ).
                                LC_UKURS = ABS( LC_UKURS ).
                              ELSE.
                                CLEAR: LC_UKURS.
                              ENDIF.
                            CATCH CX_ROOT.
                              CLEAR: LC_UKURS.
                          ENDTRY.
                          CLEAR: OBJ_COTACAO.

                          R_CTE->WAERK_VI = WA_VFKP-WAERS.
                          R_CTE->KURSK_VI = LC_UKURS.
                          ADD WA_VFKP-NETWR  TO LC_VALOR_VT.
                      ENDCASE.

                    ENDIF.
                  ENDIF.

                  LOOP AT R_CTE->IT_N55 INTO WA_N55 WHERE TKNUM EQ <FS_VT>-TKNUM.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ WA_N55-DOCNUM_NFE.
                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_VT.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS_VT.
                    ENDLOOP.
                  ENDLOOP.

                  LOOP AT R_CTE->IT_N01 INTO WA_N01 WHERE TKNUM EQ <FS_VT>-TKNUM.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ WA_N01-DOCNUM_NF.
                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_VT.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS_VT.
                    ENDLOOP.
                  ENDLOOP.

                  <FS_VT>-ZVLR_MERCADORIA = LC_VALOR_NOTAS_VT.
                  <FS_VT>-ZVLR_VI         = LC_VALOR_VT.
                  <FS_VT>-ZVLR_FRETE      = LC_VOLUME_VT / LC_VOLUME_NOTAS * R_CTE->VALOR_RECEBER.
                  <FS_VT>-PESO_ORIGEM     = LC_VOLUME_NOTAS.

                  LOOP AT R_CTE->IT_N55 ASSIGNING <FS_N55>.
                    LC_VOLUME_NOTA = 0.
                    LC_VALOR_NOTA  = 0.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTA.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTA.
                    ENDLOOP.
                    "Valor da VI por Nota Fiscal
                    <FS_N55>-ZVLR_VI         = ( LC_VOLUME_NOTA / LC_VOLUME_VT ) * LC_VALOR_VT.
                    <FS_N55>-ZVLR_FRETE      = ( LC_VOLUME_NOTA / LC_VOLUME_VT ) * <FS_VT>-ZVLR_FRETE.
                    <FS_N55>-ZVLR_MERCADORIA = LC_VALOR_NOTA.
                    <FS_N55>-PESO_ORIGEM     = LC_VOLUME_NOTA.
                    IF <FS_N55>-PESO_ORIGEM IS NOT INITIAL.
                      <FS_N55>-ZVLR_KG_TRANSP   = <FS_N55>-ZVLR_FRETE / <FS_N55>-PESO_ORIGEM.
                      <FS_N55>-ZVLR_KG_MERCAD   = <FS_N55>-ZVLR_MERCADORIA / <FS_N55>-PESO_ORIGEM.
                    ENDIF.

                    LOOP AT R_CTE->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
                      LC_VOLUME_NOTA_ITEM = 0.
                      LC_VALOR_NOTA_ITEM  = 0.

                      LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ <FS_NIT>-DOCNUM AND ITMNUM EQ <FS_NIT>-ITMNUM.
                        ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTA_ITEM.
                        ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTA_ITEM.
                        <FS_NIT>-ZMATNR_MERC = WA_J_1BNFLIN-MATNR.
                        <FS_VT>-ZMATNR_MERC  = WA_J_1BNFLIN-MATNR.
                      ENDLOOP.

                      "Valor da VI por Item de Nota Fiscal
                      <FS_NIT>-ZVLR_VI         = ( LC_VOLUME_NOTA_ITEM / LC_VOLUME_NOTA ) * <FS_N55>-ZVLR_VI.
                      <FS_NIT>-ZVLR_FRETE      = ( LC_VOLUME_NOTA_ITEM / LC_VOLUME_NOTA ) * <FS_N55>-ZVLR_FRETE.
                      <FS_NIT>-ZVLR_MERCADORIA = LC_VALOR_NOTA_ITEM.
                      <FS_NIT>-PESO_ORIGEM     = LC_VOLUME_NOTA_ITEM.
                      <FS_VT>-CK_AUTORIZADO    = <FS_NIT>-CK_AUTORIZADO.
                      <FS_VT>-PC_QUEBRA        = <FS_NIT>-PC_QUEBRA.
                      <FS_VT>-PC_TOLERANCIA    = <FS_NIT>-PC_TOLERANCIA.
                      IF <FS_NIT>-PESO_ORIGEM IS NOT INITIAL.
                        <FS_NIT>-ZVLR_KG_TRANSP   = <FS_NIT>-ZVLR_FRETE / <FS_NIT>-PESO_ORIGEM.
                        <FS_NIT>-ZVLR_KG_MERCAD   = <FS_NIT>-ZVLR_MERCADORIA / <FS_NIT>-PESO_ORIGEM.
                      ENDIF.
                    ENDLOOP.
                    <FS_VT>-AUART_VA = <FS_N55>-AUART_VA.
                  ENDLOOP.

                  LOOP AT R_CTE->IT_N01 ASSIGNING <FS_N01>.
                    LC_VOLUME_NOTA = 0.
                    LC_VALOR_NOTA  = 0.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF.
                      ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTA.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTA.
                    ENDLOOP.
                    "Valor da VI por Nota Fiscal
                    <FS_N01>-ZVLR_VI         = ( LC_VOLUME_NOTA / LC_VOLUME_VT ) * LC_VALOR_VT.
                    <FS_N01>-ZVLR_FRETE      = ( LC_VOLUME_NOTA / LC_VOLUME_VT ) * <FS_VT>-ZVLR_FRETE.
                    <FS_N01>-ZVLR_MERCADORIA = LC_VALOR_NOTA.
                    <FS_N01>-PESO_ORIGEM     = LC_VOLUME_NOTA.
                    IF <FS_N01>-PESO_ORIGEM IS NOT INITIAL.
                      <FS_N01>-ZVLR_KG_TRANSP   = <FS_N01>-ZVLR_FRETE / <FS_N01>-PESO_ORIGEM.
                      <FS_N01>-ZVLR_KG_MERCAD   = <FS_N01>-ZVLR_MERCADORIA / <FS_N01>-PESO_ORIGEM.
                    ENDIF.
                    LOOP AT R_CTE->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF.
                      LC_VOLUME_NOTA_ITEM = 0.
                      LC_VALOR_NOTA_ITEM  = 0.
                      LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ <FS_NIT>-DOCNUM AND ITMNUM EQ <FS_NIT>-ITMNUM.
                        ADD WA_J_1BNFLIN-MENGE TO LC_VOLUME_NOTA_ITEM.
                        ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTA_ITEM.
                        <FS_NIT>-ZMATNR_MERC = WA_J_1BNFLIN-MATNR.
                        <FS_VT>-ZMATNR_MERC  = WA_J_1BNFLIN-MATNR.
                      ENDLOOP.
                      "Valor da VI por Item de Nota Fiscal
                      <FS_NIT>-ZVLR_VI         = ( LC_VOLUME_NOTA_ITEM / LC_VOLUME_NOTA ) * <FS_N01>-ZVLR_VI.
                      <FS_NIT>-ZVLR_FRETE      = ( LC_VOLUME_NOTA_ITEM / LC_VOLUME_NOTA ) * <FS_N01>-ZVLR_FRETE.
                      <FS_NIT>-ZVLR_MERCADORIA = LC_VALOR_NOTA_ITEM.
                      <FS_NIT>-PESO_ORIGEM     = LC_VOLUME_NOTA_ITEM.
                      <FS_VT>-CK_AUTORIZADO    = <FS_NIT>-CK_AUTORIZADO.
                      <FS_VT>-PC_QUEBRA        = <FS_NIT>-PC_QUEBRA.
                      <FS_VT>-PC_TOLERANCIA    = <FS_NIT>-PC_TOLERANCIA.
                      IF <FS_NIT>-PESO_ORIGEM IS NOT INITIAL.
                        <FS_NIT>-ZVLR_KG_TRANSP   = <FS_NIT>-ZVLR_FRETE / <FS_NIT>-PESO_ORIGEM.
                        <FS_NIT>-ZVLR_KG_MERCAD   = <FS_NIT>-ZVLR_MERCADORIA / <FS_NIT>-PESO_ORIGEM.
                      ENDIF.
                    ENDLOOP.
                    <FS_VT>-AUART_VA         = <FS_N01>-AUART_VA.
                  ENDLOOP.
                  <FS_VT>-CK_PESO_DIGITADO = ABAP_FALSE.
                  <FS_VT>-IC_EDITAR        = ICON_CHANGE_NUMBER.

                  IF <FS_VT>-PESO_CHEGADA IS INITIAL AND R_CTE->CD_MODAL = '04'.
                    <FS_VT>-PESO_CHEGADA = <FS_VT>-PESO_ORIGEM.
                  ENDIF.

                  IF <FS_VT>-PESO_ORIGEM IS NOT INITIAL.
                    <FS_VT>-ZVLR_KG_TRANSP   = <FS_VT>-ZVLR_FRETE / <FS_VT>-PESO_ORIGEM.
                    <FS_VT>-ZVLR_KG_MERCAD   = <FS_VT>-ZVLR_MERCADORIA / <FS_VT>-PESO_ORIGEM.
                  ENDIF.

                ENDLOOP.

                "Procura Peso de Chegada
                LOOP AT R_CTE->IT_NIT ASSIGNING <FS_NIT>.
                  IF <FS_NIT>-DOCNUM IS NOT INITIAL. "AND <FS_NIT>-PESO_CHEGADA IS INITIAL.
                    CASE R_CTE->CD_MODAL.
                      WHEN '01'.  "Rodoviário
                        "Comparativo de saidas e chegadas
                        SELECT SINGLE * INTO WA_0039 FROM ZLEST0039 WHERE DOCNUM EQ <FS_NIT>-DOCNUM.
                        IF SY-SUBRC IS INITIAL.
                          IF WA_0039-PONTOTRANSB IS INITIAL.
                            IF ( WA_0039-PESOCHEGADA IS NOT INITIAL ) AND ( WA_0039-DATACHEGADA IS NOT INITIAL ).
                              MOVE-CORRESPONDING <FS_NIT> TO WA_NIT.
                              WA_NIT-PESO_CHEGADA = WA_0039-PESOCHEGADA.
                              R_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = WA_0039-DATACHEGADA ).
                              R_CTE->SET_PESO_CHEGADA_ITEM( EXPORTING I_NIT = WA_NIT ).
                            ENDIF.
                          ELSE.
                            IF ( WA_0039-PESOTRANSB IS NOT INITIAL ) AND ( WA_0039-DATATRANSB IS NOT INITIAL ).
                              MOVE-CORRESPONDING <FS_NIT> TO WA_NIT.
                              WA_NIT-PESO_CHEGADA = WA_0039-PESOTRANSB.
                              R_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = WA_0039-DATATRANSB ).
                              R_CTE->SET_PESO_CHEGADA_ITEM( EXPORTING I_NIT = WA_NIT ).
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      WHEN '02'.  "Aéreo
                      WHEN '03'.  "Aquaviário

                        "Comparativo de saidas e chegadas
                        SELECT SINGLE * INTO WA_ZLEST0061 FROM ZLEST0061 WHERE DOCNUM EQ <FS_NIT>-DOCNUM.
                        IF SY-SUBRC IS INITIAL AND WA_ZLEST0061-DT_CHEGADA IS NOT INITIAL.
                          MOVE-CORRESPONDING <FS_NIT> TO WA_NIT.
                          WA_NIT-PESO_CHEGADA = WA_ZLEST0061-PESO_CHEGADA.
                          R_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = WA_ZLEST0061-DT_CHEGADA ).
                          R_CTE->SET_PESO_CHEGADA_ITEM( EXPORTING I_NIT = WA_NIT ).
                        ENDIF.

                      WHEN '04'.  "Ferroviário
                      WHEN '05'.  "Dutoviário
                    ENDCASE.
                  ENDIF.
                ENDLOOP.

                "Restaura peso já digitado
                LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.

                  LOOP AT R_CTE->IT_N55 ASSIGNING <FS_N55> WHERE TKNUM EQ <FS_VT>-TKNUM AND DOCNUM_NFE IS NOT INITIAL.
                    LOOP AT R_CTE->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE AND PESO_ORIGEM IS NOT INITIAL.

                      IF <FS_NIT>-PESO_ORIGEM_APRO IS NOT INITIAL AND <FS_NIT>-PESO_CHEGADA_APR IS NOT INITIAL.
                        "<FS_NIT>-PESO_ORIGEM  = <FS_NIT>-PESO_ORIGEM_APRO.
                        <FS_NIT>-PESO_CHEGADA = <FS_NIT>-PESO_CHEGADA_APR.
                      ENDIF.

                      CALL METHOD ZCL_CTE_DIST_G=>CALCULA_QUEBRA_PERDA
                        EXPORTING
                          P_COD_MERCADORIA    = <FS_NIT>-ZMATNR_MERC
                          P_PESO_ORIGEM       = <FS_NIT>-PESO_ORIGEM
                          P_PESO_DESTINO      = <FS_NIT>-PESO_CHEGADA
                          P_VLR_FRETE         = <FS_NIT>-ZVLR_FRETE
                          P_VLR_KG_TRASPORT   = <FS_NIT>-ZVLR_KG_TRANSP
                          P_VLR_KG_MERCADORIA = <FS_NIT>-ZVLR_KG_MERCAD
                        IMPORTING
                          E_PESO_DIFERENCA    = <FS_NIT>-ZPESO_DIFERENCA
                          E_PESO_QUEBRA       = <FS_NIT>-ZQUEBRA
                          E_PESO_PERDA        = <FS_NIT>-ZPERDA
                          E_VLR_QUEBRA        = <FS_NIT>-ZVLR_QUEBRA
                          E_VLR_PERDA         = <FS_NIT>-ZVLR_PERDA
                          E_VLR_LIQ_PAGAR     = <FS_NIT>-ZVLR_LIQ_PAGAR
                          E_PC_QUEBRA         = <FS_NIT>-PC_QUEBRA
                          E_PC_TOLERANCIA     = <FS_NIT>-PC_TOLERANCIA.
                    ENDLOOP.
                  ENDLOOP.

                  LOOP AT R_CTE->IT_N01 ASSIGNING <FS_N01> WHERE TKNUM EQ <FS_VT>-TKNUM AND DOCNUM_NF IS NOT INITIAL.
                    LOOP AT R_CTE->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF AND PESO_ORIGEM IS NOT INITIAL.

                      IF <FS_NIT>-PESO_ORIGEM_APRO IS NOT INITIAL AND <FS_NIT>-PESO_CHEGADA_APR IS NOT INITIAL.
                        "<FS_NIT>-PESO_ORIGEM  = <FS_NIT>-PESO_ORIGEM_APRO.
                        <FS_NIT>-PESO_CHEGADA = <FS_NIT>-PESO_CHEGADA_APR.
                      ENDIF.

                      CALL METHOD ZCL_CTE_DIST_G=>CALCULA_QUEBRA_PERDA
                        EXPORTING
                          P_COD_MERCADORIA    = <FS_NIT>-ZMATNR_MERC
                          P_PESO_ORIGEM       = <FS_NIT>-PESO_ORIGEM
                          P_PESO_DESTINO      = <FS_NIT>-PESO_CHEGADA
                          P_VLR_FRETE         = <FS_NIT>-ZVLR_FRETE
                          P_VLR_KG_TRASPORT   = <FS_NIT>-ZVLR_KG_TRANSP
                          P_VLR_KG_MERCADORIA = <FS_NIT>-ZVLR_KG_MERCAD
                        IMPORTING
                          E_PESO_DIFERENCA    = <FS_NIT>-ZPESO_DIFERENCA
                          E_PESO_QUEBRA       = <FS_NIT>-ZQUEBRA
                          E_PESO_PERDA        = <FS_NIT>-ZPERDA
                          E_VLR_QUEBRA        = <FS_NIT>-ZVLR_QUEBRA
                          E_VLR_PERDA         = <FS_NIT>-ZVLR_PERDA
                          E_VLR_LIQ_PAGAR     = <FS_NIT>-ZVLR_LIQ_PAGAR
                          E_PC_QUEBRA         = <FS_NIT>-PC_QUEBRA
                          E_PC_TOLERANCIA     = <FS_NIT>-PC_TOLERANCIA.
                    ENDLOOP.
                  ENDLOOP.
                ENDLOOP.

                R_CTE->SET_TOTALIZA_VTS( ).
              ELSE.

                LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.

                  LOOP AT R_CTE->IT_N55 INTO WA_N55 WHERE TKNUM EQ <FS_VT>-TKNUM.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ WA_N55-DOCNUM_NFE.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS_VT.
                    ENDLOOP.
                  ENDLOOP.

                  LOOP AT R_CTE->IT_N01 INTO WA_N01 WHERE TKNUM EQ <FS_VT>-TKNUM.
                    LOOP AT IT_J_1BNFLIN INTO WA_J_1BNFLIN WHERE DOCNUM EQ WA_N01-DOCNUM_NF.
                      ADD WA_J_1BNFLIN-NETWR TO LC_VALOR_NOTAS_VT.
                    ENDLOOP.
                  ENDLOOP.

                  IF <FS_VT>-TKNUM IS NOT INITIAL.
                    SELECT SUM( L~NTGEW ) INTO <FS_VT>-PESO_CHEGADA
                      FROM VTTP AS P
                     INNER JOIN LIPS AS L ON L~VBELN = P~VBELN
                     WHERE P~TKNUM = <FS_VT>-TKNUM.
                  ENDIF.

                  <FS_VT>-ZVLR_MERCADORIA  = LC_VALOR_NOTAS_VT.
                  <FS_VT>-CK_PESO_DIGITADO = ABAP_FALSE.
                  <FS_VT>-IC_EDITAR        = ICON_CHANGE_NUMBER.

                ENDLOOP.

                "Faturamento Tem que informar manual o rateio.
                R_CTE->SET_TOTALIZA_VTS( ).
              ENDIF.

            WHEN '03'. "Aquaviário

              "Comparativo de saidas e chegadas
              DATA(CK_ACHOU_WA_ZLEST0061) = ABAP_FALSE.

              SELECT SINGLE * INTO WA_ZLEST0061 FROM ZLEST0061 WHERE DOCNUM EQ R_CTE->DOCNUM_CTE_P.
              IF SY-SUBRC IS INITIAL.

                CK_ACHOU_WA_ZLEST0061 = ABAP_TRUE.

                ZCL_MIRO=>GET_DATA_VENCIMENTO_COND_PAG(
                  EXPORTING
                    I_ZTERM            = WA_ZLEST0061-ZTERM " Chave de condições de pagamento
                    I_DT_DOCUMENTO     = R_CTE->DT_EMISSAO  " Data no documento
                    I_DT_LANCAMENTO    = SY-DATUM           " Data de lançamento no documento
                    I_DT_CONTABIL      = SY-DATUM           " Data da entrada do documento contábil
                  RECEIVING
                    R_DT_VENCIMENTO    = DATA(R_DT_VENCIMENTO)    " Data do vencimento
                ).

                R_CTE->SET_ZDT_VENCTO( I_ZDT_VENCTO = R_DT_VENCIMENTO ).

                SELECT * INTO TABLE @DATA(IT_ZLEST0060)
                  FROM ZLEST0060
                 WHERE BUKRS       EQ @WA_ZLEST0061-BUKRS
                   AND WERKS       EQ @WA_ZLEST0061-WERKS
                   AND NR_VIAGEM   EQ @WA_ZLEST0061-NR_VIAGEM
                   AND ANO_VIAGEM  EQ @WA_ZLEST0061-ANO_VIAGEM
                   AND EMBARCACAO  EQ @WA_ZLEST0061-EMBARCACAO
                   AND NOME_EMB    EQ @WA_ZLEST0061-NOME_EMB
                   AND RM_CODIGO   EQ @WA_ZLEST0061-RM_CODIGO
                   AND SAFRA       EQ @WA_ZLEST0061-SAFRA.

                LOOP AT R_CTE->IT_N55 ASSIGNING <FS_N55>.
                  READ TABLE IT_ZLEST0060 WITH KEY CHAVE_NFE = <FS_N55>-N55_CHAVE_ACESSO INTO DATA(WA_60).
                  IF SY-SUBRC IS INITIAL.
                    IF WA_60-PESO_LIQ_RET IS NOT INITIAL.
                      <FS_N55>-PESO_ORIGEM     = WA_60-PESO_LIQ_RET.
                      <FS_N55>-ZVLR_MERCADORIA = WA_60-VLR_LIQ_RET.
                      <FS_N55>-PESO_CHEGADA    = WA_60-PESO_LIQ_RET.
                    ELSE.
                      <FS_N55>-PESO_ORIGEM     = WA_60-PESO_FISCAL.
                      <FS_N55>-ZVLR_MERCADORIA = WA_60-NETWR.
                      <FS_N55>-PESO_CHEGADA    = WA_60-PESO_FISCAL.
                    ENDIF.
                  ENDIF.
                ENDLOOP.

                R_CTE->SET_DT_CHEGADA( EXPORTING I_DT_CHEGADA = WA_ZLEST0061-DT_MOVIMENTO ).
              ENDIF.

              R_CTE->SET_CK_FATURA_PELA_VT( EXPORTING I_CK_FATURA_PELA_VT = ABAP_TRUE ).

              "Agrupar VT's """""""""""""""""""""""""""""
              LC_VALOR_NOTAS_VT = 0.
              LOOP AT R_CTE->IT_N55 INTO WA_N55.
                MOVE-CORRESPONDING WA_N55 TO WA_VT.
                APPEND WA_VT TO R_CTE->IT_VT.
                ADD WA_N55-ZVLR_MERCADORIA TO LC_VALOR_NOTAS_VT.
              ENDLOOP.

              LOOP AT R_CTE->IT_N01 INTO WA_N01.
                MOVE-CORRESPONDING WA_N01 TO WA_VT.
                APPEND WA_VT TO R_CTE->IT_VT.
              ENDLOOP.

              SORT R_CTE->IT_VT BY TKNUM.
              DELETE ADJACENT DUPLICATES FROM R_CTE->IT_VT COMPARING TKNUM.

              LC_VALOR_VT = 0.

              LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.

                IF CK_ACHOU_WA_ZLEST0061 EQ ABAP_TRUE.
                  <FS_VT>-PESO_ORIGEM = WA_ZLEST0061-PESO_VINCULADO.
                ELSE.
                  IF <FS_VT>-TKNUM IS NOT INITIAL.
                    SELECT SUM( L~NTGEW ) INTO <FS_VT>-PESO_ORIGEM
                      FROM VTTP AS P
                     INNER JOIN LIPS AS L ON L~VBELN = P~VBELN
                     WHERE P~TKNUM = <FS_VT>-TKNUM.
                  ENDIF.
                ENDIF.

                IF <FS_VT>-FKNUM IS NOT INITIAL.

                  SELECT SINGLE * INTO WA_VFKP
                    FROM VFKP
                   WHERE FKNUM  EQ <FS_VT>-FKNUM
                     AND NETWR NE 0.

                  SELECT SUM( NETWR ) INTO WA_VFKP-NETWR
                    FROM VFKP
                   WHERE FKNUM EQ <FS_VT>-FKNUM
                     AND NETWR NE 0.

                  IF SY-SUBRC IS INITIAL AND WA_VFKP-NETWR IS NOT INITIAL.
                    R_CTE->WAERK_VI = WA_VFKP-WAERS.
                    <FS_VT>-ZVLR_VI = WA_VFKP-NETWR.

                    CASE WA_VFKP-WAERS.
                      WHEN 'BRL'.
                        ADD WA_VFKP-NETWR TO LC_VALOR_VT.
                      WHEN OTHERS.
                        TRY .
                            CREATE OBJECT OBJ_COTACAO.
                            LC_DATA = WA_VFKP-PRSDT.
                            IF LC_DATA IS NOT INITIAL.
                              OBJ_COTACAO->SET_DATA(  EXPORTING I_DATA = LC_DATA ).
                              OBJ_COTACAO->SET_KURST( EXPORTING I_KURST = WA_VFKP-KURST ).
                              OBJ_COTACAO->SET_WAERK( EXPORTING I_WAERK = WA_VFKP-WAERS ).
                              OBJ_COTACAO->SET_TCURR( EXPORTING I_TCURR = 'BRL' ).
                              LC_UKURS = ABS( OBJ_COTACAO->TAXA_CAMBIO( ) ).
                            ELSE.
                              CLEAR: LC_UKURS.
                            ENDIF.
                          CATCH CX_ROOT.
                            CLEAR: LC_UKURS.
                        ENDTRY.
                        CLEAR: OBJ_COTACAO.

                        R_CTE->WAERK_VI = WA_VFKP-WAERS.
                        R_CTE->KURSK_VI = LC_UKURS.
                        ADD WA_VFKP-NETWR TO LC_VALOR_VT.
                    ENDCASE.
                  ENDIF.
                ENDIF.

                <FS_VT>-ZVLR_FRETE       = R_CTE->VALOR_RECEBER.
                <FS_VT>-ZVLR_LIQ_PAGAR   = R_CTE->VALOR_RECEBER.
                <FS_VT>-ZVLR_VI          = LC_VALOR_VT.
                <FS_VT>-PESO_CHEGADA     = <FS_VT>-PESO_ORIGEM.
                <FS_VT>-ZVLR_MERCADORIA  = LC_VALOR_NOTAS_VT.
                <FS_VT>-CK_PESO_DIGITADO = ABAP_FALSE.
                <FS_VT>-IC_EDITAR        = ICON_CHANGE_NUMBER.

                R_CTE->ZVLR_LIQ_PAGAR    = <FS_VT>-ZVLR_FRETE.
                R_CTE->PESO_CHEGADA      = <FS_VT>-PESO_ORIGEM.
                R_CTE->ZVLR_VI           = LC_VALOR_VT.
              ENDLOOP.

          ENDCASE.

        WHEN 1. "CT-e de Complemento de Valores
          WA_VT-ZVLR_VI          = 0.
          WA_VT-ZVLR_FRETE       = R_CTE->ZVLR_FRETE.
          WA_VT-ZVLR_MERCADORIA  = 0.
          WA_VT-PESO_ORIGEM      = 0.
          WA_VT-PESO_CHEGADA     = 0.
          WA_VT-ZPESO_DIFERENCA  = 0.
          WA_VT-ZVLR_KG_TRANSP   = 0.
          WA_VT-ZVLR_KG_MERCAD   = 0.
          WA_VT-ZQUEBRA          = 0.
          WA_VT-ZPERDA           = 0.
          WA_VT-ZVLR_QUEBRA      = 0.
          WA_VT-ZVLR_PERDA       = 0.
          WA_VT-ZVLR_LIQ_PAGAR   = R_CTE->ZVLR_FRETE.
          WA_VT-PC_QUEBRA        = 0.
          WA_VT-PC_TOLERANCIA    = 0.
          APPEND WA_VT TO R_CTE->IT_VT.
      ENDCASE.
    ELSE.

      "0  CT-e Normal
      "1  CT-e de Complemento de Valores
      "2  CT-e de Anulação de Valores
      "3  CT-e Substituto
      CASE R_CTE->CD_TIPO_CTE.
        WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

          "Agrupar VT's """""""""""""""""""""""""""""
          LOOP AT R_CTE->IT_N55 INTO WA_N55.
            MOVE-CORRESPONDING WA_N55 TO WA_VT.
            APPEND WA_VT TO R_CTE->IT_VT.
          ENDLOOP.

          LOOP AT R_CTE->IT_N01 INTO WA_N01.
            MOVE-CORRESPONDING WA_N01 TO WA_VT.
            APPEND WA_VT TO R_CTE->IT_VT.
          ENDLOOP.

          SORT R_CTE->IT_VT BY TKNUM.
          DELETE ADJACENT DUPLICATES FROM R_CTE->IT_VT COMPARING TKNUM.

          IF R_CTE->CK_FATURA_PELA_VT EQ ABAP_FALSE.
            LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.

              CLEAR: <FS_VT>-ZMATNR_MERC,
                     <FS_VT>-ZVLR_VI,
                     <FS_VT>-ZVLR_FRETE,
                     <FS_VT>-ZVLR_MERCADORIA,
                     <FS_VT>-PESO_ORIGEM,
                     <FS_VT>-PESO_CHEGADA,
                     <FS_VT>-ZPESO_DIFERENCA,
                     <FS_VT>-ZQUEBRA,
                     <FS_VT>-ZPERDA,
                     <FS_VT>-ZVLR_QUEBRA,
                     <FS_VT>-ZVLR_PERDA,
                     <FS_VT>-ZVLR_LIQ_PAGAR,
                     LC_VALOR_FRETE.

              LOOP AT R_CTE->IT_N55 INTO WA_N55 WHERE TKNUM EQ <FS_VT>-TKNUM.
                ADD WA_N55-ZVLR_VI          TO <FS_VT>-ZVLR_VI.
                ADD WA_N55-ZVLR_FRETE       TO <FS_VT>-ZVLR_FRETE.
                ADD WA_N55-ZVLR_MERCADORIA  TO <FS_VT>-ZVLR_MERCADORIA.
                ADD WA_N55-PESO_ORIGEM      TO <FS_VT>-PESO_ORIGEM.
                ADD WA_N55-PESO_CHEGADA     TO <FS_VT>-PESO_CHEGADA.
                ADD WA_N55-ZPESO_DIFERENCA  TO <FS_VT>-ZPESO_DIFERENCA.
                ADD WA_N55-ZQUEBRA          TO <FS_VT>-ZQUEBRA.
                ADD WA_N55-ZPERDA           TO <FS_VT>-ZPERDA.
                ADD WA_N55-ZVLR_QUEBRA      TO <FS_VT>-ZVLR_QUEBRA.
                ADD WA_N55-ZVLR_PERDA       TO <FS_VT>-ZVLR_PERDA.
                ADD WA_N55-ZVLR_LIQ_PAGAR   TO <FS_VT>-ZVLR_LIQ_PAGAR.
                <FS_VT>-AUART_VA         = WA_N55-AUART_VA.
                <FS_VT>-CK_PESO_DIGITADO = WA_N55-CK_PESO_DIGITADO.
                <FS_VT>-ZVLR_KG_TRANSP   = WA_N55-ZVLR_KG_TRANSP.
                <FS_VT>-ZVLR_KG_MERCAD   = WA_N55-ZVLR_KG_MERCAD.

                READ TABLE R_CTE->IT_NIT INTO WA_NIT WITH KEY DOCNUM = WA_N55-DOCNUM_NFE.
                IF SY-SUBRC IS INITIAL.
                  <FS_VT>-ZMATNR_MERC   = WA_NIT-ZMATNR_MERC.
                  <FS_VT>-DOCNUM        = WA_NIT-DOCNUM.
                  <FS_VT>-ITMNUM        = WA_NIT-ITMNUM.
                  <FS_VT>-PC_QUEBRA     = WA_NIT-PC_QUEBRA.
                  <FS_VT>-PC_TOLERANCIA = WA_NIT-PC_TOLERANCIA.
                  <FS_VT>-CK_AUTORIZADO = WA_NIT-CK_AUTORIZADO.
                ENDIF.
              ENDLOOP.
              LOOP AT R_CTE->IT_N01 INTO WA_N01 WHERE TKNUM EQ <FS_VT>-TKNUM.
                ADD WA_N01-ZVLR_VI          TO <FS_VT>-ZVLR_VI.
                ADD WA_N01-ZVLR_FRETE       TO <FS_VT>-ZVLR_FRETE.
                ADD WA_N01-ZVLR_MERCADORIA  TO <FS_VT>-ZVLR_MERCADORIA.
                ADD WA_N01-PESO_ORIGEM      TO <FS_VT>-PESO_ORIGEM.
                ADD WA_N01-PESO_CHEGADA     TO <FS_VT>-PESO_CHEGADA.
                ADD WA_N01-ZPESO_DIFERENCA  TO <FS_VT>-ZPESO_DIFERENCA.
                ADD WA_N01-ZQUEBRA          TO <FS_VT>-ZQUEBRA.
                ADD WA_N01-ZPERDA           TO <FS_VT>-ZPERDA.
                ADD WA_N01-ZVLR_QUEBRA      TO <FS_VT>-ZVLR_QUEBRA.
                ADD WA_N01-ZVLR_PERDA       TO <FS_VT>-ZVLR_PERDA.
                ADD WA_N01-ZVLR_LIQ_PAGAR   TO <FS_VT>-ZVLR_LIQ_PAGAR.
                <FS_VT>-AUART_VA         = WA_N01-AUART_VA.
                <FS_VT>-CK_PESO_DIGITADO = WA_N01-CK_PESO_DIGITADO.
                <FS_VT>-ZVLR_KG_TRANSP   = WA_N01-ZVLR_KG_TRANSP.
                <FS_VT>-ZVLR_KG_MERCAD   = WA_N01-ZVLR_KG_MERCAD.

                READ TABLE R_CTE->IT_NIT INTO WA_NIT WITH KEY DOCNUM = WA_N01-DOCNUM_NF.
                IF SY-SUBRC IS INITIAL.
                  <FS_VT>-ZMATNR_MERC   = WA_NIT-ZMATNR_MERC.
                  <FS_VT>-DOCNUM        = WA_NIT-DOCNUM.
                  <FS_VT>-ITMNUM        = WA_NIT-ITMNUM.
                  <FS_VT>-PC_QUEBRA     = WA_NIT-PC_QUEBRA.
                  <FS_VT>-PC_TOLERANCIA = WA_NIT-PC_TOLERANCIA.
                  <FS_VT>-CK_AUTORIZADO = WA_NIT-CK_AUTORIZADO.
                ENDIF.
              ENDLOOP.
              <FS_VT>-IC_EDITAR        = ICON_SET_STATE.
            ENDLOOP.
          ELSE.
            LOOP AT R_CTE->IT_VT ASSIGNING <FS_VT>.

              CLEAR: <FS_VT>-ZMATNR_MERC,
                     <FS_VT>-ZVLR_VI,
                     <FS_VT>-ZVLR_FRETE,
                     <FS_VT>-ZVLR_MERCADORIA,
                     <FS_VT>-PESO_ORIGEM,
                     <FS_VT>-PESO_CHEGADA,
                     <FS_VT>-ZPESO_DIFERENCA,
                     <FS_VT>-ZQUEBRA,
                     <FS_VT>-ZPERDA,
                     <FS_VT>-ZVLR_QUEBRA,
                     <FS_VT>-ZVLR_PERDA,
                     <FS_VT>-ZVLR_LIQ_PAGAR.

              READ TABLE R_CTE->IT_N55 INTO WA_N55 WITH KEY TKNUM = <FS_VT>-TKNUM.
              IF SY-SUBRC IS INITIAL.
                <FS_VT>-ZVLR_VI          = WA_N55-ZVLR_VI.
                <FS_VT>-ZVLR_FRETE       = WA_N55-ZVLR_FRETE.
                <FS_VT>-ZVLR_MERCADORIA  = WA_N55-ZVLR_MERCADORIA.
                <FS_VT>-PESO_ORIGEM      = WA_N55-PESO_ORIGEM.
                <FS_VT>-PESO_CHEGADA     = WA_N55-PESO_CHEGADA.
                <FS_VT>-ZPESO_DIFERENCA  = WA_N55-ZPESO_DIFERENCA.
                <FS_VT>-ZQUEBRA          = WA_N55-ZQUEBRA.
                <FS_VT>-ZPERDA           = WA_N55-ZPERDA.
                <FS_VT>-ZVLR_QUEBRA      = WA_N55-ZVLR_QUEBRA.
                <FS_VT>-ZVLR_PERDA       = WA_N55-ZVLR_PERDA.
                <FS_VT>-ZVLR_LIQ_PAGAR   = WA_N55-ZVLR_LIQ_PAGAR.
                <FS_VT>-AUART_VA         = WA_N55-AUART_VA.
                <FS_VT>-CK_PESO_DIGITADO = WA_N55-CK_PESO_DIGITADO.
                <FS_VT>-ZVLR_KG_TRANSP   = WA_N55-ZVLR_KG_TRANSP.
                <FS_VT>-ZVLR_KG_MERCAD   = WA_N55-ZVLR_KG_MERCAD.
              ENDIF.

              READ TABLE R_CTE->IT_N01 INTO WA_N01 WITH KEY TKNUM = <FS_VT>-TKNUM.
              IF SY-SUBRC IS INITIAL.
                <FS_VT>-ZVLR_VI          = WA_N01-ZVLR_VI.
                <FS_VT>-ZVLR_FRETE       = WA_N01-ZVLR_FRETE.
                <FS_VT>-ZVLR_MERCADORIA  = WA_N01-ZVLR_MERCADORIA.
                <FS_VT>-PESO_ORIGEM      = WA_N01-PESO_ORIGEM.
                <FS_VT>-PESO_CHEGADA     = WA_N01-PESO_CHEGADA.
                <FS_VT>-ZPESO_DIFERENCA  = WA_N01-ZPESO_DIFERENCA.
                <FS_VT>-ZQUEBRA          = WA_N01-ZQUEBRA.
                <FS_VT>-ZPERDA           = WA_N01-ZPERDA.
                <FS_VT>-ZVLR_QUEBRA      = WA_N01-ZVLR_QUEBRA.
                <FS_VT>-ZVLR_PERDA       = WA_N01-ZVLR_PERDA.
                <FS_VT>-ZVLR_LIQ_PAGAR   = WA_N01-ZVLR_LIQ_PAGAR.
                <FS_VT>-AUART_VA         = WA_N01-AUART_VA.
                <FS_VT>-CK_PESO_DIGITADO = WA_N01-CK_PESO_DIGITADO.
                <FS_VT>-ZVLR_KG_TRANSP   = WA_N01-ZVLR_KG_TRANSP.
                <FS_VT>-ZVLR_KG_MERCAD   = WA_N01-ZVLR_KG_MERCAD.
              ENDIF.
              <FS_VT>-IC_EDITAR        = ICON_SET_STATE.
            ENDLOOP.
          ENDIF.

        WHEN 1. "CT-e de Complemento de Valores
          WA_VT-ZVLR_VI          = 0.
          WA_VT-ZVLR_FRETE       = R_CTE->ZVLR_FRETE.
          WA_VT-ZVLR_MERCADORIA  = 0.
          WA_VT-PESO_ORIGEM      = 0.
          WA_VT-PESO_CHEGADA     = 0.
          WA_VT-ZPESO_DIFERENCA  = 0.
          WA_VT-ZVLR_KG_TRANSP   = 0.
          WA_VT-ZVLR_KG_MERCAD   = 0.
          WA_VT-ZQUEBRA          = 0.
          WA_VT-ZPERDA           = 0.
          WA_VT-ZVLR_QUEBRA      = 0.
          WA_VT-ZVLR_PERDA       = 0.
          WA_VT-ZVLR_LIQ_PAGAR   = R_CTE->ZVLR_LIQ_PAGAR.
          WA_VT-PC_QUEBRA        = 0.
          WA_VT-PC_TOLERANCIA    = 0.
          APPEND WA_VT TO R_CTE->IT_VT.
      ENDCASE.
    ENDIF.

    IF I_NAO_CHAMAR_TELA EQ ABAP_FALSE.

      CALL FUNCTION 'ZCTE_DIST_FATURAR_INFO'
        EXPORTING
          I_INDEX_ULTIMO    = I_INDEX_ULTIMO
        IMPORTING
          E_COMANDO         = E_COMANDO
        CHANGING
          I_ZIB_CTE         = R_CTE
          I_INDEX           = I_INDEX
        EXCEPTIONS
          BLOQUEADO_USUARIO = 1
          OTHERS            = 2.

      CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
        EXPORTING
          CHAVE = R_CTE->CD_CHAVE_CTE.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING BLOQUEADO_USUARIO.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD SET_INICIO_IBGE.
    ME->INICIO_IBGE = I_INICIO_IBGE.
  ENDMETHOD.


  METHOD SET_INICIO_MUNI.
    ME->INICIO_MUNI = I_INICIO_MUNI.
  ENDMETHOD.


  METHOD SET_INICIO_UF.
    ME->INICIO_UF = I_INICIO_UF.
  ENDMETHOD.


  METHOD SET_KURSK_VI.
    ME->KURSK_VI = I_KURSK_VI.
  ENDMETHOD.


  METHOD SET_MATNS.
    ME->MATNS = I_MATNS.
  ENDMETHOD.


  METHOD SET_MODELO.
    ME->MODELO = I_MODELO.
  ENDMETHOD.


  METHOD SET_MWSKZ.
    IF I_MWSKZ NE ME->MWSKZ.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.
    ME->MWSKZ = I_MWSKZ.
  ENDMETHOD.


  METHOD SET_NFMONTH.
    ME->NFMONTH = I_NFMONTH.
  ENDMETHOD.


  METHOD SET_NFYEAR.
    ME->NFYEAR = I_NFYEAR.
  ENDMETHOD.


  METHOD SET_NR_PROTOCOLO.
    ME->NR_PROTOCOLO = I_NR_PROTOCOLO.
  ENDMETHOD.


  METHOD SET_NUMR_CTE.
    ME->NUMR_CTE = I_NUMR_CTE.
  ENDMETHOD.


  METHOD SET_NUMR_SERIE.
    ME->NUMR_SERIE = I_NUMR_SERIE.
  ENDMETHOD.


  METHOD SET_PESO_CHEGADA.
    ME->PESO_CHEGADA = I_PESO_CHEGADA.
  ENDMETHOD.


  METHOD SET_PESO_CHEGADA_ITEM.

    DATA: WA_N55 TYPE ZIB_CTE_DIST_N55,
          WA_N01 TYPE ZIB_CTE_DIST_N01.

    FIELD-SYMBOLS: <FS_N55>  TYPE ZIB_CTE_DIST_N55,
                   <FS_N01>  TYPE ZIB_CTE_DIST_N01,
                   <FS_NIT>  TYPE ZIB_CTE_DIST_NIT,
                   <FS_NITB> TYPE ZIB_CTE_DIST_NIT,
                   <FS_VT>   TYPE ZDE_CTE_DIST_VT_ALV.

    READ TABLE ME->IT_NIT ASSIGNING <FS_NIT> WITH KEY DOCNUM = I_NIT-DOCNUM
                                                      ITMNUM = I_NIT-ITMNUM.
    CHECK SY-SUBRC IS INITIAL.

    CHECK I_NIT-PESO_ORIGEM     NE <FS_NIT>-PESO_ORIGEM     OR
          I_NIT-PESO_CHEGADA    NE <FS_NIT>-PESO_CHEGADA    OR
          I_NIT-ZVLR_VI         NE <FS_NIT>-ZVLR_VI         OR
          I_NIT-ZVLR_FRETE      NE <FS_NIT>-ZVLR_FRETE      OR
          I_NIT-ZVLR_MERCADORIA NE <FS_NIT>-ZVLR_MERCADORIA.

    ME->CK_ALTERADO = ABAP_TRUE.

    <FS_NIT>-PESO_CHEGADA    = I_NIT-PESO_CHEGADA.
    <FS_NIT>-PESO_ORIGEM     = I_NIT-PESO_ORIGEM.
    <FS_NIT>-ZVLR_VI         = I_NIT-ZVLR_VI.
    <FS_NIT>-ZVLR_FRETE      = I_NIT-ZVLR_FRETE.
    <FS_NIT>-ZVLR_MERCADORIA = I_NIT-ZVLR_MERCADORIA.

    IF <FS_NIT>-PESO_ORIGEM IS NOT INITIAL.
      <FS_NIT>-ZVLR_KG_TRANSP  = I_NIT-ZVLR_FRETE / I_NIT-PESO_ORIGEM.
      <FS_NIT>-ZVLR_KG_MERCAD  = I_NIT-ZVLR_MERCADORIA / I_NIT-PESO_ORIGEM.
    ENDIF.

    CALL METHOD ZCL_CTE_DIST_G=>CALCULA_QUEBRA_PERDA
      EXPORTING
        P_COD_MERCADORIA    = <FS_NIT>-ZMATNR_MERC
        P_PESO_ORIGEM       = <FS_NIT>-PESO_ORIGEM
        P_PESO_DESTINO      = <FS_NIT>-PESO_CHEGADA
        P_VLR_FRETE         = <FS_NIT>-ZVLR_FRETE
        P_VLR_KG_TRASPORT   = <FS_NIT>-ZVLR_KG_TRANSP
        P_VLR_KG_MERCADORIA = <FS_NIT>-ZVLR_KG_MERCAD
      IMPORTING
        E_PESO_DIFERENCA    = <FS_NIT>-ZPESO_DIFERENCA
        E_PESO_QUEBRA       = <FS_NIT>-ZQUEBRA
        E_PESO_PERDA        = <FS_NIT>-ZPERDA
        E_VLR_QUEBRA        = <FS_NIT>-ZVLR_QUEBRA
        E_VLR_PERDA         = <FS_NIT>-ZVLR_PERDA
        E_VLR_LIQ_PAGAR     = <FS_NIT>-ZVLR_LIQ_PAGAR
        E_PC_QUEBRA         = <FS_NIT>-PC_QUEBRA
        E_PC_TOLERANCIA     = <FS_NIT>-PC_TOLERANCIA.

    IF ME->CK_FATURA_PELA_VT EQ ABAP_TRUE.
      READ TABLE ME->IT_N55 INTO WA_N55 WITH KEY DOCNUM_NFE = I_NIT-DOCNUM.
      IF SY-SUBRC IS NOT INITIAL.
        READ TABLE ME->IT_N01 INTO WA_N01 WITH KEY DOCNUM_NF = I_NIT-DOCNUM.
        IF SY-SUBRC IS INITIAL.
          WA_N55-TKNUM = WA_N01-TKNUM.
        ENDIF.
      ENDIF.
      LOOP AT ME->IT_N55 ASSIGNING <FS_N55> WHERE TKNUM EQ WA_N55-TKNUM.
        LOOP AT ME->IT_NIT ASSIGNING <FS_NITB> WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
          IF NOT ( <FS_NITB>-DOCNUM EQ I_NIT-DOCNUM AND <FS_NITB>-ITMNUM EQ I_NIT-ITMNUM ).
            <FS_NITB>-PESO_ORIGEM     = <FS_NIT>-PESO_ORIGEM    .
            <FS_NITB>-PESO_CHEGADA    = <FS_NIT>-PESO_CHEGADA   .
            <FS_NITB>-ZVLR_VI         = <FS_NIT>-ZVLR_VI        .
            <FS_NITB>-PESO_ORIGEM     = <FS_NIT>-PESO_ORIGEM    .
            <FS_NITB>-PESO_CHEGADA    = <FS_NIT>-PESO_CHEGADA   .
            <FS_NITB>-ZVLR_MERCADORIA = <FS_NIT>-ZVLR_MERCADORIA.
            <FS_NITB>-ZVLR_FRETE      = <FS_NIT>-ZVLR_FRETE     .
            <FS_NITB>-ZVLR_KG_TRANSP  = <FS_NIT>-ZVLR_KG_TRANSP .
            <FS_NITB>-ZVLR_KG_MERCAD  = <FS_NIT>-ZVLR_KG_MERCAD .
            <FS_NITB>-ZPESO_DIFERENCA = <FS_NIT>-ZPESO_DIFERENCA.
            <FS_NITB>-ZQUEBRA         = <FS_NIT>-ZQUEBRA        .
            <FS_NITB>-ZPERDA          = <FS_NIT>-ZPERDA         .
            <FS_NITB>-ZVLR_QUEBRA     = <FS_NIT>-ZVLR_QUEBRA    .
            <FS_NITB>-ZVLR_PERDA      = <FS_NIT>-ZVLR_PERDA     .
            <FS_NITB>-ZVLR_LIQ_PAGAR  = <FS_NIT>-ZVLR_LIQ_PAGAR .
            <FS_NITB>-PC_QUEBRA       = <FS_NIT>-PC_QUEBRA      .
            <FS_NITB>-PC_TOLERANCIA   = <FS_NIT>-PC_TOLERANCIA  .
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      LOOP AT ME->IT_N01 ASSIGNING <FS_N01> WHERE TKNUM EQ WA_N55-TKNUM.
        LOOP AT ME->IT_NIT ASSIGNING <FS_NITB> WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF.
          IF NOT ( <FS_NITB>-DOCNUM EQ I_NIT-DOCNUM AND <FS_NITB>-ITMNUM EQ I_NIT-ITMNUM ).
            <FS_NITB>-PESO_ORIGEM     = <FS_NIT>-PESO_ORIGEM    .
            <FS_NITB>-PESO_CHEGADA    = <FS_NIT>-PESO_CHEGADA   .
            <FS_NITB>-ZVLR_VI         = <FS_NIT>-ZVLR_VI        .
            <FS_NITB>-PESO_ORIGEM     = <FS_NIT>-PESO_ORIGEM    .
            <FS_NITB>-PESO_CHEGADA    = <FS_NIT>-PESO_CHEGADA   .
            <FS_NITB>-ZVLR_MERCADORIA = <FS_NIT>-ZVLR_MERCADORIA.
            <FS_NITB>-ZVLR_FRETE      = <FS_NIT>-ZVLR_FRETE     .
            <FS_NITB>-ZVLR_KG_TRANSP  = <FS_NIT>-ZVLR_KG_TRANSP .
            <FS_NITB>-ZVLR_KG_MERCAD  = <FS_NIT>-ZVLR_KG_MERCAD .
            <FS_NITB>-ZPESO_DIFERENCA = <FS_NIT>-ZPESO_DIFERENCA.
            <FS_NITB>-ZQUEBRA         = <FS_NIT>-ZQUEBRA        .
            <FS_NITB>-ZPERDA          = <FS_NIT>-ZPERDA         .
            <FS_NITB>-ZVLR_QUEBRA     = <FS_NIT>-ZVLR_QUEBRA    .
            <FS_NITB>-ZVLR_PERDA      = <FS_NIT>-ZVLR_PERDA     .
            <FS_NITB>-ZVLR_LIQ_PAGAR  = <FS_NIT>-ZVLR_LIQ_PAGAR .
            <FS_NITB>-PC_QUEBRA       = <FS_NIT>-PC_QUEBRA      .
            <FS_NITB>-PC_TOLERANCIA   = <FS_NIT>-PC_TOLERANCIA  .
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    ME->SET_TOTALIZA_VTS( ).

  ENDMETHOD.


  METHOD SET_PESO_ORIGEM.
    ME->PESO_ORIGEM = I_PESO_ORIGEM.
  ENDMETHOD.


  METHOD SET_P_EMISSOR.
    ME->P_EMISSOR = I_P_EMISSOR.
  ENDMETHOD.


  METHOD SET_QT_CARGA_CTE.
    ME->QT_CARGA_CTE = I_QT_CARGA_CTE.
  ENDMETHOD.


  METHOD SET_RECEB_CNPJ.
    ME->RECEB_CNPJ = I_RECEB_CNPJ.
  ENDMETHOD.


  METHOD SET_RECEB_CPF.
    ME->RECEB_CPF = I_RECEB_CPF.
  ENDMETHOD.


  METHOD SET_RECEB_FANTASIA.
    ME->RECEB_FANTASIA = I_RECEB_FANTASIA.
  ENDMETHOD.


  METHOD SET_RECEB_IE.
    ME->RECEB_IE = I_RECEB_IE.
  ENDMETHOD.


  METHOD SET_RECEB_RSOCIAL.
    ME->RECEB_RSOCIAL = I_RECEB_RSOCIAL.
  ENDMETHOD.


  METHOD SET_RECEB_TP_DOC.
    ME->RECEB_TP_DOC = I_RECEB_TP_DOC.
  ENDMETHOD.


  METHOD SET_REGIO.
    ME->REGIO = I_REGIO.
  ENDMETHOD.


  METHOD SET_REME_CNPJ.
    ME->REME_CNPJ = I_REME_CNPJ.
  ENDMETHOD.


  METHOD SET_REME_CPF.
    ME->REME_CPF = I_REME_CPF.
  ENDMETHOD.


  METHOD SET_REME_FANTASIA.
    ME->REME_FANTASIA = I_REME_FANTASIA.
  ENDMETHOD.


  METHOD SET_REME_IE.
    ME->REME_IE = I_REME_IE.
  ENDMETHOD.


  METHOD SET_REME_RSOCIAL.
    ME->REME_RSOCIAL = I_REME_RSOCIAL.
  ENDMETHOD.


  METHOD SET_REME_TP_DOC.
    ME->REME_TP_DOC = I_REME_TP_DOC.
  ENDMETHOD.


  METHOD SET_RG_LIDO_PAG_FRET.
    ME->RG_LIDO_PAG_FRET = I_RG_LIDO_PAG_FRET.
  ENDMETHOD.


  METHOD SET_TERMINO_IBGE.
    ME->TERMINO_IBGE = I_TERMINO_IBGE.
  ENDMETHOD.


  METHOD SET_TERMINO_MUNI.
    ME->TERMINO_MUNI = I_TERMINO_MUNI.
  ENDMETHOD.


  METHOD SET_TERMINO_UF.
    ME->TERMINO_UF = I_TERMINO_UF.
  ENDMETHOD.


  METHOD SET_TIMESTAMP.
    ME->TIMESTAMP = I_TIMESTAMP.
  ENDMETHOD.


  METHOD SET_TOMA4_CNPJ.
    ME->TOMA4_CNPJ = I_TOMA4_CNPJ.
  ENDMETHOD.


  METHOD SET_TOMA4_CPF.
    ME->TOMA4_CPF = I_TOMA4_CPF.
  ENDMETHOD.


  METHOD SET_TOMA4_FANTASIA.
    ME->TOMA4_FANTASIA = I_TOMA4_FANTASIA.
  ENDMETHOD.


  METHOD SET_TOMA4_IE.
    ME->TOMA4_IE = I_TOMA4_IE.
  ENDMETHOD.


  METHOD SET_TOMA4_RSOCIAL.
    ME->TOMA4_RSOCIAL = I_TOMA4_RSOCIAL.
  ENDMETHOD.


  METHOD SET_TOMA4_TP_DOC.
    ME->TOMA4_TP_DOC = I_TOMA4_TP_DOC.
  ENDMETHOD.


  METHOD SET_TOTALIZA_VTS.

    FIELD-SYMBOLS: <FS_VT>   TYPE ZDE_CTE_DIST_VT_ALV,
                   <FS_N55>  TYPE ZIB_CTE_DIST_N55,
                   <FS_N55B> TYPE ZIB_CTE_DIST_N55,
                   <FS_N01>  TYPE ZIB_CTE_DIST_N01,
                   <FS_N01B> TYPE ZIB_CTE_DIST_N01,
                   <FS_NIT>  TYPE ZIB_CTE_DIST_NIT.

    "Totaliza
    IF ME->CK_FATURA_PELA_VT EQ ABAP_FALSE.

      CLEAR: ME->ZVLR_MERCADORIA,
             ME->PESO_ORIGEM,
             ME->PESO_CHEGADA,
             ME->ZPESO_DIFERENCA,
             ME->ZQUEBRA,
             ME->ZPERDA,
             ME->ZVLR_QUEBRA,
             ME->ZVLR_PERDA,
             ME->ZVLR_VI,
             ME->ZVLR_LIQ_PAGAR.

      LOOP AT ME->IT_VT ASSIGNING <FS_VT>.

        CLEAR: <FS_VT>-PESO_ORIGEM,
               <FS_VT>-PESO_CHEGADA,
               <FS_VT>-ZVLR_MERCADORIA,
               <FS_VT>-ZPESO_DIFERENCA,
               <FS_VT>-ZQUEBRA,
               <FS_VT>-ZPERDA,
               <FS_VT>-ZVLR_QUEBRA,
               <FS_VT>-ZVLR_PERDA,
               <FS_VT>-ZVLR_VI,
               <FS_VT>-ZVLR_LIQ_PAGAR.

        LOOP AT ME->IT_N55 ASSIGNING <FS_N55> WHERE TKNUM EQ <FS_VT>-TKNUM.
          CLEAR: <FS_N55>-PESO_ORIGEM,
                 <FS_N55>-PESO_CHEGADA,
                 <FS_N55>-ZVLR_MERCADORIA,
                 <FS_N55>-ZPESO_DIFERENCA,
                 <FS_N55>-ZQUEBRA,
                 <FS_N55>-ZPERDA,
                 <FS_N55>-ZVLR_QUEBRA,
                 <FS_N55>-ZVLR_PERDA,
                 <FS_N55>-ZVLR_VI,
                 <FS_N55>-ZVLR_LIQ_PAGAR.
          LOOP AT ME->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N55>-DOCNUM_NFE.
            ADD <FS_NIT>-PESO_ORIGEM     TO <FS_N55>-PESO_ORIGEM.
            ADD <FS_NIT>-PESO_CHEGADA    TO <FS_N55>-PESO_CHEGADA.
            ADD <FS_NIT>-ZVLR_MERCADORIA TO <FS_N55>-ZVLR_MERCADORIA.
            ADD <FS_NIT>-ZPESO_DIFERENCA TO <FS_N55>-ZPESO_DIFERENCA.
            ADD <FS_NIT>-ZQUEBRA         TO <FS_N55>-ZQUEBRA.
            ADD <FS_NIT>-ZPERDA          TO <FS_N55>-ZPERDA.
            ADD <FS_NIT>-ZVLR_QUEBRA     TO <FS_N55>-ZVLR_QUEBRA.
            ADD <FS_NIT>-ZVLR_PERDA      TO <FS_N55>-ZVLR_PERDA.
            ADD <FS_NIT>-ZVLR_VI         TO <FS_N55>-ZVLR_VI.
            ADD <FS_NIT>-ZVLR_LIQ_PAGAR  TO <FS_N55>-ZVLR_LIQ_PAGAR.
          ENDLOOP.
          ADD <FS_N55>-PESO_ORIGEM     TO <FS_VT>-PESO_ORIGEM.
          ADD <FS_N55>-PESO_CHEGADA    TO <FS_VT>-PESO_CHEGADA.
          ADD <FS_N55>-ZVLR_MERCADORIA TO <FS_VT>-ZVLR_MERCADORIA.
          ADD <FS_N55>-ZPESO_DIFERENCA TO <FS_VT>-ZPESO_DIFERENCA.
          ADD <FS_N55>-ZQUEBRA         TO <FS_VT>-ZQUEBRA.
          ADD <FS_N55>-ZPERDA          TO <FS_VT>-ZPERDA.
          ADD <FS_N55>-ZVLR_QUEBRA     TO <FS_VT>-ZVLR_QUEBRA.
          ADD <FS_N55>-ZVLR_PERDA      TO <FS_VT>-ZVLR_PERDA.
          ADD <FS_N55>-ZVLR_VI         TO <FS_VT>-ZVLR_VI.
          ADD <FS_N55>-ZVLR_LIQ_PAGAR  TO <FS_VT>-ZVLR_LIQ_PAGAR.
        ENDLOOP.
        LOOP AT ME->IT_N01 ASSIGNING <FS_N01> WHERE TKNUM EQ <FS_VT>-TKNUM.
          CLEAR: <FS_N01>-PESO_ORIGEM,
                 <FS_N01>-PESO_CHEGADA,
                 <FS_N01>-ZVLR_MERCADORIA,
                 <FS_N01>-ZPESO_DIFERENCA,
                 <FS_N01>-ZQUEBRA,
                 <FS_N01>-ZPERDA,
                 <FS_N01>-ZVLR_QUEBRA,
                 <FS_N01>-ZVLR_PERDA,
                 <FS_N01>-ZVLR_VI,
                 <FS_N01>-ZVLR_LIQ_PAGAR.
          LOOP AT ME->IT_NIT ASSIGNING <FS_NIT> WHERE DOCNUM EQ <FS_N01>-DOCNUM_NF.
            ADD <FS_NIT>-PESO_ORIGEM     TO <FS_N01>-PESO_ORIGEM.
            ADD <FS_NIT>-PESO_CHEGADA    TO <FS_N01>-PESO_CHEGADA.
            ADD <FS_NIT>-ZVLR_MERCADORIA TO <FS_N01>-ZVLR_MERCADORIA.
            ADD <FS_NIT>-ZPESO_DIFERENCA TO <FS_N01>-ZPESO_DIFERENCA.
            ADD <FS_NIT>-ZQUEBRA         TO <FS_N01>-ZQUEBRA.
            ADD <FS_NIT>-ZPERDA          TO <FS_N01>-ZPERDA.
            ADD <FS_NIT>-ZVLR_QUEBRA     TO <FS_N01>-ZVLR_QUEBRA.
            ADD <FS_NIT>-ZVLR_PERDA      TO <FS_N01>-ZVLR_PERDA.
            ADD <FS_NIT>-ZVLR_VI         TO <FS_N01>-ZVLR_VI.
            ADD <FS_NIT>-ZVLR_LIQ_PAGAR  TO <FS_N01>-ZVLR_LIQ_PAGAR.
          ENDLOOP.
          ADD <FS_N01>-PESO_ORIGEM     TO <FS_VT>-PESO_ORIGEM.
          ADD <FS_N01>-PESO_CHEGADA    TO <FS_VT>-PESO_CHEGADA.
          ADD <FS_N01>-ZVLR_MERCADORIA TO <FS_VT>-ZVLR_MERCADORIA.
          ADD <FS_N01>-ZPESO_DIFERENCA TO <FS_VT>-ZPESO_DIFERENCA.
          ADD <FS_N01>-ZQUEBRA         TO <FS_VT>-ZQUEBRA.
          ADD <FS_N01>-ZPERDA          TO <FS_VT>-ZPERDA.
          ADD <FS_N01>-ZVLR_QUEBRA     TO <FS_VT>-ZVLR_QUEBRA.
          ADD <FS_N01>-ZVLR_PERDA      TO <FS_VT>-ZVLR_PERDA.
          ADD <FS_N01>-ZVLR_VI         TO <FS_VT>-ZVLR_VI.
          ADD <FS_N01>-ZVLR_LIQ_PAGAR  TO <FS_VT>-ZVLR_LIQ_PAGAR.
        ENDLOOP.
        ADD <FS_VT>-PESO_ORIGEM     TO ME->PESO_ORIGEM.
        ADD <FS_VT>-PESO_CHEGADA    TO ME->PESO_CHEGADA.
        ADD <FS_VT>-ZVLR_MERCADORIA TO ME->ZVLR_MERCADORIA.
        ADD <FS_VT>-ZPESO_DIFERENCA TO ME->ZPESO_DIFERENCA.
        ADD <FS_VT>-ZQUEBRA         TO ME->ZQUEBRA.
        ADD <FS_VT>-ZPERDA          TO ME->ZPERDA.
        ADD <FS_VT>-ZVLR_QUEBRA     TO ME->ZVLR_QUEBRA.
        ADD <FS_VT>-ZVLR_PERDA      TO ME->ZVLR_PERDA.
        ADD <FS_VT>-ZVLR_VI         TO ME->ZVLR_VI.
        ADD <FS_VT>-ZVLR_LIQ_PAGAR  TO ME->ZVLR_LIQ_PAGAR.
      ENDLOOP.

    ELSE.

      CLEAR: ME->ZVLR_MERCADORIA,
             ME->PESO_ORIGEM,
             ME->PESO_CHEGADA,
             ME->ZPESO_DIFERENCA,
             ME->ZQUEBRA,
             ME->ZPERDA,
             ME->ZVLR_QUEBRA,
             ME->ZVLR_PERDA,
             ME->ZVLR_VI,
             ME->ZVLR_FRETE,
             ME->ZVLR_LIQ_PAGAR.

      LOOP AT ME->IT_VT ASSIGNING <FS_VT>.

        CLEAR: <FS_VT>-PESO_ORIGEM,
               <FS_VT>-PESO_CHEGADA,
               <FS_VT>-ZVLR_MERCADORIA,
               <FS_VT>-ZPESO_DIFERENCA,
               <FS_VT>-ZQUEBRA,
               <FS_VT>-ZPERDA,
               <FS_VT>-ZVLR_QUEBRA,
               <FS_VT>-ZVLR_PERDA,
               <FS_VT>-ZVLR_VI,
               <FS_VT>-ZVLR_FRETE,
               <FS_VT>-ZVLR_LIQ_PAGAR.

        READ TABLE ME->IT_N55 ASSIGNING <FS_N55> WITH KEY TKNUM = <FS_VT>-TKNUM.
        IF SY-SUBRC IS INITIAL.
          CLEAR: <FS_N55>-PESO_ORIGEM,
                 <FS_N55>-PESO_CHEGADA,
                 <FS_N55>-ZVLR_MERCADORIA,
                 <FS_N55>-ZPESO_DIFERENCA,
                 <FS_N55>-ZQUEBRA,
                 <FS_N55>-ZPERDA,
                 <FS_N55>-ZVLR_QUEBRA,
                 <FS_N55>-ZVLR_PERDA,
                 <FS_N55>-ZVLR_VI,
                 <FS_N55>-ZVLR_FRETE,
                 <FS_N55>-ZVLR_LIQ_PAGAR.
          READ TABLE ME->IT_NIT ASSIGNING <FS_NIT> WITH KEY DOCNUM = <FS_N55>-DOCNUM_NFE.
          IF SY-SUBRC IS INITIAL.
            ADD <FS_NIT>-PESO_ORIGEM     TO <FS_N55>-PESO_ORIGEM.
            ADD <FS_NIT>-PESO_CHEGADA    TO <FS_N55>-PESO_CHEGADA.
            ADD <FS_NIT>-ZVLR_MERCADORIA TO <FS_N55>-ZVLR_MERCADORIA.
            ADD <FS_NIT>-ZPESO_DIFERENCA TO <FS_N55>-ZPESO_DIFERENCA.
            ADD <FS_NIT>-ZQUEBRA         TO <FS_N55>-ZQUEBRA.
            ADD <FS_NIT>-ZPERDA          TO <FS_N55>-ZPERDA.
            ADD <FS_NIT>-ZVLR_QUEBRA     TO <FS_N55>-ZVLR_QUEBRA.
            ADD <FS_NIT>-ZVLR_PERDA      TO <FS_N55>-ZVLR_PERDA.
            ADD <FS_NIT>-ZVLR_VI         TO <FS_N55>-ZVLR_VI.
            ADD <FS_NIT>-ZVLR_FRETE      TO <FS_N55>-ZVLR_FRETE.
            ADD <FS_NIT>-ZVLR_LIQ_PAGAR  TO <FS_N55>-ZVLR_LIQ_PAGAR.

            ADD <FS_N55>-PESO_ORIGEM     TO <FS_VT>-PESO_ORIGEM.
            ADD <FS_N55>-PESO_CHEGADA    TO <FS_VT>-PESO_CHEGADA.
            ADD <FS_N55>-ZVLR_MERCADORIA TO <FS_VT>-ZVLR_MERCADORIA.
            ADD <FS_N55>-ZPESO_DIFERENCA TO <FS_VT>-ZPESO_DIFERENCA.
            ADD <FS_N55>-ZQUEBRA         TO <FS_VT>-ZQUEBRA.
            ADD <FS_N55>-ZPERDA          TO <FS_VT>-ZPERDA.
            ADD <FS_N55>-ZVLR_QUEBRA     TO <FS_VT>-ZVLR_QUEBRA.
            ADD <FS_N55>-ZVLR_PERDA      TO <FS_VT>-ZVLR_PERDA.
            ADD <FS_N55>-ZVLR_VI         TO <FS_VT>-ZVLR_VI.
            ADD <FS_N55>-ZVLR_FRETE      TO <FS_VT>-ZVLR_FRETE.
            ADD <FS_N55>-ZVLR_LIQ_PAGAR  TO <FS_VT>-ZVLR_LIQ_PAGAR.
          ENDIF.

          LOOP AT ME->IT_N55 ASSIGNING <FS_N55B> WHERE TKNUM = <FS_VT>-TKNUM AND DOCNUM_NFE NE <FS_N55>-DOCNUM_NFE.
            ADD <FS_N55B>-PESO_ORIGEM     TO <FS_N55>-PESO_ORIGEM.
            ADD <FS_N55B>-PESO_CHEGADA    TO <FS_N55>-PESO_CHEGADA.
            ADD <FS_N55B>-ZVLR_MERCADORIA TO <FS_N55>-ZVLR_MERCADORIA.
            ADD <FS_N55B>-ZPESO_DIFERENCA TO <FS_N55>-ZPESO_DIFERENCA.
            ADD <FS_N55B>-ZQUEBRA         TO <FS_N55>-ZQUEBRA.
            ADD <FS_N55B>-ZPERDA          TO <FS_N55>-ZPERDA.
            ADD <FS_N55B>-ZVLR_QUEBRA     TO <FS_N55>-ZVLR_QUEBRA.
            ADD <FS_N55B>-ZVLR_PERDA      TO <FS_N55>-ZVLR_PERDA.
            ADD <FS_N55B>-ZVLR_VI         TO <FS_N55>-ZVLR_VI.
            ADD <FS_N55B>-ZVLR_FRETE      TO <FS_N55>-ZVLR_FRETE.
            ADD <FS_N55B>-ZVLR_LIQ_PAGAR  TO <FS_N55>-ZVLR_LIQ_PAGAR.
          ENDLOOP.

        ELSE.
          CLEAR: <FS_N01>-PESO_ORIGEM,
                 <FS_N01>-PESO_CHEGADA,
                 <FS_N01>-ZVLR_MERCADORIA,
                 <FS_N01>-ZPESO_DIFERENCA,
                 <FS_N01>-ZQUEBRA,
                 <FS_N01>-ZPERDA,
                 <FS_N01>-ZVLR_QUEBRA,
                 <FS_N01>-ZVLR_PERDA,
                 <FS_N01>-ZVLR_VI,
                 <FS_N01>-ZVLR_FRETE,
                 <FS_N01>-ZVLR_LIQ_PAGAR.
          READ TABLE ME->IT_N01 ASSIGNING <FS_N01> WITH KEY TKNUM = <FS_VT>-TKNUM.
          IF SY-SUBRC IS INITIAL.
            READ TABLE ME->IT_NIT ASSIGNING <FS_NIT> WITH KEY DOCNUM = <FS_N01>-DOCNUM_NF.
            IF SY-SUBRC IS INITIAL.
              ADD <FS_NIT>-PESO_ORIGEM     TO <FS_N01>-PESO_ORIGEM.
              ADD <FS_NIT>-PESO_CHEGADA    TO <FS_N01>-PESO_CHEGADA.
              ADD <FS_NIT>-ZVLR_MERCADORIA TO <FS_N01>-ZVLR_MERCADORIA.
              ADD <FS_NIT>-ZPESO_DIFERENCA TO <FS_N01>-ZPESO_DIFERENCA.
              ADD <FS_NIT>-ZQUEBRA         TO <FS_N01>-ZQUEBRA.
              ADD <FS_NIT>-ZPERDA          TO <FS_N01>-ZPERDA.
              ADD <FS_NIT>-ZVLR_QUEBRA     TO <FS_N01>-ZVLR_QUEBRA.
              ADD <FS_NIT>-ZVLR_PERDA      TO <FS_N01>-ZVLR_PERDA.
              ADD <FS_NIT>-ZVLR_VI         TO <FS_N01>-ZVLR_VI.
              ADD <FS_NIT>-ZVLR_FRETE      TO <FS_N01>-ZVLR_FRETE.
              ADD <FS_NIT>-ZVLR_LIQ_PAGAR  TO <FS_N01>-ZVLR_LIQ_PAGAR.

              ADD <FS_N01>-PESO_ORIGEM     TO <FS_VT>-PESO_ORIGEM.
              ADD <FS_N01>-PESO_CHEGADA    TO <FS_VT>-PESO_CHEGADA.
              ADD <FS_N01>-ZVLR_MERCADORIA TO <FS_VT>-ZVLR_MERCADORIA.
              ADD <FS_N01>-ZPESO_DIFERENCA TO <FS_VT>-ZPESO_DIFERENCA.
              ADD <FS_N01>-ZQUEBRA         TO <FS_VT>-ZQUEBRA.
              ADD <FS_N01>-ZPERDA          TO <FS_VT>-ZPERDA.
              ADD <FS_N01>-ZVLR_QUEBRA     TO <FS_VT>-ZVLR_QUEBRA.
              ADD <FS_N01>-ZVLR_PERDA      TO <FS_VT>-ZVLR_PERDA.
              ADD <FS_N01>-ZVLR_VI         TO <FS_VT>-ZVLR_VI.
              ADD <FS_N01>-ZVLR_FRETE      TO <FS_VT>-ZVLR_FRETE.
              ADD <FS_N01>-ZVLR_LIQ_PAGAR  TO <FS_VT>-ZVLR_LIQ_PAGAR.
            ENDIF.
            LOOP AT ME->IT_N01 ASSIGNING <FS_N01B> WHERE TKNUM = <FS_VT>-TKNUM AND DOCNUM_NF NE <FS_N01>-DOCNUM_NF.
              ADD <FS_N01B>-PESO_ORIGEM     TO <FS_N01>-PESO_ORIGEM.
              ADD <FS_N01B>-PESO_CHEGADA    TO <FS_N01>-PESO_CHEGADA.
              ADD <FS_N01B>-ZVLR_MERCADORIA TO <FS_N01>-ZVLR_MERCADORIA.
              ADD <FS_N01B>-ZPESO_DIFERENCA TO <FS_N01>-ZPESO_DIFERENCA.
              ADD <FS_N01B>-ZQUEBRA         TO <FS_N01>-ZQUEBRA.
              ADD <FS_N01B>-ZPERDA          TO <FS_N01>-ZPERDA.
              ADD <FS_N01B>-ZVLR_QUEBRA     TO <FS_N01>-ZVLR_QUEBRA.
              ADD <FS_N01B>-ZVLR_PERDA      TO <FS_N01>-ZVLR_PERDA.
              ADD <FS_N01B>-ZVLR_VI         TO <FS_N01>-ZVLR_VI.
              ADD <FS_N01B>-ZVLR_FRETE      TO <FS_N01>-ZVLR_FRETE.
              ADD <FS_N01B>-ZVLR_LIQ_PAGAR  TO <FS_N01>-ZVLR_LIQ_PAGAR.
            ENDLOOP.
          ENDIF.
        ENDIF.
        ADD <FS_VT>-PESO_ORIGEM     TO ME->PESO_ORIGEM.
        ADD <FS_VT>-PESO_CHEGADA    TO ME->PESO_CHEGADA.
        ADD <FS_VT>-ZVLR_MERCADORIA TO ME->ZVLR_MERCADORIA.
        ADD <FS_VT>-ZPESO_DIFERENCA TO ME->ZPESO_DIFERENCA.
        ADD <FS_VT>-ZQUEBRA         TO ME->ZQUEBRA.
        ADD <FS_VT>-ZPERDA          TO ME->ZPERDA.
        ADD <FS_VT>-ZVLR_QUEBRA     TO ME->ZVLR_QUEBRA.
        ADD <FS_VT>-ZVLR_PERDA      TO ME->ZVLR_PERDA.
        ADD <FS_VT>-ZVLR_VI         TO ME->ZVLR_VI.
        ADD <FS_VT>-ZVLR_FRETE      TO ME->ZVLR_FRETE.
        ADD <FS_VT>-ZVLR_LIQ_PAGAR  TO ME->ZVLR_LIQ_PAGAR.
      ENDLOOP.
    ENDIF.

    "ME->CK_ALTERADO = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_TP_PROCESSO_CTE.
    ME->TP_PROCESSO_CTE = I_TP_PROCESSO_CTE.
  ENDMETHOD.


  METHOD SET_VALOR_BASE_ICMS.
    ME->VALOR_BASE_ICMS = I_VALOR_BASE_ICMS.
  ENDMETHOD.


  METHOD SET_VALOR_ICMS.
    ME->VALOR_ICMS = I_VALOR_ICMS.
  ENDMETHOD.


  METHOD SET_VALOR_PRESTACAO.
    ME->VALOR_PRESTACAO = I_VALOR_PRESTACAO.
  ENDMETHOD.


  METHOD SET_VALOR_RECEBER.
    ME->VALOR_RECEBER = I_VALOR_RECEBER.
  ENDMETHOD.


  METHOD SET_VL_TOTAL_MERC.
    ME->VL_TOTAL_MERC = I_VL_TOTAL_MERC.
  ENDMETHOD.


  METHOD SET_WAERK_VI.
    ME->WAERK_VI = I_WAERK_VI.
  ENDMETHOD.


  METHOD SET_ZBASE_COFINS.
    ME->ZBASE_COFINS = I_ZBASE_COFINS.
  ENDMETHOD.


  METHOD SET_ZBASE_ICMS.
    ME->ZBASE_ICMS = I_ZBASE_ICMS.
  ENDMETHOD.


  METHOD SET_ZBASE_PIS.
    ME->ZBASE_PIS = I_ZBASE_PIS.
  ENDMETHOD.


  METHOD SET_ZBVTYP.
    IF ME->ZBVTYP NE I_ZBVTYP.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.
    ME->ZBVTYP = I_ZBVTYP.
  ENDMETHOD.


  METHOD SET_ZDT_MOV.
    IF ME->ZDT_MOV NE I_ZDT_MOV.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.
    ME->ZDT_MOV = I_ZDT_MOV.
  ENDMETHOD.


  METHOD SET_ZDT_VENCTO.
    IF ME->ZDT_VENCTO NE I_ZDT_VENCTO.
      ME->CK_ALTERADO = ABAP_TRUE.
    ENDIF.
    ME->ZDT_VENCTO = I_ZDT_VENCTO.
  ENDMETHOD.


  METHOD SET_ZPERDA.
    ME->ZPERDA = I_ZPERDA.
  ENDMETHOD.


  METHOD SET_ZPESO_DIFERENCA.
    ME->ZPESO_DIFERENCA = I_ZPESO_DIFERENCA.
  ENDMETHOD.


  METHOD SET_ZQUEBRA.
    ME->ZQUEBRA = I_ZQUEBRA.
  ENDMETHOD.


  METHOD SET_ZRATE_COFINS.
    ME->ZRATE_COFINS = I_ZRATE_COFINS.
  ENDMETHOD.


  METHOD SET_ZRATE_ICMS.
    ME->ZRATE_ICMS = I_ZRATE_ICMS.
  ENDMETHOD.


  METHOD SET_ZRATE_PIS.
    ME->ZRATE_PIS = I_ZRATE_PIS.
  ENDMETHOD.


  METHOD SET_ZVALOR_COFINS.
    ME->ZVALOR_COFINS = I_ZVALOR_COFINS.
  ENDMETHOD.


  METHOD SET_ZVALOR_ICMS.
    ME->ZVALOR_ICMS = I_ZVALOR_ICMS.
  ENDMETHOD.


  METHOD SET_ZVALOR_PEDAGIO.
    ME->ZVALOR_PEDAGIO = I_ZVALOR_PEDAGIO.
  ENDMETHOD.


  METHOD SET_ZVALOR_PIS.
    ME->ZVALOR_PIS = I_ZVALOR_PIS.
  ENDMETHOD.


  METHOD SET_ZVLR_FRETE.
    ME->ZVLR_FRETE = I_ZVLR_FRETE.
  ENDMETHOD.


  METHOD SET_ZVLR_LIQ_PAGAR.
    ME->ZVLR_LIQ_PAGAR = I_ZVLR_LIQ_PAGAR.
  ENDMETHOD.


  METHOD SET_ZVLR_MERCADORIA.
    ME->ZVLR_MERCADORIA = I_ZVLR_MERCADORIA.
  ENDMETHOD.


  METHOD SET_ZVLR_PERDA.
    ME->ZVLR_PERDA = I_ZVLR_PERDA.
  ENDMETHOD.


  METHOD SET_ZVLR_QUEBRA.
    ME->ZVLR_QUEBRA = I_ZVLR_QUEBRA.
  ENDMETHOD.


  METHOD SET_ZVLR_VI.
    ME->ZVLR_VI = I_ZVLR_VI.
  ENDMETHOD.


  METHOD ZIF_CADASTRO~EXCLUIR_REGISTRO.

    I_EXCLUIU = ABAP_FALSE.

    IF ME->ZIF_CADASTRO~VALIDAR_EXCLUSAO( ) EQ ABAP_TRUE.

*      DELETE .......
*      COMMIT WORK.
*      I_EXCLUIU = ABAP_TRUE.
*      MESSAGE SXXX.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GET_REGISTRO.

    DATA: WA_REGISTRO TYPE ZIB_CTE_DIST_TER.
    WA_REGISTRO-CD_CHAVE_CTE          = ME->CD_CHAVE_CTE.
    WA_REGISTRO-DOCNUM_CTE            = ME->DOCNUM_CTE.
    WA_REGISTRO-CK_FINALIZADO         = ME->CK_FINALIZADO.
    WA_REGISTRO-CD_STATUS_DIST        = ME->CD_STATUS_DIST.
    WA_REGISTRO-DS_STATUS_DIST        = ME->DS_STATUS_DIST.
    WA_REGISTRO-CD_STATUS_SEFAZ       = ME->CD_STATUS_SEFAZ.
    WA_REGISTRO-NUMR_CTE              = ME->NUMR_CTE.
    WA_REGISTRO-NUMR_SERIE            = ME->NUMR_SERIE.
    WA_REGISTRO-DT_EMISSAO            = ME->DT_EMISSAO.
    WA_REGISTRO-HR_EMISSAO            = ME->HR_EMISSAO.
    WA_REGISTRO-VALOR_PRESTACAO       = ME->VALOR_PRESTACAO.
    WA_REGISTRO-VALOR_RECEBER         = ME->VALOR_RECEBER.
    WA_REGISTRO-VALOR_BASE_ICMS       = ME->VALOR_BASE_ICMS.
    WA_REGISTRO-VALOR_ICMS            = ME->VALOR_ICMS.
    WA_REGISTRO-CST_ICMS              = ME->CST_ICMS.
    WA_REGISTRO-CD_TOMADOR            = ME->CD_TOMADOR.
    WA_REGISTRO-DS_TOMADOR            = ME->DS_TOMADOR.
    WA_REGISTRO-EMIT_TP_DOC           = ME->EMIT_TP_DOC.
    WA_REGISTRO-EMIT_CNPJ             = ME->EMIT_CNPJ.
    WA_REGISTRO-EMIT_CPF              = ME->EMIT_CPF.
    WA_REGISTRO-EMIT_IE               = ME->EMIT_IE.
    WA_REGISTRO-EMIT_RSOCIAL          = ME->EMIT_RSOCIAL.
    WA_REGISTRO-EMIT_FANTASIA         = ME->EMIT_FANTASIA.
    WA_REGISTRO-REME_TP_DOC           = ME->REME_TP_DOC.
    WA_REGISTRO-REME_CNPJ             = ME->REME_CNPJ.
    WA_REGISTRO-REME_CPF              = ME->REME_CPF.
    WA_REGISTRO-REME_IE               = ME->REME_IE.
    WA_REGISTRO-REME_RSOCIAL          = ME->REME_RSOCIAL.
    WA_REGISTRO-REME_FANTASIA         = ME->REME_FANTASIA.
    WA_REGISTRO-EXPED_TP_DOC          = ME->EXPED_TP_DOC.
    WA_REGISTRO-EXPED_CNPJ            = ME->EXPED_CNPJ.
    WA_REGISTRO-EXPED_CPF             = ME->EXPED_CPF.
    WA_REGISTRO-EXPED_IE              = ME->EXPED_IE.
    WA_REGISTRO-EXPED_RSOCIAL         = ME->EXPED_RSOCIAL.
    WA_REGISTRO-EXPED_FANTASIA        = ME->EXPED_FANTASIA.
    WA_REGISTRO-RECEB_TP_DOC          = ME->RECEB_TP_DOC.
    WA_REGISTRO-RECEB_CNPJ            = ME->RECEB_CNPJ.
    WA_REGISTRO-RECEB_CPF             = ME->RECEB_CPF.
    WA_REGISTRO-RECEB_IE              = ME->RECEB_IE.
    WA_REGISTRO-RECEB_RSOCIAL         = ME->RECEB_RSOCIAL.
    WA_REGISTRO-RECEB_FANTASIA        = ME->RECEB_FANTASIA.
    WA_REGISTRO-DEST_TP_DOC           = ME->DEST_TP_DOC.
    WA_REGISTRO-DEST_CNPJ             = ME->DEST_CNPJ.
    WA_REGISTRO-DEST_CPF              = ME->DEST_CPF.
    WA_REGISTRO-DEST_IE               = ME->DEST_IE.
    WA_REGISTRO-DEST_RSOCIAL          = ME->DEST_RSOCIAL.
    WA_REGISTRO-DEST_FANTASIA         = ME->DEST_FANTASIA.
    WA_REGISTRO-TOMA4_TP_DOC          = ME->TOMA4_TP_DOC.
    WA_REGISTRO-TOMA4_CNPJ            = ME->TOMA4_CNPJ.
    WA_REGISTRO-TOMA4_CPF             = ME->TOMA4_CPF.
    WA_REGISTRO-TOMA4_IE              = ME->TOMA4_IE.
    WA_REGISTRO-TOMA4_RSOCIAL         = ME->TOMA4_RSOCIAL.
    WA_REGISTRO-TOMA4_FANTASIA        = ME->TOMA4_FANTASIA.
    WA_REGISTRO-MODELO                = ME->MODELO.
    WA_REGISTRO-CD_MODAL              = ME->CD_MODAL.
    WA_REGISTRO-DS_MODAL              = ME->DS_MODAL.
    WA_REGISTRO-CD_TIPO_SERVICO       = ME->CD_TIPO_SERVICO.
    WA_REGISTRO-DS_TIPO_SERVICO       = ME->DS_TIPO_SERVICO.
    WA_REGISTRO-CD_TIPO_CTE           = ME->CD_TIPO_CTE.
    WA_REGISTRO-DS_TIPO_CTE           = ME->DS_TIPO_CTE.
    WA_REGISTRO-CD_FPAGAMENTO         = ME->CD_FPAGAMENTO.
    WA_REGISTRO-DS_FPAGAMENTO         = ME->DS_FPAGAMENTO.
    WA_REGISTRO-CD_FEMISSAO           = ME->CD_FEMISSAO.
    WA_REGISTRO-DS_FEMISSAO           = ME->DS_FEMISSAO.
    WA_REGISTRO-CD_APLICATIVO         = ME->CD_APLICATIVO.
    WA_REGISTRO-DS_APLICATIVO         = ME->DS_APLICATIVO.
    WA_REGISTRO-CODG_CFOP             = ME->CODG_CFOP.
    WA_REGISTRO-INICIO_IBGE           = ME->INICIO_IBGE.
    WA_REGISTRO-INICIO_MUNI           = ME->INICIO_MUNI.
    WA_REGISTRO-INICIO_UF             = ME->INICIO_UF.
    WA_REGISTRO-TERMINO_IBGE          = ME->TERMINO_IBGE.
    WA_REGISTRO-TERMINO_MUNI          = ME->TERMINO_MUNI.
    WA_REGISTRO-TERMINO_UF            = ME->TERMINO_UF.
    WA_REGISTRO-NR_PROTOCOLO          = ME->NR_PROTOCOLO.
    WA_REGISTRO-DT_PROTOCOLO          = ME->DT_PROTOCOLO.
    WA_REGISTRO-HR_PROTOCOLO          = ME->HR_PROTOCOLO.
    WA_REGISTRO-DOCSTA                = ME->DOCSTA.
    WA_REGISTRO-CANCEL                = ME->CANCEL.
    WA_REGISTRO-REGIO                 = ME->REGIO.
    WA_REGISTRO-NFYEAR                = ME->NFYEAR.
    WA_REGISTRO-NFMONTH               = ME->NFMONTH.
    WA_REGISTRO-DOCNUM9               = ME->DOCNUM9.
    WA_REGISTRO-CDV                   = ME->CDV.
    WA_REGISTRO-DS_PROD_PRED          = ME->DS_PROD_PRED.
    WA_REGISTRO-QT_CARGA_CTE          = ME->QT_CARGA_CTE.
    WA_REGISTRO-VL_TOTAL_MERC         = ME->VL_TOTAL_MERC.
    WA_REGISTRO-RG_LIDO_PAG_FRET      = ME->RG_LIDO_PAG_FRET.
    WA_REGISTRO-TP_PROCESSO_CTE       = ME->TP_PROCESSO_CTE.
    WA_REGISTRO-E_TOMADORA            = ME->E_TOMADORA.
    WA_REGISTRO-F_TOMADORA            = ME->F_TOMADORA.
    WA_REGISTRO-P_EMISSOR             = ME->P_EMISSOR.
    WA_REGISTRO-E_EMISSOR             = ME->E_EMISSOR.
    WA_REGISTRO-F_EMISSOR             = ME->F_EMISSOR.
    WA_REGISTRO-DOCNUM_CTE_C          = ME->DOCNUM_CTE_C.
    WA_REGISTRO-DOCNUM_CTE_A          = ME->DOCNUM_CTE_A.
    WA_REGISTRO-DOCNUM_CTE_S          = ME->DOCNUM_CTE_S.
    WA_REGISTRO-EBELN                 = ME->EBELN.
    WA_REGISTRO-EBELP                 = ME->EBELP.
    WA_REGISTRO-MWSKZ                 = ME->MWSKZ.
    WA_REGISTRO-BELNR                 = ME->BELNR.
    WA_REGISTRO-GJAHR                 = ME->GJAHR.
    WA_REGISTRO-DOCNUM_CTE_SUB        = ME->DOCNUM_CTE_SUB.
    WA_REGISTRO-CK_MANUAL             = ME->CK_MANUAL.
    WA_REGISTRO-WAERK_VI              = ME->WAERK_VI.
    WA_REGISTRO-KURSK_VI              = ME->KURSK_VI.
    WA_REGISTRO-ZVLR_VI               = ME->ZVLR_VI.
    WA_REGISTRO-ZVLR_FRETE            = ME->ZVLR_FRETE.
    WA_REGISTRO-ZVLR_MERCADORIA       = ME->ZVLR_MERCADORIA.
    WA_REGISTRO-PESO_ORIGEM           = ME->PESO_ORIGEM.
    WA_REGISTRO-PESO_CHEGADA          = ME->PESO_CHEGADA.
    WA_REGISTRO-DT_CHEGADA            = ME->DT_CHEGADA.
    WA_REGISTRO-ZDT_MOV               = ME->ZDT_MOV.
    WA_REGISTRO-ZDT_VENCTO            = ME->ZDT_VENCTO.
    WA_REGISTRO-ZPESO_DIFERENCA       = ME->ZPESO_DIFERENCA.
    WA_REGISTRO-ZQUEBRA               = ME->ZQUEBRA.
    WA_REGISTRO-ZPERDA                = ME->ZPERDA.
    WA_REGISTRO-ZVLR_QUEBRA           = ME->ZVLR_QUEBRA.
    WA_REGISTRO-ZVLR_PERDA            = ME->ZVLR_PERDA.
    WA_REGISTRO-ZVLR_LIQ_PAGAR        = ME->ZVLR_LIQ_PAGAR.
    WA_REGISTRO-ZBVTYP                = ME->ZBVTYP.
    WA_REGISTRO-MATNS                 = ME->MATNS.
    WA_REGISTRO-ZBASE_ICMS            = ME->ZBASE_ICMS.
    WA_REGISTRO-ZBASE_PIS             = ME->ZBASE_PIS.
    WA_REGISTRO-ZBASE_COFINS          = ME->ZBASE_COFINS.
    WA_REGISTRO-ZRATE_ICMS            = ME->ZRATE_ICMS.
    WA_REGISTRO-ZRATE_PIS             = ME->ZRATE_PIS.
    WA_REGISTRO-ZRATE_COFINS          = ME->ZRATE_COFINS.
    WA_REGISTRO-ZVALOR_ICMS           = ME->ZVALOR_ICMS.
    WA_REGISTRO-ZVALOR_PIS            = ME->ZVALOR_PIS.
    WA_REGISTRO-ZVALOR_COFINS         = ME->ZVALOR_COFINS.
    WA_REGISTRO-ZVALOR_PEDAGIO        = ME->ZVALOR_PEDAGIO.
    WA_REGISTRO-CK_PESO_CHEGADA       = ME->CK_PESO_CHEGADA.
    WA_REGISTRO-TIMESTAMP             = ME->TIMESTAMP.
    WA_REGISTRO-CK_AUTORIZADO         = ME->CK_AUTORIZADO.
    MOVE-CORRESPONDING WA_REGISTRO TO E_REGISTRO.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GRAVAR_REGISTRO.

    DATA: WA_REGISTRO TYPE ZIB_CTE_DIST_TER,
          WA_CTE_N55  TYPE ZIB_CTE_DIST_N55,
          WA_CTE_N01  TYPE ZIB_CTE_DIST_N01,
          WA_CTE_NIT  TYPE ZIB_CTE_DIST_NIT.

    I_GRAVOU = ABAP_FALSE.

    IF ME->CK_ALTERADO EQ ABAP_TRUE.
      IF ME->ZIF_CADASTRO~VALIDAR_REGISTRO( ) EQ ABAP_TRUE.

        WA_REGISTRO-CD_CHAVE_CTE          = ME->CD_CHAVE_CTE.
        WA_REGISTRO-DOCNUM_CTE            = ME->DOCNUM_CTE.
        WA_REGISTRO-CK_FINALIZADO         = ME->CK_FINALIZADO.
        WA_REGISTRO-CD_STATUS_DIST        = ME->CD_STATUS_DIST.
        WA_REGISTRO-DS_STATUS_DIST        = ME->DS_STATUS_DIST.
        WA_REGISTRO-CD_STATUS_SEFAZ       = ME->CD_STATUS_SEFAZ.
        WA_REGISTRO-NUMR_CTE              = ME->NUMR_CTE.
        WA_REGISTRO-NUMR_SERIE            = ME->NUMR_SERIE.
        WA_REGISTRO-DT_EMISSAO            = ME->DT_EMISSAO.
        WA_REGISTRO-HR_EMISSAO            = ME->HR_EMISSAO.
        WA_REGISTRO-VALOR_PRESTACAO       = ME->VALOR_PRESTACAO.
        WA_REGISTRO-VALOR_RECEBER         = ME->VALOR_RECEBER.
        WA_REGISTRO-VALOR_BASE_ICMS       = ME->VALOR_BASE_ICMS.
        WA_REGISTRO-VALOR_ICMS            = ME->VALOR_ICMS.
        WA_REGISTRO-CST_ICMS              = ME->CST_ICMS.
        WA_REGISTRO-CD_TOMADOR            = ME->CD_TOMADOR.
        WA_REGISTRO-DS_TOMADOR            = ME->DS_TOMADOR.
        WA_REGISTRO-EMIT_TP_DOC           = ME->EMIT_TP_DOC.
        WA_REGISTRO-EMIT_CNPJ             = ME->EMIT_CNPJ.
        WA_REGISTRO-EMIT_CPF              = ME->EMIT_CPF.
        WA_REGISTRO-EMIT_IE               = ME->EMIT_IE.
        WA_REGISTRO-EMIT_RSOCIAL          = ME->EMIT_RSOCIAL.
        WA_REGISTRO-EMIT_FANTASIA         = ME->EMIT_FANTASIA.
        WA_REGISTRO-REME_TP_DOC           = ME->REME_TP_DOC.
        WA_REGISTRO-REME_CNPJ             = ME->REME_CNPJ.
        WA_REGISTRO-REME_CPF              = ME->REME_CPF.
        WA_REGISTRO-REME_IE               = ME->REME_IE.
        WA_REGISTRO-REME_RSOCIAL          = ME->REME_RSOCIAL.
        WA_REGISTRO-REME_FANTASIA         = ME->REME_FANTASIA.
        WA_REGISTRO-EXPED_TP_DOC          = ME->EXPED_TP_DOC.
        WA_REGISTRO-EXPED_CNPJ            = ME->EXPED_CNPJ.
        WA_REGISTRO-EXPED_CPF             = ME->EXPED_CPF.
        WA_REGISTRO-EXPED_IE              = ME->EXPED_IE.
        WA_REGISTRO-EXPED_RSOCIAL         = ME->EXPED_RSOCIAL.
        WA_REGISTRO-EXPED_FANTASIA        = ME->EXPED_FANTASIA.
        WA_REGISTRO-RECEB_TP_DOC          = ME->RECEB_TP_DOC.
        WA_REGISTRO-RECEB_CNPJ            = ME->RECEB_CNPJ.
        WA_REGISTRO-RECEB_CPF             = ME->RECEB_CPF.
        WA_REGISTRO-RECEB_IE              = ME->RECEB_IE.
        WA_REGISTRO-RECEB_RSOCIAL         = ME->RECEB_RSOCIAL.
        WA_REGISTRO-RECEB_FANTASIA        = ME->RECEB_FANTASIA.
        WA_REGISTRO-DEST_TP_DOC           = ME->DEST_TP_DOC.
        WA_REGISTRO-DEST_CNPJ             = ME->DEST_CNPJ.
        WA_REGISTRO-DEST_CPF              = ME->DEST_CPF.
        WA_REGISTRO-DEST_IE               = ME->DEST_IE.
        WA_REGISTRO-DEST_RSOCIAL          = ME->DEST_RSOCIAL.
        WA_REGISTRO-DEST_FANTASIA         = ME->DEST_FANTASIA.
        WA_REGISTRO-TOMA4_TP_DOC          = ME->TOMA4_TP_DOC.
        WA_REGISTRO-TOMA4_CNPJ            = ME->TOMA4_CNPJ.
        WA_REGISTRO-TOMA4_CPF             = ME->TOMA4_CPF.
        WA_REGISTRO-TOMA4_IE              = ME->TOMA4_IE.
        WA_REGISTRO-TOMA4_RSOCIAL         = ME->TOMA4_RSOCIAL.
        WA_REGISTRO-TOMA4_FANTASIA        = ME->TOMA4_FANTASIA.
        WA_REGISTRO-MODELO                = ME->MODELO.
        WA_REGISTRO-CD_MODAL              = ME->CD_MODAL.
        WA_REGISTRO-DS_MODAL              = ME->DS_MODAL.
        WA_REGISTRO-CD_TIPO_SERVICO       = ME->CD_TIPO_SERVICO.
        WA_REGISTRO-DS_TIPO_SERVICO       = ME->DS_TIPO_SERVICO.
        WA_REGISTRO-CD_TIPO_CTE           = ME->CD_TIPO_CTE.
        WA_REGISTRO-DS_TIPO_CTE           = ME->DS_TIPO_CTE.
        WA_REGISTRO-CD_FPAGAMENTO         = ME->CD_FPAGAMENTO.
        WA_REGISTRO-DS_FPAGAMENTO         = ME->DS_FPAGAMENTO.
        WA_REGISTRO-CD_FEMISSAO           = ME->CD_FEMISSAO.
        WA_REGISTRO-DS_FEMISSAO           = ME->DS_FEMISSAO.
        WA_REGISTRO-CD_APLICATIVO         = ME->CD_APLICATIVO.
        WA_REGISTRO-DS_APLICATIVO         = ME->DS_APLICATIVO.
        WA_REGISTRO-CODG_CFOP             = ME->CODG_CFOP.
        WA_REGISTRO-INICIO_IBGE           = ME->INICIO_IBGE.
        WA_REGISTRO-INICIO_MUNI           = ME->INICIO_MUNI.
        WA_REGISTRO-INICIO_UF             = ME->INICIO_UF.
        WA_REGISTRO-TERMINO_IBGE          = ME->TERMINO_IBGE.
        WA_REGISTRO-TERMINO_MUNI          = ME->TERMINO_MUNI.
        WA_REGISTRO-TERMINO_UF            = ME->TERMINO_UF.
        WA_REGISTRO-NR_PROTOCOLO          = ME->NR_PROTOCOLO.
        WA_REGISTRO-DT_PROTOCOLO          = ME->DT_PROTOCOLO.
        WA_REGISTRO-HR_PROTOCOLO          = ME->HR_PROTOCOLO.
        WA_REGISTRO-DOCSTA                = ME->DOCSTA.
        WA_REGISTRO-CANCEL                = ME->CANCEL.
        WA_REGISTRO-REGIO                 = ME->REGIO.
        WA_REGISTRO-NFYEAR                = ME->NFYEAR.
        WA_REGISTRO-NFMONTH               = ME->NFMONTH.
        WA_REGISTRO-DOCNUM9               = ME->DOCNUM9.
        WA_REGISTRO-CDV                   = ME->CDV.
        WA_REGISTRO-DS_PROD_PRED          = ME->DS_PROD_PRED.
        WA_REGISTRO-QT_CARGA_CTE          = ME->QT_CARGA_CTE.
        WA_REGISTRO-VL_TOTAL_MERC         = ME->VL_TOTAL_MERC.
        WA_REGISTRO-RG_LIDO_PAG_FRET      = ME->RG_LIDO_PAG_FRET.
        WA_REGISTRO-TP_PROCESSO_CTE       = ME->TP_PROCESSO_CTE.
        WA_REGISTRO-E_TOMADORA            = ME->E_TOMADORA.
        WA_REGISTRO-F_TOMADORA            = ME->F_TOMADORA.
        WA_REGISTRO-P_EMISSOR             = ME->P_EMISSOR.
        WA_REGISTRO-E_EMISSOR             = ME->E_EMISSOR.
        WA_REGISTRO-F_EMISSOR             = ME->F_EMISSOR.
        WA_REGISTRO-DOCNUM_CTE_C          = ME->DOCNUM_CTE_C.
        WA_REGISTRO-DOCNUM_CTE_A          = ME->DOCNUM_CTE_A.
        WA_REGISTRO-DOCNUM_CTE_S          = ME->DOCNUM_CTE_S.
        WA_REGISTRO-EBELN                 = ME->EBELN.
        WA_REGISTRO-EBELP                 = ME->EBELP.
        WA_REGISTRO-MWSKZ                 = ME->MWSKZ.
        WA_REGISTRO-BELNR                 = ME->BELNR.
        WA_REGISTRO-GJAHR                 = ME->GJAHR.
        WA_REGISTRO-DOCNUM_CTE_SUB        = ME->DOCNUM_CTE_SUB.
        WA_REGISTRO-CK_MANUAL             = ME->CK_MANUAL.
        WA_REGISTRO-WAERK_VI              = ME->WAERK_VI.
        WA_REGISTRO-KURSK_VI              = ME->KURSK_VI.
        WA_REGISTRO-ZVLR_VI               = ME->ZVLR_VI.
        WA_REGISTRO-ZVLR_FRETE            = ME->ZVLR_FRETE.
        WA_REGISTRO-ZVLR_MERCADORIA       = ME->ZVLR_MERCADORIA.
        WA_REGISTRO-PESO_ORIGEM           = ME->PESO_ORIGEM.
        WA_REGISTRO-PESO_CHEGADA          = ME->PESO_CHEGADA.
        WA_REGISTRO-DT_CHEGADA            = ME->DT_CHEGADA.
        WA_REGISTRO-ZDT_MOV               = ME->ZDT_MOV.
        WA_REGISTRO-ZDT_VENCTO            = ME->ZDT_VENCTO.
        WA_REGISTRO-ZPESO_DIFERENCA       = ME->ZPESO_DIFERENCA.
        WA_REGISTRO-ZQUEBRA               = ME->ZQUEBRA.
        WA_REGISTRO-ZPERDA                = ME->ZPERDA.
        WA_REGISTRO-ZVLR_QUEBRA           = ME->ZVLR_QUEBRA.
        WA_REGISTRO-ZVLR_PERDA            = ME->ZVLR_PERDA.
        WA_REGISTRO-ZVLR_LIQ_PAGAR        = ME->ZVLR_LIQ_PAGAR.
        WA_REGISTRO-ZBVTYP                = ME->ZBVTYP.
        WA_REGISTRO-MATNS                 = ME->MATNS.

        "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE L~MATNR INTO @DATA(LC_MATNR)
          FROM J_1BNFLIN AS L
         INNER JOIN ZIB_CTE_DIST_N55 AS N ON N~DOCNUM_NFE EQ L~DOCNUM
         WHERE N~CD_CHAVE_CTE EQ @ME->CD_CHAVE_CTE
           AND N~DOCNUM_NFE   NE @SPACE.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE L~MATNR INTO LC_MATNR
            FROM J_1BNFLIN AS L
           INNER JOIN ZIB_CTE_DIST_N01 AS N ON N~DOCNUM_NF EQ L~DOCNUM
           WHERE N~CD_CHAVE_CTE EQ ME->CD_CHAVE_CTE
             AND N~DOCNUM_NF    NE SPACE.
        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        CALL METHOD ZCL_CTE_DIST_G=>BUSCA_IMPOSTOS_TAXAS
          EXPORTING
            P_IVA            = ME->MWSKZ
            P_DATA_DOCUMENTO = ME->DT_EMISSAO
            P_SHIPFROM       = ME->INICIO_UF
            P_SHIPTO         = ME->TERMINO_UF
            E_TOMADORA       = ME->E_TOMADORA
            F_TOMADORA       = ME->F_TOMADORA
            P_EMISSORA       = ME->P_EMISSOR
            P_MATNR          = LC_MATNR
          IMPORTING
            E_RATE_ICMS      = ME->ZRATE_ICMS
            E_RATE_PIS       = ME->ZRATE_PIS
            E_RATE_COFINS    = ME->ZRATE_COFINS
          EXCEPTIONS
            SEM_IVA          = 1
            OTHERS           = 2.

        IF SY-SUBRC IS INITIAL.
          IF ME->ZRATE_ICMS GT 0.
            ME->ZBASE_ICMS  = ME->ZVLR_FRETE.
            ME->ZVALOR_ICMS = ME->ZVLR_FRETE * ( ME->ZRATE_ICMS / 100 ).
          ELSE.
            ME->ZBASE_ICMS  = 0.
            ME->ZVALOR_ICMS = 0.
          ENDIF.

          DATA(LVA_BASE_CALC_PIS_COFINS) = ZCL_CTE_DIST_G=>get_base_pis_cofins(  i_valor_frete =   CONV #( ME->ZVLR_FRETE )
                                                                                 i_valor_icms  =   CONV #( ME->ZVALOR_ICMS ) ).

          IF ME->ZRATE_PIS GT 0.
            "US 110278 - WPP
            ME->ZBASE_PIS  = LVA_BASE_CALC_PIS_COFINS.
            ME->ZVALOR_PIS = LVA_BASE_CALC_PIS_COFINS * ( ME->ZRATE_PIS / 100 ).
            "ME->ZBASE_PIS  = ME->ZVLR_FRETE.
            "ME->ZVALOR_PIS = ME->ZVLR_FRETE * ( ME->ZRATE_PIS / 100 ).
            "US 110278 - WPP
          ELSE.
            ME->ZBASE_PIS  = 0.
            ME->ZVALOR_PIS = 0.
          ENDIF.

          IF ME->ZRATE_COFINS GT 0.
            "US 110278 - WPP
            ME->ZBASE_COFINS  = LVA_BASE_CALC_PIS_COFINS.
            ME->ZVALOR_COFINS = LVA_BASE_CALC_PIS_COFINS * ( ME->ZRATE_COFINS / 100 ).
            "ME->ZBASE_COFINS  = ME->ZVLR_FRETE.
            "ME->ZVALOR_COFINS = ME->ZVLR_FRETE * ( ME->ZRATE_COFINS / 100 ).
            "US 110278 - WPP
          ELSE.
            ME->ZBASE_COFINS  = 0.
            ME->ZVALOR_COFINS = 0.
          ENDIF.
        ENDIF.

        WA_REGISTRO-ZBASE_ICMS            = ME->ZBASE_ICMS.
        WA_REGISTRO-ZBASE_PIS             = ME->ZBASE_PIS.
        WA_REGISTRO-ZBASE_COFINS          = ME->ZBASE_COFINS.
        WA_REGISTRO-ZRATE_ICMS            = ME->ZRATE_ICMS.
        WA_REGISTRO-ZRATE_PIS             = ME->ZRATE_PIS.
        WA_REGISTRO-ZRATE_COFINS          = ME->ZRATE_COFINS.
        WA_REGISTRO-ZVALOR_ICMS           = ME->ZVALOR_ICMS.
        WA_REGISTRO-ZVALOR_PIS            = ME->ZVALOR_PIS.
        WA_REGISTRO-ZVALOR_COFINS         = ME->ZVALOR_COFINS.

        WA_REGISTRO-ZVALOR_PEDAGIO        = ME->ZVALOR_PEDAGIO.
        WA_REGISTRO-CK_PESO_CHEGADA       = ME->CK_PESO_CHEGADA.
        WA_REGISTRO-TIMESTAMP             = ME->TIMESTAMP.
        WA_REGISTRO-CK_AUTORIZADO         = ME->CK_AUTORIZADO.
        MODIFY ZIB_CTE_DIST_TER FROM WA_REGISTRO.

        LOOP AT IT_N01 INTO WA_CTE_N01.
          MODIFY ZIB_CTE_DIST_N01 FROM WA_CTE_N01.
        ENDLOOP.

        LOOP AT IT_N55 INTO WA_CTE_N55.
          MODIFY ZIB_CTE_DIST_N55 FROM WA_CTE_N55.
        ENDLOOP.

        LOOP AT IT_NIT INTO WA_CTE_NIT.
          MODIFY ZIB_CTE_DIST_NIT FROM WA_CTE_NIT.
        ENDLOOP.

        COMMIT WORK.
        ME->CK_ALTERADO = ABAP_FALSE.
        I_GRAVOU = ABAP_TRUE.
        MESSAGE S059.

      ENDIF.

    ELSE.
      I_GRAVOU = ABAP_TRUE.
      MESSAGE W182.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: ME->CD_CHAVE_CTE,
           ME->DOCNUM_CTE,
           ME->CK_FINALIZADO,
           ME->CD_STATUS_DIST,
           ME->DS_STATUS_DIST,
           ME->CD_STATUS_SEFAZ,
           ME->NUMR_CTE,
           ME->NUMR_SERIE,
           ME->DT_EMISSAO,
           ME->HR_EMISSAO,
           ME->VALOR_PRESTACAO,
           ME->VALOR_RECEBER,
           ME->VALOR_BASE_ICMS,
           ME->VALOR_ICMS,
           ME->CST_ICMS,
           ME->CD_TOMADOR,
           ME->DS_TOMADOR,
           ME->EMIT_TP_DOC,
           ME->EMIT_CNPJ,
           ME->EMIT_CPF,
           ME->EMIT_IE,
           ME->EMIT_RSOCIAL,
           ME->EMIT_FANTASIA,
           ME->REME_TP_DOC,
           ME->REME_CNPJ,
           ME->REME_CPF,
           ME->REME_IE,
           ME->REME_RSOCIAL,
           ME->REME_FANTASIA,
           ME->EXPED_TP_DOC,
           ME->EXPED_CNPJ,
           ME->EXPED_CPF,
           ME->EXPED_IE,
           ME->EXPED_RSOCIAL,
           ME->EXPED_FANTASIA,
           ME->RECEB_TP_DOC,
           ME->RECEB_CNPJ,
           ME->RECEB_CPF,
           ME->RECEB_IE,
           ME->RECEB_RSOCIAL,
           ME->RECEB_FANTASIA,
           ME->DEST_TP_DOC,
           ME->DEST_CNPJ,
           ME->DEST_CPF,
           ME->DEST_IE,
           ME->DEST_RSOCIAL,
           ME->DEST_FANTASIA,
           ME->TOMA4_TP_DOC,
           ME->TOMA4_CNPJ,
           ME->TOMA4_CPF,
           ME->TOMA4_IE,
           ME->TOMA4_RSOCIAL,
           ME->TOMA4_FANTASIA,
           ME->MODELO,
           ME->CD_MODAL,
           ME->DS_MODAL,
           ME->CD_TIPO_SERVICO,
           ME->DS_TIPO_SERVICO,
           ME->CD_TIPO_CTE,
           ME->DS_TIPO_CTE,
           ME->CD_FPAGAMENTO,
           ME->DS_FPAGAMENTO,
           ME->CD_FEMISSAO,
           ME->DS_FEMISSAO,
           ME->CD_APLICATIVO,
           ME->DS_APLICATIVO,
           ME->CODG_CFOP,
           ME->INICIO_IBGE,
           ME->INICIO_MUNI,
           ME->INICIO_UF,
           ME->TERMINO_IBGE,
           ME->TERMINO_MUNI,
           ME->TERMINO_UF,
           ME->NR_PROTOCOLO,
           ME->DT_PROTOCOLO,
           ME->HR_PROTOCOLO,
           ME->DOCSTA,
           ME->CANCEL,
           ME->REGIO,
           ME->NFYEAR,
           ME->NFMONTH,
           ME->DOCNUM9,
           ME->CDV,
           ME->DS_PROD_PRED,
           ME->QT_CARGA_CTE,
           ME->VL_TOTAL_MERC,
           ME->RG_LIDO_PAG_FRET,
           ME->TP_PROCESSO_CTE,
           ME->E_TOMADORA,
           ME->F_TOMADORA,
           ME->P_EMISSOR,
           ME->E_EMISSOR,
           ME->F_EMISSOR,
           ME->DOCNUM_CTE_C,
           ME->DOCNUM_CTE_A,
           ME->DOCNUM_CTE_S,
           ME->EBELN,
           ME->EBELP,
           ME->MWSKZ,
           ME->BELNR,
           ME->GJAHR,
           ME->DOCNUM_CTE_SUB,
           ME->CK_MANUAL,
           ME->WAERK_VI,
           ME->KURSK_VI,
           ME->ZVLR_VI,
           ME->ZVLR_FRETE,
           ME->ZVLR_MERCADORIA,
           ME->PESO_ORIGEM,
           ME->PESO_CHEGADA,
           ME->DT_CHEGADA,
           ME->ZDT_MOV,
           ME->ZDT_VENCTO,
           ME->ZPESO_DIFERENCA,
           ME->ZQUEBRA,
           ME->ZPERDA,
           ME->ZVLR_QUEBRA,
           ME->ZVLR_PERDA,
           ME->ZVLR_LIQ_PAGAR,
           ME->ZBVTYP,
           ME->MATNS,
           ME->ZBASE_ICMS,
           ME->ZBASE_PIS,
           ME->ZBASE_COFINS,
           ME->ZRATE_ICMS,
           ME->ZRATE_PIS,
           ME->ZRATE_COFINS,
           ME->ZVALOR_ICMS,
           ME->ZVALOR_PIS,
           ME->ZVALOR_COFINS,
           ME->ZVALOR_PEDAGIO,
           ME->CK_PESO_CHEGADA,
           ME->TIMESTAMP,
           ME->CK_AUTORIZADO,
           ME->IT_N01,
           ME->IT_N55,
           ME->IT_NIT.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.

    ME->ZIF_CADASTRO~LIMPAR_REGISTRO( ).

  ENDMETHOD.


  method zif_cadastro~set_registro.

    data: it_zib_n01 type table of zib_cte_dist_n01,
          it_zib_n55 type table of zib_cte_dist_n55,
          it_zib_nit type table of zib_cte_dist_nit,
          it_zib_dup type table of zib_cte_dist_dup.

    data: wa_zib_cte          type zib_cte_dist_ter,
          wa_zib_n01          type zib_cte_dist_n01,
          wa_zib_n55          type zib_cte_dist_n55,
          wa_zib_dup          type zib_cte_dist_dup,
          wa_zib_nit          type zib_cte_dist_nit,
          wa_zib_cte_dist_eap type zib_cte_dist_eap,
          lc_matnr            type matnr,
          lc_grupo            type matkl,
          lc_tipo             type zde_tp_aut_frete.

    me->zif_cadastro~limpar_registro( ).

    select single * into wa_zib_cte
      from zib_cte_dist_ter
     where cd_chave_cte = i_id_registro.

    if sy-subrc is initial.
      me->set_cd_chave_cte( exporting i_cd_chave_cte = wa_zib_cte-cd_chave_cte ).
      me->set_docnum_cte( exporting i_docnum_cte = wa_zib_cte-docnum_cte ).
      me->set_ck_finalizado( exporting i_ck_finalizado = wa_zib_cte-ck_finalizado ).
      me->set_cd_status_dist( exporting i_cd_status_dist = wa_zib_cte-cd_status_dist ).
      me->set_ds_status_dist( exporting i_ds_status_dist = wa_zib_cte-ds_status_dist ).
      me->set_cd_status_sefaz( exporting i_cd_status_sefaz = wa_zib_cte-cd_status_sefaz ).
      me->set_numr_cte( exporting i_numr_cte = wa_zib_cte-numr_cte ).
      me->set_numr_serie( exporting i_numr_serie = wa_zib_cte-numr_serie ).
      me->set_dt_emissao( exporting i_dt_emissao = wa_zib_cte-dt_emissao ).
      me->set_hr_emissao( exporting i_hr_emissao = wa_zib_cte-hr_emissao ).
      me->set_valor_prestacao( exporting i_valor_prestacao = wa_zib_cte-valor_prestacao ).
      me->set_valor_receber( exporting i_valor_receber = wa_zib_cte-valor_receber ).
      me->set_valor_base_icms( exporting i_valor_base_icms = wa_zib_cte-valor_base_icms ).
      me->set_valor_icms( exporting i_valor_icms = wa_zib_cte-valor_icms ).
      me->set_cst_icms( exporting i_cst_icms = wa_zib_cte-cst_icms ).
      me->set_cd_tomador( exporting i_cd_tomador = wa_zib_cte-cd_tomador ).
      me->set_ds_tomador( exporting i_ds_tomador = wa_zib_cte-ds_tomador ).
      me->set_emit_tp_doc( exporting i_emit_tp_doc = wa_zib_cte-emit_tp_doc ).
      me->set_emit_cnpj( exporting i_emit_cnpj = wa_zib_cte-emit_cnpj ).
      me->set_emit_cpf( exporting i_emit_cpf = wa_zib_cte-emit_cpf ).
      me->set_emit_ie( exporting i_emit_ie = wa_zib_cte-emit_ie ).
      me->set_emit_rsocial( exporting i_emit_rsocial = wa_zib_cte-emit_rsocial ).
      me->set_emit_fantasia( exporting i_emit_fantasia = wa_zib_cte-emit_fantasia ).
      me->set_reme_tp_doc( exporting i_reme_tp_doc = wa_zib_cte-reme_tp_doc ).
      me->set_reme_cnpj( exporting i_reme_cnpj = wa_zib_cte-reme_cnpj ).
      me->set_reme_cpf( exporting i_reme_cpf = wa_zib_cte-reme_cpf ).
      me->set_reme_ie( exporting i_reme_ie = wa_zib_cte-reme_ie ).
      me->set_reme_rsocial( exporting i_reme_rsocial = wa_zib_cte-reme_rsocial ).
      me->set_reme_fantasia( exporting i_reme_fantasia = wa_zib_cte-reme_fantasia ).
      me->set_exped_tp_doc( exporting i_exped_tp_doc = wa_zib_cte-exped_tp_doc ).
      me->set_exped_cnpj( exporting i_exped_cnpj = wa_zib_cte-exped_cnpj ).
      me->set_exped_cpf( exporting i_exped_cpf = wa_zib_cte-exped_cpf ).
      me->set_exped_ie( exporting i_exped_ie = wa_zib_cte-exped_ie ).
      me->set_exped_rsocial( exporting i_exped_rsocial = wa_zib_cte-exped_rsocial ).
      me->set_exped_fantasia( exporting i_exped_fantasia = wa_zib_cte-exped_fantasia ).
      me->set_receb_tp_doc( exporting i_receb_tp_doc = wa_zib_cte-receb_tp_doc ).
      me->set_receb_cnpj( exporting i_receb_cnpj = wa_zib_cte-receb_cnpj ).
      me->set_receb_cpf( exporting i_receb_cpf = wa_zib_cte-receb_cpf ).
      me->set_receb_ie( exporting i_receb_ie = wa_zib_cte-receb_ie ).
      me->set_receb_rsocial( exporting i_receb_rsocial = wa_zib_cte-receb_rsocial ).
      me->set_receb_fantasia( exporting i_receb_fantasia = wa_zib_cte-receb_fantasia ).
      me->set_dest_tp_doc( exporting i_dest_tp_doc = wa_zib_cte-dest_tp_doc ).
      me->set_dest_cnpj( exporting i_dest_cnpj = wa_zib_cte-dest_cnpj ).
      me->set_dest_cpf( exporting i_dest_cpf = wa_zib_cte-dest_cpf ).
      me->set_dest_ie( exporting i_dest_ie = wa_zib_cte-dest_ie ).
      me->set_dest_rsocial( exporting i_dest_rsocial = wa_zib_cte-dest_rsocial ).
      me->set_dest_fantasia( exporting i_dest_fantasia = wa_zib_cte-dest_fantasia ).
      me->set_toma4_tp_doc( exporting i_toma4_tp_doc = wa_zib_cte-toma4_tp_doc ).
      me->set_toma4_cnpj( exporting i_toma4_cnpj = wa_zib_cte-toma4_cnpj ).
      me->set_toma4_cpf( exporting i_toma4_cpf = wa_zib_cte-toma4_cpf ).
      me->set_toma4_ie( exporting i_toma4_ie = wa_zib_cte-toma4_ie ).
      me->set_toma4_rsocial( exporting i_toma4_rsocial = wa_zib_cte-toma4_rsocial ).
      me->set_toma4_fantasia( exporting i_toma4_fantasia = wa_zib_cte-toma4_fantasia ).
      me->set_modelo( exporting i_modelo = wa_zib_cte-modelo ).
      me->set_cd_modal( exporting i_cd_modal = wa_zib_cte-cd_modal ).
      me->set_ds_modal( exporting i_ds_modal = wa_zib_cte-ds_modal ).
      me->set_cd_tipo_servico( exporting i_cd_tipo_servico = wa_zib_cte-cd_tipo_servico ).
      me->set_ds_tipo_servico( exporting i_ds_tipo_servico = wa_zib_cte-ds_tipo_servico ).
      me->set_cd_tipo_cte( exporting i_cd_tipo_cte = wa_zib_cte-cd_tipo_cte ).
      me->set_ds_tipo_cte( exporting i_ds_tipo_cte = wa_zib_cte-ds_tipo_cte ).
      me->set_cd_fpagamento( exporting i_cd_fpagamento = wa_zib_cte-cd_fpagamento ).
      me->set_ds_fpagamento( exporting i_ds_fpagamento = wa_zib_cte-ds_fpagamento ).
      me->set_cd_femissao( exporting i_cd_femissao = wa_zib_cte-cd_femissao ).
      me->set_ds_femissao( exporting i_ds_femissao = wa_zib_cte-ds_femissao ).
      me->set_cd_aplicativo( exporting i_cd_aplicativo = wa_zib_cte-cd_aplicativo ).
      me->set_ds_aplicativo( exporting i_ds_aplicativo = wa_zib_cte-ds_aplicativo ).
      me->set_codg_cfop( exporting i_codg_cfop = wa_zib_cte-codg_cfop ).
      me->set_inicio_ibge( exporting i_inicio_ibge = wa_zib_cte-inicio_ibge ).
      me->set_inicio_muni( exporting i_inicio_muni = wa_zib_cte-inicio_muni ).
      me->set_inicio_uf( exporting i_inicio_uf = wa_zib_cte-inicio_uf ).
      me->set_termino_ibge( exporting i_termino_ibge = wa_zib_cte-termino_ibge ).
      me->set_termino_muni( exporting i_termino_muni = wa_zib_cte-termino_muni ).
      me->set_termino_uf( exporting i_termino_uf = wa_zib_cte-termino_uf ).
      me->set_nr_protocolo( exporting i_nr_protocolo = wa_zib_cte-nr_protocolo ).
      me->set_dt_protocolo( exporting i_dt_protocolo = wa_zib_cte-dt_protocolo ).
      me->set_hr_protocolo( exporting i_hr_protocolo = wa_zib_cte-hr_protocolo ).
      me->set_docsta( exporting i_docsta = wa_zib_cte-docsta ).
      me->set_cancel( exporting i_cancel = wa_zib_cte-cancel ).
      me->set_regio( exporting i_regio = wa_zib_cte-regio ).
      me->set_nfyear( exporting i_nfyear = wa_zib_cte-nfyear ).
      me->set_nfmonth( exporting i_nfmonth = wa_zib_cte-nfmonth ).
      me->set_docnum9( exporting i_docnum9 = wa_zib_cte-docnum9 ).
      me->set_cdv( exporting i_cdv = wa_zib_cte-cdv ).
      me->set_ds_prod_pred( exporting i_ds_prod_pred = wa_zib_cte-ds_prod_pred ).
      me->set_qt_carga_cte( exporting i_qt_carga_cte = wa_zib_cte-qt_carga_cte ).
      me->set_vl_total_merc( exporting i_vl_total_merc = wa_zib_cte-vl_total_merc ).
      me->set_rg_lido_pag_fret( exporting i_rg_lido_pag_fret = wa_zib_cte-rg_lido_pag_fret ).
      me->set_tp_processo_cte( exporting i_tp_processo_cte = wa_zib_cte-tp_processo_cte ).
      me->set_e_tomadora( exporting i_e_tomadora = wa_zib_cte-e_tomadora ).
      me->set_f_tomadora( exporting i_f_tomadora = wa_zib_cte-f_tomadora ).
      me->set_p_emissor( exporting i_p_emissor = wa_zib_cte-p_emissor ).
      me->set_e_emissor( exporting i_e_emissor = wa_zib_cte-e_emissor ).
      me->set_f_emissor( exporting i_f_emissor = wa_zib_cte-f_emissor ).
      me->set_docnum_cte_c( exporting i_docnum_cte_c = wa_zib_cte-docnum_cte_c ).
      me->set_docnum_cte_a( exporting i_docnum_cte_a = wa_zib_cte-docnum_cte_a ).
      me->set_docnum_cte_s( exporting i_docnum_cte_s = wa_zib_cte-docnum_cte_s ).
      me->set_docnum_cte_p( exporting i_docnum_cte_p = wa_zib_cte-docnum_cte_p ).
      me->set_ebeln( exporting i_ebeln = wa_zib_cte-ebeln ).
      me->set_ebelp( exporting i_ebelp = wa_zib_cte-ebelp ).
      me->set_mwskz( exporting i_mwskz = wa_zib_cte-mwskz ).
      me->set_belnr( exporting i_belnr = wa_zib_cte-belnr ).
      me->set_gjahr( exporting i_gjahr = wa_zib_cte-gjahr ).
      me->set_docnum_cte_sub( exporting i_docnum_cte_sub = wa_zib_cte-docnum_cte_sub ).
      me->set_ck_manual( exporting i_ck_manual = wa_zib_cte-ck_manual ).
      me->set_waerk_vi( exporting i_waerk_vi = wa_zib_cte-waerk_vi ).
      me->set_kursk_vi( exporting i_kursk_vi = wa_zib_cte-kursk_vi ).
      me->set_zvlr_vi( exporting i_zvlr_vi = wa_zib_cte-zvlr_vi ).
      me->set_zvlr_frete( exporting i_zvlr_frete = wa_zib_cte-zvlr_frete ).
      me->set_zvlr_mercadoria( exporting i_zvlr_mercadoria = wa_zib_cte-zvlr_mercadoria ).
      me->set_peso_origem( exporting i_peso_origem = wa_zib_cte-peso_origem ).
      me->set_peso_chegada( exporting i_peso_chegada = wa_zib_cte-peso_chegada ).
      me->set_dt_chegada( exporting i_dt_chegada = wa_zib_cte-dt_chegada ).
      me->set_zdt_mov( exporting i_zdt_mov = wa_zib_cte-zdt_mov ).
      me->set_zdt_vencto( exporting i_zdt_vencto = wa_zib_cte-zdt_vencto ).
      me->set_zpeso_diferenca( exporting i_zpeso_diferenca = wa_zib_cte-zpeso_diferenca ).
      me->set_zquebra( exporting i_zquebra = wa_zib_cte-zquebra ).
      me->set_zperda( exporting i_zperda = wa_zib_cte-zperda ).
      me->set_zvlr_quebra( exporting i_zvlr_quebra = wa_zib_cte-zvlr_quebra ).
      me->set_zvlr_perda( exporting i_zvlr_perda = wa_zib_cte-zvlr_perda ).
      me->set_zvlr_liq_pagar( exporting i_zvlr_liq_pagar = wa_zib_cte-zvlr_liq_pagar ).
      me->set_zbvtyp( exporting i_zbvtyp = wa_zib_cte-zbvtyp ).
      me->set_matns( exporting i_matns = wa_zib_cte-matns ).
      me->set_zbase_icms( exporting i_zbase_icms = wa_zib_cte-zbase_icms ).
      me->set_zbase_pis( exporting i_zbase_pis = wa_zib_cte-zbase_pis ).
      me->set_zbase_cofins( exporting i_zbase_cofins = wa_zib_cte-zbase_cofins ).
      me->set_zrate_icms( exporting i_zrate_icms = wa_zib_cte-zrate_icms ).
      me->set_zrate_pis( exporting i_zrate_pis = wa_zib_cte-zrate_pis ).
      me->set_zrate_cofins( exporting i_zrate_cofins = wa_zib_cte-zrate_cofins ).
      me->set_zvalor_icms( exporting i_zvalor_icms = wa_zib_cte-zvalor_icms ).
      me->set_zvalor_pis( exporting i_zvalor_pis = wa_zib_cte-zvalor_pis ).
      me->set_zvalor_cofins( exporting i_zvalor_cofins = wa_zib_cte-zvalor_cofins ).
      me->set_zvalor_pedagio( exporting i_zvalor_pedagio = wa_zib_cte-zvalor_pedagio ).
      me->set_ck_peso_chegada( exporting i_ck_peso_chegada = wa_zib_cte-ck_peso_chegada ).
      me->set_timestamp( exporting i_timestamp = wa_zib_cte-timestamp ).
      me->set_ck_autorizado( exporting i_ck_autorizado = wa_zib_cte-ck_autorizado ).

      select * into table it_zib_n55
        from zib_cte_dist_n55
       where cd_chave_cte = i_id_registro.

      loop at it_zib_n55 into wa_zib_n55.
        append wa_zib_n55 to it_n55.
      endloop.

      select * into table it_zib_n01
        from zib_cte_dist_n01
       where cd_chave_cte = i_id_registro.

      loop at it_zib_n01 into wa_zib_n01.
        append wa_zib_n01 to it_n01.
        move-corresponding wa_zib_n01 to wa_zib_n55.
        append wa_zib_n55 to it_zib_n55.
      endloop.

      select * into table it_zib_nit
        from zib_cte_dist_nit
       where cd_chave_cte = i_id_registro.

      "Seta Autorização de Pagamento
      me->set_ck_autorizado_pagamento( exporting i_ck_autorizado_pagamento = abap_true ).

      me->set_ck_fatura_pela_vt( exporting i_ck_fatura_pela_vt = abap_false ).

      loop at it_zib_nit into wa_zib_nit.

        append wa_zib_nit to it_nit.

        translate wa_zib_nit-meins to upper case.

        if wa_zib_nit-meins ne 'KG' and wa_zib_nit-meins ne 'TO'.
          me->set_ck_fatura_pela_vt( exporting i_ck_fatura_pela_vt = abap_true ).
        endif.

        select single matnr into lc_matnr
          from j_1bnflin
         where docnum eq wa_zib_nit-docnum
           and itmnum eq wa_zib_nit-itmnum.

        select single matkl into lc_grupo
          from mara
         where matnr eq lc_matnr.

*&----------Inicio CS2024000608 / USER STORY 146317 / AOENNING.
*        SELECT SINGLE TP_AUT_FRETE
*          INTO LC_TIPO
*          FROM ZIB_CTE_DIST_GM
*         WHERE MATKL EQ LC_GRUPO.
*&----------Inicio CS2024000608 / USER STORY 146317 / AOENNING.

*        if ( sy-subrc is initial ) and ( wa_zib_nit-ck_autorizado ne abap_true ) and ( wa_zib_cte-cd_modal ne '04' ).
*          me->set_ck_autorizado_pagamento( exporting i_ck_autorizado_pagamento = abap_false ).
*        endif.
*&----------Inicio CS2024000608 / USER STORY 146317 / AOENNING.

        if ( lc_tipo is not initial ) and ( wa_zib_nit-ck_autorizado ne abap_true ) and ( wa_zib_cte-cd_modal ne '04' ).
          me->set_ck_autorizado_pagamento( exporting i_ck_autorizado_pagamento = abap_false ).
        endif.



      endloop.

      select * into table it_zib_dup
        from zib_cte_dist_dup
       where cd_chave_cte = i_id_registro.

      loop at it_zib_dup into wa_zib_dup.
        append wa_zib_dup to it_dup.
      endloop.

      "Somente considerar lançamentos com VT e DOCNUM de Nota
      delete me->it_n55 where docnum_nfe eq space.
      delete me->it_n55 where tknum      eq space.
      delete me->it_n01 where docnum_nf  eq space.
      delete me->it_n01 where tknum      eq space.

      describe table me->it_n55 lines me->qt_reg_nf.
      describe table me->it_nit lines me->qt_reg_ni.

      "Autorização de Peso Manual
      me->set_ck_entrada_manual_peso( exporting i_ck_entrada_manual_peso = abap_true ).
      if ( me->tp_processo_cte eq '01' or me->tp_processo_cte eq '08' ) and me->cd_modal = '01' and me->cd_tipo_cte ne '1'.
        loop at me->it_n55 into wa_zib_n55.
          if wa_zib_n55-auart_va eq 'ZRFL' or "Remessa Form. Lote.
             wa_zib_n55-auart_va eq 'ZRDC'.   "Rem Form Lote DCO
            me->set_ck_entrada_manual_peso( exporting i_ck_entrada_manual_peso = abap_false ).
          endif.
        endloop.
        loop at me->it_n01 into wa_zib_n01.
          if wa_zib_n01-auart_va eq 'ZRFL' or "Remessa Form. Lote.
             wa_zib_n01-auart_va eq 'ZRDC'.   "Rem Form Lote DCO
            me->set_ck_entrada_manual_peso( exporting i_ck_entrada_manual_peso = abap_false ).
          endif.
        endloop.

        wa_zib_cte_dist_eap-tp_aprovacao  = '04'. "Peso/Data de Chegada
        wa_zib_cte_dist_eap-tp_autorizado = '01'. "Autorizado
        wa_zib_cte_dist_eap-cd_chave_cte  = me->cd_chave_cte.

        select single * into wa_zib_cte_dist_eap
          from zib_cte_dist_eap
         where tp_aprovacao  eq wa_zib_cte_dist_eap-tp_aprovacao
           and cd_chave_cte  eq wa_zib_cte_dist_eap-cd_chave_cte
           and tp_autorizado eq wa_zib_cte_dist_eap-tp_autorizado
           and ck_ultimo     eq abap_true.

        if sy-subrc is initial.
          me->set_ck_entrada_manual_peso( exporting i_ck_entrada_manual_peso = abap_true ).
        endif.
      elseif me->tp_processo_cte eq zcl_cte_dist_g=>tipo_03.
        me->set_ck_entrada_manual_peso( exporting i_ck_entrada_manual_peso = abap_false ).
      endif.

      delete it_zib_n55 where tknum eq space.
      sort it_zib_n55 by tknum.
      delete adjacent duplicates from it_zib_n55 comparing tknum.
      describe table it_zib_n55 lines me->qt_reg_vt.
      me->ck_alterado = abap_false.
    endif.

  endmethod.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_REGISTRO.

    E_VALIDOU = ABAP_FALSE.

    DATA(CK_ERRO) = ABAP_FALSE.

    IF ME->CD_TIPO_CTE EQ '1'.
      ME->SET_CK_PESO_CHEGADA( EXPORTING I_CK_PESO_CHEGADA = ABAP_TRUE ).
    ELSEIF ME->PESO_CHEGADA IS NOT INITIAL AND ME->DT_CHEGADA IS NOT INITIAL.
      ME->SET_CK_PESO_CHEGADA( EXPORTING I_CK_PESO_CHEGADA = ABAP_TRUE ).
    ELSE.

      CASE ME->CD_MODAL.
        WHEN '03'.
          "207  Não informado peso de chegada p/ CT-e &1/&2! Transação: ZLES0088
          "208  Não informado Data de chegada p/ CT-e &1/&2! Transação: ZLES0088
          "209  Não informado Data/Peso de chegada p/ CT-e &1/&2! Transação: ZLES0088
          IF ME->PESO_CHEGADA IS INITIAL AND ME->DT_CHEGADA IS INITIAL.
            MESSAGE S209 WITH ME->NUMR_CTE ME->NUMR_SERIE DISPLAY LIKE 'E'.
            CK_ERRO = ABAP_TRUE.
          ELSEIF ME->DT_CHEGADA IS INITIAL.
            MESSAGE S208 WITH ME->NUMR_CTE ME->NUMR_SERIE DISPLAY LIKE 'E'.
            CK_ERRO = ABAP_TRUE.
          ELSEIF ME->PESO_CHEGADA IS INITIAL.
            MESSAGE S207 WITH ME->NUMR_CTE ME->NUMR_SERIE DISPLAY LIKE 'E'.
            CK_ERRO = ABAP_TRUE.
          ENDIF.
        WHEN OTHERS.
          MESSAGE S087 WITH ME->NUMR_CTE ME->NUMR_SERIE DISPLAY LIKE 'E'.
          CK_ERRO = ABAP_TRUE.
      ENDCASE.

    ENDIF.

    CHECK CK_ERRO EQ ABAP_FALSE.

    IF ME->DOCNUM_CTE IS NOT INITIAL OR ME->CK_FINALIZADO IS NOT INITIAL.
      MESSAGE S181 WITH ME->NUMR_CTE ME->NUMR_SERIE DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->EBELN IS INITIAL OR ME->EBELP IS INITIAL.
      MESSAGE S068 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->MWSKZ IS INITIAL.
      MESSAGE S069 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->ZBVTYP IS INITIAL.
      MESSAGE S078 WITH ME->P_EMISSOR DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->ZDT_MOV LT SY-DATUM.
      MESSAGE S101 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->ZQUEBRA GT 0 AND ME->ZVLR_QUEBRA LE 0 AND ME->CD_TIPO_CTE NE '1'.
      "172  Existe quantidade de quebra e não foi calculado valor de quebra!
      MESSAGE S172 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->ZPERDA GT 0 AND ME->ZVLR_PERDA LE 0 AND ME->CD_TIPO_CTE NE '1'.
      "173  Existe quantidade de perda e não foi calculado valor de perda!
      MESSAGE S173 DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF ME->ZPERDA GT 0 AND ME->CD_TIPO_CTE NE '1' AND ME->ZQUEBRA LE 0.
      "173  Existe quantidade de perda e não foi calculado quantidade de quebra!
      MESSAGE S174 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->CK_FATURA_PELA_VT EQ ABAP_TRUE.
      IF ME->ZVLR_FRETE NE ME->VALOR_RECEBER.
        MESSAGE S183 WITH ME->VALOR_RECEBER ME->ZVLR_FRETE DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    R_PERMITIDO = ABAP_FALSE.

  ENDMETHOD.
ENDCLASS.
