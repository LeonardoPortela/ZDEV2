interface ZIF_REMESSA_TERCEIRO
  public .


  class-data AT_REMESSA_TERCEIRO type ref to ZIF_REMESSA_TERCEIRO .
  data AT_VBAK type VBAK .
  data AT_VBAP type TAB_VBAP .
  data AT_VBKD type TAB_VBKD .
  data AT_VBPA type TAB_VBPA .
  data AT_KONV type TAB_KONV .
  class-data AT_T_RETURN type TB_BAPIRET2 .
  constants C_ZFNT type AUART value 'ZFNT' ##NO_TEXT.
  constants C_PR00 type KSCHA value 'PR00' ##NO_TEXT.
  constants C_ETAPA_CRIAR_OV_DUMMY type ZETAPA_PROC value '01' ##NO_TEXT.
  constants C_ETAPA_CRIAR_REMESSA_DUMMY type ZETAPA_PROC value '02' ##NO_TEXT.
  constants C_ETAPA_CALCULO_FRETE type ZETAPA_PROC value '03' ##NO_TEXT.
  constants C_ETAPA_GERAR_VT type ZETAPA_PROC value '04' ##NO_TEXT.
  constants C_ETAPA_GERAR_DOC_CUSTO type ZETAPA_PROC value '05' ##NO_TEXT.
  constants C_ETAPA_GERAR_ORDEM_SERV type ZETAPA_PROC value '06' ##NO_TEXT.
  constants C_ETAPA_GERAR_FATURA_SERV type ZETAPA_PROC value '07' ##NO_TEXT.
  constants C_ETAPA_GERAR_DACTE type ZETAPA_PROC value '08' ##NO_TEXT.
  constants C_AJUSTAR_OV_DUMMY type ZETAPA_PROC value '10' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_VT type ZETAPA_PROC value '14' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_DOC_CUSTO type ZETAPA_PROC value '15' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_ORDEM_SERV type ZETAPA_PROC value '16' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_FATURA_SERV type ZETAPA_PROC value '17' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_DACTE type ZETAPA_PROC value '18' ##NO_TEXT.
  constants C_E type CHAR1 value 'E' ##NO_TEXT.
  constants C_SD type CHAR2 value 'SD' ##NO_TEXT.
  constants C_024 type CHAR3 value '024' ##NO_TEXT.
  constants C_02 type CHAR2 value '02' ##NO_TEXT.
  constants C_C type CHAR1 value 'C' ##NO_TEXT.
  constants C_ZLF type LFART value 'ZLF' ##NO_TEXT.
  constants C_J type CHAR1 value 'J' ##NO_TEXT.
  constants C_8 type CHAR1 value '8' ##NO_TEXT.
  class-data T_BDCDATA type BDCDATA_TAB .
  class-data W_BDCDATA type BDCDATA .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO .
  class-methods SET_CRIAR_LOG
    importing
      !I_VBELN type VBELN_VL
      !I_POSNR type POSNR_VL optional
      !I_NF_VENDA type J_1BNFNUM9
      !I_ETAPA_PROC type ZETAPA_PROC
      !I_COMMIT type CHAR1 default 'X'
      !I_DOC_GERADO type VBELN_VL optional
    changing
      !T_RETURN type TB_BAPIRET2 .
  class-methods GET_STATUS_OV_DUMMY
    importing
      !I_VBELN type VBELN_VA
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
    returning
      value(E_OV_DUMMY) type VBELN_VA .
  class-methods GET_STATUS_REMESSA_DUMMY
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_VBELN_DUMMY type VBELN_VL
      !I_NF_VENDA type J_1BNFNUM9
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
    returning
      value(E_REMESSA_DUMMY) type VBELN_VL .
  class-methods GET_STATUS_VT
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
      !I_NF_VENDA type J_1BNFNUM9
    exporting
      !E_SHTYP type SHTYP
    returning
      value(E_TRANSP) type TKNUM .
  class-methods SET_TAB_RETURN
    importing
      !I_TYPE type BAPI_MTYPE
      !I_ID type SYMSGID
      !I_NUMBER type SYMSGNO
      !I_MESSAGE type CHAR255 .
  class-methods GET_TAB_RETURN
    returning
      value(T_RETURN) type TB_BAPIRET2 .
  class-methods SET_FREE_RETURN .
  class-methods SET_FREE_LOG_PROC
    importing
      !I_VBELN type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_ETAPA_PROC type ZETAPA_PROC optional .
  class-methods SET_SALVA_TRANSPORTE
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_OV_DUMMY type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
      !I_REFKEY type J_1BREFKEY
      !I_AG_FRETE type LIFNR
      !I_COD_MOTORISTA type LIFNR
      !I_NF_VENDA type J_1BNFNUM9
      !I_NF_REMESSA type J_1BNFNUM9
      !T_TAB_TRANSP type ZLEST0211_TAB
      !I_NR_SAFRA type CHAR04 optional
      !I_NR_ORDEM type CHAR14 optional
      !I_ID_ORDEM type ZDE_ID_ORDEM optional
      !I_P_BRUTO type NTGEW
      !I_P_LIQUIDO type NTGEW
    raising
      ZCX_VEICULOS
      ZCX_REMESSA_TERCEIRO .
  class-methods SET_CHANGE_AGENTE_FRETE
    importing
      !I_REMESSA_DUMMY type VBELN_VL
      !I_AG_FRETE type LIFNR .
  class-methods SET_ELIMINA_TRANSPORTE
    importing
      !I_REMESSA_DUMMY type VBELN_VL .
  class-methods GET_DADOS_TRANSPORTE
    importing
      !I_OV_DUMMY type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
    exporting
      !E_PLACA type ZPLACA
      !E_QUANTIDADE type DZMENG
      !E_TP_FRETE type ZDE_TP_FRETE
      !E_ITINERARIO type ROUTE
      !E_VLR_FRETE type DZWERT
      !E_UNID_COND type KONWA
      !E_DADOS_TRANSP type CHAR5
      !E_LOCK_AG_FRETE type CHAR1
    changing
      !C_AG_FRETE type LIFNR .
  class-methods SET_SALVA_VINCULACAO
    importing
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
      !I_CHAVE_NF_CTA_ORDEM type ZDE_CHAVE_DOC_E
      !I_VBELN_VENDA type VBELN .
  class-methods SET_LIMPA_ERROS_LOG
    importing
      !T_VBELN_VENDA type ZLEST0212_TAB .
  class-methods GET_STATUS_NF_REMESSA
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_NFE
    returning
      value(E_NF_REMESSA) type J_1BNFNUM9 .
  class-methods SET_CALCULA_FRETE
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
      !I_NF_VENDA type J_1BNFNUM9
      !I_AGENTE_FRETE type LIFNR
    exporting
      value(E_UNID_COND) type KONWA
    returning
      value(E_VALOR_FRETE) type KBETR_KOND
    raising
      ZCX_VEICULOS
      ZCX_REMESSA_TERCEIRO .
  class-methods SET_ELIMINA_VT
    importing
      !I_TKNUM type TKNUM
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL
      !I_ESTORNO type CHAR1 default ' '
    raising
      ZCX_REMESSA_TERCEIRO .
  class-methods GET_STATUS_OUTROS_DOCS
    importing
      !I_OV_DUMMY type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
    exporting
      !E_DOC_CUSTO type FKNUM
      !E_ORDEM_SERV type VBELN_VA
      !E_FATURA_SERV type VBELN_VF
      !E_DACTE type CHAR10 .
  class-methods SET_ESTORNO_FATURA_SERV
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL .
  class-methods SET_ESTORNO_ORDEM_SERV
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL .
  class-methods SET_BDC_DATA
    importing
      !I_PROG type CHAR40
      !I_DYNPRO type CHAR40
      !I_START type CHAR40
      !I_FNAM type CHAR40
      !I_FVAL type CHAR40 .
  class-methods SET_FREE_BDC_DATA .
  class-methods SET_ESTORNO_DOC_CUSTO
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL .
  class-methods GET_AGENTE_FRETE
    importing
      !I_VBELN type VBELN_VA
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
    exporting
      !E_PLACA type ZDE_TX_CAMPO
      !E_VLR_UNIT_FRETE type ZDE_VLR15_02
      !E_CHAVE_CTE type ZDE_CHAVE_DOC_E
    changing
      !E_AGENTE_FRETE type LIFNR .
  methods SET_ESTORNA_CTE
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
  methods SET_CRIA_VT
    importing
      !I_VBELN_VENDA type VBELN_VA
      !I_OV_DUMMY type VBELN_VA
      !I_REMESSA_DUMMY type VBELN_VL
      !I_NF_VENDA type J_1BNFNUM9
    exporting
      !E_DOC_TRANSP type TKNUM
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
  methods SET_CRIA_OV_DUMMY
    importing
      !I_NF_VENDA type J_1BNFNUM9
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
    exporting
      !E_OV_DUMMY type VBELN_VA
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
  methods SET_CRIA_REMESSA_DUMMY
    importing
      !I_VBELN type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_REMESSA_DUMMY type VBELN_VL
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
  methods SET_ORDEM_VENDA
    importing
      !I_VBELN_PRINC type VBELN_VA
      !I_VBELN type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_ETAPA_PROC type ZETAPA_PROC
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
  methods SET_CRIA_DOC_CUSTO
    importing
      !I_TKNUM type TKNUM
      !I_VBELN_VENDA type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_REMESSA_DUMMY type VBELN_VL
      !I_AGENTE_FRETE type LIFNR
    exporting
      !E_DOC_CUSTO type FKNUM
      !E_ORDEM_SERV type VBELN_VA
      !E_FATURA_SERV type VBELN_VF
      !E_DACTE type CHAR10
    returning
      value(R_INSTANCIA) type ref to ZIF_REMESSA_TERCEIRO
    raising
      ZCX_REMESSA_TERCEIRO .
endinterface.
