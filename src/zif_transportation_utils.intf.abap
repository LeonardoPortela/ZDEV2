interface ZIF_TRANSPORTATION_UTILS
  public .


  class-data AT_REMESSA type ref to ZIF_TRANSPORTATION_UTILS .
  constants C_ETAPA_GERAR_VT type ZETAPA_PROC value '04' ##NO_TEXT.
  constants C_ETAPA_GERAR_DOC_CUSTO type ZETAPA_PROC value '05' ##NO_TEXT.
  constants C_ETAPA_GERAR_ORDEM_SERV type ZETAPA_PROC value '06' ##NO_TEXT.
  constants C_ETAPA_GERAR_FATURA_SERV type ZETAPA_PROC value '07' ##NO_TEXT.
  constants C_ETAPA_GERAR_DACTE type ZETAPA_PROC value '08' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_VT type ZETAPA_PROC value '14' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_DOC_CUSTO type ZETAPA_PROC value '15' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_ORDEM_SERV type ZETAPA_PROC value '16' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_FATURA_SERV type ZETAPA_PROC value '17' ##NO_TEXT.
  constants C_ETAPA_ESTORNO_DACTE type ZETAPA_PROC value '18' ##NO_TEXT.
  class-data T_BDCDATA type BDCDATA_TAB .
  class-data W_BDCDATA type BDCDATA .
  class-data AT_T_RETURN type TB_BAPIRET2 .
  constants C_E type CHAR1 value 'E' ##NO_TEXT.
  data AT_VBAK type VBAK .
  constants C_C type CHAR1 value 'C' ##NO_TEXT.
  data AT_VBKD type TAB_VBKD .
  constants C_ZFNT type AUART value 'ZFNT' ##NO_TEXT.
  constants C_PR00 type KSCHA value 'PR00' ##NO_TEXT.
  constants C_ETAPA_CRIAR_OV_DUMMY type ZETAPA_PROC value '01' ##NO_TEXT.
  constants C_ETAPA_CRIAR_REMESSA_DUMMY type ZETAPA_PROC value '02' ##NO_TEXT.
  constants C_02 type CHAR2 value '02' ##NO_TEXT.
  data AT_VBAP type TAB_VBAP .
  data AT_KONV type TAB_KONV .
  data AT_VBPA type TAB_VBPA .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS .
  class-methods ELIMINAR_VT
    importing
      !I_TKNUM type TKNUM
      !I_DELIVERY type VBELN_VL
      !I_ESTORNO type CHAR1 optional
    exporting
      !E_RETURN type TB_BAPIRET2
    raising
      ZCX_TRANSPORTATION_UTILS .
  class-methods ESTORNAR_DOC_CUSTO
    importing
      !I_FKNUM type FKNUM
      !I_KTOKK type KTOKK
    exporting
      !E_RETURN type TB_BAPIRET2
    raising
      ZCX_TRANSPORTATION_UTILS .
  class-methods ESTORNAR_OV_SERVICO
    importing
      !I_SALESDOCUMENT type BAPIVBELN-VBELN
    exporting
      !E_RETURN type TB_BAPIRET2
    raising
      ZCX_TRANSPORTATION_UTILS .
  class-methods ESTORNAR_FATURA_SERVICO
    importing
      !I_DOCUMENT type BAPIVBRKSUCCESS-BILL_DOC
    exporting
      !E_RETURN type TB_BAPIRET2
    raising
      ZCX_TRANSPORTATION_UTILS .
  class-methods CRIAR_LOG
    importing
      !I_VBELN type VBELN_VL
      !I_POSNR type POSNR_VL optional
      !I_NF_VENDA type J_1BNFNUM9
      !I_ETAPA_PROC type ZETAPA_PROC
      !I_COMMIT type CHAR1 default 'X'
      !I_DOC_GERADO type VBELN_VL optional
    changing
      !T_RETURN type TB_BAPIRET2 .
  class-methods FREE_RETURN .
  class-methods FREE_LOG_PROC
    importing
      !I_VBELN type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_ETAPA_PROC type ZETAPA_PROC optional .
  class-methods BDC_DATA
    importing
      !I_PROG type CHAR40
      !I_DYNPRO type CHAR40
      !I_START type CHAR40
      !I_FNAM type CHAR40
      !I_FVAL type CHAR40 .
  class-methods FREE_BDC_DATA .
  class-methods GET_TAB_RETURN
    returning
      value(T_RETURN) type TB_BAPIRET2 .
  class-methods SET_TAB_RETURN
    importing
      !I_TYPE type BAPI_MTYPE
      !I_ID type SYMSGID
      !I_NUMBER type SYMSGNO
      !I_MESSAGE type CHAR255 .
  methods GERAR_VT
    importing
      !I_REM_VBELN type VBELN_VL
      !I_HEADERDATA type BAPISHIPMENTHEADER
      !I_ITEMDATA type BAPISHIPMENTITEM
      !I_STAGEDATA type BAPISHIPMENTSTAGE
    exporting
      !E_DOC_TRANSP type TKNUM
      !E_RETURN type TB_BAPIRET2
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS
    raising
      ZCX_TRANSPORTATION_UTILS .
  methods FINALIZAR_VT
    importing
      !I_REM_VBELN type VBELN_VL
      !I_TKNUM type TKNUM
    exporting
      !E_DOC_TRANSP type TKNUM
      !E_RETURN type TB_BAPIRET2
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS
    raising
      ZCX_TRANSPORTATION_UTILS .
  methods GERAR_DOC_CUSTO
    importing
      !I_TKNUM type TKNUM
      !I_VBELN type VBELN_VA
      !I_AGENTE_FRETE type LIFNR
    exporting
      !E_DOC_CUSTO type FKNUM
      !E_ORDEM_SERV type VBELN_VA
      !E_FATURA_SERV type VBELN_VF
      !E_DACTE type CHAR10
      !E_RETURN type TB_BAPIRET2
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS
    raising
      ZCX_TRANSPORTATION_UTILS .
  methods GERAR_OV_SERVICO
    importing
      !I_NF_VENDA type J_1BNFNUM9
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
    exporting
      !E_OV_DUMMY type VBELN_VA
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS
    raising
      ZCX_TRANSPORTATION_UTILS .
  methods GERAR_FATURA_SERVICO
    importing
      !I_VBELN type VBELN_VA
      !I_NF_VENDA type J_1BNFNUM9
      !I_CHAVE_NF_VENDA type ZDE_CHAVE_DOC_E
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_REMESSA_DUMMY type VBELN_VL
    returning
      value(R_INSTANCIA) type ref to ZIF_TRANSPORTATION_UTILS
    raising
      ZCX_TRANSPORTATION_UTILS .
endinterface.
