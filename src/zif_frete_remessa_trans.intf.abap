interface ZIF_FRETE_REMESSA_TRANS
  public .


  class-data AT_T_RETURN type TB_BAPIRET2 .
  constants C_ETAPA_CALCULO_FRETE type ZETAPA_PROC value '03' ##NO_TEXT.
  constants C_E type CHAR1 value 'E' ##NO_TEXT.
  constants C_ETAPA_GERAR_DOC_CUSTO type ZETAPA_PROC value '05' ##NO_TEXT.
  constants C_ETAPA_GERAR_ORDEM_SERV type ZETAPA_PROC value '06' ##NO_TEXT.
  constants C_ETAPA_GERAR_FATURA_SERV type ZETAPA_PROC value '07' ##NO_TEXT.
  constants C_ETAPA_GERAR_DACTE type ZETAPA_PROC value '08' ##NO_TEXT.
  class-data AT_FRETE_REMESSA_TRANS type ref to ZIF_FRETE_REMESSA_TRANS .
  constants C_ETAPA_GERAR_VT type ZETAPA_PROC value '04' ##NO_TEXT.
  constants C_J type CHAR1 value 'J' ##NO_TEXT.
  constants C_8 type CHAR1 value '8' ##NO_TEXT.

  class-methods SET_CALCULA_FRETE
    importing
      !I_REM_VBELN type VBELN_VL
      !I_EBELN type VGBEL
      !I_EBELP type VGPOS
      !I_AGENTE_FRETE type LIFNR
      !I_PED_BSART type BSART
      !I_VEI_ID_ORDEM type ZDE_ID_ORDEM
    exporting
      value(E_UNID_COND) type KONWA
    returning
      value(E_VALOR_FRETE) type KBETR_KOND
    raising
      ZCX_VEICULOS
      ZCX_FRETE_REMESSA_TRANS .
  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZIF_FRETE_REMESSA_TRANS .
  class-methods SET_FREE_RETURN .
  class-methods SET_FREE_LOG_PROC
    importing
      !I_VBELN type VBELN_VA
      !I_EBELN type VGBEL
      !I_EBELP type VGPOS
      !I_ETAPA_PROC type ZETAPA_PROC .
  class-methods SET_TAB_RETURN
    importing
      !I_TYPE type BAPI_MTYPE
      !I_ID type SYMSGID
      !I_NUMBER type SYMSGNO
      !I_MESSAGE type CHAR255 .
  class-methods GET_TAB_RETURN
    returning
      value(T_RETURN) type TB_BAPIRET2 .
  class-methods SET_CRIAR_LOG
    importing
      !I_VBELN type VBELN_VL
      !I_EBELN type VGBEL
      !I_EBELP type VGPOS
      !I_ETAPA_PROC type ZETAPA_PROC
      !I_COMMIT type CHAR1 default 'X'
      !I_DOC_GERADO type VBELN_VL optional
    changing
      !T_RETURN type TB_BAPIRET2 .
  class-methods GET_DADOS_TRANSPORTE
    importing
      !I_REM_VBELN type VBELN_VL
      !I_EBELN type VGBEL
      !I_EBELP type VGPOS
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
  class-methods SET_SALVA_TRANSPORTE
    importing
      !I_REM_VBELN type VBELN_VL
      !I_REM_POSNR type POSNR_VL
      !I_PED_BSART type ESART
      !I_AG_FRETE type LIFNR
      !I_COD_MOTORISTA type LIFNR
      !T_TAB_TRANSP type ZLEST0211_TAB
      !I_NR_SAFRA type CHAR04
      !I_NR_ORDEM type CHAR14
      !I_ID_ORDEM type ZDE_ID_ORDEM
      !I_P_BRUTO type NTGEW
      !I_P_LIQUIDO type NTGEW
    raising
      ZCX_VEICULOS
      ZCX_FRETE_REMESSA_TRANS .
  class-methods SET_ELIMINA_TRANSPORTE
    importing
      !I_REM_VBELN type VBELN_VL .
  class-methods SET_LIMPA_ERROS_LOG
    importing
      !T_VBELN_TRANSF type ZLEST0214_TAB .
  class-methods GET_STATUS_VT
    importing
      !I_REM_VBELN type VBELN_VL
    returning
      value(E_TRANSP) type TKNUM .
  class-methods GET_STATUS_OUTROS_DOCS
    importing
      !I_REM_VBELN type VBELN_VL
      !I_EBELN type VGBEL
      !I_EBELP type VGPOS
    exporting
      !E_DOC_CUSTO type FKNUM
      !E_ORDEM_SERV type VBELN_VA
      !E_FATURA_SERV type VBELN_VF
      !E_DACTE type CHAR10 .
endinterface.
