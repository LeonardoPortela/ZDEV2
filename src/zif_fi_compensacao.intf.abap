interface ZIF_FI_COMPENSACAO
  public .


  data AT_BUKRS type BUKRS .
  data AT_BUDAT type BUDAT .
  data AT_BLDAT type BLDAT .
  data AT_WAERS type WAERS .
  data AT_KURSF type KURSF .
  data AT_BLART type BLART .
  data AT_MONAT type MONAT .
  data AT_WWERT type WWERT_D .
  data AT_XBLNR type XBLNR1 .
  data AT_BKTXT type BKTXT .
  data AT_AUGTX type AUGTX_F05A .
  data AT_MODE type ALLGAZMD .
  data AT_AUGLV type AUGLV .
  data AT_TCODE type SYST_TCODE .
  data AT_SGFUNCT type SGFUNCT_PI .
  data AT_SIMULACAO type CHAR1 .
  data AT_PARTIDAS_COMP type Z_PARTIDA_COMP_FI_T .
  data AT_WAERS_EMP type WAERS .
  data AT_HWAE2_EMP type HWAE2 .
  data AT_HWAE3_EMP type HWAE3 .

  methods SET_BUKRS
    importing
      !I_BUKRS type BUKRS
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_DT_COMPENSACAO
    importing
      !I_AUGDT type AUGDT
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_DT_LCTO
    importing
      !I_BUDAT type BUDAT
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_DT_DOCUMENTO
    importing
      !I_BLDAT type BLDAT
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_MOEDA
    importing
      !I_WAERS type WAERS
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TAXA_CAMBIO
    importing
      !I_KURSF type KURSF
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TP_DOCUMENTO
    importing
      !I_BLART type BLART
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_PERIODO
    importing
      !I_MONAT type MONAT
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_DT_CONVERSAO
    importing
      !I_WWERT type WWERT_D
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_REFERENCIA
    importing
      !I_XBLNR type XBLNR1
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TEXTO_CAB_DOC
    importing
      !I_BKTXT type BKTXT
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TEXTO_COMPENSACAO
    importing
      !I_AUGTX type AUGTX_F05A
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_MODE
    importing
      !I_MODE type ALLGAZMD
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_AUGLV
    importing
      !I_AUGLV type AUGLV
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TCODE
    importing
      !I_TCODE type SYST_TCODE
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_SGFUNCT
    importing
      !I_SGFUNCT type SGFUNCT_PI
    raising
      ZCX_FI_COMPENSACAO .
  methods ADD_PARTIDA
    importing
      !I_PARTIDA_COMP type ZDE_PARTIDA_COMP_FI
    raising
      ZCX_FI_COMPENSACAO .
  methods VALIDAR_COMPENSACAO
    returning
      value(R_VALIDADO) type CHAR01
    raising
      ZCX_FI_COMPENSACAO .
  methods PREPARAR_DADOS_COMPENSACAO
    raising
      ZCX_FI_COMPENSACAO .
  methods COMPENSAR
    exporting
      !E_BELNR type BELNR_D
    returning
      value(R_COMPENSADO) type CHAR01
    raising
      ZCX_FI_COMPENSACAO .
  methods TRANSF_DADOS_PARTIDA_K
    importing
      !I_BSIK type BSIK
    changing
      !C_PARTIDA type ZDE_PARTIDA_COMP_FI
    raising
      ZCX_FI_COMPENSACAO .
  methods TRANSF_DADOS_PARTIDA_D
    importing
      !I_BSID type BSID
    changing
      !C_PARTIDA type ZDE_PARTIDA_COMP_FI
    raising
      ZCX_FI_COMPENSACAO .
  methods TRANSF_DADOS_PARTIDA_S
    importing
      !I_BSIS type BSIS
    changing
      !C_PARTIDA type ZDE_PARTIDA_COMP_FI
    raising
      ZCX_FI_COMPENSACAO .
  methods CHECK_PARTIDA_OPEN
    importing
      !I_BUKRS type BUKRS
      !I_BELNR type BELNR_D
      !I_GJAHR type GJAHR
      !I_BUZEI type BUZEI
    exporting
      !E_BSEG type BSEG
      !E_BSIK type BSIK
      !E_BSID type BSID
      !E_BSIS type BSIS
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_SIMULACAO
    importing
      !I_SIMULACAO type CHAR1
    raising
      ZCX_FI_COMPENSACAO .
  methods ADD_PARTIDA_RESIDUAL
    importing
      !I_PARTIDA_ORIGINAL type ZDE_PARTIDA_COMP_FI
    changing
      !C_FTPOST_T type RE_T_EX_FTPOST
      !C_COUNT_FT type COUNT_PI
    raising
      ZCX_FI_COMPENSACAO .
  methods SET_TAXA_PARTIDA
    changing
      !C_PARTIDA type ZDE_PARTIDA_COMP_FI
    raising
      ZCX_FI_COMPENSACAO .
endinterface.
