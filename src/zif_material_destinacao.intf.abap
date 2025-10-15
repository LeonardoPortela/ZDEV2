interface ZIF_MATERIAL_DESTINACAO
  public .


  class-data IF_MATERIAL_DESTINACAO type ref to ZIF_MATERIAL_DESTINACAO .
  data AT_ZMMT0114 type ZMMT0114 .
  data AT_ZMMT0115 type ZDE_ZMMT0115_T .
  data AT_ZMMT0116 type ZDE_ZMMT0116_T .
  data AT_ZMMT0118 type ZDE_ZMMT0118_T .
  constants ST_TP_DESTINACAO_ARMAZENAR type ZDE_TP_DESTINACAO value '00' ##NO_TEXT.
  constants ST_TP_DESTINACAO_DEVOLUCAO type ZDE_TP_DESTINACAO value '01' ##NO_TEXT.
  constants ST_TP_DESTINACAO_QUEBRA type ZDE_TP_DESTINACAO value '02' ##NO_TEXT.
  constants ST_TP_DESTINACAO_PERDA type ZDE_TP_DESTINACAO value '03' ##NO_TEXT.
  constants ST_TP_DESTINACAO_RETORNO type ZDE_TP_DESTINACAO value '04' ##NO_TEXT.
  data AT_TP_DESTINACAO type ZDE_TP_DESTINACAO .
  data AT_NAO_GERAR_BLOQUEIO type CHAR01 .
  constants ST_TP_ORIGEM_NFE type ZDE_ORIGEM_DESTINO value '00' ##NO_TEXT.
  constants ST_TP_ORIGEM_ROMANEIO type ZDE_ORIGEM_DESTINO value '01' ##NO_TEXT.
  data AT_TIPO_MOVIMENTO type BWART .
  data AT_MOTIVO_MOVIMENTO type MB_GRBEW .

  class-methods GET_INSTANCE
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO .
  class-methods SET_SALDO_ITEM_MSEG
    importing
      !I_MBLNR type MBLNR
      !I_MJAHR type MJAHR
      !I_ZEILE type MBLPO
    exporting
      !E_VALOR type DMBTR
    returning
      value(R_MENGE) type MENGE_D .
  class-methods SET_SALDO_ITEM_ROM
    importing
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !E_VALOR type DMBTR
    returning
      value(R_MENGE) type MENGE_D .
  class-methods GET_TEXTO_NOTA_FISCAL_DOCNUM
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_TEXTO) type STRING
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_REGISTRO
    importing
      !I_ID_DESTINACAO type ZDE_DESTINACAO
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_CLEAR
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO .
  methods SET_NEW
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO .
  methods SET_VALIDAR
    exporting
      !E_VALIDOU type CHAR01
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_GRAVAR
    exporting
      !E_ID_DESTINACAO type ZDE_DESTINACAO
      !E_GRAVOU type CHAR01
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_GERAR_MOVIMENTO
    importing
      !I_DT_MOVIMENTO type DATUM default SY-DATUM
      !I_GERAR_VIA_JOB type CHAR01 optional
    exporting
      !E_GEROU type CHAR01
      !E_MBLNR type MBLNR
      !E_MJAHR type MJAHR
      !E_DOCNUM type J_1BDOCNUM
      !E_BELNR_DEV type RE_BELNR
      !E_GJAHR_DEV type GJAHR
      !E_DOCNUM_DEV type J_1BDOCNUM
      !E_RETORNO type BAPIRET2_T
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_ESTORNAR_MOVIMENTO
    exporting
      !E_ESTORNOU type CHAR01
      !E_RETORNO type BAPIRET2_T
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_ADD_DOC_MATERIAL_ORIGEM
    importing
      !I_ORIG_MBLNR type MBLNR
      !I_ORIG_MJAHR type MJAHR
      !I_ORIG_ZEILE type MBLPO
      !I_ORIG_NFE type ZDE_CHAVE_NFE optional
      !I_ORIG_ROMANEIO type ZCH_REF optional
      !I_MENGE type MENGE_D optional
      !I_MEINS type MEINS optional
      !I_VALOR type J_1BBASE
      !I_FORNE type LIFNR optional
      !I_CK_TOTAL_ORIGEM type CHAR01 optional
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_DENQUEUE
    importing
      !I_ID_DESTINACAO type ZDE_DESTINACAO
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_ENQUEUE
    importing
      !I_ID_DESTINACAO type ZDE_DESTINACAO optional
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods FREE
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods GET_NEW_ID
    exporting
      !E_ID_DESTINACAO type ZDE_DESTINACAO
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_DEFAULT_CONFIG
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_REGISTRO_DOC_DESTINACAO
    importing
      !I_MBLNR type MBLNR
      !I_MJAHR type MJAHR
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_GERAR_NOTA_DEVOLUCAO
    importing
      !I_GERAR_VIA_JOB type CHAR01 optional
    exporting
      !E_BELNR_DEV type RE_BELNR
      !E_GJAHR_DEV type GJAHR
      !E_DOCNUM_DEV type J_1BDOCNUM
    returning
      value(IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_GERAR_NOTA_DEVOLUCAO_JOB
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_HEADERDATA type BAPI_INCINV_CREATE_HEADER
      !I_ITEMDATA type BAPI_INCINV_CREATE_ITEM_T
      !I_CFOP type ZDE_PO_CFOP_T
    exporting
      !E_BELNR_DEV type RE_BELNR
      !E_GJAHR_DEV type GJAHR .
  methods SET_ESTORNAR_NOTA_DEVOLUCAO
    returning
      value(IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods GET_REGISTRO
    exporting
      !E_ZMMT0114 type ZMMT0114
    returning
      value(IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO .
  methods GET_TEXTO_NOTA_FISCAL
    exporting
      !E_STRING type STRING
    returning
      value(IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods GET_TEXTO_NOTA_FISCAL_PADRAO
    exporting
      !E_STRING type STRING
    returning
      value(IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_AJUSTAR_CFOP_NF
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_TP_DESTINACAO type ZDE_TP_DESTINACAO
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
  methods SET_AJUSTAR_INCOTERMS_NF
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_INCO1 type INCO1
      !I_INCO2 type INCO2
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO .
  methods CHECK_PARAMETROS_ZMM0185
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_TP_DESTINACAO type ZDE_TP_DESTINACAO
    returning
      value(R_IF_MATERIAL_DESTINACAO) type ref to ZIF_MATERIAL_DESTINACAO
    raising
      ZCX_MATERIAL_DESTINACAO .
endinterface.
