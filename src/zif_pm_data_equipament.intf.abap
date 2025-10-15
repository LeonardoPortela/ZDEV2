interface ZIF_PM_DATA_EQUIPAMENT
  public .


  data AT_NUMERO_ORDEM type AUFNR .
  data AT_NUMERO_EQUNR type EQUNR .
  class-data AT_IF_PM_DATA_EQUIPAMENT type ref to ZIF_PM_DATA_EQUIPAMENT .
  data AT_NUMERO_ORDEM_COMB type AUFNR .
  data AT_NUMERO_ORDEM_LUB type AUFNR .
  data AT_WORK_CTR type ARBPL .
  data AT_CHECK_CATEGORIA type CHAR01 .
  data AT_EARTX type EARTX .
  data AT_DESC_FABRICANTE type ZDESC_MOD .
  data AT_DESC_MODELO type TYPBZ .
  data AT_DADOS_EQPTO type BAPI_ITOB .
  data AT_CHECK_EQPTO_SUP type CHAR01 .
  data AT_ZPMT0074 type ZPMT0074_T .
  data AT_DADOS_PARAM_PLANO type ZPME0070_T .
  data AT_PONTO_REF_TRANSF type IMRC_POINT .
  data AT_PONTO_REC_TRANSF type IMRC_POINT .
  data AT_DOC_MEDICAO type IMRC_MDOCM .
  data AT_POS_CONTADOR type IMRC_CNTRC .
  data AT_CHECK_IMPL type CHAR01 .
  data AT_TIPO_CONTADOR type DZEIEH .
  data AT_TIPO_VEICULO type TYPBZ .
  data AT_PLANO type ZPME0074_T .

  class-methods GET_INSTANCE
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods CRIAR_ORDEM_MANUTENCAO
    importing
      !ORDER_TYPE type AUFART
      !SHORT_TEXT type AUFTEXT
      !PLANPLANT type IWERK
      !BUS_AREA type GSBER
      !FUNCT_LOC type TPLNR
      !MN_WK_CTR type ARBPL
      !PLANT type WERGW
      !MAINTPLANT type SWERK
      !LOC_BUS_AREA type GSBER
      !PLANGROUP type INGRP
      !EQUIPMENT type EQUNR
      !COSTCENTER type KOSTL
      !PMACTTYPE type CHAR3
      !PRIORITY type PRIOK
      !ACTIVITY type VORNR
      !CONTROL_KEY type STEUS
      !DESCRIPTION type LTXA1
      value(TEXT_LONGO) type /BCV/UIF_STRING optional
      !SORTFIELD type EQFNR optional
      !ESTIMATED_COSTS type AUFUSER4 optional
    exporting
      !E_ORDEM type AUFNR
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods MODIFICAR_EQUIPAMENTO
    importing
      !EQUIPMENT type EQUNR
      !MAINTPLANT type SWERK
      !BUS_AREA type GSBER
      !PLANPLANT type IWERK
      !COSTCENTER type KOSTL
      !WORK_CTR type NUM8
      !STANDORDER type DAUFN
      !SETTLORDER type ILOM_ORDST
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_DATA_EQPTO
    importing
      !I_EQUI type EQUI
      !I_EQUZ type EQUZ
      !I_ILOA type ILOA
      !I_ITOBATTR type ITOBATTR
      !I_FLEET type FLEET
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_WORK_CTR
    importing
      !I_WORK_CTR type LGWID
      !I_WERKS type WERKS_D
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_ORDEM_MANUTENCAO
    exporting
      value(E_AUFNR) type AUFNR
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods CHECK_CATEGORIA
    importing
      !I_EQTYP type EQTYP
      !I_EQART type EQART
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_EARTX
    importing
      !I_EQART type EQART
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_DESC_FABRICANTE
    importing
      !I_HERST type HERST
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_DESC_MODELO
    importing
      !I_TYPBZ type TYPBZ
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_DESC_EQPTO
    exporting
      !E_KTX01 type KTX01
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods GET_DADOS_EQPTO
    importing
      !I_EQUNR type EQUNR
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods CRIAR_PONTO_MEDICAO
    importing
      !I_EQUNR type EQUNR
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_DADOS_ZPMT0074
    importing
      !I_KLASSE type KLASSE_D
      !I_FLEET_CAT type FLEET_CAT
      !I_EQTYP type EQTYP
      !I_HERST type HERST
      !I_TYPBZ type TYPBZ
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SHDB_IK01
    importing
      !I_EQUNR type EQUNR
      !I_TPLNR type TPLNR
      !I_WERKS type WERKS_D
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_PARAMETROS_PLANO
    importing
      !I_LOCAS type IMRC_LOCAS
      !I_EQUNR type EQUNR
      !I_TPLNR type TPLNR
      !I_WERKS type WERKS_D
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods GET_DADOS_PARAM_PLANO
    importing
      !I_LOCAS type IMRC_LOCAS
    exporting
      !T_ZPMT0075 type ZPMT0075_T
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods CRIAR_PLANOS_MANUT
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods EXEC_TRANSF_CONTADOR
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods CRIAR_DOC_PARA_PONTO_MEDICAO
    importing
      value(I_POINT) type IMRC_POINT
      value(I_IDATE) type IMRC_IDATE
      value(I_ITIME) type IMRC_ITIME
      value(I_POS_CONTADOR) type IMRC_CNTRC
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods SET_POS_CONTADOR
    importing
      value(I_POS_CONTADOR) type IMRC_CNTRC
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods START_PLAN_MANUTENCAO
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods GRAVA_LOGS
    importing
      !I_TIPO_MSG type BAPI_MTYPE
      !I_MENSAGEM type BAPI_MSG
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
  methods VALIDA_CAPACIDADE_COMBUSTIVEL
    importing
      !I_COD_CLASSE type EQART
      !I_FABRICANTE type HERST
      !I_MODELO type TYPBZ
      !I_QTD_ABASTEC type DEC_16_02_S OPTIONAL
    exporting
      !E_TQ_COMB type KEY_NUM
      !E_DENTRO_DA_TOLERANCIA type DTC_COLFLG
    returning
      value(R_IF_PM_DATA_EQUIPAMENT) type ref to ZIF_PM_DATA_EQUIPAMENT .
endinterface.
