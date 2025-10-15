interface ZIF_CONTROLE_RETORNO_RFL
  public .


  class-data AT_IF_CONTROLE_RETORNO_RFL type ref to ZIF_CONTROLE_RETORNO_RFL .
  constants ST_COM_CCT type CHAR01 value '1' ##NO_TEXT.
  constants ST_SEM_CCT type CHAR01 value '2' ##NO_TEXT.
  data AT_NOTAS_SELECIONADAS type ZDE_NOTA_RETORNO_RFL_T .
  data AT_BUKRS type BUKRS .
  data AT_FKART type ZFKART .
  data AT_WERKS type WERKS_D .
  data AT_LGORT type LGORT_D .
  data AT_CHARG type CHARG_D .
  data AT_KUNNR type KUNNR .
  data AT_TERMINAL type LIFNR .
  data AT_MATNR type MATNR .
  data AT_QTDE_RETORNO type MENGE .
  data AT_DT_RETORNO type ERDAT .
  data AT_STATUS_CCT type CHAR01 .
  data AT_DOCNUM type J_1BNFE_T_DOCNUM .
  data AT_CONSULTA_TERMINAL type ZSDCT_CONSULTA_TERMINAL .

  class-methods GET_INSTANCE
    returning
      value(R_IF_CONTROLE_RETORNO_RFL) type ref to ZIF_CONTROLE_RETORNO_RFL
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods NOVO_LANCAMENTO
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods SELECIONAR_NOTAS
    importing
      !I_BUKRS type BUKRS
      !I_FKART type ZFKART
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D optional
      !I_CHARG type CHARG_D
      !I_KUNNR type KUNNR
      !I_TERMINAL type LIFNR optional
      !I_MATNR type MATNR
      !I_STATUS_CCT type CHAR01 optional
      !I_COM_SALDO type CHAR01 optional
      !I_DT_INI_REC type ERDAT optional
      !I_DT_FIM_REC type ERDAT optional
      !I_DT_INI_EMI type ERDAT
      value(I_DT_FIM_EMI) type ERDAT
    exporting
      !E_NOTAS type ZDE_NOTA_RETORNO_RFL_T
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods SET_QTDE_VINC_NF
    importing
      !I_NOTA_VINC type ZDE_NOTA_RETORNO_RFL_SEL
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods GERAR_RETORNO
    importing
      !I_DT_RETORNO type ERDAT
      !I_FINALIDADE type ZFIN_EXPORT
      !I_CHECK_EXISTS_RET_FINALIDADE type CHAR01 optional
      !I_PARCEIRO type LIFNR
    returning
      value(E_DOCNUM_RETORNO) type J_1BDOCNUM
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods VALIDA_RETORNO
    importing
      !I_CHECK_EXISTS_RET_FINALIDADE type CHAR01 optional
      !I_FINALIDADE type ZFIN_EXPORT
    exporting
      !E_MENGE_RETORNO type J_1BNETQTY
      !E_NETWR_RETORNO type J_1BNFNETT
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods DETERMINAR_CFOP
    importing
      !I_MATNR type MATNR
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_KUNNR type J_1BPARID
      !I_REFKEY type J_1BREFKEY
    exporting
      !E_CFOP type J_1BCFOP
      !E_LIFNR type LIFNR
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods PREENCHE_TAX
    importing
      !I_TAXTYP type J_1BTAXTYP
      !I_ITMNUM type J_1BITMNUM
      !I_OTHBAS type J_1BOTHBAS
    changing
      !C_ITEM_TAX_TAB type BAPI_J_1BNFSTX_TAB
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods PREENCHE_MSG
    importing
      !I_LINNUM type J_1BLINNUM
      !I_MESSAGE type J_1BMESSAG
    changing
      !C_MSG_TAB type BAPI_J_1BNFFTX_TAB
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods CANCELAR_RETORNO
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_DOCNUM_ESTORNO) type J_1BDOCNUM
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods VALIDAR_QUANTIDADE_RETORNO
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_MENGE_VINC type J_1BNETQTY
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods CONSULTA_TERMINAR_NFE
    importing
      value(IT_DOCNUM) type J_1BNFE_T_DOCNUM
    exporting
      !ET_CONSULTA_TERMINAL type ZSDCT_CONSULTA_TERMINAL .
  methods VALIDAR_CANCELAMENTO_RETORNO
    importing
      value(I_DOCNUM) type J_1BDOCNUM
      !I_DOC_CANCEL type CHAR1 default SPACE
    exporting
      !E_ERRO type CHAR1
    raising
      ZCX_CONTROLE_RETORNO_RFL .
  methods REVERTE_RETORNO_1X1
    importing
      value(I_DOCNUM) type J_1BDOCNUM .
  methods ATUALIZAR_TABELAS_Z
    importing
      value(I_DOCNUM) type J_1BDOCNUM .
  methods VINC_FLOTE_REVERTE_RETORNO
    importing
      value(IT_VINC_P_FLOTE_IN) type ZSDTVINC_P_FLOTE_T
    exporting
      !ET_VINC_P_FLOTE_OUT type ZSDTVINC_P_FLOTE_T .
  methods CNST_E_VALIDA_TERM_NFE_SAIDA
    importing
      value(IT_DOCNUM) type J_1BNFE_T_DOCNUM
      !I_COD_RA_EMBARQUE type ZDE_CODIGO_RA_EMBARQUE
      !I_EUDR type ZEUDR
      !I_RETORNAR_EUDR type C default SPACE
      !I_DIRECT type ZTP_MOVI
    exporting
      !ET_CONSULTA_TERMINAL type ZSDCT_CONSULTA_TERMINAL .
endinterface.
