interface ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
  public .


  class-data AT_CRIA_MODIFICA_ORDEM_MOBMAN type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  class-data AT_SERVICO type STRING .
  data AT_JSON type STRING .
  data AT_COMBOIO type ZPMT0058_T .
  class-data AT_RETURN type BAPIRET2 .
  class-data AT_DADOS_ABAST type ZPMT0058 .
  class-data AT_EPTO type EQUNR .
  class-data AT_MATERIAL type MATNR .
  class-data AT_DEPOSITO type STATION_T .
  class-data AT_PONTO_MEDICAO type Z_IMPTT_T .
  class-data AT_CENTRO type WERKS_D .
  class-data AT_DOC_MED type ZIMRG_DOCM_T .
  class-data AT_DATE type SY-DATUM .
  class-data AT_HORA type SY-UZEIT .
  class-data AT_DADOS_EQUIPAMENTO type BAPI_ITOB .
  class-data AT_STATUS_PROC type CHAR01 .
  class-data AT_COMBUST type ZDE_COMBOIO_ABASTECIMENTO_T .
  class-data AT_ORDEM type ZTPM_D_M_ORDEM .
  class-data AT_CONFIRMATION type AFRU .

  class-methods GET_INSTANCE
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO .
  methods SET_INT_COMB
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_PERMISSAO_USUARIO) type ref to ZIF_PERMISSAO_USUARIOS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_PERMISSAO_USUARIO) type ref to ZIF_PERMISSAO_USUARIOS .
  methods GET_INT_COMB
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_COMBOIO type ZDE_COMBOIO_ABASTECIMENTO_T
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PROCESSA_TRANSFERENCIA
    importing
      !I_COMBOIO type ZPMT0058_T
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PROCESSA_CONSUMO
    importing
      !I_COMBOIO type ZPMT0058_T
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB .
  methods CHECK_VEICULO .
  methods CHECK_MATERIAL .
  methods CHECK_PERIODO .
  methods CHECK_DEPOSITO .
  methods SET_LOG
    importing
      !I_DADOS type ZPMT0058
      value(I_RETURN) type BAPIRET2 .
  methods SET_PONTO_MEDICAO .
  methods SET_DADOS_EQPTO
    importing
      !I_DADOS type ZPMT0058 .
  methods ESTORNA_DOC_MEDICAO .
  methods PROC_CONTADOR_ODOM_HOM .
  methods PROC_BAIXA_ESTOQUE .
  methods REGIST_JUSTIF
    importing
      value(I_LINE) type CHAR72 optional
      value(W_ZPMT0058) type ZPMT0058 optional
      value(I_ODOMETRO) type IMRC_CNTRC optional .
  methods SET_DS_URL_PUT
    returning
      value(R_IF_INTEGRACAO_COMB) type ref to ZIF_INTEGRACAO_COMB .
  methods POST_CRIA_MODIFICA_ORDEM_APP
    importing
      !I_ORDEM type ZTPM_D_M_ORDEM
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DATA type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods MONTA_JSON
    importing
      !I_COMBOIO type ZDE_COMBOIO_ABASTECIMENTO_T
    returning
      value(E_JSON) type STRING .
  methods SET_DADOS_ORDEM
    importing
      !I_DATA type ZTPM_D_M_ORDEM
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  methods SET_DADOS_CONFIRMATION
    importing
      !I_DATA type AFRU
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  methods SET_DS_URL_ESTORNAR_APONT
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  methods SET_DS_URL_EXCLUIR_OPER
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  methods SET_DS_URL_ORDEM_STATUS
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN .
  methods POST_ATUALIZA_STATUS_ORDEM
    importing
      !I_ORDEM type ZTPM_D_M_ORDEM
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DATA type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods POST_ESTORNAR_APONT
    importing
      !I_AFRU type AFRU
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DATA type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods POST_EXCLUIR_OPER
    importing
      !I_ORDEM type ZTPM_D_M_ORDEM
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DATA type STRING
    returning
      value(R_IF_CRIA_MODIFICA_ORDEM_MOBMA) type ref to ZIF_CRIA_MODIFICA_ORDEM_MOBMAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
