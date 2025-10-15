interface ZIF_FMCALL_APP_MOBILE
  public .


  constants ST_GET_MM_QTD_TAREFAS type STRING value 'GET_MM_QTD_TAREFAS' ##NO_TEXT.
  constants ST_GET_MM_PEDIDOS_APROVACAO type STRING value 'GET_MM_PEDIDOS_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_PEDIDO_ITENS type STRING value 'GET_MM_PEDIDO_ITENS' ##NO_TEXT.
  constants ST_SET_MM_PEDIDO_APROVACAO type STRING value 'SET_MM_PEDIDO_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_REQUISICAO_APROVACAO type STRING value 'GET_MM_REQUISICAO_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_REQUISICAO_ITENS type STRING value 'GET_MM_REQUISICAO_ITENS' ##NO_TEXT.
  constants ST_SET_MM_REQUISICAO_APROVACAO type STRING value 'SET_MM_REQUISICAO_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_RESERVA_APROVACAO type STRING value 'GET_MM_RESERVA_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_RESERVA_ITENS type STRING value 'GET_MM_RESERVA_ITENS' ##NO_TEXT.
  constants ST_SET_MM_RESERVA_APROVACAO type STRING value 'SET_MM_RESERVA_APROVACAO' ##NO_TEXT.
  constants ST_GET_MM_ADIANT_APROVACAO type STRING value 'GET_MM_ADIANTAMENTO_APROVACAO' ##NO_TEXT.
  constants ST_APROVAR type CHAR01 value 'A' ##NO_TEXT.
  constants ST_RECUSAR type CHAR01 value 'R' ##NO_TEXT.
  constants ST_SET_MM_ADIANT_APROVACAO type STRING value 'SET_MM_ADIANTAMENTO_APROVACAO' ##NO_TEXT.
  constants ST_GET_QTD_TAREFAS_USUARIO type STRING value 'GET_QTD_TAREFAS_USUARIO' ##NO_TEXT.
  constants ST_GET_LAC_MANUAIS type STRING value 'GET_LAC_MANUAIS' ##NO_TEXT.
  constants ST_GET_PAG_IMPOSTOS type STRING value 'GET_PAG_IMPOSTOS' ##NO_TEXT.
  constants ST_GET_PAG_INVOICES type STRING value 'GET_PAG_INVOICES' ##NO_TEXT.
  constants ST_GET_LIB_EMBARQUES_INSUMOS type STRING value 'GET_LIB_EMBARQUES_INSUMOS' ##NO_TEXT.
  constants ST_GET_FAT_LIM_CREDITO type STRING value 'GET_FAT_LIM_CREDITO' ##NO_TEXT.
  constants ST_GET_SOL_ORD_VENDA type STRING value 'GET_SOL_ORD_VENDA' ##NO_TEXT.
  constants ST_GET_RH_PAG_SALARIO type STRING value 'GET_RH_PAG_SALARIO' ##NO_TEXT.
  constants ST_GET_RH_MOV_PESSOAL type STRING value 'GET_RH_MOV_PESSOAL' ##NO_TEXT.
  constants ST_GET_PM_ORC_ORDEM type STRING value 'GET_PM_ORC_ORDEM' ##NO_TEXT.
  constants ST_GET_PM_ORC_SUPLE_ORDEM type STRING value 'GET_PM_ORC_SUPLE_ORDEM' ##NO_TEXT.
  constants ST_SET_LAC_MANUAIS_APROVACAO type STRING value 'SET_LAC_MANUAIS_APROVACAO' ##NO_TEXT.
  constants ST_SET_PAG_IMPOSTOS_APROVACAO type STRING value 'SET_PAG_IMPOSTOS_APROVACAO' ##NO_TEXT.
  constants ST_SET_PAG_INVOICES_APROVACAO type STRING value 'SET_PAG_INVOICES_APROVACAO' ##NO_TEXT.
  constants ST_SET_LIB_EMB_INSUMOS_APROV type STRING value 'SET_LIB_EMB_INSUMOS_APROVACAO' ##NO_TEXT.
  constants ST_SET_FAT_LIM_CREDITO_APROV type STRING value 'SET_FAT_LIM_CREDITO_APROVACAO' ##NO_TEXT.
  constants ST_SET_SOL_ORD_VENDA_APROVACAO type STRING value 'SET_SOL_ORD_VENDA_APROVACAO' ##NO_TEXT.
  constants ST_SET_RH_PAG_SALARIO_APROV type STRING value 'SET_RH_PAG_SALARIO_APROVACAO' ##NO_TEXT.
  constants ST_SET_RH_MOV_PESSOAL_APROV type STRING value 'SET_RH_MOV_PESSOAL_APROVACAO' ##NO_TEXT.
  constants ST_SET_PM_ORC_ORDEM_APROVACAO type STRING value 'SET_PM_ORC_ORDEM_APROVACAO' ##NO_TEXT.
  constants ST_GET_IM_INVESTIMENTO type STRING value 'GET_IM_INVESTIMENTO' ##NO_TEXT.
  constants ST_SET_IM_INVESTIMENTO_APROV type STRING value 'SET_IM_INVESTIMENTO_APROVACAO' ##NO_TEXT.
  constants ST_GET_IM_INVESTIMENTO_ITENS type STRING value 'GET_IM_INVESTIMENTO_ITENS' ##NO_TEXT.
  constants ST_GET_SOL_ORD_VENDA_ITENS type STRING value 'GET_SOL_ORD_VENDA_ITENS' ##NO_TEXT.
  constants ST_SET_PM_ORC_SUPLE_ORDEM_APR type STRING value 'SET_PM_ORC_SUPLE_ORDEM_APROV' ##NO_TEXT.

  methods GET_MM_QTD_TAREFAS
    exporting
      !E_QTD_PEDIDOS type I
      !E_QTD_REQUISICAO type I
      !E_QTD_RESERVA type I
      !E_QTD_ADIANTAMENTOS type I
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_PEDIDOS_APROVACAO
    exporting
      !E_PEDIDOS type ZDE_PEDIDOS_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_PEDIDO_ITENS
    importing
      !I_JSON type STRING
    exporting
      !E_PEDIDOS_ITENS type ZDE_PEDITENS_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_MM_PEDIDO_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_REQUISICAO_APROVACAO
    exporting
      !E_REQUISICAO type ZDE_REQUISICAO_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_REQUISICAO_ITENS
    importing
      !I_JSON type STRING optional
      !I_BANFN type BANFN optional
    exporting
      !E_REQUISICAO_ITENS type ZDE_REQITENS_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE .
  methods SET_MM_REQUISICAO_APROVACAO
    importing
      !I_JSON type STRING optional
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_RESERVA_APROVACAO
    exporting
      !E_RESERVAS type ZDE_RESERVA_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_RESERVA_ITENS
    importing
      !I_JSON type STRING
    exporting
      !E_RESERVA_ITENS type ZDE_RESITENS_APROVACAO_T
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_MM_RESERVA_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_MM_ADIANTAMENTO_APROVACAO
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_MM_ADIANTAMENTO_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_QTD_TAREFAS_USUARIO
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_LAC_MANUAIS
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_PAG_IMPOSTOS
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_PAG_INVOICES
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_LIB_EMBARQUES_INSUMOS
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_FAT_LIM_CREDITO
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_SOL_ORD_VENDA
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_SOL_ORD_VENDA_ITENS
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_RH_PAG_SALARIO
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_RH_MOV_PESSOAL
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_PM_ORC_ORDEM
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_PM_ORC_SUPLE_ORDEM
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_LAC_MANUAIS_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_PAG_IMPOSTOS_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_PAG_INVOICES_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_LIB_EMB_INSUMOS_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_FAT_LIM_CREDITO_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_SOL_ORD_VENDA_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_RH_PAG_SALARIO_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_RH_MOV_PESSOAL_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_PM_ORC_ORDEM_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_IM_INVESTIMENTO
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods GET_IM_INVESTIMENTO_ITENS
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_IM_INVESTIMENTO_APROVACAO
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  methods SET_PM_ORC_SUPLE_ORDEM_APROV
    importing
      !I_JSON type STRING
    exporting
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
endinterface.
