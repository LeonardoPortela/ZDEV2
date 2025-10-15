@AbapCatalog.sqlViewName: 'ZIINFILTROSALDO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro por Saldo'
define view ZI_IN_FILTRO_SALDO
  //as select from ZI_IN_AGREG_02
  as select from ZI_IN_FILTRO_SALDO_OV
{
  key doc_simulacao,
  key vbeln_p,

      sum( case when waerk = 'BRL' then saldo_ov_brl else saldo_ov_usd end )       as SaldoOV,
      sum( case when waerk = 'BRL' then saldo_juros_brl else saldo_juros_usd end ) as SaldoJurosFiltro
}
group by
  doc_simulacao,
  vbeln_p
