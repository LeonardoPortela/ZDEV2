@AbapCatalog.sqlViewName: 'ZVCONTAS15'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Contas - 15 Resultado do período'
define view ZI_EQ_PAT_CONTAS_RESULT
  as select from ska1
{
  key saknr as Saknr
}

where
      ktopl = '0050'
  and saknr not like '00001%'
  and saknr not like '00002%'
