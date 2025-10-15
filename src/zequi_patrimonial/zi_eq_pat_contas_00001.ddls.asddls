@AbapCatalog.sqlViewName: 'ZVCONTAS1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Contas 000001'
define view ZI_EQ_PAT_CONTAS_00001
  as select from ska1
{
  key saknr as Saknr
}

where
      ktopl =    '0050'
  and saknr like '00001%'
