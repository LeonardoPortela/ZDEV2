@AbapCatalog.sqlViewName: 'ZVFIDTBASE101'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar ultima data base'
define view ZI_FI_DT_BASE_ZFI0101
  as select from zfit0079
{
  key bukrs                 as Bukrs,
  key dt_base_versao        as dt_base_dia,
  key max( dt_base_versao ) as dt_base_versao

}
group by
  bukrs,
  dt_base_versao
