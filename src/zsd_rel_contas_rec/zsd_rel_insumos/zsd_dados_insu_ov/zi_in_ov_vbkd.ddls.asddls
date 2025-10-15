@AbapCatalog.sqlViewName: 'ZIINOVVBKD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Encapsulamento VBKD para Insumos'
define view ZI_IN_OV_VBKD
  as select from vbkd
{
  key vbeln,
      max( kurrf ) as kurrf,
      max(valdt)   as valdt,
      max(zterm)   as zterm
}
group by
  vbeln
