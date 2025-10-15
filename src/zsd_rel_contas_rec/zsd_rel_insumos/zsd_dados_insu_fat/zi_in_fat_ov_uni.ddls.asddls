@AbapCatalog.sqlViewName: 'ZIINFATOVUNI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'União Faturamento por OV'
define view ZI_IN_FAT_OV_UNI
  as select from ZI_IN_FAT_OV
{
  key vbeln_va,
      sum(menge_fat ) as menge_fat
}
group by
  vbeln_va
union all select from ZI_IN_FAT_OV_REMESSA as remessa
{
  key  remessa.vbeln_va,
       sum(remessa.menge_fat) as menge_fat
}
group by
  remessa.vbeln_va
