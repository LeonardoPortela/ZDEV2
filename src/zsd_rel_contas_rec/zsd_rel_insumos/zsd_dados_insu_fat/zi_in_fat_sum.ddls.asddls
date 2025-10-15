@AbapCatalog.sqlViewName: 'ZVINFATSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamento e Impostos por OV'
define view ZI_IN_FAT_SUM
  as select from ZI_IN_FAT_OV_UNI as vbfa
  //as select from ZI_IN_FAT_OV_2 as vbfa // 10.06.2025
  //  inner join   vbrk on vbrk.vbeln = vbfa.vbeln_vf 08.05.2025
  // left outer join vbrk on vbrk.vbeln = vbfa.vbeln_vf //10.06.2025
{
  key vbeln_va,

      sum(vbfa.menge_fat) as menge_fat
      //sum(vbfa.netwr_fat) as netwr_fat
      //sum(vbrk.mwsbk)     as mwsbk_fat //10.06.2025
}
group by
  vbeln_va
