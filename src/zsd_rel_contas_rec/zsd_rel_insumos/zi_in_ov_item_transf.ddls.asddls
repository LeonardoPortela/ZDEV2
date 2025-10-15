@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'OV/Item com transferencia'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_OV_ITEM_TRANSF
  as select distinct from vbep as vbep
{
  key vbep.vbeln,
  key vbep.posnr,

      case lifsp when '12' then
         'X'
         else
         '' end as transferido
}
where
  lifsp = '12'
group by
  vbeln,
  posnr,
  lifsp
