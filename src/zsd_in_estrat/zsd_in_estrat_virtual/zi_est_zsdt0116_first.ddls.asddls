@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Primeiro registro de uma ov'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_ZSDT0116_FIRST
  as select from zsdt0116
{
  key vbeln    as vbeln,
  key min(seq) as seq
}
group by
  vbeln
