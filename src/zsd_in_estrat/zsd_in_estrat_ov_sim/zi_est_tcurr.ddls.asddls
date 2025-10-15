@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Formatação da TCURR para consultas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_TCURR
  as select from tcurr
{
  key  kurst,
  key  fcurr,
  key  tcurr,
  key cast(cast(99999999 - cast(gdatu as abap.int4) as char20) as abap.dats) as gdatu,
  tcurr.ukurs
}
