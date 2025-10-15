@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AJUDA F4 OPERAÇÃO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CLASS_F4_ZFIT0232
  as select from dd07t
{
  ddtext                     ,
  cast( domvalue_l as char1 ) as domvalue_l
}
where
  domname = 'Z_CLASS_ZFIT0232'
