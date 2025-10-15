@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LOCAL OPERACAO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_F1_LOCAL_OPERACAO_ZSDR0156
  as select from dd07t
{
  domvalue_l as ZLOCOPER
}
where
  domname = 'Z_LOCAL_OPERACAO_ZSDR0156'
