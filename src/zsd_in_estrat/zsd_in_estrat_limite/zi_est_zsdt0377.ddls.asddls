@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Normaliza ZSDT0377 para acesso limite'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_ZSDT0377
  as select from zsdt0377 as limite
{
  key bukrs,
  key cultura,
  key safra,
      waerk,
      @Semantics.amount.currencyCode: 'waerk'
      valor
}
where
  deleted is initial
