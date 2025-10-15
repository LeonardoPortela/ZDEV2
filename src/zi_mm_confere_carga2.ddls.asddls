@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida saldo da carga '
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_CONFERE_CARGA2
  as select from zib_nfe_dist_itm
{
  key chave_nfe,
      ebeln,
      ebelp,
      meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      @DefaultAggregation: #SUM
      sum(menge) as total
}
group by
  chave_nfe,
  ebeln,
  ebelp,
  meins
