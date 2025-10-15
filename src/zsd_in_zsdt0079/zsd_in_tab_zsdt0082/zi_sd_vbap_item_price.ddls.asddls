@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valores do Item'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_VBAP_ITEM_PRICE
  as select from ZI_SD_VBAP_CALC_ITEM
{
  key vbeln,
  key posnr,
      waerk,
      kmein,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      kwmeng,
      @Semantics.amount.currencyCode: 'waerk'
      netpr,
      @Semantics.amount.currencyCode: 'waerk'
      netwr,
      @Semantics.amount.currencyCode: 'waerk'
      mwsbp,
      @Semantics.amount.currencyCode: 'waerk'
      mwsbp_u,

      @Semantics.amount.currencyCode: 'waerk'
      netpr + mwsbp_u as netpr_mwsbp,

      @Semantics.amount.currencyCode: 'waerk'
      netwr + mwsbp   as netwr_mwsbp

}
