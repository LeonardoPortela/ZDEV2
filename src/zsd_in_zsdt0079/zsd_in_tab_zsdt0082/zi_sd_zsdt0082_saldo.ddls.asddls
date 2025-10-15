@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Visão por saldo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_ZSDT0082_SALDO
  as select from    ZI_SD_VBAP_ITEM_PRICE as vbap
    left outer join ZI_SD_ZSDT0082_SUM    as z82 on  vbap.vbeln = z82.Vbeln
                                                 and vbap.posnr = z82.Posnr
{
  key vbap.vbeln,
  key vbap.posnr,
      vbap.waerk,
      vbap.kmein,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      z82.QteSol,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      z82.QteLib,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      vbap.kwmeng                                                                                      as QtdMax,
      @Semantics.amount.currencyCode: 'waerk'
      division(cast(vbap.netwr_mwsbp as abap.dec( 15, 2 ) ),cast(vbap.kwmeng as abap.dec( 15, 2 )),10) as VlrUnit,

      @Semantics.amount.currencyCode: 'waerk'
      vbap.netwr                                                                                       as VlrMax,
      @Semantics.amount.currencyCode: 'waerk'
      cast( ( z82.QteSol * cast(vbap.netpr as abap.dec( 15, 2 ) ) ) as netwr_ap )                      as VlrSol,
      @Semantics.amount.currencyCode: 'waerk'
      cast(  ( z82.QteLib * cast(vbap.netpr as abap.dec( 15, 2 ) ) ) as netwr_ap )                     as VlrLib
}
