@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Regra VBAP para ZSDT0100'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_VBAP_2
  as select from    ZSD_IN_EST_VBAP       as vbak
    inner join      ZI_IN_SIMU_OV         as simu on simu.vbeln = vbak.vbeln
    left outer join ZI_EST_TAXA_USD       as vbkd on vbkd.vbeln = vbak.vbeln
    left outer join ZSD_IN_EST_ZSDT0094_1 as z94  on z94.doc_simulacao = simu.doc_simulacao
{
  key vbak.vbeln,
      vbak.waerk,
      @Semantics.amount.currencyCode: 'waerk'
      vbak.netwr,
      coalesce(z94.taxa_cambio,coalesce(vbkd.kurrf,1)) as kurrf
}
