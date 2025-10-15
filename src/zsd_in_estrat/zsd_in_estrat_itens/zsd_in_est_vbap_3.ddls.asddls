@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Regra VBAP para ZSDT0100'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_VBAP_3
  as select distinct from ZSD_IN_EST_VBAP_2
{
  key vbeln,
      waerk,
      @Semantics.amount.currencyCode: 'waerk'
      case when waerk = 'USD' then
         netwr
      else
          cast( division(cast( netwr as abap.dec( 12, 2 ) ),kurrf,2) as netwr_ap )
      end   as netwr,
      @Semantics.amount.currencyCode: 'waerk'
      netwr as netwr_m
}
