@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'VBAP com itens calculados'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_VBAP_CALC_ITEM
  as select from vbap as vbap
  association [0..1] to vbep as _vbep on  _vbep.vbeln = vbap.vbeln
                                      and _vbep.posnr = vbap.posnr
                                      and _vbep.etenr = '0001'
{
  key vbap.vbeln,
  key vbap.posnr,
      cast('USD' as waerk)                                                                                   as waerk,
      vbap.kmein,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      case coalesce(_vbep.lifsp,'') when '' then
        vbap.kwmeng
      else case when _vbep.lifsp = '12' then
        cast(0 as kwmeng)
      else
        vbap.kwmeng
      end end                                                                                                as kwmeng,

      @Semantics.amount.currencyCode: 'waerk'
      case coalesce(_vbep.lifsp,'') when '' then
        netpr
      else case when _vbep.lifsp = '12' then
        cast(0 as netpr)
      else
        vbap.netpr
      end end                                                                                                as netpr,

      @Semantics.amount.currencyCode: 'waerk'
      case coalesce(_vbep.lifsp,'') when '' then
        vbap.netwr
      else case when _vbep.lifsp = '12' then
        cast(0 as netpr)
      else
        vbap.netwr
      end end                                                                                                as netwr,

      @Semantics.amount.currencyCode: 'waerk'
      cast( division(cast(mwsbp as abap.dec( 15, 2 ) ),cast(vbap.kwmeng as abap.dec(15,2)),2 ) as netwr_ap ) as mwsbp_u,

      @Semantics.amount.currencyCode: 'waerk'
      mwsbp



}
where
  _vbep.lifsp <> '12'
