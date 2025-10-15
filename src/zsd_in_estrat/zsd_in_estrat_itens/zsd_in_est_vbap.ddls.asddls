@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Regra VBAP para ZSDT0100'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_VBAP
  as select from    vbap   as vbap
    inner join      vbak            on vbak.vbeln = vbap.vbeln
    inner join      v_konv as konv  on  konv.knumv = vbak.knumv
                                    and konv.kposn = vbap.posnr
                                    and konv.stunr = '300'
                                    and konv.zaehk = '001'
                                    and konv.kappl = 'V'
                                    and konv.kschl = 'ICMI'
    left outer join vbep   as _vbep on  _vbep.vbeln = vbap.vbeln
                                    and _vbep.posnr = vbap.posnr
                                    and _vbep.etenr = '0001'
{
  key vbap.vbeln,
      vbap.waerk,
      @Semantics.amount.currencyCode: 'waerk'
      sum(
          case coalesce(_vbep.lifsp,'') when '' then
            konv.kwert
          else
            case when _vbep.lifsp <> '12' then
                konv.kwert
            else
                cast( 0 as netwr_ap )
            end
          end
      ) as netwr
}
group by
  vbap.vbeln,
  vbap.waerk
