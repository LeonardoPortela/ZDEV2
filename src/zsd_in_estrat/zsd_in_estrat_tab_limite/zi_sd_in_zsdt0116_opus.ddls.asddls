@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Utilização do saldo OPUS em Ovs'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_ZSDT0116_OPUS
  as select from    zsdt0116     as z016
    left outer join zsdt0116_rec as zrec on  zrec.seq       = z016.seq
                                         and zrec.vbeln     = z016.vbeln
                                         and zrec.id_limite = z016.id_limite
{
  key z016.id_limite,
      cast('USD' as waers)                                                         as waers,
      @Semantics.amount.currencyCode: 'waers'
      sum(z016.vlr_liberado - coalesce(zrec.vlr_recebido, cast( 0 as netwr_ap ) )) as VlrLiberado
}
where
      z016.saldo_origem    =  'O' // Origem OPUS
  and z016.status          <> 'X'
  and z016.status_workflow <> 'R'
  and z016.id_limite       is not initial
group by
  z016.id_limite
