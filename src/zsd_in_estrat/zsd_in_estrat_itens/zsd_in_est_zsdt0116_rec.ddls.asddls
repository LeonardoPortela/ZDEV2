@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Z0116 com recebimentos calculados'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_ZSDT0116_REC
  as select from    ZSD_IN_EST_ZSDT0116_GERAL as z0116
    left outer join zsdt0116_rec              as rec on  rec.vbeln = z0116.vbeln
                                                     and rec.seq   = z0116.seq
{
  key z0116.vbeln,
  key z0116.seq,
      z0116.waerk,
      z0116.kursf,
      z0116.saldo_origem,

      @Semantics.amount.currencyCode : 'waerk'
      coalesce(rec.vlr_recebido, cast( 0 as netwr_ap ) )       as vlr_recebido,

      @Semantics.amount.currencyCode : 'waerk'
      coalesce(rec.vlr_recebido_moeda, cast( 0 as netwr_ap ) ) as vlr_recebido_moeda,

      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_aprovado,
      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_liberado,
      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_rejeitado,

      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_aprovado_moeda,
      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_liberado_moeda,
      @Semantics.amount.currencyCode : 'waerk'
      z0116.vlr_rejeitado_moeda

}
