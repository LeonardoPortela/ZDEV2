@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Limites disponiveis no OPUS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_LIMITE_OPUS
  as select from ZI_SD_IN_ZSDT0405 as opus
  //as select from zsdt0405 as opus
  association [0..1] to ZI_SD_IN_ZSDT0116_OPUS as z116 on opus.id_limite_sap = z116.id_limite
{
  key opus.id_limite_sap,
  key opus.id_limite,
  key opus.vkorg,
  key opus.emitente,
  key opus.safra,
  key opus.cultura,
      opus.waers,
      @Semantics.amount.currencyCode: 'waers'
      sum(opus.vlr_limite - coalesce(z116.VlrLiberado,cast( 0 as netwr_ap )) ) as VlrLimite
}
where
      cancelado is initial
  and waers     = 'USD'
group by
  opus.id_limite_sap,
  opus.id_limite,
  opus.vkorg,
  opus.emitente,
  opus.safra,
  opus.cultura,
  opus.waers
