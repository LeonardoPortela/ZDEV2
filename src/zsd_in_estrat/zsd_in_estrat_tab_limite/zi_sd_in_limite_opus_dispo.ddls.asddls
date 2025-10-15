@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Limites disponiveis no OPUS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_LIMITE_OPUS_DISPO
  as select from ZI_SD_IN_LIMITE_OPUS as opus
{
  key opus.vkorg,
  key opus.emitente,
  key opus.safra,
  key opus.cultura,
      opus.waers,
      @Semantics.amount.currencyCode: 'waers'
      sum(VlrLimite) as VlrLimite

}
group by
  opus.vkorg,
  opus.emitente,
  opus.safra,
  opus.cultura,
  opus.waers
