@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Virtualização da tabela ZSDT405'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_ZSDT0405
  as select from zsdt0405 as z405
  association [1..*] to zsdt0405_emit as emit on  emit.id_limite_sap = z405.id_limite_sap
                                              and emit.id_limite     = z405.id_limite

{
  key id_limite_sap,
  key id_limite,
  key vkorg,
  key emit.emitente,
  key safra,
  key cultura,
      waers,
      @Semantics.amount.currencyCode: 'waers'
      vlr_limite,

      cancelado,
      user_create,
      date_create,
      time_create,
      user_change,
      date_change,
      time_change
}
where
  cancelado is initial
