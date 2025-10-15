@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Saldo disponivel'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_SALDO_05
  as select from ZSD_IN_EST_SALDO_04
{
  key cnpj_cpf,
  key safra,
  key cultura,
  key vkorg,
  key waerk,
      @Semantics.amount.currencyCode: 'waerk'
      max(vlr_limite_sap)                         as vlr_limite_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum(valor_bloqueado_sap)                    as valor_acumulado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum(vlr_limite_sap - valor_bloqueado_sap)   as valor_limite_disponivel_sap,

      @Semantics.amount.currencyCode: 'waerk'
      max(vlr_limite_opus)                        as vlr_limite_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum(valor_bloqueado_opus)                   as valor_acumulado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum(vlr_limite_opus - valor_bloqueado_opus) as valor_limite_disponivel_opus,


      @Semantics.amount.currencyCode: 'waerk'
      sum(valor_bloqueado_fin)                    as valor_acumulado_fin,

      @Semantics.amount.currencyCode: 'waerk'
      sum(valor_bloqueado_apr)                    as valor_acumulado_apr
}
group by
  cnpj_cpf,
  safra,
  cultura,
  vkorg,
  waerk
