@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Saldo disponivel'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_SALDO_04
  as select from ZSD_IN_EST_SALDO_03
{
  key    cnpj_cpf,
  key    safra,
  key    cultura,
  key    vkorg,
  //key    vkbur,
  key    waerk,
         @Semantics.amount.currencyCode: 'waerk'
         max(vlr_limite_SAP)         as vlr_limite_sap,
         @Semantics.amount.currencyCode: 'waerk'
         max(vlr_limite_opus)        as vlr_limite_opus,

         @Semantics.amount.currencyCode: 'waerk'
         sum( valor_bloqueado_sap )  as valor_bloqueado_sap,
         @Semantics.amount.currencyCode: 'waerk'
         sum( valor_bloqueado_opus ) as valor_bloqueado_opus,

         @Semantics.amount.currencyCode: 'waerk'
         sum( valor_bloqueado_fin )  as valor_bloqueado_fin,

         @Semantics.amount.currencyCode: 'waerk'
         sum( valor_bloqueado_apr )  as valor_bloqueado_apr


}
group by
  cnpj_cpf,
  safra,
  cultura,
  vkorg,
  //vkbur,
  waerk
