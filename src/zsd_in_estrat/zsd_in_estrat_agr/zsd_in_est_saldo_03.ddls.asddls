@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agregação dos dados'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_SALDO_03
  //  as select from ZSD_IN_EST_SALDO_01 as saldo
  as select from ZSD_IN_EST_SALDO_02 as saldo
{
  key    cnpj_cpf,
  key    safra,
  key    cultura,
  key    vkorg,
  key    vkbur,
  key    waerk,
         //kurrf,
         @Semantics.amount.currencyCode: 'waerk'
         max(vlr_limite_sap)                                                                                          as vlr_limite_SAP,

         @Semantics.amount.currencyCode: 'waerk'
         sum(valor_liberado_ov_sap)                                                                                   as valor_liberado_sap,
         @Semantics.amount.currencyCode: 'waerk'
         sum( case when valor_bloqueado_ov_sap < 0 then cast( 0 as netwr_ap ) else valor_bloqueado_ov_sap end       ) as valor_bloqueado_sap,


         @Semantics.amount.currencyCode: 'waerk'
         max(vlr_limite_opus)                                                                                         as vlr_limite_opus,
         @Semantics.amount.currencyCode: 'waerk'
         sum(valor_liberado_ov_opus)                                                                                  as valor_liberado_opus,
         @Semantics.amount.currencyCode: 'waerk'
         sum( case when valor_bloqueado_ov_opus < 0 then cast( 0 as netwr_ap ) else valor_bloqueado_ov_opus end )     as valor_bloqueado_opus,


         @Semantics.amount.currencyCode: 'waerk'
         sum(valor_liberado_ov_fin)                                                                                   as valor_liberado_fin,
         @Semantics.amount.currencyCode: 'waerk'
         sum( case when valor_bloqueado_ov_fin < 0 then cast( 0 as netwr_ap ) else valor_bloqueado_ov_fin end )       as valor_bloqueado_fin,

         @Semantics.amount.currencyCode: 'waerk'
         sum(valor_liberado_ov_apr)                                                                                   as valor_liberado_apr,
         @Semantics.amount.currencyCode: 'waerk'
         sum( case when valor_bloqueado_ov_apr < 0 then cast( 0 as netwr_ap ) else valor_bloqueado_ov_apr end )       as valor_bloqueado_apr

}
group by
  cnpj_cpf,
  safra,
  cultura,
  vkorg,
  vkbur,
  waerk
