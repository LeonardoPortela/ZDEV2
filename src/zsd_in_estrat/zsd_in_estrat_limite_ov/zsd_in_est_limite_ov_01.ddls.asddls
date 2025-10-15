@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Recuperar Limite disponivel por OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_LIMITE_OV_01
  as select from    ZI_EST_SIMU_OV      as simu
    inner join      ZSD_IN_EST_SALDO_01 as saldo_ov on  simu.doc_simulacao = saldo_ov.doc_simulacao
                                                    and simu.vbeln         = saldo_ov.vbeln
    left outer join ZSD_IN_EST_SALDO_05 as saldo    on  saldo.cnpj_cpf = simu.cnpj_cpf
                                                    and saldo.safra    = simu.safra
                                                    and saldo.cultura  = simu.cultura
                                                    and saldo.vkorg    = simu.vkorg
                                                    and saldo.waerk    = 'USD'
{
  key simu.doc_simulacao,
  key simu.vbeln,
  key simu.cnpj_cpf,
  key simu.safra,
  key simu.cultura,
  key simu.vkorg,
  key cast('USD' as waerk ) as waerk,

      @Semantics.amount.currencyCode: 'waerk'
      saldo_ov.valor_total_aprovado_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_limite_disponivel_sap,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_acumulado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_limite_disponivel_opus,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_acumulado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_acumulado_fin,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_acumulado_apr

//      @Semantics.amount.currencyCode: 'waerk'
//      saldo_ov.valor_total_aprovado_ov_apr_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_limite_disponivel_sap_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_acumulado_sap_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_limite_disponivel_opus_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_acumulado_opus_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_acumulado_fin_m,
//      @Semantics.amount.currencyCode: 'waerk'
//      saldo.valor_acumulado_apr_m


}
