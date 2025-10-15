@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Partidas Desconto Antecipação - ZFI0064'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_JUROS_MI_C1
  as select from ZI_FI_JUROS_MI_C1_INFO
{
  bukrs,
  butxt,
  gsber,
  kunnr,
  ds_cliente,
  Tipo,
  banco_liq,
  buzei,
  belnr,
  augbl,
  augdt,
  waers,
  @Semantics.amount.currencyCode: 'waers'
  total_docs_comp,
  percentual,
  Valor_Desc_Prop_dmbtr,
  Valor_Desc_Prop_dmbe2,
  VBEL2,
  @Semantics.amount.currencyCode: 'waers'
  DMBTR,
  @Semantics.amount.currencyCode: 'waers'
  DMBE2,
  @Semantics.amount.currencyCode: 'waers'
  DESC_ANTECIPACAO_DMBTR,
  @Semantics.amount.currencyCode: 'waers'
  DESC_ANTECIPACAO_DMBE2,
  TPSIM,
  AUART,
  MATNR,
  Matkl,
  NRO_SOL,
  TP_VENDA,
  maktx,
  charg,
  tipo_compensacao
}
where
  tipo_compensacao = '1'

union all

select from ZI_FI_JUROS_MI_C1_INFO
{
  bukrs,
  butxt,
  gsber,
  kunnr,
  ds_cliente,
  Tipo,
  banco_liq,
  buzei,
  belnr,
  augbl,
  augdt,
  waers,
  total_docs_comp,
  percentual,
  Valor_Desc_Prop_dmbtr,
  Valor_Desc_Prop_dmbe2,
  VBEL2,
  DMBTR,
  DMBE2,
  DESC_ANTECIPACAO_DMBTR,
  DESC_ANTECIPACAO_DMBE2,
  TPSIM,
  AUART,
  MATNR,
  Matkl,
  NRO_SOL,
  TP_VENDA,
  maktx,
  charg,
  tipo_compensacao
}
where
  tipo_compensacao = '2'
