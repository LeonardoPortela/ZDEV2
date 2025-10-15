@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Calcula o total dos itens da carga'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_TOTAL_ITENS_CARGA
  as select from zmmt0202 as item
  inner join zmmt0203 as notas on item.nro_cg = notas.nro_cg
{
  key item.nro_cg,
      item.ebeln,
      item.ebelp,
      item.unidade,
      notas.chave_nfe,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      @DefaultAggregation: #SUM
      sum(item.qtd_vinc_carga) as total
}

group by
  item.nro_cg,
  item.ebeln,
  item.ebelp,
  item.unidade,
  notas.chave_nfe
