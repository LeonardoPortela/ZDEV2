@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca se há itens para a carga'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_QTD_ITENS_CARGA
  as select from zmmt0202
{
  key nro_cg,
      count( * ) as qtd
}
where cancel = ''
group by
  nro_cg
