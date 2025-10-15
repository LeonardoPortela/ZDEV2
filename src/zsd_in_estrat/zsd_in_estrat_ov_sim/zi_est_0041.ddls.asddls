@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados simplificados da tabela ZSDT0041'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_0041
  as select from zsdt0041 as Z41
{
  key min(Z41.doc_simulacao) as doc_simulacao,
      min(Z41.dtvenc)        as DTVENC
}
group by
  doc_simulacao
