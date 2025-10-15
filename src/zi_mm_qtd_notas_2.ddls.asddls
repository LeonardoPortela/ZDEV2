@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida se existe notas com processo = 2'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_QTD_NOTAS_2 as select from zmmt0203
{
  key nro_cg,
      count (*) as qtd
}

where processo = '2'
  and cancel = ''
group by
  nro_cg
