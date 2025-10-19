@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar quantidade faturado NFE'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_SUM_FAT
  as select from vbrp
{
  key vbeln        as Vbeln,
      @Semantics.quantity.unitOfMeasure: 'vrkme'
      sum( fkimg ) as Qtd_fat,
      vrkme
}
group by
  vbeln,
  vrkme
