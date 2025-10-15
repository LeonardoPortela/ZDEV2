@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somatorio das solicitações'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_ZSDT0082_SUM
  as select from zsdt0082 as z82
{
  key z82.vbeln            as Vbeln,
  key z82.posnr            as Posnr,
      cast('KG' as meins ) as dummy_meins,
      @Semantics.quantity.unitOfMeasure: 'dummy_meins'
      sum(z82.qte_sol  )   as QteSol,
      @Semantics.quantity.unitOfMeasure: 'dummy_meins'
      sum(z82.qte_lib  )   as QteLib
}
where
  (
       z82.status = '1'
    or z82.status = '2'
  )
group by
  z82.vbeln,
  z82.posnr
