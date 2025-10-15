@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estoques de lotes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_MCHB_02
  as select from ZI_SD_IN_GE_MCHB as mchb
  association [1..1] to kna1 as kna1 on mchb.kunnr = kna1.kunnr
{
  key mchb.matnr,
  key mchb.werks,
  key cast('0000000000' as lifnr) as lifnr,
      cast('AMAGGI' as name1_gp)  as name1,
      kna1.ort01,
      cast('UN' as meins )        as meins_dummy,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.clabs)             as clabs,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.cinsm)             as cinsm,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.cspem)             as cspem
}
where
  (
       mchb.clabs is not initial
    or mchb.cinsm is not initial
    or mchb.cspem is not initial
  )
group by
  matnr,
  werks,
  kna1.ort01
