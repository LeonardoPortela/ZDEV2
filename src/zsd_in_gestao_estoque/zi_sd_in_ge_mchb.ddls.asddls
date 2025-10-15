@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estoques de lotes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_MCHB
  as select from nsdm_e_mchb as mchb
{
  key mchb.matnr,
  key mchb.werks,
  key lpad( cast( mchb.werks as abap.char(10) ), 10, '0' ) as kunnr,
      cast('UN' as meins )                                 as meins_dummy,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.clabs)                                      as clabs,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.cinsm)                                      as cinsm,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mchb.cspem)                                      as cspem
}
where
  (
       mchb.clabs is not initial
    or mchb.cinsm is not initial
    or mchb.cspem is not initial
  )
group by
  matnr,
  werks
