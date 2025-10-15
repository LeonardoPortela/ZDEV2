@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estoque especial'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_MSLB
  as select from    nsdm_e_mslb as mslb
    left outer join lfa1        as lfa1 on lfa1.lifnr = mslb.lifnr
{
  key mslb.matnr,
  key mslb.werks,
  key mslb.lifnr,
      lfa1.name1,
      lfa1.ort01,
      cast('UN' as meins ) as meins_dummy,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mslb.lblab)      as clabs,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mslb.lbins)      as cinsm,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      sum(mslb.lbvla)      as cspem
}
where
  (
       mslb.lblab is not initial
    or mslb.lbins is not initial
    or mslb.lbvla is not initial
  )
group by
  mslb.matnr,
  mslb.werks,
  mslb.lifnr,
  lfa1.name1,
  lfa1.ort01
