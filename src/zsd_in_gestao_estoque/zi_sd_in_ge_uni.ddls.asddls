@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estoques'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_UNI
  as select from ZI_SD_IN_GE_MCHB_02
{
  key matnr,
  key werks,
  key lifnr,
      name1,
      ort01,
      meins_dummy,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      clabs,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      cinsm,
      @Semantics.quantity.unitOfMeasure: 'meins_dummy'
      cspem
}
union all select from ZI_SD_IN_GE_MSLB
{
  key matnr,
  key werks,
  key lifnr,
      name1,
      ort01,
      meins_dummy,
      clabs,
      cinsm,
      cspem
}
