@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro para Materiais'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_MARA
  as select from ZI_SD_IN_MAT_CARACT as _caract
  association [1..1] to mara as _mara on _caract.Material = _mara.matnr
  association [1..*] to marc as _marc on _caract.Material = _marc.matnr
{
  key _caract.Material   as matnr,
  key _marc.werks,
      _mara.matkl,
      _caract.ValorClasf as cultivar

}
