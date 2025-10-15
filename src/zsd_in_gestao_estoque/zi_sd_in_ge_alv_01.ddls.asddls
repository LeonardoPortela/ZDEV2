@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição Gestão de Estoque'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_ALV_01
  as select from I_Material as mara
  association [0..1] to ZI_SD_IN_MAT_CARACT as _caract on mara.Material = _caract.Material

{
  key mara.Material             as matnr,
  key mara._MaterialPlant.Plant as werks,
      mara.MaterialGroup        as matkl,
      _caract.ValorClasf        as cultivar

}
