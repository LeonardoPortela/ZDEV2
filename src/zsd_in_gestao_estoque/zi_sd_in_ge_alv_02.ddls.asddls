@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição ALV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_ALV_02
  as select from ZI_SD_IN_GE_ALV_01 as mara
  association [0..*] to ZI_SD_IN_GE_UNI    as _estoque on  mara.matnr = _estoque.matnr
                                                       and mara.werks = _estoque.werks
  association [0..1] to ZI_SD_IN_GE_VENDAS as _vendas  on  mara.matnr = _vendas.matnr
                                                       and mara.werks = _vendas.werks
{
  key matnr,
  key werks,
      matkl,
      cultivar
}
