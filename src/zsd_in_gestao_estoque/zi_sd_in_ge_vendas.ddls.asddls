@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição das Vendas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_VENDAS
  as select from ZI_SD_IN_GE_VBAP as vbap
  association [1] to vbak as _vbak on vbap.vbeln = _vbak.vbeln
{
  key vbap.matnr,
  key vbap.werks,
      vbap.kmein,
      _vbak.auart,
      _vbak.vkorg,
      //vbak.erdat,
      @Semantics.quantity.unitOfMeasure: 'kmein'
      sum(vbap.kwmeng) as kwmeng
}
group by
  vbap.matnr,
  vbap.werks,
  vbap.kmein,
  _vbak.auart,
  _vbak.vkorg
