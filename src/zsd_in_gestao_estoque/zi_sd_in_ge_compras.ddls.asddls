@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição das Compras'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_COMPRAS
  as select from ekko as ekko
  association [0..*] to ekpo as _ekpo on ekko.ebeln = _ekpo.ebeln
{
  key _ekpo.matnr,
  key _ekpo.werks,
      _ekpo.meins,
      ekko.bsart,
      ekko.bukrs,
      //ekko.aedat,
      @Semantics.quantity.unitOfMeasure: 'meins'
      sum(_ekpo.menge) as menge
}
group by
  _ekpo.matnr,
  _ekpo.werks,
  _ekpo.meins,
  ekko.bsart,
  ekko.bukrs
