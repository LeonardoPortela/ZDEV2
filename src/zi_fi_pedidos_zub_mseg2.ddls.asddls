@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Definição de registros da MSEG'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_fi_pedidos_zub_mseg2
  as select from nsdm_e_mseg
{
  key mblnr,
  key mjahr,
  key zeile,
  ebeln,
  ebelp
}
