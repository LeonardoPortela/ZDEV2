@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MSEG para consultar BKPF'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_fi_pedidos_zub_mseg
  as select from nsdm_e_mseg as mseg

{

  key mseg.mblnr,
  key mseg.mjahr,
  key mseg.zeile,
      budat_mkpf                     as budat,
      CONCAT(mseg.mblnr, mseg.mjahr) as awkey,
      mseg.ebeln,
      mseg.xauto,
      mseg.werks                     as gsber,
      LPAD( mseg.werks,10,'0')       as kunnr,
      'PEDIDO ZUB'                   as tipo,
      mseg.meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      mseg.menge,
      matnr,
      mseg.buzei,
      mseg.charg

}
