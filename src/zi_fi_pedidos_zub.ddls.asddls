@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pedidos ZUB - Chamada principal'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_fi_pedidos_zub
  as select from ZI_FI_PEDIDOS_INFO_ZUB
{
  key awkey,
      bukrs,
      gsber,
      shkzg,
      kunnr,
      name1,
      tipo,
      belnr,
      mjahr,
      zeile,
      augbl,
      augdt,
      budat,
      vbel2,
      vbeln,
      auart,
      nro_sol_ov,
      tp_venda,
      gewei,
      meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      menge,
      menge_conv,
      ( menge_conv *  dmbtr_0053 ) * ( case when shkzg = 'S' then -1 else 1 end )                             as dmbtr,
      ( ( menge_conv * dmbtr_0053 ) * ( case when shkzg = 'S' then -1 else 1 end ) ) / ( dmbtr_bseg / dmbe2_bseg ) as dmbe2,
      ( dmbtr_bseg / dmbe2_bseg )                               as tx_camb,
      banco_liq,
      waers,
      zfbdt,
      butxt,
      matnr,
      matkl,
      maktx,
      buzei,
      charg,
      tpsim,
      spart,
      ktokd
}
where
  auart = 'ZUB'
