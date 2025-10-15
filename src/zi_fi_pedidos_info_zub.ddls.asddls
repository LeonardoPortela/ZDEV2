@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pedidos ZUB para filtrar'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_PEDIDOS_INFO_ZUB
  as select from ZI_FI_PEDIDOS_INFO
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
      case meins when 'L' then menge / fator
      else menge / 1 end                         as menge_conv,
      GET_NUMERIC_VALUE ( dmbtr_0053 ) as dmbtr_0053,
      GET_NUMERIC_VALUE ( dmbtr_bseg ) as dmbtr_bseg,
      GET_NUMERIC_VALUE (dmbe2_bseg) as dmbe2_bseg,
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
