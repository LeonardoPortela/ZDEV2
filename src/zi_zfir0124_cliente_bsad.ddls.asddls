@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BSAD - PEGA AS PARTIDAS DOS CLIENTES'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZFIR0124_CLIENTE_BSAD
  with parameters
    p_emp_bsad : bukrs,
    p_abe_bsad : dats
  as select from bsad_view as bsad
  association [0..1] to kna1     as kna1     on bsad.kunnr = kna1.kunnr
  association [0..1] to zfit0231 as zfit0231 on bsad.hkont = zfit0231.hkont_ativo
{
  bsad.bukrs,
  bsad.hkont,
  bsad.kunnr                              as clifor,
  kna1.name1,
  bsad.vbel2                              as ov,
  bsad.belnr,
  bsad.blart,
  bsad.budat,
  bsad.bldat,
  bsad.zfbdt,
  bsad.gjahr,
  bsad.gsber,
  bsad.shkzg,
  cast( bsad.dmbtr as abap.dec( 15, 2 ) ) as VLR_BRL,
  cast( bsad.dmbe2 as abap.dec( 15, 2 ) ) as VLR_USD
}
where
      bsad.bstat           <> 'S'
  and bsad.bukrs           = $parameters.p_emp_bsad
  and bsad.augdt           > $parameters.p_abe_bsad
  and bsad.budat           <= $parameters.p_abe_bsad
  and bsad.augbl           <> bsad.belnr
  and bsad.blart           <> 'VC'
  and zfit0231.hkont_ativo is not null
