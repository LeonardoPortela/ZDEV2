@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BSID - PEGA AS PARTIDAS DOS CLIENTES'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZFIR0124_CLIENTE_BSID
  with parameters
    p_emp_bsid : bukrs,
    p_abe_bsid : dats
  as select from bsid_view as bsid
  association [0..1] to kna1     as _Cliente on bsid.kunnr = _Cliente.kunnr
  association [0..1] to zfit0231 as _Conta   on bsid.hkont = _Conta.hkont_ativo
{
  bsid.bukrs,
  bsid.hkont,
  bsid.kunnr                              as clifor,
  _Cliente.name1,
  bsid.vbel2                              as ov,
  bsid.belnr,
  bsid.blart,
  bsid.budat,
  bsid.bldat,
  bsid.zfbdt,
  bsid.gjahr,
  bsid.gsber,
  bsid.shkzg,
  cast( bsid.dmbtr as abap.dec( 15, 2 ) ) as VLR_BRL,
  cast( bsid.dmbe2 as abap.dec( 15, 2 ) ) as VLR_USD
}
where
      bsid.bstat         <> 'S'
  and bsid.bukrs         = $parameters.p_emp_bsid
  and bsid.budat         <= $parameters.p_abe_bsid
  and bsid.blart         <> 'VC'
  and _Conta.hkont_ativo is not null
