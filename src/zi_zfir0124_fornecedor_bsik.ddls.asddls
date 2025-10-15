@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BSIK - PEGA AS PARTIDAS DOS CLIENTES'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZFIR0124_FORNECEDOR_BSIK
  with parameters
    p_emp_bsik : bukrs,
    p_abe_bsik : dats
  as select from bsik_view as bsik
  association [0..1] to lfa1     as _Fornecedor on bsik.lifnr = _Fornecedor.lifnr
  association [0..1] to zfit0231 as _Conta      on bsik.hkont = _Conta.hkont_ativo

{
  bsik.bukrs,
  bsik.hkont,
  bsik.lifnr                              as clifor,
  _Fornecedor.name1,
  cast( '' as abap.char(10) )             as ov,
  bsik.belnr,
  bsik.blart,
  bsik.budat,
  bsik.bldat,
  bsik.zfbdt,
  bsik.gjahr,
  bsik.gsber,
  bsik.shkzg,
  cast( bsik.dmbtr as abap.dec( 15, 2 ) ) as VLR_BRL,
  cast( bsik.dmbe2 as abap.dec( 15, 2 ) ) as VLR_USD
}
where
      bsik.bstat         <> 'S'
  and bsik.bukrs         = $parameters.p_emp_bsik
  and bsik.budat         <= $parameters.p_abe_bsik
  and bsik.blart         <> 'VC'
  and _Conta.hkont_ativo is not null
