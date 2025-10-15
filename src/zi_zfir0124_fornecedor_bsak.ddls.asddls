@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BSAK - PEGA AS PARTIDAS DOS CLIENTES'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ZFIR0124_FORNECEDOR_BSAK
  with parameters
    p_emp_bsak : bukrs,
    p_abe_bsak : dats
  as select from bsak_view as bsak
  association [0..1] to lfa1     as _Fornecedor on bsak.lifnr = _Fornecedor.lifnr
  association [0..1] to zfit0231 as _Conta      on bsak.hkont = _Conta.hkont_ativo
{
  bsak.bukrs,
  bsak.hkont,
  bsak.lifnr                              as clifor,
  _Fornecedor.name1,
  cast( '' as abap.char(10) )             as ov,
  bsak.belnr,
  bsak.blart,
  bsak.budat,
  bsak.bldat,
  bsak.zfbdt,
  bsak.gjahr,
  bsak.gsber,
  bsak.shkzg,
  cast( bsak.dmbtr as abap.dec( 15, 2 ) ) as VLR_BRL,
  cast( bsak.dmbe2 as abap.dec( 15, 2 ) ) as VLR_USD
}
where
      bsak.bstat         <> 'S'
  and bsak.bukrs         = $parameters.p_emp_bsak
  and bsak.augdt         > $parameters.p_abe_bsak
  and bsak.budat         <= $parameters.p_abe_bsak
  and bsak.augbl         <> bsak.belnr
  and bsak.blart         <> 'VC'
  and _Conta.hkont_ativo is not null
