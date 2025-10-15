@AbapCatalog.sqlViewName: 'ZVINLCTOSOV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lançamentos por OV'
define view ZI_IN_LCTOS_OV
  as select from ZI_IN_LCTOS_UNI as uni
{
  key uni.vbeln          as vbeln_va,
  key uni.zid_lanc,
  key uni.seq,
      uni.moeda,
      uni.belnr,
      uni.augbl,
      uni.data_venc,
      uni.data_pgto,
      uni.taxa,
      uni.forma_pag,

      uni.dmbtr_usd      as vlr_rec_usd,
      uni.dmbtr          as vlr_rec_brl,

      //uni.mont_rbdo,

      uni.vlr_multa_calc as multa_calc,
      uni.vlr_multa_rbdo as multa_rec,

      uni.vlr_desc_mult  as desc_multa,
      uni.vlr_juros_calc as juros_calc,

      uni.vlr_juros_rbdo as juros_rec,
      uni.vlr_desc_jros  as desc_juros,
      uni.num_comp_adiant,
      uni.observacao
}
