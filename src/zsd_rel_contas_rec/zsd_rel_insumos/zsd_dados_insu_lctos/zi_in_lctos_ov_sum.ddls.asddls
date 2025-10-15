@AbapCatalog.sqlViewName: 'ZVINLCTOSOVSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lctos ZSFIT0026 Somados'
define view ZI_IN_LCTOS_OV_SUM
  as select from ZI_IN_LCTOS_OV
{
  key vbeln_va,

      count(*)         as Num_Reg,
      sum(vlr_rec_usd) as vlr_rec_usd_sum,
      sum(vlr_rec_brl) as vlr_rec_brl_sum,

      sum(multa_calc)  as multa_calc_sum,
      sum(multa_rec)   as multa_rec,

      sum(desc_multa)  as desc_multa_sum,
      sum(juros_calc)  as juros_calc_sum,

      sum(juros_rec)   as juros_rec_sum,
      sum(desc_juros)  as desc_juros_sum

}
group by
  vbeln_va
