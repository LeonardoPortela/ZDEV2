@AbapCatalog.sqlViewName: 'ZVMIZFIT26_MIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação propria da tabela'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_ZFIT0026_MIN
  as select from zfit0026
{
  key vbeln           as vbeln,
  key doc_fatura      as doc_fatura,
      min(seq)        as seq_min,
      max(seq)        as seq_max,
      count(*)        as num_reg,
      sum(mont_moeda) as sum_mont_moeda

}
where
  doc_fatura is not initial
group by
  vbeln,
  doc_fatura
