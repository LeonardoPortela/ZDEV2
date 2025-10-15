@AbapCatalog.sqlViewName: 'ZVMIOVFATUNI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Uniao dos documentos faturados e estornados'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_UNI
  as select from ZI_MI_OV_FAT_3
//  as select from ZI_MI_OV_FAT_2  // comentado OV: 13512604, pq tem que ter remessa para preencher o campo
{
  key Ordem,
  key Faturamento,
      qtd,
      Valor
}
// teste ramon ----
//union select from ZI_MI_OV_FAT_EST
//{

  //key Ordem,
  //key Faturamento,
  //    Qtd * -1 as Qtd

//}
