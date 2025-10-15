@AbapCatalog.sqlViewName: 'ZVMIOVFATPROCSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca faturamento das Ordens'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_PROC_SUM
  as select from ZI_MI_OV_FAT_PROC

{
  key Ordem,
      sum(qtd)   as qtdCab,
      sum(Valor) as valorCab
}
group by
  Ordem
