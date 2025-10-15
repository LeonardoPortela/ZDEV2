@AbapCatalog.sqlViewName: 'ZVMIOVFATSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca somatorio Qtd Faturada'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_SUM
  as select from ZI_MI_OV_FAT_UNI
{
  key Ordem,
      sum( qtd ) as Qtd,
      sum( Valor) as Valor
}
group by
  Ordem
