@AbapCatalog.sqlViewName: 'ZVMIOVSUMNEG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar valores dos itens de devoluções'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FINAN_SUM_SALDO_D2
  as select from ZI_MI_OV_FINAN_SUM_SALDO_D
{
  key Ordem,
      sum( ValorTotLiq ) as ValorTot
}
group by
  Ordem
