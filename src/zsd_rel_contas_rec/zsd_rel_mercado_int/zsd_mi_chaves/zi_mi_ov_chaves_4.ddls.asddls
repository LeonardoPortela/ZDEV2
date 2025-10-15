@AbapCatalog.sqlViewName: 'ZVMIOVCHAVES4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados de chaves das OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_4
  as select distinct from ZI_MI_OV_CHAVES_3
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada

}
where origem <> '3Y'
