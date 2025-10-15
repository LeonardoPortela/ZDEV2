@AbapCatalog.sqlViewName: 'ZVMIOVCHAVES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados de chaves das OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES
  as select from    ZI_MI_OV_CHAVES_2          as chave
{
  key chave.Simulador,
  key chave.ItemSimulador,
  key chave.Ordem,
  key chave.Empresa,
  key chave.OrdemDerivada
}
