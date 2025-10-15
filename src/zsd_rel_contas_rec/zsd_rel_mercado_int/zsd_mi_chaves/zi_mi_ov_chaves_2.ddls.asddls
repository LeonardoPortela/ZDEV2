@AbapCatalog.sqlViewName: 'ZVMIOVCHAVES2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados de chaves das OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_2
  as select from ZI_MI_OV_CHAVE_PRINCIPAL

{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada
}

union select distinct from ZI_MI_OV_CHAVE_PRINCIPAL_3
{

  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada
}
