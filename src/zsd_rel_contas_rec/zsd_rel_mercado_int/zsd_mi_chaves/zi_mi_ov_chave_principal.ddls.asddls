@AbapCatalog.sqlViewName: 'ZVMIOVPRINC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca OV principal'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVE_PRINCIPAL
  as select from ZI_MI_OV_CHAVE_PRINCIPAL_2 as OVPrinc
  association to ZI_MI_OV_FILHA_FLUXO_2 as OVs on OVPrinc.Ordem = OVs.Ordem

{
  key OVPrinc.Simulador,
  key OVPrinc.ItemSimulador,
  key OVPrinc.Ordem,
  key OVPrinc.Empresa,
  key OVs.OrdemFilha as OrdemDerivada
}
