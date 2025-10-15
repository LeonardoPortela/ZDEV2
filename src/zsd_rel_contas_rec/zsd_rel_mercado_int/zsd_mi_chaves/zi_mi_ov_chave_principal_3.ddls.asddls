@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca de dados de chaves das OVs'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MI_OV_CHAVE_PRINCIPAL_3
  as select distinct from ZI_MI_OV_CHAVE_PRINCIPAL_2 as OVPrinc
  //left outer join       ZI_MI_OV_FILHA_FLUXO_2     as OVs on OVPrinc.Ordem = OVs.Ordem
{
  key OVPrinc.Simulador,
  key OVPrinc.ItemSimulador,
  key OVPrinc.Ordem,
  key OVPrinc.Empresa,
  key OVPrinc.Ordem as OrdemDerivada
}
//where
//  OVs.Ordem is null
