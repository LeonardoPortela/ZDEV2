@AbapCatalog.sqlViewName: 'ZVMIOVPRINC2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca OV principal'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVE_PRINCIPAL_2
  as select distinct from ZI_MI_OV_FILHA_FLUXO_2 as OV //adicionado distinct 31.01.2025
    left outer join       zsdt0053               as OVPrinc on OV.Ordem = OVPrinc.vbeln
{
  key OV.Ordem,
  key OV.Empresa,
  key OVPrinc.nro_sol_ov as Simulador,
  key OVPrinc.posnr      as ItemSimulador


}
where
  (
    OVPrinc.status     <> 'Y'
  )
  or OVPrinc.nro_sol_ov is null
