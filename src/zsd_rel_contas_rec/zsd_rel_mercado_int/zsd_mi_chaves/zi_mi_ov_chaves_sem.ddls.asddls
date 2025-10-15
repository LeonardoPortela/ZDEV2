@AbapCatalog.sqlViewName: 'ZVMIOVCHAVESSEM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sem vinculo simulador na ZSDT0053'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_SEM
  as select distinct from vbak
    inner join            tvarvc                 as Spart on Spart.name = 'Z_SPART_REL_MERCADO_INTER'
    inner join            ZI_MI_OV_FAT           as fat   on fat.Ordem = vbak.vbeln
    inner join            ZI_MI_OV_CHAVES_TP_ORD as tp    on tp.Ordem = vbak.vbeln
    left outer join       ZI_MI_OV_CHAVES_EXT    as ext   on ext.OrdemDerivada = vbak.vbeln
    left outer join       zsdt0053               as z53   on z53.vbeln = vbak.vbeln
{
  key '0000000000'  as Simulador,
  key '000000'      as ItemSimulador,
  key vbak.vbeln    as Ordem,
  key vbak.bukrs_vf as Empresa,
  key vbak.vbeln    as OrdemDerivada
}
where
      z53.nro_sol_ov is null
  and ext.Simulador  is null
