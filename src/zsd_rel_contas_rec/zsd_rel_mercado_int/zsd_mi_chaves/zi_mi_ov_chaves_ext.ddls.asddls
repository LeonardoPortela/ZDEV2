@AbapCatalog.sqlViewName: 'ZVMIOVCHAVESEXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ordens sem simulador na ZSDT0053'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_EXT
  as select distinct from vbak 
    inner join            vbfa              on vbak.vbeln = vbfa.vbeln
    inner join            zsdt0053 as z53    on z53.vbeln = vbfa.vbelv
    left outer join       zsdt0053 as zdevol on z53.vbeln = zdevol.doc_precedente
{
  key z53.nro_sol_ov as Simulador,
  key z53.posnr      as ItemSimulador,
  key vbfa.vbelv     as Ordem,
  key vbak.bukrs_vf  as Empresa,
  key vbfa.vbeln     as OrdemDerivada
}
where
      ( vbtyp_n       = 'H' or vbtyp_n       = 'L' )
  and vbtyp_v       = 'C'

  and zdevol.status is null
