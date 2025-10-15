@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Carregamento Carga Saida Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_DT_CAR_CG_INSUMOS_INFO as 

 select from  zsdt0133 as cg
    
    inner join zsdt0410 as nc      on  cg.nro_cg   = nc.nro_cg
                                   and nc.processo = '2'
                                   and nc.cancel   = ''
    
    inner join zib_nfe_dist_ter as nt on nc.chave_nfe = nt.chave_nfe

    {
      key cg.nro_cg            as nro_cg,
          min( nt.dt_emissao ) as dt_carregamento
    }

group by cg.nro_cg

 union 
 
   select from zsdt0133 as cg 
    inner join zsdt0376 as bd  on cg.nro_cg = bd.id_autorizacao_embarque 
   
    {
    
      key cg.nro_cg       as nro_cg,
      min( bd.date_create )  as dt_carregamento
    
    }

group by cg.nro_cg
