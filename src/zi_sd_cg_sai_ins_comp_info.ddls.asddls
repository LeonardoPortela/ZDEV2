@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cargas Saida Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_CG_SAI_INS_COMP_INFO as 
 select from zsdt0133 as cg  left outer join zsdt0410 as nf on cg.nro_cg = nf.nro_cg 
                                                           and nf.cancel = ''                                                      
                             left outer join zsdt0376 as bd on cg.nro_cg   = bd.id_autorizacao_embarque

{
   key cg.nro_cg,
               
       sum(
           case 
                 when nf.processo = '2' then 1
            else 0 end 
        ) as count_nf_venda,
         
       sum(
          case 
             when nf.processo = '1' then 1
          else 0 end 
        ) as count_nf_transf_forn,
        
       sum(
          case 
             when coalesce( bd.quantidade,0) > 0 then 1
          else 0 end 
        ) as count_bordero
         
}

 group by cg.nro_cg
