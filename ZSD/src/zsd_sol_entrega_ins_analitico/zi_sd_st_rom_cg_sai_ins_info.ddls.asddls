@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status Faturamento Romaneio Carga Saida Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_ST_ROM_CG_SAI_INS_INFO as 
  
  select from zsdt0001 as ro
     inner join zsdt0129 as lt on ro.nro_cg   = lt.nro_cg
     inner join zsdt0131 as ic on lt.nro_lote = ic.nro_lote         
                                 and ro.vbeln = ic.vbeln
     left outer join j_1bnfdoc as nf on ro.nro_nf_prod = nf.docnum                                 
  
{ 

  key  ro.nro_cg,
               
       sum(
           case 
                 when ro.st_proc = '99' then 1
            else 0 end 
        ) as count_romaneio_finalizado,
         
       sum(
          case 
             when ro.st_proc <> '99' then 1
          else 0 end 
        ) as count_romaneio_pendente,
        
       max(nf.docdat) as dt_finalizacao_faturamento
    
} 
 
 group by ro.nro_cg
  
  
