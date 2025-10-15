@AbapCatalog.sqlViewName: 'ZVMIOVDEVOL2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca devolucao de uma ov'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_DEVOLUCAO_2
  as select from    ZI_MI_OV_DEVOLUCAO     as dev
    inner join      vbak                   as vbak   on dev.vbeln2 = vbak.vbeln
    left outer join tvarvc                 as tvarvc on  tvarvc.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                                     and tvarvc.low  = vbak.auart
    inner join      ZI_MI_OV_FAT_SUM_PRINC as fat    on fat.Ordem = dev.vbeln2
  //inner join   ZI_MI_OV_FAT_SUM_PRINC as fat on fat.Ordem = dev.vbeln2
{
  key dev.vbeln1,
      sum(QtdFatRef) as qtd_fat_devol,
      sum(

       case coalesce(tvarvc.low, '')
       when '' then
       fat.SaldoRef
       else
           VlrTotRef
        end  )       as vlr_fat_devol
}
group by
  dev.vbeln1
