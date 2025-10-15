@AbapCatalog.sqlViewName: 'ZVMIOVFATPROC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca faturamento das Ordens'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_PROC
  as select from ZI_MI_OV_FAT_2 as fat
    inner join   t052           as t052 on  t052.zterm = fat.zterm
                                        and t052.ztagg = '00'

{
  key fat.Ordem,
  key fat.Faturamento,
      fat.qtd,
      fat.Valor,
      fat.valdt,
      fat.zterm,

      case t052.zdart when 'B' then

        case coalesce(t052.ztag1,'') when '' then
            fat.valdt
        else
            dats_add_days(fat.valdt, cast(t052.ztag1 as abap.int4), 'INITIAL' )
        end
      else
        fat.valdt end as dataVenc,

      fat.fat_m_c

}
