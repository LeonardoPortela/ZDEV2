@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'reg DIST_TER com STATUS  = 99'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zi_mm_conf_total as select from zmmt0203 as notas
inner join zib_nfe_dist_ter as ter on notas.chave_nfe = ter.chave_nfe
{
    key notas.nro_cg,
    count(*) as  qtd
}
where ter.st_fiscal = '99'
group by notas.nro_cg
