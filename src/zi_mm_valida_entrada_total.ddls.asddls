@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida entrada total'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
// Valida se tem registros com miro
define view entity zi_mm_valida_entrada_total as select from zib_nfe_dist_itm
{
    key chave_nfe,
        ebeln,
        ebelp,
    count(*) as qtd
}
where belnr_ft <> ''
group by
chave_nfe,
ebeln,
ebelp
