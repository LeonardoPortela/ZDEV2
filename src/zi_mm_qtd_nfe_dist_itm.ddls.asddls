@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida se há reg na ZIB_NFE_DIST_ITM'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
// Valida se tem entrada parcial
define view entity ZI_MM_QTD_NFE_DIST_ITM as select from zib_nfe_dist_itm
{
    key chave_nfe,
        ebeln,
        ebelp,
    count(*) as qtd
}
where belnr_ft = ''
group by
chave_nfe,
ebeln,
ebelp

