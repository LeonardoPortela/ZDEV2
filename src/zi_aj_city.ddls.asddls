@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Substitudo do F4 ZSDST_AJ_CITY/ZSDV_AJ_CITY'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_AJ_CITY as select from j_1btxjurt as M 
inner join j_1btreg_city as C on M.country = C.country and M.taxjurcode = C.taxjurcode
{
    M.taxjurcode, M.country, M.text, M.spras
}
where M.spras = 'P'
and M.text <> ''
