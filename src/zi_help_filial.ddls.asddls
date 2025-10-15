@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'EMPRESAS DA T001'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_HELP_FILIAL as select from t001w
    inner join t001 on t001w.vkorg = t001.bukrs
{
    key t001w.werks,
    t001w.name1,
    t001.bukrs
}
