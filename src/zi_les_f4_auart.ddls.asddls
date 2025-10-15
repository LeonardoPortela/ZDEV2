@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'HELP AUART'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_LES_F4_AUART as select from tvarvc
{
    low as AUART
}
where 1 = 1
and name = 'OV_ARMAZ_TRANSB'
and low  <> ''
