@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'AJUDA F1/F4 SIM OU NÃO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_F1_SIM_NAO
  as select from dd07t
{
  cast(ddtext as abap.char(3))     as name,
  cast(domvalue_l as abap.char(1)) as id
}
where
  domname = 'ZD_SIM_NAO'
