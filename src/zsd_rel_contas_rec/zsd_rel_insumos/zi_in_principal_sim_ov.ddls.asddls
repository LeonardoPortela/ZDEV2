@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'NAO USADO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_PRINCIPAL_SIM_OV
  as select distinct  from ZI_IN_OV_SIMULACAO as sim
{
  key sim.doc_simulacao,
  
  key sim.vbeln_p as vbeln,
  
  case sim.vbeln when sim.vbeln_p
    then 'X'
  else
    '' end as principal 
}
