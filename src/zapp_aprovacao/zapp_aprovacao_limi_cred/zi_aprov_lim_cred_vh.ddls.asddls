@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help Cliente'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_APROV_LIM_CRED_VH
  as select from kna1 as _KNA1
{
      @ObjectModel.text.element: ['nomecliente']
  key _KNA1.kunnr as Cliente,
      _KNA1.name1 as nomecliente
}
