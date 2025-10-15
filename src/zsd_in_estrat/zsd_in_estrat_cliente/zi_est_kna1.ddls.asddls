@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados KNA1 para estrategia'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_KNA1
  as select from kna1 as kna1
{
  key kna1.kunnr,
  key case coalesce(kna1.stcd1,'') when '' then kna1.stcd2 else kna1.stcd1 end as cnpj_cpf
}
