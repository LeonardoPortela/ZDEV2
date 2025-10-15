@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Escritorios de vendas de Insumos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_SPART_CONF
  as select distinct from setleaf
{
  key cast(valfrom as spart) as spart
}
where
      valfrom is not initial
  and setname = 'ZFIS26_SA_IN'
