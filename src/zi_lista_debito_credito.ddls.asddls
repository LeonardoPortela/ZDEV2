@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LISTA DEBITO CREDITO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_LISTA_DEBITO_CREDITO
  as

  select from dd07l
{
  case when domvalue_l = 'C' then 'Credito' else 'Debito' end  as nome,
  domvalue_l as tipo
}
where
      domname    =  'ZCRED_DEB'
  and domvalue_l <> ''
