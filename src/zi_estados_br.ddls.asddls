@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estados do brasil'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_ESTADOS_BR as select distinct from t005s as a 
inner join t005u as b on a.bland = b.bland and a.land1 = b.land1
{
key a.bland as Bland,
b.bezei
}
where a.land1 = 'BR'
