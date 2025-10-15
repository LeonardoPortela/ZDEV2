@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Formatação da T056P para consultas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_BUSCA_PTAX as select from tcurr
{
    key kurst as Kurst,
    key fcurr as Fcurr,
    key tcurr as Tcurr,
    key cast(cast(99999999 - cast(gdatu as abap.int4) as char20) as abap.dats) as datab,
    ukurs as Ukurs,
    ffact as Ffact,
    tfact as Tfact
}
