@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LOCAL OPERACAO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_F1_TIPO_CALCULO_ZSDR0156
  as select from dd07t
{
  
  key cast( domvalue_l as abap.dec( 1, 0 ) ) as ztpcalc,
  cast( ddtext as abap.char( 35 ) ) as ZTPCALCDESC
}
where
  domname = 'Z_TIPO_CALCULO_ZSDR0156'
