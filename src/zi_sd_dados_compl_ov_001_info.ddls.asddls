@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Complementares OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_DADOS_COMPL_OV_001_INFO
  as select from vbap as v
{

  v.vbeln,
  max(charg) as charg,
  max(matnr) as matnr

}

group by
  v.vbeln
