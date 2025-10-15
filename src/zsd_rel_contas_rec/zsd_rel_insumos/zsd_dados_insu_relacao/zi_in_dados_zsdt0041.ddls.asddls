@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de simuladores e OV da ZSDT0041'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_ZSDT0041
  as select distinct from zsdt0041 as Z0041
//    inner join            zsdt0090 as Z0090 on  Z0041.doc_simulacao = Z0090.doc_simulacao
//                                            and Z0041.vbeln         = Z0090.vbelv
{
  key Z0041.doc_simulacao,
  key Z0041.vbeln as vbeln_p,
  key Z0041.vbeln as vbeln
}
