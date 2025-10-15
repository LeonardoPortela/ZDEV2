@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Simuladores e OV União'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_SIMU_OV
  as select distinct from ZI_IN_SIMU_OV_UNI as simu
    inner join            ZI_IN_DADOS_VBAK  as vbak on vbak.vbeln = simu.vbeln
{
  key cast(simu.doc_simulacao as zsded003) as doc_simulacao,
  key cast(simu.vbeln_p as vbeln)          as vbeln_p,
  key cast(simu.vbeln as vbeln)            as vbeln
}
