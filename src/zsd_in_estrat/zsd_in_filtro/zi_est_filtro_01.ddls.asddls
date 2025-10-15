@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro para relatorio'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_FILTRO_01
  as select from ZI_IN_SIMU_OV as simu
    inner join   vbak          as vbak on simu.vbeln = vbak.vbeln
    inner join   kna1          as kna1 on vbak.kunnr = kna1.kunnr
{
  simu.doc_simulacao,
  simu.vbeln,
  kna1.kunnr,
  kna1.name1,
  vbak.vkorg,
  vbak.spart,
  vbak.vkbur,
  vbak.vkgrp,
  vbak.auart,
  vbak.erdat
}
