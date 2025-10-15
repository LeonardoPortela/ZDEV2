@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Combinação já existente na zsdt0116'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_EST_FILTRO_Z16_Z16_REC
  as select from ZI_EST_SIMU_OV as simu
    inner join   zsdt0116       as z16  on  simu.vbeln = z16.vbeln
                                        and z16.posnr  = '000000'
    inner join   zsdt0040       as z040 on z040.doc_simulacao = simu.doc_simulacao
    inner join   ZI_EST_KNA1    as kna1 on kna1.kunnr = z040.kunnr
{
  key simu.vbeln,
  key kna1.cnpj_cpf,
  key z040.safra,
  key z040.cultura
}
