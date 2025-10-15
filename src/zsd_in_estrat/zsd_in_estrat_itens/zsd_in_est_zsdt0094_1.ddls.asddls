@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Taxa ZSDT0094'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_ZSDT0094_1
  as select from zsdt0094 as z94
{
  key max(z94.nro_sol_ov)  as doc_simulacao,
      max(z94.taxa_cambio) as taxa_cambio
}
where
      z94.tipo_taxa = 'C'
  and z94.tipo      = 'VDI'
  and z94.vbeln     = ''
  and z94.estorno   = '0000000000'
group by
  z94.nro_sol_ov
