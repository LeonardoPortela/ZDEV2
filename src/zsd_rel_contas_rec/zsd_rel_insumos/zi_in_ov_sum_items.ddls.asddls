@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somatório dos Itens'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_OV_SUM_ITEMS
  as select from ZI_IN_DADOS_VBAP as vbap
{
  key vbap.vbeln,
      sum(vbap.totalq_ov) as xtotalq_ov,
      sum(netwr + mwsbp)  as xtotalvl_ov,
      sum(netwr)          as netwr_l,
      sum(mwsbp)          as mwsbp
}
group by
  vbap.vbeln
