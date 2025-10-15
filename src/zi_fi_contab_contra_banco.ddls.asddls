@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Contabilização Contra Banco'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CONTAB_CONTRA_BANCO
  as select from bsis_view  as BC
    inner join   bkpf       as BK1 on  BC.bukrs   = BK1.bukrs
                                   and BC.belnr   = BK1.belnr
                                   and BK1.stblg  = ''
                                 
    inner join   ska1   as SK on  BC.hkont = SK.saknr
                              and SK.ktopl = '0050'
                              and SK.ktoks = 'YB04'
{
  BC.bukrs,
  BC.belnr,
  BC.gjahr
}
group by
  BC.bukrs,
  BC.belnr,
  BC.gjahr
