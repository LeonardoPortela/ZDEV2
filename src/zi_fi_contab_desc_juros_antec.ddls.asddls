@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Contabilização Descontos/Juros Antecipação'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CONTAB_DESC_JUROS_ANTEC
  as select from bsis_view  as D
    inner join   bkpf       as BK1 on  D.bukrs   = BK1.bukrs
                                   and D.belnr   = BK1.belnr
                                   and BK1.stblg = ''
    inner join   zfit0211 as CD  on D.hkont = CD.hkont
{
  D.bukrs,
  D.belnr,
  BK1.waers,
  @Semantics.amount.currencyCode: 'waers'
  sum( D.dmbtr ) as dmbtr,
  @Semantics.amount.currencyCode: 'waers'
  sum( D.dmbe2 )  as dmbe2

}
  
group by
  D.bukrs,
  D.belnr,
  BK1.waers
