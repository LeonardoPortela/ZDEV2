@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Compensação Documentos Contabeis - Totais'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_COMPENSACAO_DOC_TOTAIS
 as select from bsad_view as A
    inner join   bkpf   as BK on  A.bukrs = BK.bukrs
                              and A.augbl = BK.belnr


{
  A.bukrs,
  A.augbl,
  'D' as KOART,
  A.umskz,
  BK.waers,
  @Semantics.amount.currencyCode: 'waers'
  sum( A.dmbtr ) as DMBTR,
  @Semantics.amount.currencyCode: 'waers'
  sum( A.dmbe2 ) as DMBE2
}

where A.belnr <> A.augbl
  and A.augbl <> ''
  and BK.stblg = ''

group by
  A.bukrs,
  A.augbl,
  A.umskz,
  BK.waers
