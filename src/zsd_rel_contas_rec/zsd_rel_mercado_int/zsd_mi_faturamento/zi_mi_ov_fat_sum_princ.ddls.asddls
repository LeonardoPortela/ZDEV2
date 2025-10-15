@AbapCatalog.sqlViewName: 'ZVMIOVFATPRIS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_SUM_PRINC
  //as select from ZI_MI_OV_FAT_PRINCIPAL - 27.01.2025
  as select from ZI_MI_OV_FAT_MAIN
{
  key Ordem,
      count(*)           as NumDocs,
      sum(QtdFatCab)     as QtdFatCab,
      sum(QtdFatRef)     as QtdFatRef,
      sum(VlrTotRef)     as VlrTotRef,
      sum(ValorRecUSD)   as ValorRecUSD,
      sum(ValorRec)      as ValorRec,
      sum(Vlr_NFE)       as VlrFatNFE,
      sum(SaldoJurosusd) as SaldoJurosusd,
      sum(SaldoJuros)    as SaldoJuros,
      sum(SaldoRef)      as SaldoRef
}
group by
  Ordem
