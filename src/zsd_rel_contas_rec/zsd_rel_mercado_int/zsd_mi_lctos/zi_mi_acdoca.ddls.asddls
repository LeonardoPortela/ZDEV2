@AbapCatalog.sqlViewName: 'ZIMIACDOCA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Formatação da ACDOCA para lctos'
define view ZI_MI_ACDOCA
  as select from acdoca as acdoca
{
  key awref as Ordem,
  key belnr as DocCont,
  key augbl as DocComp,

      rtcur as Moeda,
      ksl   as ValorRecUSD,
      hsl   as ValorRec,
      augdt as DtPagto,
      netdt as DtVenc,

      case when ksl is initial then
        1
      else
        division(hsl,ksl,6)
      end   as kurrf

}
where
      rldnr     = '0L'
  and bschl     = '09' //Adiantamento
  and xreversed = ''
