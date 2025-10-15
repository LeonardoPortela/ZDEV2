@AbapCatalog.sqlViewName: 'ZVMIOVDOCFFIN2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros documentos contabeis'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_DOC_CONT_2
  as select from    ZI_MI_OV_FIN_DOC_CONT as cont
    left outer join acdoca                as doca on  doca.rldnr = '0L'
                                                  and doca.buzei = '001'
                                                  and doca.belnr = cont.DocCont
{
  key cont.Ordem,
  key cont.Faturamento,
  key cont.DocCont,
  key case coalesce(doca.belnr,'') when '' then
        cont.DocComp
      else
        doca.augbl end as DocComp,

      cont.ID26,
      cont.moeda,

      //case coalesce(doca.belnr,'') when '' then
      //  cont.ValorRecUSD
     // else
     //   doca.ksl end   as ValorRecUSD,
     cont.ValorRecUSD as ValorRecUSD,

      cont.ValorRec,

      cont.DtPagto,
      cont.DtVenc,

      cont.docnum,
      cont.NFE,
      cont.Vlr_NFE,
      cont.Ptax,
      cont.ForPagto,

      cont.vlr_multa_calc,
      cont.vlr_juros_calc,
      cont.vlr_multa_rbdo,
      cont.vlr_juros_rbdo,
      cont.vlr_desc_mult,
      cont.vlr_desc_jros,
      cont.observacao,
      cont.SaldoJurosusd,
      cont.SaldoJuros,
      cont.Mont_Calc,
      cont.ValorFatRef,
      cont.QtdFatRef,
      cont.VlrTotRef,
      cont.VlrTotRef26,
      cont.TipoVencimento,
      cont.ajuste
}

where
  (
        cont.TipoVencimento = 'V'
    and cont.DocCont        is not initial
  )
  or    cont.TipoVencimento = 'F'
