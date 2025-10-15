@AbapCatalog.sqlViewName: 'ZVMIOVZACDFIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros acdoca'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_ACDC
  as select from    ZI_MI_OV_TP_VENC as Ordem

    inner join      acdoca           as Boleto on  Boleto.rldnr     = '0L'
                                               and Boleto.awref     = Ordem.Ordem
                                               and Boleto.bschl     = '09' //Adiantamento
                                               and Boleto.xreversed = ''

    left outer join zfit0026         as Fit26  on  Fit26.vbeln  = Ordem.Ordem
                                               and Fit26.docnum = Boleto.belnr

{
  key Ordem.Ordem,
  key '          '                   as Faturamento,
  key Boleto.belnr                   as DocCont,
  key Boleto.augbl                   as DocComp,
      Fit26.zid_lanc                 as ID26,
      Fit26.moeda                    as Moeda,
      Boleto.ksl                     as ValorRecUSD,
      Boleto.hsl                     as ValorRec,
      Boleto.augdt                   as DtPagto,

      case coalesce(Fit26.data_venc,' ') when ' ' then
        Boleto.netdt
      else
        Fit26.data_venc end          as DtVenc,

      //Boleto.netdt    as DtVenc, // 09.01.2025 - ramon
      '          '                   as NFE,
      Fit26.taxa                     as Ptax,
      Fit26.forma_pag                as ForPagto,
      Fit26.vlr_multa_calc,
      Fit26.vlr_juros_calc,
      Fit26.vlr_multa_rbdo,
      Fit26.vlr_juros_rbdo,
      Fit26.vlr_desc_mult,
      Fit26.vlr_desc_jros,
      Fit26.observacao,

      case Fit26.moeda
        when 'USD'
            then
           ( Fit26.vlr_multa_calc +
             Fit26.vlr_juros_calc  )
             -
             ( Fit26.vlr_multa_rbdo +
             Fit26.vlr_juros_rbdo +
             Fit26.vlr_desc_mult +
             Fit26.vlr_desc_jros )
        else
            0
        end                          as SaldoJurosusd,

      case Fit26.moeda
        when 'USD'
           then
            0
        else
            ( Fit26.vlr_multa_calc +
             Fit26.vlr_juros_calc  )
             -
             ( Fit26.vlr_multa_rbdo +
             Fit26.vlr_juros_rbdo +
             Fit26.vlr_desc_mult +
             Fit26.vlr_desc_jros )
        end                          as SaldoJuros,
      cast( 0 as abap.dec( 15, 3 ) ) as QtdFat,
      Boleto.augbl                   as augbl,

      //20.01.2025 --->
      case coalesce(Fit26.data_venc,' ') when ' ' then
        Boleto.ksl
      else

      // 20.01.2025 -->
      case Fit26.moeda when 'BRL' then
        Fit26.mont_mi
      else
        Fit26.mont_moeda
      end
      // 20.01.2025 --<
      end                            as VlrTotRef,

      TipoVencimento,
      Fit26.ajuste
}


where
  TipoVencimento = 'V'
