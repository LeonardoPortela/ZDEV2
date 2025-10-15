@AbapCatalog.sqlViewName: 'ZVMIOVADIAFIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros adiantamentos'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_ADIA
  as select from ZI_MI_OV_TP_VENC as Ordem

    inner join   bseg             as Adia  on  Adia.vbel2 = Ordem.Ordem
                                           and Adia.zumsk = 'A' //Adiantamento

    inner join   zfit0026         as Fit26 on  Fit26.vbeln  = Ordem.Ordem
                                           and Fit26.docnum = Adia.belnr

{
  key Ordem.Ordem,
  key '          '                   as Faturamento,
  key Adia.belnr                     as DocCont,
  key Adia.augbl                     as DocComp,
      Fit26.zid_lanc                 as ID26,
      Fit26.moeda                    as Moeda,
      Adia.dmbe2                     as ValorRecUSD,
      Adia.wrbtr                     as ValorRec,
      Adia.augdt                     as DtPagto,
      Adia.netdt                     as DtVenc,
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
        Fit26.vlr_juros_calc +
        Fit26.vlr_multa_rbdo +
        Fit26.vlr_juros_rbdo +
        Fit26.vlr_desc_mult +
        Fit26.vlr_desc_jros )
      else
       0
      end                            as SaldoJurosusd,

      case Fit26.moeda
        when 'USD'
           then
            0
        else
            ( Fit26.vlr_multa_calc +
             Fit26.vlr_juros_calc +
             Fit26.vlr_multa_rbdo +
             Fit26.vlr_juros_rbdo +
             Fit26.vlr_desc_mult +
             Fit26.vlr_desc_jros )
        end                          as SaldoJuros,
      cast( 0 as abap.dec( 15, 3 ) ) as QtdFat,

      // 20.01.2025 -->
      case Fit26.moeda when 'BRL' then
        Fit26.mont_mi
      else
        Fit26.mont_moeda
      end                            as VlrTotRef,
      // 20.01.2025 --<

      TipoVencimento,
      Fit26.ajuste
}


where
  TipoVencimento = 'V'
