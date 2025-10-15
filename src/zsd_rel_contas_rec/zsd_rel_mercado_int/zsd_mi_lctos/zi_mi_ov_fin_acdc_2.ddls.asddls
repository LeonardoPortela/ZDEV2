@AbapCatalog.sqlViewName: 'ZVMIOVZACDFIN2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca de docs financeiro'
define view ZI_MI_OV_FIN_ACDC_2
  as select from    ZI_MI_OV_TP_VENC as Ordem
    inner join      ZI_MI_ACDOCA     as boleto on boleto.Ordem = Ordem.Ordem
    left outer join zfit0026         as fit26  on  fit26.vbeln  = Ordem.Ordem
                                               and fit26.docnum = boleto.DocCont
                                               and fit26.ajuste = ' ' // teste ov 13512604 - 23/06/2025

{
  key Ordem.Ordem,
  key '          '                                                                              as Faturamento,
  key boleto.DocCont,
  key boleto.DocComp,
      fit26.zid_lanc                                                                            as ID26,
      fit26.moeda                                                                               as Moeda,

      // 02.06.2025 --->
      boleto.ValorRec - ( coalesce(fit26.vlr_juros_rbdo,0) + coalesce(fit26.vlr_multa_rbdo,0) ) as ValorRec,

      case when fit26.moeda = 'BRL' then
        boleto.ValorRecUSD - ( division(coalesce(fit26.vlr_juros_rbdo,0),boleto.kurrf,2) + division(coalesce(fit26.vlr_multa_rbdo,0),boleto.kurrf,2))
      else
        boleto.ValorRecUSD - ( coalesce(fit26.vlr_juros_rbdo,0) + coalesce(fit26.vlr_multa_rbdo,0) )
      end                                                                                       as ValorRecUSD,
      // 02.06.2025 ---<


      ////////////      // 27.05.2025 --->
      ////////////      boleto.ValorRec - coalesce(fit26.vlr_juros_rbdo,0) as ValorRec,
      ////////////
      ////////////      case when fit26.moeda = 'BRL' then
      ////////////        boleto.ValorRecUSD - division(coalesce(fit26.vlr_juros_rbdo,0),boleto.kurrf,2)
      ////////////      else
      ////////////        boleto.ValorRecUSD - coalesce(fit26.vlr_juros_rbdo,0)
      ////////////      end                                                as ValorRecUSD,
      ////////////      // 27.05.2025 ---<


      boleto.DtPagto,

      case coalesce(fit26.data_venc,' ') when ' ' then
        boleto.DtVenc
      else
        fit26.data_venc end                                                                     as DtVenc,

      //Boleto.netdt    as DtVenc, // 09.01.2025 - ramon
      '          '                                                                              as NFE,
      fit26.taxa                                                                                as Ptax,
      fit26.forma_pag                                                                           as ForPagto,
      fit26.vlr_multa_calc,
      fit26.vlr_juros_calc,
      fit26.vlr_multa_rbdo,
      fit26.vlr_juros_rbdo,
      fit26.vlr_desc_mult,
      fit26.vlr_desc_jros,
      fit26.observacao,

      case fit26.moeda
        when 'USD'
            then
           ( fit26.vlr_multa_calc +
             fit26.vlr_juros_calc  )
             -
             ( fit26.vlr_multa_rbdo +
             fit26.vlr_juros_rbdo +
             fit26.vlr_desc_mult +
             fit26.vlr_desc_jros )
        else
            0
        end                                                                                     as SaldoJurosusd,

      case fit26.moeda
        when 'USD'
           then
            0
        else
            ( fit26.vlr_multa_calc +
             fit26.vlr_juros_calc  )
             -
             ( fit26.vlr_multa_rbdo +
             fit26.vlr_juros_rbdo +
             fit26.vlr_desc_mult +
             fit26.vlr_desc_jros )
        end                                                                                     as SaldoJuros,
      cast( 0 as abap.dec( 15, 3 ) )                                                            as QtdFat,
      boleto.DocComp                                                                            as augbl,

      //20.01.2025 --->
      case coalesce(fit26.data_venc,' ') when ' ' then
        boleto.ValorRecUSD
      else

      // 20.01.2025 -->
      case fit26.moeda when 'BRL' then
        fit26.mont_mi
      else
        fit26.mont_moeda
      end
      // 20.01.2025 --<
      end                                                                                       as VlrTotRef,

      Ordem.TipoVencimento,
      fit26.ajuste
}


where
      Ordem.TipoVencimento = 'V'

  and fit26.doc_fatura     is initial //180604 - 28.05.2025 - Cenário 03
