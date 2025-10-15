@AbapCatalog.sqlViewName: 'ZVMIOVZFITFIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros ZFIT0026'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_ZFIT
  as select from ZI_MI_OV_TP_VENC as Ordem

    inner join   zfit0026         as Fit26 on  Fit26.vbeln  = Ordem.Ordem
                                           and Fit26.ajuste = 'X'

{
  key Ordem.Ordem,
  key '          '     as Faturamento,
  key '          '     as DocCont,
  key '          '     as DocComp,
      Fit26.zid_lanc   as ID26,
      Fit26.moeda,
      case Fit26.moeda
       when 'BRL'
         then
          Fit26.mont_mi
       else
         Fit26.mont_moeda
       end             as ValorRecUSD,

      case Fit26.moeda
        when 'BRL'
          then
          Fit26.mont_moeda
        else
           Fit26.mont_mi
        end            as ValorRec,

      Fit26.data_pgto  as DtPagto,
      Fit26.data_venc  as DtVenc,
      '          '     as NFE,
      Fit26.taxa       as Ptax,
      Fit26.forma_pag  as ForPagto,
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
      end              as SaldoJurosusd,

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
        end            as SaldoJuros,
      //cast( 0 as abap.dec( 15, 3 ) ) as QtdFat,
      Fit26.mont_moeda as VlrTotRef,
      TipoVencimento,
      Fit26.ajuste
}


where
  TipoVencimento = 'V'
