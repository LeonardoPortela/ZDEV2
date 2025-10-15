@AbapCatalog.sqlViewName: 'ZVMIOVZFITFIN2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros ZFIT0026'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_ZFIT_2
  as select from    ZI_MI_OV_TP_VENC as Ordem

    inner join      ZI_MI_DADOS_ZFIT0026        as Fit26  on Fit26.Vbeln = Ordem.Ordem
    left outer join ZI_MI_ACDOCA     as acdoca on  acdoca.Ordem   = Ordem.Ordem
                                               and acdoca.DocCont = Fit26.Docnum
    left outer join ZI_IN_BUSCA_PTAX     as ptax on  ptax.datab = Fit26.Data_Pgto
                                                 and ptax.Kurst = 'B'
                                                 and ptax.Fcurr = 'USD'
                                                 and ptax.Tcurr = 'BRL'                                            
  //and Fit26.ajuste = 'X'

{
  key Ordem.Ordem,
  key '          '     as Faturamento,
  key case when Fit26.Ajuste = 'X' then
  Fit26.Docnum
  else
  '          ' 
  end as  DocCont,
  key '          '     as DocComp,
      Fit26.zid_lanc   as ID26,
      Fit26.Moeda,
 

      case Fit26.Moeda when 'BRL' then
       
       // 26.06.2025 - ov 1351260, lanc 13512604 -->
       
       case when Fit26.taxa <> 0 then
        division(Fit26.mont_moeda, Fit26.taxa, 2)
       else
        Fit26.mont_moeda
       end
       
          //Fit26.mont_mi
       
       // 26.06.2025 - ov 1351260, lanc 13512604 --<
       else
         Fit26.mont_moeda
       end             as ValorRecUSD,

      case Fit26.Moeda
        when 'BRL'
          then
          Fit26.mont_moeda
        else
           Fit26.Mont_Mi
        end            as ValorRec,

      Fit26.Data_Pgto  as DtPagto,
      Fit26.data_venc  as DtVenc,
      '          '     as NFE,
          
      
      Fit26.taxa       as Ptax,
      Fit26.Forma_Pag  as ForPagto,
      Fit26.Vlr_Multa_Calc,
      Fit26.Vlr_Juros_Calc,
      Fit26.Vlr_Multa_Rbdo,
      Fit26.Vlr_Juros_Rbdo,
      Fit26.Vlr_Desc_Mult,
      Fit26.Vlr_Desc_Jros,
      Fit26.Observacao,

      case Fit26.Moeda
      when 'USD'
      then
      ( Fit26.Vlr_Multa_Calc +
       Fit26.Vlr_Juros_Calc  )
             -
             ( Fit26.Vlr_Multa_Rbdo +
             Fit26.Vlr_Juros_Rbdo +
             Fit26.Vlr_Desc_Mult +
             Fit26.Vlr_Desc_Jros )
      else
      0
      end              as SaldoJurosusd,

      case Fit26.Moeda
        when 'USD'
           then
            0
        else
            ( Fit26.Vlr_Multa_Calc +
              Fit26.Vlr_Juros_Calc  )
             -
             ( Fit26.Vlr_Multa_Rbdo +
             Fit26.Vlr_Juros_Rbdo +
             Fit26.Vlr_Desc_Mult +
             Fit26.Vlr_Desc_Jros )
        end            as SaldoJuros,
      //cast( 0 as abap.dec( 15, 3 ) ) as QtdFat,
      Fit26.mont_moeda as VlrTotRef,
      TipoVencimento,
      Fit26.Ajuste
}

where
            TipoVencimento   = 'V'
  and(
    (
            Fit26.Ajuste     = 'X'
      or(
            Fit26.Ajuste     = ' '
        and Fit26.Docnum     is initial
        and Fit26.Doc_Fatura is initial
        and acdoca.Ordem     is null
      )
    )
  )
