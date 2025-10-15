@AbapCatalog.sqlViewName: 'ZVMIOVFATCONS_2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_CONSOLIDADO_2
  as select from    ZI_MI_OV_FINAN_UNI as uni
  //as select from ZI_MI_OV_FIN_DOCS_2 as uni     ---- 28.01.2025

    left outer join ZI_MI_ZFIT0026_2   as z26 on  uni.ID26        = z26.zid_lanc
                                              and uni.Ordem       = z26.vbeln
                                              and uni.Faturamento = z26.doc_fatura
{
  key uni.Ordem,
  key uni.Faturamento,
  key DocCont,
  key uni.DocComp,


      uni.ID26,
      uni.moeda,
      uni.QtdFatRef,


      case when uni.TipoVencimento = 'V' then
        0
      else
         uni.VlrTotRef
      end            as VlrTotRef,

        case when z26.ajuste = 'X' then
              z26.mont_moeda
        else
        uni.ValorRec
        end  as VlrTotREC_SUM,
        
      case coalesce(uni.ID26, '') when '' then // teste sem docComp. dia 14.02.2025
      //case coalesce(uni.DocComp, '')  when '' then
        0
      else

      // 17.02.2025 --->
        case coalesce(uni.DocComp, '') when '' then
            case coalesce(uni.ajuste, '') when '' then
                0
            else
                uni.ValorRecUSD
            end
        else
      // 17.02.2025 ---<

        uni.ValorRecUSD
      end end        as ValorRecUSD,

      case coalesce(uni.ID26, '') when '' then // teste sem docComp. dia 14.02.2025
      //case coalesce(uni.DocComp, '')  when '' then
        0
      else

      // 17.02.2025 --->
        case coalesce(uni.DocComp, '') when '' then
            case coalesce(uni.ajuste, '') when '' then
                0
            else
                uni.ValorRec
            end
        else
      // 17.02.2025 ---<
    
        // teste ramon 27.05.2025 ----->
        case when uni.ValorRec = uni.vlr_juros_rbdo then 0 else uni.ValorRec end
        // teste ramon 27.05.2025 -----<
        //uni.ValorRec
      end    end        as ValorRec,

      // 17.02.2025 -->
      case coalesce(uni.ID26, '') when '' then // teste sem docComp. dia 14.02.2025
       
        cast('00000000' as abap.dats)
      
      else
        
        case coalesce(uni.DocComp, '')  when '' then
            
            // 11.03.2025 -->
            case when uni.ajuste = 'X' then
                uni.DtPagto
            else
            // 11.03.2025 --<          
                cast('00000000' as abap.dats)
            end
        
        else
            uni.DtPagto
      end end as DtPagto,    

      // 17.02.2025 --<
      //uni.DtPagto,
      uni.DtVenc,
      uni.NFE,

      case when z26.Nivel_Lcto = 2 then
        0
      else
        uni.Vlr_NFE
      end            as Vlr_NFE,

      uni.Ptax,
      uni.ForPagto,
      uni.vlr_multa_calc,
      uni.vlr_juros_calc,
      uni.vlr_multa_rbdo,
      uni.vlr_juros_rbdo,
      uni.vlr_desc_mult,
      uni.vlr_desc_jros,
      uni.observacao,
      uni.SaldoJurosusd,
      uni.SaldoJuros,
      uni.VlrTotRef26,
      uni.TipoVencimento,
      z26.Nivel_Lcto,
      z26.mont_moeda,
      uni.ajuste

}
