@AbapCatalog.sqlViewName: 'ZVMIOVFATCONS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_CONSOLIDADO
  as select from ZI_MI_OV_FINAN_UNI as uni
  left outer join ZI_MI_ZFIT0026 as z26 on  uni.ID26 = z26.zid_lanc
                                        and uni.Ordem = z26.vbeln
                                        and uni.Faturamento = z26.doc_fatura
{
  key uni.Ordem,
  key uni.Faturamento,
  key DocCont,
  key uni.DocComp,
      uni.ID26,
      uni.moeda,
      uni.QtdFatRef,
      uni.VlrTotRef,
      
      case coalesce(ID26, ' ') when ' ' then
        0
      else
        uni.ValorRecUSD
      end as ValorRecUSD,

      case coalesce(ID26, ' ')  when ' ' then
        0
      else
        uni.ValorRec
      end as ValorRec,
      uni.DtPagto,
      uni.DtVenc,
      uni.NFE,
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
      z26.mont_lcto,
      z26.Nivel_Lcto
//      case coalesce(ID26, ' ') when ' ' then
//        0
//      else
//        
//        case when z26.Nivel_Lcto = 1 then      
//            0 
//        else
//            z26.mont_lcto   
//        end
//      end as mont_lcto
}
