@AbapCatalog.sqlViewName: 'ZVMIOVFATMAIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_MAIN
  as select from ZI_MI_OV_FAT_CONSOLIDADO_2
{
  key Ordem,
  key Faturamento,
  key DocCont,
  key DocComp,
      ID26,
      moeda,
      QtdFatRef   as QtdFatCab,

      case when Nivel_Lcto = 2 then
        0
      else
        QtdFatRef
      end         as QtdFatRef,


      case when Nivel_Lcto = 2 then
        0
      else
        VlrTotRef
      end         as VlrTotRef,

      VlrTotREC_SUM,
      ValorRecUSD,
      ValorRec,
      DtPagto,
      DtVenc,
      NFE,
      Vlr_NFE,
      Ptax,
      ForPagto,
      vlr_multa_calc,
      vlr_juros_calc,
      vlr_multa_rbdo,
      vlr_juros_rbdo,
      vlr_desc_mult,
      vlr_desc_jros,
      observacao,
      SaldoJurosusd,
      SaldoJuros,
      Nivel_Lcto,
      VlrTotRef26,
      ajuste,

      case when Nivel_Lcto = 2 then
          0
          else

      case coalesce(NFE,'') when '' then
      
      // 05.08.2025 -->
//     case when Nivel_Lcto = 0 then
//        ( 0 - coalesce(VlrTotREC_SUM,0) )
//     else
//       ( coalesce(VlrTotRef26,0) - coalesce(VlrTotREC_SUM,0) )
//     end
     
      ( coalesce(VlrTotRef26,0) - coalesce(VlrTotREC_SUM,0) )
      // 05.08.2025 --<
     
      
      
      else

      // 17.02.2025 --->
        case coalesce(DocComp, '') when '' then
            case coalesce(ajuste, '') when '' then

                coalesce(VlrTotRef,0)


            else
                ( coalesce(VlrTotRef,0) - coalesce(VlrTotREC_SUM,0) )
            end
        else
      // 17.02.2025 ---<

          ( coalesce(VlrTotRef,0) - coalesce(VlrTotREC_SUM,0) )
      end end end as SaldoRef


}
