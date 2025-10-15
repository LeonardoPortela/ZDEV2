@AbapCatalog.sqlViewName: 'ZVMIOVFATPRINC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_PRINCIPAL
  as select from ZI_MI_OV_FAT_MAIN
  //as select from ZI_MI_OV_FAT_CONSOLIDADO 27/01/2025
{
  key Ordem,
  key Faturamento,
  key DocCont,
  key DocComp,
      ID26,

      ////      case when Nivel_Lcto = 1 then
      ////      0
      ////      else
      ////        QtdFatRef
      ////      end  as
      QtdFatRef,

      //      case TipoVencimento when 'F' then
      //
      //        case when Nivel_Lcto = 1 then
      //            0
      //        else
      //            VlrTotRef
      //        end
      //
      //      else
      //        0
      //       end as
      VlrTotRef,
      ValorRecUSD,
      ValorRec,
      DtPagto,
      DtVenc,
      NFE,
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
      //mont_lcto,
      Nivel_Lcto,

     SaldoRef

      //      case TipoVencimento when 'F' then
      //
      //          case coalesce(NFE,'') when '' then
      //            case moeda when 'USD' then
      //                ( 0 - ValorRecUSD )
      //            else
      //                ( 0 - ValorRec )
      //            end
      //          else
      //            case moeda when 'USD' then
      //                ( VlrTotRef - ValorRecUSD ) - coalesce(mont_lcto,0)
      //            else
      //               case coalesce(DocCont,'0000000000') when '0000000000' then
      //                    //( VlrTotRef - mont_lcto )
      //                    ( VlrTotRef - ValorRec ) - coalesce(mont_lcto,0)
      //               else
      //                    ( VlrTotRef - ValorRec ) - coalesce(mont_lcto,0)// se tiver que mudar aqui, quando id26 ==nul nao soma
      //               end
      //           end
      //         end
      //      else
      //        0
      //      end  as

      //VlrTotRef26,
      //TipoVencimento
}
