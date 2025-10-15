@AbapCatalog.sqlViewName: 'ZVMIOVDOCFFIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca documentos financeiros documentos contabeis'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FIN_DOC_CONT
  as select from    ZI_MI_OV_TP_VENC  as Ordem

    inner join      ZI_MI_OV_FAT_PROC as Fat     on Ordem.Ordem = Fat.Ordem
  //left outer join ZI_MI_OV_FAT_PROC as Fat     on Ordem.Ordem = Fat.Ordem /// 27.05.2025
    left outer join acdoca            as DocCont on  DocCont.rldnr = '0L'
                                                 and DocCont.awref = Fat.Faturamento
                                                 and DocCont.bschl = '01'

    left outer join j_1bnflin         as NfeIt   on NfeIt.refkey = Fat.Faturamento
    left outer join j_1bnfdoc         as NfeDoc  on NfeDoc.docnum = NfeIt.docnum

    left outer join ZI_MI_DADOS_ZFIT0026 as Fit26 on  Fit26.Vbeln      = Ordem.Ordem
                                                  and Fit26.Doc_Fatura = Fat.Faturamento                                              
{
  key Ordem.Ordem,
  key coalesce(Fat.Faturamento,'0000000000') as Faturamento,
  key Fit26.Docnum                           as DocCont,

  key case coalesce ( Fit26.Docnum, '0000000000')
  when '0000000000' then
   ''
  else
   DocCont.augbl
  end                                        as DocComp,
      Fit26.zid_lanc                         as ID26,

      case coalesce(Fit26.Moeda,'') when '' then
        DocCont.rfccur
      else
        Fit26.Moeda end                      as moeda,
      //key DocCont.augbl    as DocComp,
      //DocCont.ksl      as ValorRecUSD,
        DocCont.hsl as hsl,
        DocCont.ksl as ksl,    
      case coalesce ( Fit26.Docnum, '')
          when '' then
             DocCont.ksl
          else

             case Fit26.Moeda when 'BRL' then

               case when Fit26.mont_moeda > 0 then

               case when Fit26.Ajuste = 'X' then

                     division(Fit26.mont_moeda,Fit26.taxa,6)

               else
                //Fit26.mont_mi
                Fit26.ksl - division(( Fit26.Vlr_Multa_Rbdo + Fit26.Vlr_Juros_Rbdo ),Fit26.taxa,6)
               end
               else
                  case when Fit26.Ajuste = 'X' then
                    division(Fit26.mont_moeda,Fit26.taxa,6)
                  else
                 Fit26.Mont_Mi
               end end
             else
               Fit26.mont_moeda
             end
          end                                as ValorRecUSD,

      //Fit26.mont_mi    as ValorRecUSD,
      case coalesce ( Fit26.Docnum, '')
          when '' then
      DocCont.hsl
      else
      Fit26.mont_moeda
      end                                    as ValorRec,

      Fit26.Data_Pgto                        as DtPagto,
      //DocCont.netdt    as DtVenc,

      case coalesce ( Fit26.Docnum, '0000000000')
          when '0000000000' then
        Fat.dataVenc
      else
        Fit26.data_venc
      end                                    as DtVenc,

      NfeDoc.docnum                          as docnum,

      case when TipoVencimento = 'F' then

        case when fat_m_c = 'X' then
            NfeDoc.nfenum
        else
            '         '
        end
      else
        '         '
      end                                    as NFE,

      case when TipoVencimento = 'F' then

        case when fat_m_c = 'X' then
            NfeIt.netwr
        else
            0
        end

      else
        0
      end                                    as Vlr_NFE,
      Fit26.ptax                             as Ptax,
      Fit26.Forma_Pag                        as ForPagto,
      Fit26.Vlr_Multa_Calc,
      Fit26.Vlr_Juros_Calc,
      Fit26.Vlr_Multa_Rbdo,
      Fit26.Vlr_Juros_Rbdo,
      Fit26.Vlr_Desc_Mult,
      Fit26.Vlr_Desc_Jros,
      Fit26.Observacao,

      case Fit26.Moeda when 'USD' then

      // 12.03.2025
          case when Fit26.Ajuste = 'X' then

             ( Fit26.Vlr_Multa_Calc +
                Fit26.Vlr_Juros_Calc -
                Fit26.Vlr_Multa_Rbdo -
                Fit26.Vlr_Juros_Rbdo ) -
             (   Fit26.Vlr_Desc_Mult +
                Fit26.Vlr_Desc_Jros )

          else

              ( Fit26.Vlr_Multa_Calc +
                Fit26.Vlr_Juros_Calc -
                Fit26.Vlr_Multa_Rbdo -
                Fit26.Vlr_Juros_Rbdo +
                Fit26.Vlr_Desc_Mult +
                Fit26.Vlr_Desc_Jros )
          end
      else
       0
      end                                    as SaldoJurosusd,

      case Fit26.Moeda
        when 'USD' then
            0
        else
      // 12.03.2025
          case when Fit26.Ajuste = 'X' then

        ( Fit26.Vlr_Multa_Calc + Fit26.Vlr_Juros_Calc -
             Fit26.Vlr_Multa_Rbdo - Fit26.Vlr_Juros_Rbdo )
             - ( Fit26.Vlr_Desc_Mult + Fit26.Vlr_Desc_Jros )

        else



            ( Fit26.Vlr_Multa_Calc + Fit26.Vlr_Juros_Calc -
             Fit26.Vlr_Multa_Rbdo - Fit26.Vlr_Juros_Rbdo +
             Fit26.Vlr_Desc_Mult + Fit26.Vlr_Desc_Jros )
        end
        end                                  as SaldoJuros,

      Fit26.mont_moeda                       as Mont_Calc,
      DocCont.wsl                            as ValorFatRef,

      // 13.01.2025 RAMON --->
      coalesce(Fat.qtd,0)                    as QtdFatRef,

      //      case coalesce ( Fit26.docnum, '') when '' then
      //        Fat.Valor
      //      else
      //        case Fit26.moeda when 'BRL' then
      //               Fit26.mont_mi
      //             else
      //               Fit26.mont_moeda
      //             end
      //      end as VlrTotRef,
      coalesce(Fat.Valor,0)                  as VlrTotRef,

      // 13.01.2025 RAMON ---<


      case Fit26.Moeda when 'BRL' then
        Fit26.Mont_Mi
      else
        Fit26.mont_moeda
      end                                    as VlrTotRef26,

      TipoVencimento,
      Fit26.Ajuste
}

//where TipoVencimento = 'F'
