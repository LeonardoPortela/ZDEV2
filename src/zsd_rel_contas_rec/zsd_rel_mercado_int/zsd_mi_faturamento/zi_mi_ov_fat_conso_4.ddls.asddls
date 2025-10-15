@AbapCatalog.sqlViewName: 'ZVMIOVFATCONS4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Exibição principal dos docs de fat'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FAT_CONSO_4
  as select from    ZI_MI_OV_CHAVES             as Chave // 30.01.2025 ----- ov 60049073
  //as select from    ZI_MI_OV_CHAVES_2           as Chave  /// cenario deu ruim - 343264
  //as select from    ZI_MI_OV_CHAVES_3           as Chave
  //as select from    ZI_MI_OV_CHAVES_4           as Chave //17.02.2025
    left outer join ZI_MI_OV_FAT_SUM            as SumCab    on Chave.OrdemDerivada = SumCab.Ordem
    left outer join ZI_MI_OV_FAT_SUM_PRINC      as SumFin    on Chave.OrdemDerivada = SumFin.Ordem
    left outer join ZI_MI_OV_FINAN_SUM_SALDO_D2 as SumDev    on Chave.OrdemDerivada = SumDev.Ordem
    left outer join ZI_MI_OV_DEVOL_AGREG        as SumAgr    on Chave.OrdemDerivada = SumAgr.vbeln1

    left outer join zsdt0051                    as Zsd51     on Zsd51.nro_sol_ov = Chave.Simulador
    left outer join zsdt0052                    as Zsd52     on Zsd52.nro_sol_ov = Chave.Simulador
    left outer join zsdt0053                    as zsd53     on  zsd53.nro_sol_ov = Chave.Simulador
    //and zsd53.posnr      = Chave.ItemSimulador
                                                             and zsd53.vbeln      = Chave.OrdemDerivada
    left outer join tvgrt                       as TVGRT_051 on  TVGRT_051.vkgrp = Zsd51.vkgrp
                                                             and TVGRT_051.spras = $session.system_language
  //left outer join tvkgr as tvkgr2 on tvkgr1.vkgrp = Ordem.EqVendas
  association to ZI_MI_DADOS_BASICOS_OV_2 as Ordem on Chave.OrdemDerivada = Ordem.Vbeln

  //  association to ZI_DADOS_OV_VENDA   as Venda on Chave.Ordem = Venda.Vbeln
{
  key Chave.Simulador,
  key Chave.ItemSimulador,
  key Chave.Ordem,
  key Chave.Empresa,
  key Chave.OrdemDerivada,

      coalesce(SumFin.NumDocs,0) as NumDocs,
      case coalesce(Zsd51.vkbur, ' ')
      when ' '
       then Ordem.EscVendas
       else Zsd51.vkbur
       end                       as EscVendas,

      case coalesce(Zsd51.vkgrp, ' ')
      when ' '
       then Ordem.EqVendas
       else Zsd51.vkgrp
       end                       as EqVendas,

      case coalesce(TVGRT_051.vkgrp, ' ')
      when ' '
       then Ordem.bezei
       else TVGRT_051.bezei
       end                       as EqVendas_txt,


      //Ordem.EscVendas,
      //Ordem.EqVendas,
      Ordem.TpOV,
      Ordem.TpDevVenda,
      Ordem.meins,
      //Ordem.Safra,

      Ordem.Cliente,
      Ordem.NomeCliente,
      //bseg.valdt as bseg_valdt,


      //      // 29.01.2025 - RAMON
      //      case coalesce(SumFin.NumDocs,0) when 0 then
      Ordem.Valdt                as data_venc, // 25.02.2025
      //      else
      //         Zsd52.valdt
      //      end                           as data_venc,


      ////            case coalesce(Ordem.Valdt,'00000000') when '00000000' then
      ////
      ////              case coalesce(Zsd52.valdt,'00000000') when '00000000' then
      ////                  bseg.valdt
      ////              else
      ////      Zsd52.valdt                      as data_venc,
      ////              end
      ////            else
      ////              Ordem.Valdt
      ////            end                              as data_venc,


      //------------
      zsd53.charg                as Safra,

      ////////      case coalesce(zsd53.charg, ' ') // 11.03.2025
      ////////        when '' then ''
      ////////        else







      case coalesce(Zsd52.pgto_ant, ' ')
        when 'X'
           then ' Com Boleto '
        when 'N'
           then ' Sem Boleto '
        when ' '
           then ' Não Antecipado '
        end                      as Pgto_ant,

      //      case coalesce(zsd53.charg, '') when '' then
      //        cast( '0' as zde003 )
      //      else
      Zsd51.tx_multa,
      //      end     as tx_multa,

      //      case coalesce(zsd53.charg, '') when '' then
      //        cast( '0' as zde003 )
      //      else
      Zsd51.tx_juros,
      //      end    as tx_juros,

      //      case coalesce(zsd53.charg, '') when '' then
      //        ''
      //      else
      Ordem.CondPagto,

      //      end                           as CondPagto, // 11.03.2025

      //      case coalesce(zsd53.charg, '') when '' then
      //        ''
      //      else
      Ordem.DescCondPagto,
      //            end as DescCondPagto, // 11.03.2025



      //---------------


      Ordem.DataCriacao,
      Ordem.Moeda,
      //Ordem.vbeln_p,

      case when Ordem.TpOV = 'ZREM' or Ordem.TpOV = 'ZRFU' then
        0
      else
        Ordem.Qtd
      end                        as Qtd,

      Ordem.ValorLiq,
      Ordem.ValorImp,

      case when Ordem.TpOV = 'ZREM' or Ordem.TpOV = 'ZRFU' then
        0
      else
        Ordem.ValorTotal
      end                        as ValorTotal,

      //      Ordem.ValorTotal,

      //Ordem.ValorTotal,
      SumFin.ValorRec            as TotalRecebido,


      case when Ordem.Qtd < 0 then
        ( SumCab.Qtd * -1 )
      else
        SumCab.Qtd
      end                        as QtdFat,
      //      case when SumCab.qtdCab > 0 then
      //        SumCab.qtdCab
      //      else
      //        Ordem.Qtd
      //      end as QtdFat,
      //SumCab.valorCab         as VlrFat,

      case Ordem.Moeda
        when 'BRL' then

      case when Ordem.TpOV = 'ZREM' or Ordem.TpOV = 'ZRFU' then
        0
      else
        case coalesce(SumFin.Ordem, '' ) when '' then
      //Verifica se for devolucao
                case Ordem.TpDevVenda when 'V' then
                    Ordem.ValorTotal
                else

      //                    // 17.02.2025 -->
                    case coalesce(SumDev.Ordem,'') when '' then

      // 28.05.2025 --->>
                       case coalesce(SumCab.Valor,0) when 0 then
                        Ordem.ValorTotal   // parte nova
                       else
                        coalesce(SumCab.Valor,0) * -1
                       end
      // 28.05.2025 ---<<

      //coalesce(SumCab.Valor,0) * -1         //XXXXXXXXXXXXXXXXXXX
                    else
      //                    // 17.02.2025 --<

                    coalesce(SumDev.ValorTot,0) * -1 //0    - 30.01.2025   ---- 17.02.2025 XXXXX
                end end
          else
            Ordem.ValorTotal - coalesce(SumFin.ValorRec,0)
          end
      end
        when 'USD'
            then
            0
        end                      as SaldoOV,

      case Ordem.Moeda
        when 'BRL' then
            0
        when 'USD' then

              case when Ordem.TpOV = 'ZREM' or Ordem.TpOV = 'ZRFU' then
        0
        else
            case coalesce(SumFin.Ordem, '' ) when '' then
      //Verifica se for devolucao
                case Ordem.TpDevVenda  when 'V' then
                    Ordem.ValorTotal
                else

      //                    // 17.02.2025 -->
                    case coalesce(SumDev.Ordem,'') when '' then
                       coalesce(SumCab.Valor,0) * -1
                    else
      //                    // 17.02.2025 --<


                    coalesce(SumDev.ValorTot,0) * -1 //0    - 30.01.2025   ---- 17.02.2025 XXXXX
      //SumDev.ValorTot - SumDev26.ValorTotRecUSD
                end end
            else
                Ordem.ValorTotal - coalesce(SumFin.ValorRecUSD,0)
            end end
        end                      as SaldoOVUSD,

      //SumFin.MontTotCalc,
      //SumDev26.ValorTotRec as ValorTotDev26,


      SumFin.SaldoJuros,

      SumFin.SaldoJurosusd,

      case Ordem.Moeda when 'USD' then
        SumFin.SaldoRef
      else
        0
      end                        as SaldoRefUSD,

      case Ordem.Moeda when 'BRL' then
        SumFin.SaldoRef
      else
        0
      end                        as SaldoRefBRL,

      SumFin.SaldoRef            as SaldoRefFiltro,

      case Ordem.Moeda when 'BRL' then

         case when Ordem.TpDevVenda = 'D' then
            SumAgr.SaldoJuros
         else
            SumFin.SaldoJuros
         end
      else

         case when Ordem.TpDevVenda = 'D' then
            SumAgr.SaldoJuros
         else
            SumFin.SaldoJurosusd
         end

      end                        as SaldoJurosFiltro,

      //      Ordem.com_devolucao,
      //      //// ---->
      //      Ordem.Qtd_Devol,
      //      Ordem.Vlr_Devol,
      //( Ordem.ValorTotal - Ordem.Vlr_Devol ) as Saldo_VlrDevol,
      //// ----<
      //( SumCab.Qtd - Ordem.Qtd_Devol )       as SaldoDevolucao,

      // 05.08.2025 -->
      //coalesce(SumFin.VlrFatNFE,0) - coalesce(SumFin.VlrTotRef,0) as sald_referencia,
      case coalesce(SumFin.QtdFatCab,0) when 0 then
        0
      else
       0 //coalesce(SumFin.VlrFatNFE,0) - ( coalesce(SumFin.ValorRec,0) + coalesce(SumFin.SaldoRef,0) )
      end                        as sald_referencia,
      // 05.08.2025 --<


      cast(  10 as rfmng )       as Tolerancia,
      cast(  -10 as rfmng )      as ToleranciaN,
      Ordem.vtweg                as vtweg,
      Ordem.spart                as spart,
      
      case zsd53.status 
      when 'E' then
      'Encerrada'
      when 'B' then
      'Bloqueada'
      when 'C' then
      'Cancelada'
      when 'Y' then
      'Devolução/Recusa'
      when 'W' then
      'Complemento'
      when 'D' then
      'Temp'
      when '' then
      'Item inicial'
      else
      ''
      end as status
}
