@AbapCatalog.sqlViewName: 'ZVINAGREG01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agregação de dados para o Relatorio'
define view ZI_IN_AGREG_01
  as select from    ZI_IN_SIMU_OV      as relacao
    left outer join ZI_IN_DADOS_SIM    as simulador  on relacao.doc_simulacao = simulador.doc_simulacao
    left outer join ZI_IN_OV_DADOS_2   as doc_vendas on relacao.vbeln = doc_vendas.vbeln
    left outer join ZI_IN_FAT_SUM      as fat        on relacao.vbeln = fat.vbeln_va
    left outer join ZI_IN_LCTOS_OV_SUM as lctos_sum  on relacao.vbeln = lctos_sum.vbeln_va
    left outer join ZI_IN_LCTOS_OV     as lctos      on relacao.vbeln = lctos.vbeln_va
{
  key relacao.doc_simulacao,
  key relacao.vbeln_p,
  key relacao.vbeln,
      simulador.id_order_ecommerce   as id_ecomm,
      coalesce(lctos_sum.Num_Reg,0 ) as Num_Reg,
      doc_vendas.bukrs,
      doc_vendas.vkbur,
      doc_vendas.vkgrp,
      doc_vendas.vkgrp_name,
      doc_vendas.auart,
      simulador.ecommerce,
      simulador.safra,
      simulador.cultura,
      simulador.cultura_desc,

      doc_vendas.kunnr,
      doc_vendas.name1,

      simulador.pgto_ant,
      doc_vendas.zterm,
      doc_vendas.zterm_name,

      simulador.tx_juros,

      doc_vendas.erdat,
      doc_vendas.waerk,

      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
        0
      else

        case when doc_vendas.auart = 'ZFUT'  or doc_vendas.auart = 'ZTRI' then
            doc_vendas.zmeng
        else
            doc_vendas.kwmeng * doc_vendas.fator_devol //07.03.2025
        end
      end                            as qtde,

      ////// volta de versao 11.06.2025 - doc: 13582131 ------->
      //////////////      // 29.05.2025 - RAMON -->
      //////////////      case coalesce(fat.menge_fat,0) when 0 then
      //////////////
      //////////////      // 03.06.2025 - cenario 3 -  --> 181301
      //////////////          case coalesce(lctos_sum.Num_Reg,0 ) when 0 then
      //////////////            0
      //////////////          else
      //////////////            doc_vendas.kwmeng * doc_vendas.fator_devol
      //////////////          end
      //////////////      // doc_vendas.kwmeng * doc_vendas.fator_devol
      //////////////      // 03.06.2025 - cenario 3 -  --< 181301
      //////////////
      //////////////
      //////////////      else
      //////////////
      //////////////      // 10.06.2025 - cenario ov 60047567 -->
      //////////////        case when doc_vendas.fator_devol = -1 then
      //////////////            doc_vendas.kwmeng * doc_vendas.fator_devol
      //////////////        else
      //////////////      // 10.06.2025 - cenario ov 60047567 --<
      //////////////
      //////////////        fat.menge_fat * doc_vendas.fator_devol
      //////////////      end     end                    as qtde_fat,

      // se nao for devolução pega o valor do faturamento
      case when doc_vendas.fator_devol = 1 then
        
        // 31.07.2025 - causa: 13112333 -->
        case coalesce(fat.menge_fat,0) when 0 then
            0
        else
            
            case when fat.menge_fat < 0 then
                0
            else
            
                coalesce(fat.menge_fat,0) * doc_vendas.fator_devol
            
            end
            
        end
        
        //coalesce(fat.menge_fat,0) * doc_vendas.fator_devol
        
        // 31.07.2025 - causa: 13112333 --<
      else

      // se for devolução e não tiver faturamento, então coloca o vlr da ov ( linha 3104, ZSDR0020)
        //case coalesce(fat.menge_fat,0) when 0 then
        //    doc_vendas.kwmeng  * doc_vendas.fator_devol
        //else

           // case when fat.menge_fat < 0 then
                doc_vendas.kwmeng * doc_vendas.fator_devol // se faturamento menor que zero em uma devolução, recebeu a OV total
           // else
           //     coalesce(fat.menge_fat,0) * doc_vendas.fator_devol
           // end
        //end
      end                            as qtde_fat,

      // 29.05.2025 - RAMON --<
      //coalesce(fat.menge_fat,0) * doc_vendas.fator_devol as qtde_fat,
      ////// volta de versao 11.06.2025 - doc: 13582131 <-----

      coalesce(fat.menge_fat,0)      as menge_fat,

      doc_vendas.netwr               as vlr_liq,
      doc_vendas.mwsbp               as vlr_imp,

      doc_vendas.fator_devol,
      lctos_sum.vlr_rec_brl_sum,
      lctos_sum.vlr_rec_usd_sum,

      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
      0
      else
      doc_vendas.vlr_tot_ov
      end                            as vlr_tot_ov,

      case when  doc_vendas.waerk = 'BRL' then
        lctos_sum.vlr_rec_brl_sum
      else
        lctos_sum.vlr_rec_usd_sum
      end                            as vlr_liq_sum,


      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
          0 - lctos_sum.vlr_rec_brl_sum
      else
           case when  doc_vendas.waerk = 'BRL' then
              doc_vendas.vlr_tot_ov - lctos_sum.vlr_rec_brl_sum
            else
              0
            end
      end                            as saldo_ov_brl,

      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
        0 - lctos_sum.vlr_rec_usd_sum
      else
         case when  doc_vendas.waerk = 'USD' then
            doc_vendas.vlr_tot_ov - lctos_sum.vlr_rec_usd_sum
         else
            0
         end
       end                           as saldo_ov_usd,

      ////// DADOS DE LANÇAMENTO

      lctos.zid_lanc,
      doc_vendas.valdt,

      //lctos.data_venc,

      lctos.data_pgto,
      lctos.taxa,
      lctos.forma_pag,

      lctos.belnr,
      lctos.augbl,

      lctos.vlr_rec_brl,
      lctos.vlr_rec_usd,
      lctos.multa_calc,
      lctos.multa_rec,

      lctos.desc_multa,
      lctos.juros_calc,
      lctos.juros_rec,

      lctos.desc_juros,

      case when  doc_vendas.waerk = 'BRL' then
        ( lctos.multa_calc + lctos.juros_calc ) - ( lctos.multa_rec + lctos.juros_rec + lctos.desc_multa + lctos.desc_juros )
      else
        0 end                        as saldo_juros_brl,

      case when  doc_vendas.waerk = 'USD' then
        ( lctos.multa_calc + lctos.juros_calc ) - ( lctos.multa_rec + lctos.juros_rec + lctos.desc_multa + lctos.desc_juros )
      else
        0 end                        as saldo_juros_usd,


      lctos.num_comp_adiant,
      lctos.observacao,
      doc_vendas.vtweg

      //      doc_vendas.kurrf,
}
