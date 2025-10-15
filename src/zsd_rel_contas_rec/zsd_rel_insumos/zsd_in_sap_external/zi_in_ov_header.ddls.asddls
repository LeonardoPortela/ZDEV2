@AbapCatalog.sqlViewName: 'ZIINOVHEADER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de cabeçalho para consultas'
define view ZI_IN_OV_HEADER
  as select from    ZI_IN_SIMU_OV      as relacao
    inner join      vbap                             on vbap.vbeln = relacao.vbeln
    left outer join ZI_IN_DADOS_SIM    as simulador  on relacao.doc_simulacao = simulador.doc_simulacao
    left outer join ZI_IN_FAT_SUM      as fat        on relacao.vbeln = fat.vbeln_va
    left outer join ZI_IN_LCTOS_OV_SUM as lctos_sum  on relacao.vbeln = lctos_sum.vbeln_va
    left outer join ZI_IN_OV_DADOS_2   as doc_vendas on relacao.vbeln = doc_vendas.vbeln
    left outer join ZI_IN_LCTOS_OV     as lctos      on relacao.vbeln = lctos.vbeln_va
{
  key relacao.doc_simulacao,
  key relacao.vbeln_p,
  key relacao.vbeln,
      lctos_sum.Num_Reg                      as NumDocs,


      doc_vendas.bukrs                       as Empresa,
      doc_vendas.vkbur                       as EscVendas,
      doc_vendas.vkgrp                       as EqVendas,
      doc_vendas.vkgrp_name                  as EqVendas_txt,
      doc_vendas.auart,
        
      simulador.safra,
      simulador.cultura,
      simulador.cultura_desc,
      simulador.status,
      doc_vendas.kunnr                       as Cliente,
      doc_vendas.name1                       as NomeCliente,

      simulador.pgto_ant,
      doc_vendas.zterm                       as CondPagto,
      doc_vendas.zterm_name                  as DescCondPagto,

      simulador.tx_juros,

      doc_vendas.erdat                       as DataCriacao,
      doc_vendas.waerk                       as Moeda,

      doc_vendas.waerk,
      vbap.kmein,
      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
        0
      else

        case when doc_vendas.auart = 'ZFUT'  or doc_vendas.auart = 'ZTRI' then
            doc_vendas.zmeng
        else
            doc_vendas.kwmeng
        end
      end                                    as qtde,

      fat.menge_fat * doc_vendas.fator_devol as qtde_fat,

      doc_vendas.netwr                       as vlr_liq,
      doc_vendas.mwsbp                       as vlr_imp,

      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
      0
      else
      doc_vendas.vlr_tot_ov
      end                                    as vlr_tot_ov,

      case when  doc_vendas.waerk = 'BRL' then
        lctos_sum.vlr_rec_brl_sum
      else
        lctos_sum.vlr_rec_usd_sum end        as vlr_liq_sum,

      case when  doc_vendas.waerk = 'BRL' then

      case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
        0
      else
        doc_vendas.vlr_tot_ov - coalesce(lctos_sum.vlr_rec_brl_sum,0)
        end
      else
        0 end                                as saldo_ov_brl,

      case when  doc_vendas.waerk = 'USD' then
        case when doc_vendas.auart = 'ZREM' or doc_vendas.auart = 'ZRFU' then
            0
        else
            doc_vendas.vlr_tot_ov - coalesce(lctos_sum.vlr_rec_usd_sum,0)
        end
      else
        0 end                                as saldo_ov_usd,

      case when  doc_vendas.waerk = 'BRL' then
      ( lctos_sum.multa_calc_sum + lctos_sum.juros_calc_sum ) - ( lctos_sum.multa_rec + lctos_sum.juros_rec_sum + lctos_sum.desc_multa_sum + lctos_sum.desc_juros_sum )
      else
      0 end                                  as saldo_juros_brl,

      case when  doc_vendas.waerk = 'USD' then
        ( lctos_sum.multa_calc_sum + lctos_sum.juros_calc_sum ) - ( lctos_sum.multa_rec + lctos_sum.juros_rec_sum + lctos_sum.desc_multa_sum + lctos_sum.desc_juros_sum )
      else
        0 end                                as saldo_juros_usd,

      coalesce(lctos_sum.vlr_rec_brl_sum,0)  as vlr_rec_brl,
      coalesce(lctos_sum.vlr_rec_usd_sum,0)  as vlr_rec_usd,


      // Lançamentos ----------------------------------------------

      lctos.zid_lanc,
      lctos.seq,
      lctos.belnr,
      lctos.augbl,
      lctos.data_venc,
      lctos.data_pgto,
      lctos.taxa,
      lctos.forma_pag,
      lctos.vlr_rec_usd                      as vlr_rec_usd_lcto,
      lctos.vlr_rec_brl                      as vlr_rec_brl_lcto,
      lctos.multa_calc,
      lctos.multa_rec,
      lctos.desc_multa,
      lctos.juros_calc,
      lctos.juros_rec,
      lctos.desc_juros,
      lctos.observacao
}
