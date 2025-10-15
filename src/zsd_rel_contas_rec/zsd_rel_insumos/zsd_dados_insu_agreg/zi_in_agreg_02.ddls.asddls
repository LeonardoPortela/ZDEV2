@AbapCatalog.sqlViewName: 'ZVINAGREG02'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agregação 2 de dados para o Relatorio'
define view ZI_IN_AGREG_02
  as select from ZI_IN_AGREG_01
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln,
      id_ecomm,
      Num_Reg,
      bukrs,
      vkbur,
      vkgrp,
      vkgrp_name,
      auart,
      ecommerce,
      safra,
      cultura,
      cultura_desc,

      kunnr,
      name1,

      pgto_ant,
      zterm,
      zterm_name,

      tx_juros,

      erdat,
      waerk,

      qtde,

      case when auart = 'ZFUT' then
        0
      else
         qtde_fat
      end as qtde_fat,

      vlr_liq,
      vlr_imp,
      vlr_tot_ov,

      vlr_liq_sum,

      case when waerk = 'USD' then
        0
      else
          case when Num_Reg > 0 then
            saldo_ov_brl
          else
            vlr_tot_ov
          end
      end as saldo_ov_brl,

      case when waerk = 'BRL' then
        0
      else
          case when Num_Reg > 0 then
            saldo_ov_usd
          else
            vlr_tot_ov
          end
      end as saldo_ov_usd,

      zid_lanc,
      valdt,

      data_pgto,
      taxa,
      forma_pag,

      belnr,
      augbl,

      vlr_rec_brl,
      vlr_rec_usd,
      multa_calc,
      multa_rec,

      desc_multa,
      juros_calc,
      juros_rec,

      desc_juros,
      saldo_juros_brl,


      saldo_juros_usd,
      num_comp_adiant,
      observacao,
      vtweg
}
