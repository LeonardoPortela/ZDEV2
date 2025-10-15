@AbapCatalog.sqlViewName: 'ZINLCTOSUNI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lctos ZSFIT0026 e BSAD unidos por OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_IN_LCTOS_UNI
  as select from ZI_IN_LCTOS_BSAD as bsad
{
  key bsad.vbeln,
  key bsad.seq,
      bsad.zid_lanc,
      bsad.bukrs,

      bsad.belnr,
      bsad.gjahr,

      bsad.kunnr,
      bsad.augbl,

      bsad.data_venc,
      bsad.data_pgto,
      bsad.forma_pag,

      bsad.moeda,
      bsad.taxa,

      bsad.bsad_dmbe2,
      bsad.budat,
      bsad.augdt,
      bsad.ajuste,

      case bsad.moeda when 'USD' then
        bsad.mont_mi
      else
        //bsad.mont_moeda end as dmbtr,
        bsad.dmbtr - bsad.vlr_juros_rbdo end as dmbtr,

      case bsad.moeda when 'USD' then
        bsad.mont_moeda
      else
      case when bsad.ajuste = 'X' then
      division(bsad.mont_moeda,bsad.taxa,2)
      else
        bsad.bsad_dmbe2 - juros_multa_usd
        end
      end                as dmbtr_usd,


      mont_rbdo,


      bsad.vlr_multa_calc,
      bsad.vlr_juros_calc,
      bsad.vlr_multa_rbdo,
      bsad.vlr_juros_rbdo,
      bsad.vlr_desc_mult,
      bsad.vlr_desc_jros,
      //bsad.juros_multa_usd,
      bsad.rec_vlr_total,
      bsad.num_comp_adiant,
      bsad.observacao,
      'BSAD'                as origem
}

union all select from ZI_IN_LCTOS_SEM_BSAD
{
  key vbeln,
  key seq,
      zid_lanc,
      bukrs,

      belnr,
      gjahr,

      kunnr,
      augbl,

      data_venc,
      data_pgto,
      forma_pag,

      moeda,
      taxa,

      bsad_dmbe2,
      budat,
      augdt,
      ajuste,

      case moeda when 'BRL' then
        mont_moeda
      else
        mont_mi
      end    as dmbtr,

      case moeda when 'BRL' then
      case when taxa is initial then
        division(mont_moeda,1,2)
       else
        division(mont_moeda,taxa,2)
        end
      else
        mont_moeda
      end    as dmbtr_usd,

      mont_rbdo,

      vlr_multa_calc,
      vlr_juros_calc,
      vlr_multa_rbdo,
      vlr_juros_rbdo,
      vlr_desc_mult,
      vlr_desc_jros,
      rec_vlr_total,
      num_comp_adiant,
      observacao,
      'Z26S' as origem
}
union all select from ZI_IN_LCTOS_SEM
{
  key vbeln,
  key seq,
      zid_lanc,
      bukrs,

      belnr,
      gjahr,

      kunnr,
      augbl,

      data_venc,
      data_pgto,
      forma_pag,

      moeda,
      taxa,

      bsad_dmbe2,
      budat,
      augdt,
      ajuste,

      case moeda when 'BRL' then
        mont_moeda
      else
        mont_mi
      end    as dmbtr,

      case moeda when 'BRL' then
      case when taxa is initial then
        division(mont_moeda,1,2)
       else
        division(mont_moeda,taxa,2)
        end
      else
        mont_moeda
      end    as dmbtr_usd,

      mont_rbdo,

      vlr_multa_calc,
      vlr_juros_calc,
      vlr_multa_rbdo,
      vlr_juros_rbdo,
      vlr_desc_mult,
      vlr_desc_jros,
      rec_vlr_total,
      num_comp_adiant,
      observacao,
      'Z26X' as origem
}
