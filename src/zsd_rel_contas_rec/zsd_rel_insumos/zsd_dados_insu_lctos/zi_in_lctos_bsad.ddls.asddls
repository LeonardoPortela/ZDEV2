@AbapCatalog.sqlViewName: 'ZVLCTOSBSAD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lctos ZSFIT0026 com BSAD'
define view ZI_IN_LCTOS_BSAD
  as select from ZI_NORMAL_ZFIT0026 as z26
    inner join   bsad_view          as bsad on bsad.kunnr =  z26.kunnr //bsad.bukrs = z26.bukrs
                                                                       //and bsad.gjahr = z26.gjahr
                                            and bsad.belnr = z26.belnr
{

  key z26.vbeln,
  key z26.seq,

      z26.zid_lanc,
      z26.bukrs,
      z26.belnr,
      z26.gjahr,
      coalesce(bsad.kunnr,'0000000000') as kunnr,
      coalesce(bsad.augbl,'0000000000') as augbl,

      z26.data_venc,

      // 20.05.2025 - data venc vazia....
      case coalesce(z26.data_pgto,'00000000') when '00000000' then
        coalesce(bsad.augdt,'0000000000')
      else
        z26.data_pgto
      end                               as data_pgto,

      z26.mont_rbdo,
      z26.moeda,

      z26.mont_moeda,

      coalesce(bsad.dmbe2,0)            as bsad_dmbe2,
      coalesce(bsad.budat,'0000000000') as budat,
      coalesce(bsad.augdt,'0000000000') as augdt,

      coalesce(bsad.dmbtr,0)            as dmbtr,


      z26.mont_mi,
      z26.taxa,

      case z26.moeda when 'BRL' then
        division(coalesce(z26.mont_rbdo,0), coalesce(bsad.dmbe2,1), 5)
      else
        0
      end                               as ptax_BRL,

      case z26.moeda when 'BRL' then

        case when z26.mont_rbdo > 0 then
            division( coalesce(z26.juros_multa_rbdo,0), coalesce(division(z26.mont_rbdo, coalesce(bsad.dmbe2,1), 5),1) ,2)
        else
            0
        end
      else
        0
      end                               as juros_multa_usd,

      z26.forma_pag,
      z26.observacao,
      z26.obj_key,
      z26.doc_fatura,

      z26.vlr_multa_calc,

      z26.vlr_juros_calc,

      z26.vlr_multa_rbdo,

      z26.vlr_juros_rbdo,

      z26.vlr_desc_mult,

      z26.vlr_desc_jros,
      z26.ajuste,
      z26.rec_vlr_total,
      z26.num_comp_adiant,
      z26.juros_multa_rbdo
}
where
  (
       bsad.bschl = '01'
    or bsad.bschl = '09'
  ) and z26.ajuste is initial
