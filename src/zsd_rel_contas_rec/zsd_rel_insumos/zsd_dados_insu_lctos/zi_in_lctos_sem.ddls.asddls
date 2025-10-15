@AbapCatalog.sqlViewName: 'ZVLCTOSSEM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lctos ZSFIT0026 sem Doc Contabil'
define view ZI_IN_LCTOS_SEM
  as select from    ZI_NORMAL_ZFIT0026   as z26
    left outer join ZI_IN_LCTOS_SEM_BSAD as sem  on  sem.vbeln = z26.vbeln
                                                 and sem.seq   = z26.seq
    left outer join ZI_IN_LCTOS_BSAD     as bsad on  bsad.bukrs = z26.bukrs
                                                 and bsad.gjahr = z26.gjahr
                                                 and bsad.belnr = z26.belnr
    left outer join ZI_IN_BUSCA_PTAX     as ptax on  ptax.datab = z26.data_pgto
                                                 and ptax.Kurst = 'B'
                                                 and ptax.Fcurr = 'USD'
                                                 and ptax.Tcurr = 'BRL' 
{
  key z26.vbeln,
  key z26.seq,

      z26.zid_lanc,
      z26.bukrs,
      z26.belnr,
      z26.gjahr,
      '          '               as kunnr,
      '          '               as augbl,

      z26.data_venc,
      //z26.data_pgto,
      '        '                 as data_pgto,

      case when z26.ajuste = 'X' then
        z26.mont_rbdo
      else
         cast( 0  as fins_vkcur12  )
      end                        as mont_rbdo,

      z26.moeda,

      case when z26.ajuste = 'X' then
        z26.mont_moeda
      else
        cast( 0 as fins_vkcur12  ) -- 21.02.2025
      end                        as mont_moeda,

      cast( 0 as fins_vkcur12  ) as bsad_dmbe2,
      '        '                 as budat,
      '        '                 as augdt,

      cast( 0 as fins_vkcur12  ) as dmbtr,

      case when z26.ajuste = 'X' then
        z26.mont_mi
      else
        cast( 0 as fins_vkcur12  )
      end                        as mont_mi,


      case when z26.moeda = 'BRL' and z26.taxa = 1.00000 then
      ptax.Ukurs
      else
      z26.taxa
      end                        as taxa,
      ptax.Ukurs,

      //z26.taxa,
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
      z26.num_comp_adiant
}
where
      z26.ajuste = 'X'
  //and bsad.belnr is null
  and sem.vbeln  is null
