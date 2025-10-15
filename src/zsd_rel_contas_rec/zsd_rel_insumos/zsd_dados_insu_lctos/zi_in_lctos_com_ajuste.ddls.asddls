@AbapCatalog.sqlViewName: 'ZVLCTOSCOMAJST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lanc. com ajuste sem buscar bsad'
define view ZI_IN_LCTOS_COM_AJUSTE
  as select from ZI_NORMAL_ZFIT0026 as z26
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

      z26.taxa,
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
  z26.ajuste is not initial
