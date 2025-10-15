@AbapCatalog.sqlViewName: 'ZVNORMAL0026'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Normalização ZFIT0026'
define view ZI_NORMAL_ZFIT0026
  as select from    zfit0026         as z26
    left outer join zib_contabil_chv as chv on chv.obj_key = z26.obj_key
    left outer join vbak as vbak on vbak.vbeln = z26.vbeln 
{
  key $session.client                                 as mandt,
  key z26.vbeln,
  key z26.seq,
      z26.zid_lanc,
      z26.bukrs,

      case when z26.docnum = '0000000000' then
        chv.belnr
      else
        z26.docnum
      end                                             as belnr,

      case coalesce(data_pgto,'00000000') when '00000000' then
        left( data_registro, 4)
      else
        left( data_pgto, 4)
      end                                             as gjahr,

      //      case coalesce(z26.obj_key,'') when '' then
      //        left( data_registro, 4)
      //      else
      //        right( z26.obj_key, 4 )
      //      end                                     as gjahr,
      z26.data_venc,
      z26.moeda,
      z26.mont_moeda,
      z26.taxa,
      z26.mont_mi,
      z26.forma_pag,
      z26.status,
      z26.uname,
      z26.data_registro,
      z26.obj_key_v,
      z26.obj_key,
      z26.razao_especial,
      z26.eliminado,
      z26.observacao,
      z26.zterm,
      z26.ajuste,
      z26.doc_fatura,
      z26.data_pgto,
      z26.mont_rbdo,
      z26.vlr_multa_calc,
      z26.vlr_juros_calc,
      z26.vlr_multa_rbdo,
      z26.vlr_juros_rbdo,
      z26.vlr_desc_mult,
      z26.vlr_desc_jros,
      z26.rec_vlr_total,
      z26.num_comp_adiant,
      z26.vlr_usd_sigam,
      z26.moeda_sigam,
      z26.objkey_sigam,
      z26.tp_baixa_vlr_ov,
      z26.taxa_sigam,
      //z26.docnum_forn,

      z26.vlr_multa_rbdo + z26.vlr_juros_rbdo         as juros_multa_rbdo,
      substring(z26.obj_key,14,3)                     as buzei,
      vbak.kunnr                                      as kunnr
}
where
  eliminado is initial //21.02.2025
