@AbapCatalog.sqlViewName: 'ZVMIZFIT26N'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Normalização'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_ZFIT0026_NORMAL
  as select distinct from zfit0026           as z26
    left outer join       ZI_MI_ZFIT0026_MIN as grp on  z26.vbeln      = grp.vbeln
                                                    and z26.doc_fatura = grp.doc_fatura
                                                    and z26.seq        = grp.seq_min

{
  key z26.vbeln          as vbeln,
  key z26.seq            as seq,
      z26.zid_lanc       as zid_lanc,

      case coalesce(z26.doc_fatura,'') when '' then
        'X'
      else
          case coalesce(grp.seq_min,'') when '' then
            ''
          else
            'X'
          end
      end                as primeira,

      case coalesce(z26.doc_fatura,'') when '' then
        'X'
      else
          case when grp.seq_min = grp.seq_max then
            'X'
          else
            ''
          end
       end               as unica,
      z26.docnum,
      z26.data_venc,
      z26.moeda,

      grp.sum_mont_moeda as mont_moeda,

      //z26.mont_moeda,
      z26.taxa,
      z26.mont_mi,
      z26.forma_pag,
      z26.status,
      z26.uname,
      z26.data_registro,
      z26.bukrs,
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
      z26.taxa_sigam
      //z26.docnum_forn
}
