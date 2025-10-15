@AbapCatalog.sqlViewName: 'ZVMIZFIT26_2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação propria da tabela'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_ZFIT0026_2
  as select distinct from ZI_MI_ZFIT0026_NORMAL as Fit26_01
{
  key Fit26_01.zid_lanc,
      Fit26_01.docnum,
      Fit26_01.vbeln,
      Fit26_01.seq,
      Fit26_01.data_venc,
      Fit26_01.moeda,
      Fit26_01.mont_moeda,
      Fit26_01.taxa,
      Fit26_01.mont_mi,
      Fit26_01.forma_pag,
      Fit26_01.status,
      Fit26_01.uname,
      Fit26_01.data_registro,
      Fit26_01.bukrs,
      Fit26_01.obj_key_v,
      Fit26_01.obj_key,
      Fit26_01.razao_especial,
      Fit26_01.eliminado,
      Fit26_01.observacao,
      Fit26_01.zterm,
      Fit26_01.ajuste,
      Fit26_01.doc_fatura,
      Fit26_01.data_pgto,
      Fit26_01.mont_rbdo,
      Fit26_01.vlr_multa_calc,
      Fit26_01.vlr_juros_calc,
      Fit26_01.vlr_multa_rbdo,
      Fit26_01.vlr_juros_rbdo,
      Fit26_01.vlr_desc_mult,
      Fit26_01.vlr_desc_jros,
      Fit26_01.rec_vlr_total,
      Fit26_01.num_comp_adiant,
      Fit26_01.vlr_usd_sigam,
      Fit26_01.moeda_sigam,
      Fit26_01.objkey_sigam,
      Fit26_01.tp_baixa_vlr_ov,
      Fit26_01.taxa_sigam,
      //Fit26_01.docnum_forn,

      case when Fit26_01.unica = 'X' then
        0
      when Fit26_01.primeira = 'X' then
        1
      else
        2
        end as Nivel_Lcto
}
