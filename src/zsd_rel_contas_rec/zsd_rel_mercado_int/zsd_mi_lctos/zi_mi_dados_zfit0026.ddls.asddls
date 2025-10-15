@AbapCatalog.sqlViewName: 'ZVZFIT0026'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados da ZFIT0026'
define view ZI_MI_DADOS_ZFIT0026
  as select from    zfit0026         as Z26
    left outer join acdoca            as doccont on  doccont.rldnr = '0L'
                                                 and doccont.belnr = Z26.docnum
                                                 and doccont.buzei = '001'
    left outer join ZI_IN_BUSCA_PTAX     as ptax on  ptax.datab = Z26.data_pgto
                                                 and ptax.Kurst = 'B'
                                                 and ptax.Fcurr = 'USD'
                                                 and ptax.Tcurr = 'BRL'                                                
{
  key Z26.zid_lanc        as zid_lanc,
      Z26.docnum          as Docnum,
      Z26.vbeln           as Vbeln,
      Z26.seq             as Seq,
      Z26.data_venc       as data_venc ,
      Z26.moeda           as Moeda,
      Z26.mont_moeda      as mont_moeda,
      doccont.ksl          as ksl,
      
      case when Z26.moeda = 'BRL' and Z26.taxa = 1.00000 and Z26.docnum = '0000000000' then
      ptax.Ukurs 
      else
      case when Z26.moeda = 'BRL' and Z26.taxa = 1.00000 then
      division(doccont.hsl,doccont.ksl,6)
      else
      Z26.taxa
      end  end               as taxa,
      //
      Z26.taxa            as ptax,
      Z26.mont_mi         as Mont_Mi,
      Z26.forma_pag       as Forma_Pag,
      Z26.status          as Status,
      Z26.uname           as Uname,
      Z26.data_registro   as Data_Registro,
      Z26.bukrs           as Bukrs,
      Z26.obj_key_v       as Obj_Key_V,
      Z26.obj_key         as Obj_Key,
      Z26.razao_especial  as Razao_Especial,
      Z26.eliminado       as Eliminado,
      Z26.observacao      as Observacao,
      Z26.zterm           as Zterm,
      Z26.ajuste          as Ajuste,
      Z26.doc_fatura      as Doc_Fatura,
      Z26.data_pgto       as Data_Pgto,
      Z26.mont_rbdo       as Mont_Rbdo,
      Z26.vlr_multa_calc  as Vlr_Multa_Calc,
      Z26.vlr_juros_calc  as Vlr_Juros_Calc,
      Z26.vlr_multa_rbdo  as Vlr_Multa_Rbdo,
      Z26.vlr_juros_rbdo  as Vlr_Juros_Rbdo,
      Z26.vlr_desc_mult   as Vlr_Desc_Mult,
      Z26.vlr_desc_jros   as Vlr_Desc_Jros,
      Z26.rec_vlr_total   as Rec_Vlr_Total,
      Z26.num_comp_adiant as Num_Comp_Adiant,
      Z26.vlr_usd_sigam   as Vlr_Usd_Sigam,
      Z26.moeda_sigam     as Moeda_Sigam,
      Z26.objkey_sigam    as Objkey_Sigam,
      Z26.tp_baixa_vlr_ov as Tp_Baixa_Vlr_Ov,
      Z26.taxa_sigam      as Taxa_Sigam,
      Z26.docnum_forn     as Docnum_Forn
}
