@AbapCatalog.sqlViewName: 'ZVCHKLISTSIM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação entre checklist e simulador'
define view ZI_IN_CHKLIST_SIM
  as select from    zsdt0040   as z0040
  //    inner join      zsdt0041   as z0041 on  z0041.doc_simulacao = z0040.doc_simulacao
  //                                        and z0041.posnr         = '000010'
    left outer join I_Customer as kna1  on kna1.Customer = z0040.kunnr
    left outer join zsdt0381   as z0381 on z0040.doc_simulacao = z0381.doc_simulacao

    left outer join dd07t      as dd07t on  dd07t.domvalue_l = z0381.status
                                        and dd07t.domname    = 'ZSDCHCK_STAT'
                                        and dd07t.ddlanguage = $session.system_language
{
  key z0040.doc_simulacao,

      case coalesce(z0381.checklistid,'') when '' then
        ''
      else
        z0381.checklistid
      end                  as checklistid,
      case coalesce(z0381.checklistid,'') when '' then
        ''
      else
        z0381.flag_incoformidade
      end                  as flag_incoformidade,
      z0040.kunnr,
      kna1.CustomerName    as name1, //
      z0040.vkorg,
      z0040.vkbur,
      z0040.vtweg,
      z0040.spart,
      z0040.cultura,
      z0040.safra,
      z0040.erdat,

      //z0041.dtvenc         as venci,


      z0040.vlrtot, //
      z0040.waerk, //
      z0040.tpsim,
      z0381.user_create,
      'VP - Venda A Prazo' as tp_sim_desc,
      z0381.descr,
      case coalesce(z0381.status,'') when '' then
       ''
      else
        z0381.status
      end                  as status,

      case coalesce(dd07t.domname,'') when '' then
       ''
      else
         dd07t.ddtext
      end                  as status_desc

}
where
  z0040.tpsim = 'VP'
