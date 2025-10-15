@AbapCatalog.sqlViewName: 'ZVCHKLISTSNOVOIT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Checklist - Novo - Item'
define view ZI_IN_CHKLIST_NOVO_ITEM
  as select from    ZI_IN_CHKLIST_SIM as zchck_sim
    left outer join zsdt0379          as check_novo      on  zchck_sim.vkorg      = check_novo.bukrs
                                                         and check_novo.desativar = ''
    left outer join zsdt0380          as check_novo_item on check_novo_item.checklistid = check_novo.checklistid
{
  key zchck_sim.doc_simulacao,

  key case coalesce(zchck_sim.checklistid,'') when '' then
    check_novo_item.checklistid
  else
    zchck_sim.checklistid
  end    as checklistid,

      check_novo_item.checkid,
      check_novo_item.pergunta,

      check_novo_item.tpcheck,
      check_novo_item.tpcond,
      check_novo_item.tpinconf,

      '' as flag_sim,
      '' as flag_nao,
      '' as texto,
      '' as Ativo
}
