@AbapCatalog.sqlViewName: 'ZVCHKLISTSATIVO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Checklist - Ativo - Item'
define view ZI_IN_CHKLIST_ATIVO_ITEM
  as select distinct from ZI_IN_CHKLIST_SIM as zchck_sim
    left outer join       zsdt0381          as check_ativo      on zchck_sim.doc_simulacao = check_ativo.doc_simulacao
    left outer join       zsdt0382          as check_ativo_item on  zchck_sim.doc_simulacao      = check_ativo_item.doc_simulacao
                                                                and check_ativo_item.checklistid = check_ativo.checklistid

{
  key zchck_sim.doc_simulacao,
  key check_ativo_item.checklistid,
  key check_ativo_item.checkid,
      check_ativo_item.pergunta,

      check_ativo_item.tpcheck,
      check_ativo_item.tpcond,
      check_ativo_item.tpinconf,

      check_ativo_item.flag_sim,
      check_ativo_item.flag_nao,

      check_ativo_item.texto,

      case when check_ativo.status <> '' then 'X' else '' end as Ativo

}
where
  check_ativo.status <> '03'
