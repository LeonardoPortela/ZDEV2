@AbapCatalog.sqlViewName: 'ZVCHKLISTSDEP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação entre checkids'
define view ZI_IN_CHKLIST_ATIVO_RELACAO
  as select from ZI_IN_CHKLIST_ATIVO_ITEM as item
    inner join   zsdt0380_r               as zrelacao on zrelacao.checkid = item.checkid
{
  key doc_simulacao,
  key zrelacao.checklistid,
      zrelacao.checkid,
      zrelacao.checkid_r,
      pergunta,
      tpcheck,
      tpcond,
      tpinconf,
      flag_sim,
      flag_nao,
      texto,
      Ativo
}
