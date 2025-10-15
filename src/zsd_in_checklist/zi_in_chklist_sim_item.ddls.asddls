@AbapCatalog.sqlViewName: 'ZVCHKLISTSIMIT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relação entre checklist itens e sim'
define view ZI_IN_CHKLIST_SIM_ITEM
  as select from    ZI_IN_CHKLIST_NOVO_ITEM  as novo
    left outer join ZI_IN_CHKLIST_ATIVO_ITEM as ativo on  ativo.doc_simulacao = novo.doc_simulacao
                                                      and ativo.checklistid   = novo.checklistid
                                                      and ativo.checkid       = novo.checkid
{
  key case coalesce(ativo.doc_simulacao,'') when '' then novo.doc_simulacao else ativo.doc_simulacao end as doc_simulacao,
  key case coalesce(ativo.checklistid,'') when '' then novo.checklistid else ativo.checklistid end       as checklistid,
  key case coalesce(ativo.checkid,'') when '' then novo.checkid else ativo.checkid end                   as checkid,
      case coalesce(ativo.pergunta,'') when '' then novo.pergunta else ativo.pergunta end                as pergunta,

      case coalesce(ativo.tpcheck,'') when '' then novo.tpcheck else ativo.tpcheck end                   as tpcheck,
      case coalesce(ativo.tpcond,'') when '' then novo.tpcond else ativo.tpcond end                      as tpcond,
      case coalesce(ativo.tpinconf,'') when '' then novo.tpinconf else ativo.tpinconf end                as tpinconf,

      case coalesce(ativo.flag_sim,'') when '' then novo.flag_sim else ativo.flag_sim end                as flag_sim,
      case coalesce(ativo.flag_nao,'') when '' then novo.flag_nao else ativo.flag_nao end                as flag_nao,
      case coalesce(ativo.texto,'')    when '' then novo.texto  else ativo.texto end                     as texto,

      case coalesce(ativo.Ativo,'') when '' then novo.Ativo else ativo.Ativo end                         as Ativo
}
