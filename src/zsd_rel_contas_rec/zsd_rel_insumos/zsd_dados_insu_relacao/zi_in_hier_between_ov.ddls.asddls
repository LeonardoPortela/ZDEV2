@AbapCatalog.sqlViewName: 'ZVHIERBETOV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ovs por rank'
define view ZI_IN_HIER_BETWEEN_OV
  as select from     ZI_IN_HIER_RANK_OV  as rank
    right outer join Z_IN_HIER_0090_VIEW as hier on hier.doc_simulacao = rank.doc_simulacao
{
  key rank.doc_simulacao,
  key rank.PrecedingDocument  as vbeln_p,
      hier.SubsequentDocument as vbeln
}
where
  hier.HierarchyRank between rank.HierachyFirst and rank.HierachyLast
