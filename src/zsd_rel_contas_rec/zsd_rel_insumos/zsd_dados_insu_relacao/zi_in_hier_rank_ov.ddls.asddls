@AbapCatalog.sqlViewName: 'ZVHIERANK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rank de OVs para consulta'
define view ZI_IN_HIER_RANK_OV
  as select from Z_IN_HIER_0090_VIEW
{
  key doc_simulacao,
  key PrecedingDocument,
      SubsequentDocument,
      com_41,
      HierarchyRank + 1                         as HierachyFirst,
      ( HierarchyRank + HierarchyTreeSize ) - 1 as HierachyLast
}
where
      HierarchyLevel    = 1
  and HierarchyTreeSize > 1
