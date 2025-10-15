@AbapCatalog.sqlViewName: 'ZVHIER90MAIN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarquia principal de uma OV'
define view ZI_IN_HIER_MAIN_OV
  as select from Z_IN_HIER_0090_VIEW
{
  key doc_simulacao,
      PrecedingDocument  as vbeln_p,
      SubsequentDocument as vbeln,
      HierarchyRank
}
where
      com_41              = 'X'
  and HierarchyParentRank = 0
  and HierarchyTreeSize   > 1
