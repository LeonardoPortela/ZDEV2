define hierarchy Z_IN_HIER_0090_VIEW
  as parent child hierarchy(
    source ZI_IN_HIERARCHY_41
    child to parent association _Recursion
    //start where element_name = value
    siblings order by
      doc_simulacao     ascending,
      PrecedingDocument ascending
    orphans root
  )
{
  key doc_simulacao,
  key PrecedingDocument,

      SubsequentDocument,
      com_41,
      $node.parent_id             as ParentNode,
      $node.node_id               as ChildNode,
      $node.hierarchy_is_orphan   as HierarchyIsOrphan,
      $node.hierarchy_level       as HierarchyLevel,
      $node.hierarchy_rank        as HierarchyRank,
      $node.hierarchy_parent_rank as HierarchyParentRank,
      $node.hierarchy_tree_size   as HierarchyTreeSize,
      $node.hierarchy_is_cycle    as HierarchyIsCycle
}
