@AbapCatalog.sqlViewName: 'ZCHIER41'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarquia de OVs'
@Hierarchy.parentChild: { name: 'Recursion',
                          recurse : {
                            parent: [ 'doc_simulacao','PrecedingDocument'],
                            child:  [ 'doc_simulacao','SubsequentDocument']
                        }
}
define view ZI_IN_HIERARCHY_41
  as select from ZI_IN_DADOS_90_41_CHECK as check
  association [0..1] to ZI_IN_HIERARCHY_41 as _Recursion on  $projection.doc_simulacao     = _Recursion.doc_simulacao
                                                         and $projection.PrecedingDocument = _Recursion.SubsequentDocument
{
  key check.doc_simulacao,
  key check.vbelv as PrecedingDocument,
  key check.vbeln as SubsequentDocument,
      check.com_41,
      _Recursion
}
