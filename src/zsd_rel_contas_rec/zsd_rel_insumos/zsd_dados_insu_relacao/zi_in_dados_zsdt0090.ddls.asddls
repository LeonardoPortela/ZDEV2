@AbapCatalog.sqlViewName: 'ZVIN0090'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ovs da ZSDT0090'
define view ZI_IN_DADOS_ZSDT0090
  as select from ZI_IN_HIER_MAIN_OV
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln

}
union all select from ZI_IN_HIER_BETWEEN_OV
{
  key doc_simulacao,
  key vbeln_p,
  key vbeln

}
