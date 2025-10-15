@AbapCatalog.sqlViewName: 'ZVCFOP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tabelas de Parametro de CFOP'
@Metadata.ignorePropagatedAnnotations: true
define view zicfop(
    cfop
  )
  as select from zsdt0353 as _0353
{
  key _0353.cfop
}
union select from zsdt0352 as _0352
{
  key _0352.cfop
}
union select from zsdt0354 as _0354
{
  key _0354.cfop
}
union select from zsdt0362 as _0362
{
  key _0362.cfop
}
