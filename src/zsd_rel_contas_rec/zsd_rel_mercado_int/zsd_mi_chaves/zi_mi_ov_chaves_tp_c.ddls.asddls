@AbapCatalog.sqlViewName: 'ZVMIOVCHAVPOSF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somente ordens positivas'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_TP_C
  as select distinct from ZI_MI_OV_CHAVES_TP_ORD
{
  key OrdemDerivada
}

where
  TpOV = 'C' //OV normal ou complemento
