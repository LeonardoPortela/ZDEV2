@AbapCatalog.sqlViewName: 'ZVMIOVDEVOLSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Soma da devolução'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_DEVOLUCAO_SUM
  as select from ZI_MI_OV_DEVOLUCAO

{
  key vbeln1,
      min(vbeln2) as vbeln2,
      sum(menge)  as menge
}
group by
  vbeln1
