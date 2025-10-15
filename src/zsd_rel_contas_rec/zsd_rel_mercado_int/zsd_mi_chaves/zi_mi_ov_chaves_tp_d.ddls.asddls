@AbapCatalog.sqlViewName: 'ZVMIOVCHAVNEG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somente ordens negativas/devolução'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_CHAVES_TP_D
  as select from ZI_MI_OV_CHAVES_TP_ORD
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key OrdemDerivada,
      TpOV
}
where
  TpOV = 'D' //Devolucao
