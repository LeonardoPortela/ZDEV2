@AbapCatalog.sqlViewName: 'ZVINOVDADOSVBEP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agregação da VBEP'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_IN_OV_VBEP
  as select from vbep
{
  key vbeln,
  key posnr,
  key max(etenr) as etenr,
      max(lifsp) as lifsp
}
where
  lifsp = '12'
group by
  vbeln,
  posnr
