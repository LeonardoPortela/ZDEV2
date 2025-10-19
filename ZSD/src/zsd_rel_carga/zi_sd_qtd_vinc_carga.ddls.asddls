@AbapCatalog.sqlViewName: 'ZVRELQTDVINC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Solicitação - Qtd Vinculado em carga'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SD_QTD_VINC_CARGA
  as select from zsdt0131
{
  key nro_sol         as NroSol,
  key kunnr           as Kunnr,
  key vbeln           as Vbeln,
  key posnr           as Posnr,
      sum( qtd_vinc ) as QtdVinc,
      um              as Um
}
where
  status <> 'X'
group by
  nro_sol,
  kunnr,
  vbeln,
  posnr,
  um
