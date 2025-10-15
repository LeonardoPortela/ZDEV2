@AbapCatalog.sqlViewName: 'ZIMIFILTRO53'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro para o report'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_FILTRO_ZSDT0053
  as select distinct from zsdt0053 as z053_1
    right outer join      zsdt0053 as z053_2 on z053_1.nro_sol_ov = z053_2.nro_sol_ov
{
  key z053_1.nro_sol_ov,
  key z053_1.posnr,
  key z053_1.vbeln,
  key z053_2.vbeln as vbeln2
}
