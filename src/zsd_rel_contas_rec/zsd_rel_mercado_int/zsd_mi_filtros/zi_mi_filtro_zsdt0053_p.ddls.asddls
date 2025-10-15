@AbapCatalog.sqlViewName: 'ZIMIFILTRO53P'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro por ov principal'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_FILTRO_ZSDT0053_p
  as select distinct from zsdt0053 as z053_1
    inner join            zsdt0053 as z053_2 on  z053_1.nro_sol_ov = z053_2.nro_sol_ov
                                             and z053_1.vbeln      = z053_2.vbeln
{
  key z053_1.nro_sol_ov,
  key z053_1.posnr,
  key z053_1.vbeln,
  key case when z053_2.doc_precedente = '' then
    z053_1.vbeln
  else
    z053_2.doc_precedente
  end as vbeln2
}
