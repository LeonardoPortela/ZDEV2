@AbapCatalog.sqlViewName: 'ZVMIOVDEVOLSALDO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados das Ordens principais - Relatorio MI'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_DEVOLUCAO_SALDO
  as select from ZI_MI_OV_DEVOLUCAO as devol
    inner join   zfit0026           as z26 on z26.vbeln = devol.vbeln2

{
  devol.vbeln1,
  sum( z26.vlr_multa_calc ) as vlr_multa_calc,
  sum( z26.vlr_juros_calc ) as vlr_juros_calc,
  sum( z26.vlr_multa_rbdo ) as vlr_multa_rbdo,
  sum( z26.vlr_juros_rbdo ) as vlr_juros_rbdo,
  sum( z26.vlr_desc_mult )  as vlr_desc_mult,
  sum( z26.vlr_desc_jros )  as vlr_desc_jros
}
group by

  devol.vbeln1
