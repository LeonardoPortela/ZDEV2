@AbapCatalog.sqlViewName: 'ZVMIOVDEVOLAGR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados das Ordens principais - Relatorio MI'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_DEVOL_AGREG
  as select from ZI_MI_OV_DEVOLUCAO_SALDO as saldo
    inner join   ZI_MI_OV_DEVOLUCAO       as devol on devol.vbeln1 = saldo.vbeln1
{
  devol.vbeln1,

 ( saldo.vlr_juros_calc - saldo.vlr_juros_rbdo - saldo.vlr_desc_jros ) + 
 ( saldo.vlr_multa_calc - saldo.vlr_multa_rbdo - saldo.vlr_desc_mult ) as SaldoJuros
 
//  ( saldo.vlr_multa_calc + saldo.vlr_juros_calc -
//   saldo.vlr_multa_rbdo - saldo.vlr_juros_rbdo +
//   saldo.vlr_desc_mult + saldo.vlr_desc_jros ) as SaldoJuros

}
