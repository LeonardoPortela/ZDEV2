@AbapCatalog.sqlViewName: 'ZVMIOVFINSUMN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somar valores dos itens de devoluções'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FINAN_SUM_SALDO_D
  as select from ZI_MI_OV_CHAVES_TP_D as Chave

  association to vbap as Item on Chave.OrdemDerivada = Item.vbeln
{
  key Chave.Ordem,
  key Chave.OrdemDerivada,
      Item.netwr + Item.mwsbp as ValorTotLiq
}
