@AbapCatalog.sqlViewName: 'ZVMIOVFINSUM3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Soma financeiro da ordem derivada'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FINAN_SUM_SALDO_D3
  as select from ZI_MI_OV_CHAVES_TP_D as Chave

    inner join   ZI_MI_OV_FINAN_UNI   as Valores on Chave.Ordem = Valores.Ordem
{
      //key Chave.OrdemDerivada        as Ordem,
      //sum( Valores.ValorRecUSD ) as ValorTotRecUSD,
      //sum( Valores.ValorRec )    as ValorTotRec,
      //sum( Valores.Mont_Calc)    as MontTotCalc

        key Valores.Ordem,
        sum(Valores.ValorRecUSD) as ValorTotRecUSD,
        sum(Valores.ValorRec)    as ValorTotRec
}
group by Valores.Ordem
