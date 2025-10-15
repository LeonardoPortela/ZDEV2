@AbapCatalog.sqlViewName: 'ZVMIOVFINSUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somariza valor financeiro da ordem'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_FINAN_SUM_SALDO
  as select from ZI_MI_OV_CHAVES_TP_C as Chave

    inner join   ZI_MI_OV_FINAN_UNI   as Valores on Chave.OrdemDerivada = Valores.Ordem
{
  key Chave.OrdemDerivada         as Ordem,
      sum( Valores.ValorRecUSD )  as ValorTotRecUSD,
      sum( Valores.ValorRec )     as ValorTotRec,
      sum( Valores.VlrTotRef )    as VlrTotRef,

      sum( Valores.SaldoJuros)    as SaldoJuros,
      sum( Valores.SaldoJurosusd) as SaldoJurosusd,

      //sum( case coalesce(Valores.DocCont,'') when '' then
      sum( case coalesce(Valores.ID26,'') when '' then
            0
            else
             Valores.VlrTotRef  -----20.01.2025
              //Valores.VlrTotRef26
            end          ) as VlrTotLanc

}

group by
  Chave.OrdemDerivada
