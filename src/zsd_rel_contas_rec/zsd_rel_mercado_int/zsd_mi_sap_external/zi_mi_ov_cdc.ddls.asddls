@AbapCatalog.sqlViewName: 'ZIMIOVCDC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.dataExtraction.enabled : true
@EndUserText.label: 'Acesso externo MI'
define view ZI_MI_OV_CDC
  as select from ZI_MI_OV_PRINCIPAL_3 as sim
  association [0..*] to ZI_MI_OV_FAT_PRINCIPAL as _lcto on sim.OrdemDerivada = _lcto.Ordem
{
  key Simulador,
  key ItemSimulador,
  key Ordem,
  key Empresa,
  key OrdemDerivada,
      NumDocs,
      EscVendas,
      EqVendas,
      EqVendas_txt,
      TpOV,
      TpDevVenda,
      Cliente,
      NomeCliente,
      data_venc,
      Safra,
      Pgto_ant,
      tx_multa,
      tx_juros,
      CondPagto,
      DescCondPagto,
      DataCriacao,
      Moeda,
      meins,
      Qtd,
      ValorLiq,
      ValorImp,
      ValorTotal,
      TotalRecebido,
      QtdFat,
      SaldoOV,
      SaldoOVUSD,
      SaldoRefUSD,
      SaldoRefBRL,
      /* Associations */
      _lcto
}
