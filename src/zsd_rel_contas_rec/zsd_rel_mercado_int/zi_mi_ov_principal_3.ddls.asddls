@AbapCatalog.sqlViewName: 'ZVMIOV3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados das Ordens'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_PRINCIPAL_3
  as select from ZI_MI_OV_FAT_CONSO_4 as princ
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
      sald_referencia as sald_referencia,

      SaldoJurosFiltro,
      vtweg,
      spart,
      status
      
}
