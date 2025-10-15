@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Acesso externo report MI'
@Metadata.ignorePropagatedAnnotations: true
@Analytics:{
    dataCategory: #FACT,
    dataExtraction: {
       enabled: true,
       delta.changeDataCapture: {
     automatic : true
         }
      }
}
@ObjectModel.representativeKey: 'Simulador'
define view entity ZI_MI_OV_REPORT
  as select from ZI_MI_OV_PRINCIPAL_3 as sim
  association [0..*] to ZI_MI_OV_FAT_PRINCIPAL as _lcto on sim.OrdemDerivada = _lcto.Ordem
{
  key sim.Simulador,
  key sim.ItemSimulador,
  key sim.Ordem,
  key sim.Empresa,
  key sim.OrdemDerivada,
      sim.NumDocs,
      sim.EscVendas,
      sim.EqVendas,
      sim.EqVendas_txt,
      sim.TpOV,
      sim.TpDevVenda,
      sim.Cliente,
      sim.NomeCliente,
      sim.data_venc,
      sim.Safra,
      sim.Pgto_ant,
      sim.tx_multa,
      sim.tx_juros,
      sim.CondPagto,
      sim.DescCondPagto,
      sim.DataCriacao,
      sim.Moeda,
      sim.meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      sim.Qtd,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.ValorLiq,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.ValorImp,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.ValorTotal,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.TotalRecebido,
      @Semantics.quantity.unitOfMeasure: 'meins'
      sim.QtdFat,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.SaldoOV,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.SaldoOVUSD,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.SaldoRefUSD,
      @Semantics.amount.currencyCode: 'Moeda'
      sim.SaldoRefBRL,
      _lcto
}
