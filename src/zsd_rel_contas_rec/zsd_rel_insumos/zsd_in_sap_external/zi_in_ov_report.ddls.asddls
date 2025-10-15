@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Acesso externo report IN'
@Metadata.ignorePropagatedAnnotations: true
@Analytics.query: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_OV_REPORT
  as select from ZI_IN_OV_HEADER as sim
  association [0..*] to ZI_IN_LCTOS_OV as _lcto on sim.vbeln = _lcto.vbeln_va
{
  key doc_simulacao     as Simulador,
  key vbeln_p           as Ordem,
  key Empresa,
  key vbeln             as OrdemDerivada,
      NumDocs,
      EscVendas,
      EqVendas,
      EqVendas_txt,
      //TpOV,
      //TpDevVenda,
      sim.Cliente,
      NomeCliente,
      //data_venc,
      safra,
      pgto_ant,
      //tx_multa,
      tx_juros,
      CondPagto,
      DescCondPagto,
      DataCriacao,
      Moeda,
      kmein             as meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      qtde              as Qtd,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_liq           as ValorLiq,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_imp           as ValorImp,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_tot_ov        as ValorTotal,
      @Semantics.amount.currencyCode: 'Moeda'
      case when Moeda = 'BRL' then
        vlr_rec_brl
      else
        vlr_rec_usd end as TotalRecebido,
      @Semantics.quantity.unitOfMeasure: 'meins'
      qtde_fat          as QtdFat,
      @Semantics.amount.currencyCode: 'Moeda'
      saldo_ov_brl      as SaldoOV,
      @Semantics.amount.currencyCode: 'Moeda'
      saldo_ov_usd      as SaldoOVUSD,
      _lcto
}
