@AbapCatalog.sqlViewName: 'ZIINEXTERNAL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Acesso externo Insumos'
@Analytics: {
    dataCategory: #FACT,
    dataExtraction: {
    enabled: true,

    delta.changeDataCapture: {

    mapping: [{
                role: #MAIN,
                table: 'ZSDT0040',
                viewElement: [ 'Simulador' ],
                tableElement: [ 'doc_simulacao' ]
               },
               {
                role: #LEFT_OUTER_TO_ONE_JOIN,
                table: 'vbak',
                viewElement: [ 'Ordem' ],
                tableElement: [ 'vbeln' ]
               },
               {
                role: #LEFT_OUTER_TO_ONE_JOIN,
                table: 'T001',
                viewElement: [ 'Empresa' ],
                tableElement: [ 'bukrs' ]
               },
               {
                role: #LEFT_OUTER_TO_ONE_JOIN,
                table: 'vbak',
                viewElement: [ 'OrdemDerivada' ],
                tableElement: [ 'vbeln' ]
               }
        ]
    }
}

 }
define view ZI_IN_EXTERNAL
  as select from ZI_IN_OV_HEADER as sim
{
  key doc_simulacao         as Simulador,
      vbeln_p               as Ordem,
      Empresa               as Empresa,
      sim.vbeln             as OrdemDerivada,
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
      sim.Moeda,
      kmein                 as meins,
      @Semantics.quantity.unitOfMeasure: 'meins'
      qtde                  as Qtd,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_liq               as ValorLiq,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_imp               as ValorImp,
      @Semantics.amount.currencyCode: 'Moeda'
      vlr_tot_ov            as ValorTotal,
      @Semantics.amount.currencyCode: 'Moeda'
      case when sim.Moeda = 'BRL' then
        sim.vlr_rec_brl
      else
        sim.vlr_rec_usd end as TotalRecebido,
      @Semantics.quantity.unitOfMeasure: 'meins'
      qtde_fat              as QtdFat,
      @Semantics.amount.currencyCode: 'Moeda'
      saldo_ov_brl          as SaldoOV,
      @Semantics.amount.currencyCode: 'Moeda'
      saldo_ov_usd          as SaldoOVUSD,
      zid_lanc,
      seq,
      belnr,
      augbl,
      data_venc,
      data_pgto,
      taxa,
      forma_pag,
      vlr_rec_usd_lcto,
      vlr_rec_brl_lcto,
      multa_calc,
      multa_rec,
      desc_multa,
      juros_calc,
      juros_rec,
      desc_juros,
      observacao
}
