@AbapCatalog.sqlViewName: 'ZIMIEXTERNAL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Acesso externo Mercado Interno'
@Analytics: {
    dataCategory: #FACT,
    dataExtraction: {
    enabled: true,

    delta.changeDataCapture: {

    mapping: [{
                role: #MAIN,
                table: 'ZSDT0051',
                viewElement: [ 'Simulador' ],
                tableElement: [ 'NRO_SOL_OV' ]
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
define view ZI_MI_EXTERNAL
  as select from ZI_MI_OV_CDC
{
  key Simulador,
      //key ItemSimulador,
      Ordem,
      Empresa,
      OrdemDerivada,
      NumDocs,
      EscVendas,
      EqVendas,
      EqVendas_txt,
      TpOV,
      TpDevVenda,
      Cliente,
      NomeCliente,
      //data_venc,
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
      _lcto.ID26           as zid_lanc,
      //_lcto.seq,
      _lcto.DocCont        as belnr,
      _lcto.DocComp        as augbl,
      _lcto.DtVenc         as data_venc,
      _lcto.DtPagto        as data_pgto,
      //_lcto. taxa,
      _lcto.ForPagto       as forma_pag,
      _lcto.vlr_juros_rbdo as vlr_rec_usd_lcto,
      //_lcto.vlr_rec_brl_lcto,
      _lcto.vlr_multa_calc as multa_calc,
      _lcto.vlr_multa_rbdo as multa_rec,
      _lcto.vlr_desc_mult  as desc_multa,
      _lcto.vlr_juros_calc as juros_calc,
      _lcto.vlr_juros_rbdo as juros_rec,
      _lcto.vlr_desc_jros  as desc_juros,
      _lcto.observacao


















}
