@EndUserText.label: 'Aprovação - Ordem Venda - Item'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIB_OV_IT'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Lib. Solicitação Ord.Venda - Item',
                typeNamePlural: 'Aprov. Lib. Solicitação Ord.Venda - Item',
                title: { type: #STANDARD, label: 'Aprov. Lib. Solicitação Ord.Venda - Item', value: 'nrosolov' } } }

define custom entity ZC_APROV_PAGTO_OV_IT
{
      @UI.facet     : [
         {
           id       :       'id',
           purpose  :  #STANDARD,
           type     :     #IDENTIFICATION_REFERENCE,
           label    :    'Lib. Embarque Insumo - Item',
           position : 10 }
       ]

      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH }],
      identification: [{position: 10}],
      selectionField: [{position: 10}]     }
      @EndUserText.label: 'N. Solicitação'
  key nrosolov      : zsded013;

      @UI           : {
          lineItem  : [{position: 20, importance: #HIGH }],
          identification : [{position: 20}] }
      @EndUserText.label: 'Item'
  key item          : posnr;

      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH }],
      identification: [{position: 30}] }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['descmatnr']
      matnr         : matnr;

      @UI.hidden    : true
      descmatnr     : char30;

      @UI           : {
      lineItem      : [{position: 40, importance: #HIGH }],
      identification: [{position: 40}] }
      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['desccentro']
      centro        : werks_ext;

      @UI.hidden    : true
      desccentro    : name1;

      @UI           : {
          lineItem  : [{position: 50, importance: #HIGH }],
          identification : [{position: 50}] }
      @EndUserText.label: 'Ponto Coleto'
      ponto_coleta  : lifnr;

      @UI           : {
      lineItem      : [{position: 60, importance: #HIGH }],
      identification: [{position: 60}] }
      @EndUserText.label: 'Terminal'
      terminal      : lifnr;

      @UI           : {
      lineItem      : [{position: 70, importance: #HIGH }],
      identification: [{position: 70}] }
      @EndUserText.label: 'Deposito'
      @ObjectModel.text.element: ['desc_deposito']
      deposito      : lgort_d;

      @UI.hidden    : true
      desc_deposito : lgobe;

      @UI           : {
      lineItem      : [{position: 80, importance: #HIGH }],
      identification: [{position: 80}] }
      @EndUserText.label: 'Lote'
      lote          : charg_d;

      @UI           : {
      lineItem      : [{position: 90, importance: #HIGH }],
      identification: [{position: 90}] }
      @EndUserText.label: 'Qtd Prevista'
      @Semantics.quantity.unitOfMeasure: 'MEINS'
      qtdprev       : zsded054;


      @UI           : {
      lineItem      : [{position: 100, importance: #HIGH }],
      identification: [{position: 100}] }
      @EndUserText.label: 'UM'
      um            : dzieme;

      @UI           : {
      lineItem      : [{position: 110, importance: #HIGH }],
      identification: [{position: 110}] }
      @EndUserText.label: 'Preço'
      @Semantics.amount.currencyCode : 'waerk'
      preco         : dmbtr;

      @UI           : {
      lineItem      : [{position: 120, importance: #HIGH }],
      identification: [{position: 120}] }
      @EndUserText.label: 'UnPreço'
      um_preco      : pmein;

      @UI           : {
      lineItem      : [{position: 130, importance: #HIGH }],
      identification: [{position: 130}] }
      @EndUserText.label: 'Valor Total'
      @Semantics.amount.currencyCode : 'waerk'
      valor_total   : dmbtr;

      @UI           : {
      lineItem      : [{position: 140, importance: #HIGH }],
      identification: [{position: 140}] }
      @EndUserText.label: 'Data Vencimento'
      data_venc     : valdt;

      @UI.hidden    : true
      waerk         : waerk;

      @UI.hidden    : true
      meins         : meins;

      _header       : association to parent ZC_APROV_PAGTO_OV on _header.NROSOLOV = $projection.nrosolov;
}
