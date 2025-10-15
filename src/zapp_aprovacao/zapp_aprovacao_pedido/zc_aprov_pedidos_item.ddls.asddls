@EndUserText.label: 'Aprovação - Pedidos Itens'


@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_PEDIDOS_IT'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Pedidos',
                typeNamePlural: 'Itens',
                title: { type: #STANDARD, label: 'Aprov Itens Pedidos', value: 'ebeln' } } }
define custom entity zc_aprov_pedidos_item
{

      @UI.facet     : [
            {
              id    : 'id',
              purpose: #STANDARD,
              type  : #IDENTIFICATION_REFERENCE,
              label : 'Aprov. Pedidos',
              position : 10 }
          ]

      @UI.hidden    : true
  key sww_wiid      : sww_wiid;

      @UI.hidden    : true
  key ebeln         : ebeln;
      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH }] }
      @EndUserText.label: 'Item'
  key ebelp         : ebelp;
      @UI.hidden    : true
      bukrs         : bukrs;
      @UI           : {
      lineItem      : [{position: 20, importance: #HIGH }] }
      @EndUserText.label: 'Texto'
      desc_item     : txz01;
      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH }] }
      @EndUserText.label: 'Material'
      matnr         : matnr;
      @UI           : {
      lineItem      : [{position: 40, importance: #HIGH }] }
      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['desc_centro']
      werks         : werks_d;
      @UI.hidden    : true
      desc_centro   : name1;
      @UI           : {
      lineItem      : [{position: 50, importance: #HIGH }] }
      @EndUserText.label: 'Depósito'
      @ObjectModel.text.element: ['desc_deposito']
      lgort         : lgort_d;
      @UI.hidden    : true
      desc_deposito : lgobe;
      @UI           : {
      lineItem      : [{position: 60, importance: #HIGH }] }
      @EndUserText.label: 'Quantidade'
      @Semantics.quantity.unitOfMeasure : 'meins'
      menge         : bstmg;
      @UI.hidden    : true
      meins         : bstme;
      @UI           : {
      lineItem      : [{position: 70, importance: #HIGH }] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'waers'
      brtwr         : bbwert;
      waers         : waers;

      @UI           : {
      lineItem      : [{position: 80, importance: #HIGH }] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'waers'
      netpr         : bbwert;

      @UI           : {
      lineItem      : [{position: 90, importance: #HIGH }] }
      @EndUserText.label: 'Data Remessa'
      prdat         : prdat;

      _header       : association to parent ZC_APROV_PEDIDOS on  _header.sww_wiid = $projection.sww_wiid
                                                             and _header.ebeln    = $projection.ebeln;

}
