@EndUserText.label: 'Aprovação - Adiatamento Fornecedor IT'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_ADIA_FORN_IT'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Adia. Fornecedor',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Adia. Fornecedor', value: 'lote' } } }

define custom entity ZC_APROV_ADIA_FORN_IT
{


      @UI.facet        : [
              {
                id     :       'id',
                purpose:  #STANDARD,
                type   :     #IDENTIFICATION_REFERENCE,
                label  :    'Documentos',
                position  : 10 }
            ]

      @UI              : {
      lineItem         : [{position: 10, importance: #HIGH }],
      identification   : [{position: 10}],
      selectionField   : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote             : zlote_imp;

      @UI              : {
      lineItem         : [{position: 20, importance: #HIGH }],
      identification   : [{position: 20}],
      selectionField   : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['descEmpresa']
  key empresa          : bukrs;

      @UI              : {
      lineItem         : [{position: 30, importance: #HIGH } ],
      identification   : [{position: 30}],
      selectionField   : [{position: 30}]     }
  key ebeln            : ebeln;

      @UI              : {
          lineItem     : [{position: 40, importance: #HIGH } ],
          identification  : [{position: 40}],
          selectionField  : [{position: 40}]     }
  key ebelp            : ebelp;

      @UI.hidden       : true
      descempresa      : butxt;

      @UI              : {
      lineItem         : [{position: 50 , importance: #HIGH } ],
      identification   : [{position: 50}]     }
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['name1']
      lifnr            : lifnr;

      @UI.hidden       : true
      name1            : name1_gp;

      @UI              : {
      lineItem         : [{position: 60, importance: #HIGH } ],
      identification   : [{position: 60}]     }
      @EndUserText.label: 'Fornecedor Pedido'
      @ObjectModel.text.element: ['name1_po']
      lifnr_po         : lifnr;

      @UI.hidden       : true
      name1_po         : name1;

      @UI              : {
      lineItem         : [{position: 70, importance: #HIGH } ],
      identification   : [{position: 70}] }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['maktx']
      matnr            : matnr;
      @UI.hidden       : true
      maktx            : maktx;

      @UI              : {
      lineItem         : [{position: 80, importance: #HIGH } ],
      identification   : [{position: 80}] }
      @EndUserText.label: 'Saldo Item'
      @Semantics.amount.currencyCode : 'moeda_pgto'
      saldo_item       : hslxx12;

      @UI              : {
      lineItem         : [{position: 90, importance: #HIGH } ],
      identification   : [{position: 90}] }
      @EndUserText.label: 'Pagamento'
      @Semantics.amount.currencyCode : 'moeda_pgto'
      pgtos_real       : hslxx12;

      @UI              : {
      lineItem         : [{position: 100, importance: #HIGH } ],
      identification   : [{position: 100}] }
      @EndUserText.label: 'Saldo Disponivel'
      @Semantics.amount.currencyCode : 'moeda_pgto'
      sdo_disponivel   : hslxx12;

      @UI              : {
      lineItem         : [{position: 110, importance: #HIGH } ],
      identification   : [{position: 110}] }
      @EndUserText.label: 'Valor Adiantamento'
      @Semantics.amount.currencyCode : 'moeda_pgto'
      vlr_adiantamento : hslxx12;

      @UI              : {
      lineItem         : [{position: 120, importance: #HIGH } ],
      identification   : [{position: 120}] }
      @EndUserText.label: 'Data Pagamento'
      dt_pgto          : dats;

      @UI              : {
      lineItem         : [{position: 130, importance: #HIGH } ],
      identification   : [{position: 130}] }
      @EndUserText.label: 'Motivo'
      motivo           : char255;

      @UI              : {
      lineItem         : [{position: 140, importance: #HIGH } ],
      identification   : [{position: 140}] }
      @EndUserText.label: 'Data Prevista'
      dt_prev_liq      : dats;

      @UI.hidden       : true
      moeda_pgto       : waers;

      @UI              : {
      lineItem         : [{position: 150, importance: #HIGH } ],
      identification   : [{position: 150}] }
      @EndUserText.label: 'Solicitante'
      solicitante      : ad_namtext;

      @UI              : {
      lineItem         : [{position: 160, importance: #HIGH } ],
      identification   : [{position: 160}] }
      @EndUserText.label: 'Negociador'
      negociador       : ad_namtext;

      _header          : association to parent ZC_APROV_ADIA_FORN on  _header.lote    = $projection.lote
                                                                  and _header.empresa = $projection.empresa;

}
