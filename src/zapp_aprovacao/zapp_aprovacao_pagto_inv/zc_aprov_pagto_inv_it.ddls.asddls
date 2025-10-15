@EndUserText.label: 'Aprovação - Pagto Invoice'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_INV_IT'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Pagto. Invoice',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Pagto Invoice', value: 'lote' } } }

define custom entity ZC_APROV_PAGTO_INV_IT

{


      @UI.facet   : [
        {
          id      :       'id',
          purpose :  #STANDARD,
          type    :     #IDENTIFICATION_REFERENCE,
          label   :    'Documentos',
          position: 10 }
      ]

      @UI         : {
      lineItem    : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote        : zlote_num;

      @UI         : {
        lineItem  : [{position: 20, importance: #HIGH  } ],
        identification  : [{position: 20}],
        selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['descempresa']
  key empresa     : bukrs;

      @UI.hidden  : true
      descempresa : butxt;

      @UI         : {
      lineItem    : [{position: 30, importance: #HIGH }],
      identification  : [{position: 30}],
      selectionField  : [{position: 30}]     }
      @EndUserText.label: 'Chave'
      OBJ_KEY     : awkey;


      @UI         : {
      lineItem    : [{position: 40 , importance: #HIGH } ],
      identification  : [{position: 40}],
      selectionField  : [{position: 40}]     }
      @EndUserText.label: 'Invoice'
      INVOICE     : char15;

      @UI         : {
      lineItem    : [{position: 50, importance: #HIGH  } ],
      identification  : [{position: 50}] }
      @EndUserText.label: 'NRSol OV'
      NRO_SOL_OV  : zsded013;

      @UI         : {
      lineItem    : [{position: 60, importance: #HIGH  } ],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Navio'
      NAVIO       : char30;

      @UI         : {
      lineItem    : [{position: 70, importance: #HIGH  } ],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'waers'
      VLR_PGTO    : pad_vgbtr;

      @UI.hidden  : true
      waers       : waers;

      @UI         : {
      lineItem    : [{position: 80, importance: #HIGH  } ],
      identification  : [{position: 80}] }
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['descname1']
      NAME1       : name1_gp;

      @UI.hidden  : true
      descname1   : name2_gp;

      @UI         : {
      lineItem    : [{position: 90 , importance: #HIGH } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Observação'
      OBSERVACAO  : char50;

      _header     : association to parent ZC_APROV_PAGTO_INV on  _header.lote    = $projection.lote
                                                             and _header.empresa = $projection.empresa;
}
