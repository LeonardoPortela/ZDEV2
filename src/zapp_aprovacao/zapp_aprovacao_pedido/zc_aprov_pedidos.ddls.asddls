@EndUserText.label: 'Aprovação - Pedidos'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_PEDIDOS'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Pedidos',
                typeNamePlural: 'Pedidos',
                title: { type: #STANDARD, label: 'Aprov Pedidos', value: 'ebeln' } } }

@UI.presentationVariant: [{ sortOrder: [{ by:'IHRAN', direction:#ASC }] }]

define root custom entity ZC_APROV_PEDIDOS
{

      @UI.facet   : [
              {
                id: 'id',
                purpose: #STANDARD,
                type   : #IDENTIFICATION_REFERENCE,
                label  : 'Aprov. Pedidos',
                position : 10 },
                {
                     label: 'Documentos',
                     id : 'items',
                     type:  #LINEITEM_REFERENCE,
                     position:20,
                     targetElement: '_Item'
                 },
                {
                     label: 'Estratégia de aprovadores',
                     id : 'estra',
                     type:  #LINEITEM_REFERENCE,
                     position:30,
                     targetElement: '_Estr'
                 }
            ]


      @UI.hidden  : true
  key sww_wiid    : sww_wiid;

      @UI         : {
      lineItem    : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Pedido'
  key ebeln       : ebeln;


      @UI         : {
      lineItem    : [{position: 20, importance: #HIGH }],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]      }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descEmpresa']
      bukrs       : bukrs;

      @UI.hidden  : true
      descempresa : butxt;

      @UI         : {
      lineItem    : [{position: 22, importance: #HIGH}],
      identification  : [{position: 22}] }
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['name1']
      lifnr       : lifnr;

      @UI.hidden  : true
      name1       : name1;

      @UI         : {
      lineItem    : [{position: 24, importance: #HIGH}],
      identification  : [{position: 24 }] }
      @EndUserText.label: 'Data de Vencimento'
      IHRAN       : ihran;

      @UI         : {
      lineItem    : [{position: 30, importance: #HIGH}],
      identification  : [{position: 30}] }
      @EndUserText.label: 'Tipo Doc.'
      @ObjectModel.text.element: ['desc_bsart']
      bsart       : bsart;

      @UI.hidden  : true
      desc_bsart  : batxt;

      @UI         : {
      lineItem    : [{position: 32, importance: #HIGH}],
      identification  : [{position: 32}] }
      @EndUserText.label: 'Texto da Negociação'
      texto       : char300;


      @UI         : {
      lineItem    : [{position: 34, importance: #HIGH}],
      identification  : [{position: 34}] }
      @EndUserText.label: 'Data de criação'
      aedat       : aedat;

      @UI         : {
      lineItem    : [{position: 36, importance: #HIGH}],
      identification  : [{position: 36}] }
      @EndUserText.label: 'Usuário de Criação'
      ernam       : ernam;

      @UI         : {
      lineItem    : [{position: 38, importance: #HIGH}],
      identification  : [{position: 38}] }
      @EndUserText.label: 'Cond. Pagto '
      @ObjectModel.text.element: ['desc_zterm']
      zterm       : farp_dzterm;

      @UI.hidden  : true
      desc_zterm  : text1_052;

      @UI         : {
      lineItem    : [{position: 40, importance: #HIGH}],
      identification  : [{position: 40}] }
      @EndUserText.label: 'Valor em aprovação '
      @Semantics.amount.currencyCode: 'waers'
      RLWRT       : rlwrt;

      @UI.hidden  : true
      waers       : waers;

      _Item       : composition [1..*] of zc_aprov_pedidos_item;
      _Estr       : composition [1..*] of ZC_APROV_PEDI_ESTR;

}
