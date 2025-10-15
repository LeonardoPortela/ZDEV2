@EndUserText.label: 'Aprovação - Adiatamento Fornecedor'
@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_ADIA_FORN'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Adia. Fornecedor',
                typeNamePlural: 'Adia. Fornecedor',
                title: { type: #STANDARD, label: 'Adia. Fornecedor', value: 'lote' } } }
define root custom entity ZC_APROV_ADIA_FORN
{

      @UI.facet     : [
              {
                id  :     'id',
                purpose:  #STANDARD,
                type:     #IDENTIFICATION_REFERENCE,
                label :    'Adia. Fornecedor',
                position  : 10 },

                {
                     label: 'Documentos',
                     id : 'items',
                     type:  #LINEITEM_REFERENCE,
                     position:20,
                     targetElement: '_Item'
                 },

                {
                     label: 'Estratégia de Aprovadores',
                     id : 'estr',
                     type:  #LINEITEM_REFERENCE,
                     position:30,
                     targetElement: '_Estr'
                 }
            ]


      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH } , { type: #FOR_ACTION,  dataAction: 'aprovar' , invocationGrouping:#CHANGE_SET }],
      identification: [{position: 10}],
      selectionField: [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote          : zlote_imp;
      @UI           : {
          lineItem  : [{position: 20, importance: #HIGH }],
          identification  : [{position: 20}],
          selectionField  : [{position: 20}]      }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descEmpresa']
  key empresa       : bukrs;

      @UI.hidden    : true
      descempresa   : butxt;

      @UI           : {
      lineItem      : [{position: 22 , importance: #HIGH } ],
      identification: [{position: 22}]     }
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['name1']
      lifnr         : lifnr;

      @UI.hidden    : true
      name1         : name1_gp;

      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH}],
      identification: [{position: 30}] }
      @EndUserText.label: 'Dep. Responsavel'
      @ObjectModel.text.element: ['dep_resp_desc']
      dep_resp      : zdep_resp;

      @UI.hidden    : true
      dep_resp_desc : zdep_resp_desc;

      @UI           : {
      lineItem      : [{position: 40, importance: #HIGH}],
      identification: [{position: 40}] }
      @EndUserText.label: 'Data Vencimento'
      dt_venc       : datum;

      @UI           : {
      lineItem      : [{position: 50, importance: #HIGH}],
      identification: [{position: 50}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'moeda'
      valor         : hslxx12;

      @UI.hidden    : true
      moeda         : waers;

      @UI           : {
      lineItem      : [{position: 60, importance: #HIGH}],
      identification: [{position: 60}] }
      @EndUserText.label: 'Limite'
      limite        : char20;

      @UI           : {
      lineItem      : [{position: 70, importance: #HIGH}],
      identification: [{position: 70}] }
      @EndUserText.label: 'Saldo'
      saldo         : char20;

      _Item         : composition [1..*] of ZC_APROV_ADIA_FORN_IT;
      _Estr         : composition [1..*] of ZC_APROV_ADIA_FORN_EST;
}
