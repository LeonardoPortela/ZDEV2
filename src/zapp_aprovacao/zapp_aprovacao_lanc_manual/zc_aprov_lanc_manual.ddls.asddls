@EndUserText.label: 'Aprovação - Lancamento manual'
@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LANC_MANUAL'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Lanc. Manual',
                typeNamePlural: 'Lanc. Manual',
                title: { type: #STANDARD, label: 'Lanc Manual', value: 'lote' } } }

define root custom entity ZC_APROV_LANC_MANUAL

{

      @UI.facet       : [
              {
                id    :       'id',
                purpose:  #STANDARD,
                type  :     #IDENTIFICATION_REFERENCE,
                label :    'Lanc. Manual',
                position  : 10 },

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


      @UI             : {
      lineItem        : [{position: 10, importance: #HIGH } , { type: #FOR_ACTION,  dataAction: 'aprovar' , invocationGrouping:#CHANGE_SET }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote            : zlote_imp;

      @UI             : {
      lineItem        : [{position: 20, importance: #HIGH }],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]      }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descEmpresa']
  key empresa         : bukrs;

      @UI.hidden      : true
      descEmpresa     : butxt;

      @UI             : {
      lineItem        : [{position: 40, importance: #HIGH}],
      identification  : [{position: 40}] }
      @EndUserText.label: 'Dep. Responsavel'
      dep_resp        : char25;

      @UI             : {
      lineItem        : [{position: 60, importance: #HIGH}],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Valor moeda Interna'
      @Semantics.amount.currencyCode : 'moeda_int'
      vlr_moeda_int   : hslxx12;

      @UI             : {
      lineItem        : [{position: 62, importance: #HIGH}],
      identification  : [{position: 62}] }
      @EndUserText.label: 'Valor moeda Forte'
      @Semantics.amount.currencyCode : 'moeda_forte'
      valorMoedaForte : hslxx12;

      @UI.hidden      : true
      moeda_int       : waers;

      @UI.hidden      : true
      moeda_forte     : waers;

      @UI             : {
      lineItem        : [{position: 70, importance: #HIGH}],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Texto Contábil'
      sgtxt           : sgtxt;

      _Item           : composition [1..*] of ZC_APROV_LANC_MANUAL_IT;
      _Estr           : composition [1..*] of ZC_APROV_LANC_MANUAL_EST;
}
