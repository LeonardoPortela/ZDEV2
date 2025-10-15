@EndUserText.label: 'Aprovação - Pagto Impostos'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_IMP'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Pagto Impostos',
                typeNamePlural: 'Pagto. Impostos',
                title: { type: #STANDARD, label: 'Pagto. Impostos', value: 'lote' } } }

define root custom entity ZC_APROV_PAGTO_IMP
{

      @UI.facet   : [
              {
                id:       'id',
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
                     label: 'Estratégia de Aprovadores',
                     id : 'estra',
                     type:  #LINEITEM_REFERENCE,
                     position:30,
                     targetElement: '_Estr'
                 }
            ]

      @UI         : {
      lineItem    : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote        : zlote_imp;

      @UI         : {
      lineItem    : [{position: 20, importance: #HIGH }],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]      }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descEmpresa']
      empresa     : bukrs;

      @UI.hidden  : true
      descempresa : butxt;

      @UI         : {
      lineItem    : [{position: 30, importance: #HIGH }],
      identification  : [{position: 30}] }
      @EndUserText.label: 'Dep. Responsavel'
      dep_resp    : char25;

      @UI         : {
      lineItem    : [{position: 40, importance: #HIGH }],
      identification  : [{position: 40}] }
      @EndUserText.label: 'Data Vencimento'
      dt_venc     : datum;

      @UI         : {
      lineItem    : [{position: 50, importance: #HIGH }],
      identification  : [{position: 50}] }
      @EndUserText.label: 'Valor Total'
      @Semantics.amount.currencyCode : 'moeda'
      total       : hslxx12;

      @UI         : {
      lineItem    : [{position: 60, importance: #HIGH }],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Observação'
      sgtxt       : sgtxt;

      @UI.hidden  : true
      moeda       : waers;

      _Item       : composition [1..*] of ZC_APROV_PAGTO_IMP_IT;
      _Estr       : composition [1..*] of ZC_APROV_PAGTO_IMP_EST;

}
