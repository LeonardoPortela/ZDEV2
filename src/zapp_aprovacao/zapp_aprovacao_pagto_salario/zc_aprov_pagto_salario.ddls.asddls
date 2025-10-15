@EndUserText.label: 'Aprovação - Pagamento Salário'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_SALARIO'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Pagto. Salario',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Pagto Salario', value: 'lote' } } }

define root custom entity ZC_APROV_PAGTO_SALARIO

{

      @UI.facet   : [
              {
                id:       'id',
                purpose:  #STANDARD,
                type  :     #IDENTIFICATION_REFERENCE,
                label :    'Documentos',
                position  : 10 },
             {
                     label: 'Documentos',
                     id : 'items',
                     type:  #LINEITEM_REFERENCE,
                     position:20,
                     targetElement: '_Item'
                 }
            ]

      @UI         : {
      lineItem    : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote        : zlote_num;


      @UI         : {
      lineItem    : [{position: 20, importance: #HIGH  } ],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descempresa']
  key empresa     : char30;

      @UI.hidden  : true
      descempresa : butxt;

      @UI         : {
      lineItem    : [{position: 30, importance: #HIGH  } ],
      identification  : [{position: 30}]     }
      @EndUserText.label: 'Area FPG'
      @ObjectModel.text.element: ['descareafpg']
      AREAFPG     : char20;

      @UI.hidden  : true
      descareafpg : abktx;

      @UI         : {
      lineItem    : [{position: 40, importance: #HIGH  } ],
      identification  : [{position: 40}]     }
      @EndUserText.label: 'Tp. de Pagamento'
      TP_PGTO     : char10;

      @UI         : {
      lineItem    : [{position: 50, importance: #HIGH  } ],
      identification  : [{position: 50}]     }
      @EndUserText.label: 'Total'
      @Semantics.amount.currencyCode : 'WAERS'
      TOTAL       : pad_vgbtr;

      @UI         : {
      lineItem    : [{position: 60, importance: #HIGH  } ],
      identification  : [{position: 60}]     }
      @EndUserText.label: 'Txt. Item'
      SGTXT       : sgtxt;

      @UI.hidden  : true
      WAERS       : waers;

      _Item       : composition [1..*] of ZC_APROV_PAGTO_SALARIO_IT;

}
