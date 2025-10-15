@EndUserText.label: 'Aprovação - Isenção de Juros'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLFI_APP_APROV_ISENCAO_JUROS'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Isenção de Juros',
                typeNamePlural: 'Isenção de Juros',
                title: { type: #STANDARD, label: 'Isenção de Juros', value: 'ov_principal' } } }

define root custom entity ZC_APROV_ISENCAO_JUROS
{

      @UI.facet     : [
              {
                id  :       'id',
                purpose:  #STANDARD,
                type:     #IDENTIFICATION_REFERENCE,
                label :    'Isenção de Juros',
                position  : 10 },

                {
                     label: 'Documentos',
                     id : 'items',
                     type:  #LINEITEM_REFERENCE,
                     position:20,
                     targetElement: '_Item'
                 }
            ]


      @UI.hidden    : true
  key simul_venda   : zsded003;

      @UI           : {
      lineItem      : [{position: 20, importance: #HIGH }],
      identification: [{position: 20}],
      selectionField: [{position: 10}]      }
      @EndUserText.label: 'OV Principal'
  key ov_principal  : vbeln;

      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH }],
      identification: [{position: 10}] }
      @EndUserText.label: 'Tipo Negócio'
      tipo_negocio  : zdefi_tp_neg;

      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH }],
      identification: [{position: 30}] }
      @EndUserText.label: 'Moeda'
      moeda         : waerk;

      @UI           : {
      lineItem      : [{position: 40, importance: #HIGH }],
      identification: [{position: 40}] }
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['desccliente']
      cliente       : kunnr;

      @UI.hidden    : true
      desccliente   : name1_gp;

      @UI           : {
      lineItem      : [{position: 50, importance: #HIGH}],
      identification: [{position: 50}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'moeda'
      valor         : netwr;

      @UI           : {
      lineItem      : [{position: 60, importance: #HIGH }],
      identification: [{position: 60}] }
      @EndUserText.label: 'Justificativa Workflow'
      just_workflow : char255;

      _Item         : composition [0..*] of ZC_APROV_ISENCAO_JUROS_IT;

}
