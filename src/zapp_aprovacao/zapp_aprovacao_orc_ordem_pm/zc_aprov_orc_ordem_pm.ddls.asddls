@EndUserText.label: 'Aprovação - Orç. Ordem PM'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_ORC_ORDEM_PM'
    }
}


@UI: {
  headerInfo: {
    typeName: 'Aprovação - Orç. Ordem PM',
    typeNamePlural: 'Aprovação - Orç. Ordem PM',
    title: {
        type: #STANDARD, label: 'Aprovação - Orç. Ordem PM', value: 'aufnr'
    }
  },
  presentationVariant: [
    {
        sortOrder: [{ by: 'erdat', direction:  #DESC }]
    }
  ],
  lineItem: [{ criticality: 'color' }]
}

define root custom entity ZC_APROV_ORC_ORDEM_PM
{
      @UI.facet : [
          {
              id      :  'id',
              purpose :  #STANDARD,
              type    :  #IDENTIFICATION_REFERENCE,
              label   :  'Orç. Ordem PM',
              position: 10
           }
      ]

      @UI.lineItem: [
          {
              label: 'Solicitação',
              dataAction: 'SOLICITA',
              type: #FOR_ACTION
          },
          {
              label: 'Alterar Valor',
              dataAction: 'MUDAVALOR',
              type: #FOR_ACTION
          }
      ]

      @UI       : {
          lineItem: [
              {
                  position: 15,
                  importance: #HIGH,
                  criticality: 'color',
                  criticalityRepresentation: #WITHOUT_ICON
               }],
          identification : [{position: 15}],
          selectionField : [{position: 15}]
      }
      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['descwerks']
  key werks     : werks_d;

      @UI       : {
         lineItem: [
             {
                 position: 40,
                 criticality: 'color',
                 criticalityRepresentation: #WITHOUT_ICON
              }
          ],
          identification : [{position: 40}],
          selectionField : [{position: 30}]
      }
      @EndUserText.label: 'Nº equipamento'
      @ObjectModel.text.element: ['eqktx']
  key equnr     : equnr;

      @UI       : {
        lineItem: [
            {
                position: 30,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 30}],
        selectionField : [{position: 20}]
      }
      @EndUserText.label: 'Nº ordem'
      @ObjectModel.text.element: ['ktext']
  key aufnr     : aufnr;

      @UI.hidden: true
      descwerks : name1;

      @UI.hidden: true
      ktext     : auftext;

      @UI.hidden: true
      eqktx     : ktx01;

      @UI       : {
        lineItem: [
            {
                position: 20,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
             }
        ],
        identification : [{position: 20}],
        selectionField : [{position: 20}]
      }
      @EndUserText.label: 'Data'
      erdat     : erdat;

      //      @UI       : {
      //      lineItem  : [{position: 60}],
      //      identification : [{position: 60}] }
      //      @EndUserText.label: 'Local de instalação'
      //      tplnr     : tplnr;

      @UI       : {
        lineItem: [
            {
                position: 80,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 80}],
        selectionField : [{position: 40}]
      }
      @EndUserText.label: 'Centro de trabalho'
      arbpl     : arbpl;

      @UI       : {
        lineItem: [
            {
                position: 100,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 100}]
      }
      @EndUserText.label: 'Vlr. Estimado'
      @Semantics.amount.currencyCode : 'moeda_1'
      user4     : aufuser4;

      @UI       : {
        lineItem: [
            {
                position: 110,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 110}]
      }
      @EndUserText.label: 'Vlr. Planejado'
      @Semantics.amount.currencyCode : 'moeda_1'
      wert2     : bp_wert2;

      @UI       : {
        lineItem: [
            {
                position: 120,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 120}]
      }
      @EndUserText.label: 'Status do Usuario'
      asttx     : co_asttx;

      @UI       : {
        lineItem: [
            {
                position: 130,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
             }
        ],
        identification : [{position: 130}]
      }
      @EndUserText.label: 'Observação'
      pltxt     : pltxt;

      @UI       : {
        lineItem: [
            {
                position: 140,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 140}]
      }
      @EndUserText.label: 'Moeda'
      moeda_1   : waers;

      @UI       : {
        lineItem  : [
            {
                position: 10,
                criticality: 'color',
                criticalityRepresentation: #WITHOUT_ICON
            }
        ],
        identification : [{position: 10}],
        selectionField : [{position: 10}] 
      }
      @EndUserText.label: 'Tipo'
      @ObjectModel.text.element: ['desctipo']
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_PP_TIPO_ORDEM_VH', element: 'Tipo' }}]
      tipo      : zde_tipo_ordem;

      @UI.hidden: true
      desctipo  : val_text;
      @UI.hidden: true
      color     : char1;

}
