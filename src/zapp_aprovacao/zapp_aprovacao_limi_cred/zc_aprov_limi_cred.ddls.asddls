@EndUserText.label: 'Aprovação - Limi. Credito'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIMI_CRED'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Limite Crédito',
                typeNamePlural: 'Limi. Crédito',
                title: { type: #STANDARD, label: 'Limi. Crédito', value: 'lote' } } }

define root custom entity ZC_APROV_LIMI_CRED
{

      @UI.facet    : [
          {
            id     :       'id',
            purpose:  #STANDARD,
            type   :     #IDENTIFICATION_REFERENCE,
            label  :    'Limi. Crédito',
            position  : 10 },

            {
                 label: 'Documentos',
                 id: 'items',
                 type:  #LINEITEM_REFERENCE,
                 position:20,
                 targetElement: '_Item'
             },
             {
                 label: 'Estratégia de liberação',
                 id: 'est',
                 type:  #LINEITEM_REFERENCE,
                 position:30,
                 targetElement: '_Est'
             }
        ]

      @UI          : {
      lineItem     : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote         : zlote_imp;

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_APROV_LIM_CRED_VH',
                                                              element: 'Cliente'} }]
      @UI          : {
      lineItem     : [{position: 20, importance: #HIGH }],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]  }
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['DESC_CLIENTE']
      CLIENTE      : kunnr;

      @UI.hidden   : true
      DESC_CLIENTE : name1_gp;

      @UI          : {
      lineItem     : [{position: 30, importance: #HIGH }],
      identification  : [{position: 30}] }
      @EndUserText.label: 'OV'
      VBELN        : vbeln_va;

      @UI          : {
      lineItem     : [{position: 40, importance: #HIGH }],
      identification  : [{position: 40}] }
      @EndUserText.label: 'Romaneio'
      NR_ROMANEIO  : znr_romaneio;

      @UI          : {
      lineItem     : [{position: 50, importance: #HIGH }],
      identification  : [{position: 50}] }
      @EndUserText.label: 'Qtd'
      @Semantics.quantity.unitOfMeasure: 'MEINS'
      LFIMG        : lfimg;

      @Semantics.unitOfMeasure
      @UI.hidden   : true
      MEINS        : meins;

      @UI          : {
      lineItem     : [{position: 60, importance: #HIGH }],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Moeda'
      MOEDA        : waers;

      @UI          : {
      lineItem     : [{position: 70, importance: #HIGH }],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Valor Fatura BRL'
      @Semantics.amount.currencyCode : 'moeda_1'
      TOTAL        : netwr_ak;

      @UI          : {
      lineItem     : [{position: 80, importance: #HIGH }],
      identification  : [{position: 80}] }
      @EndUserText.label: 'Valor Aprovar'
      @Semantics.amount.currencyCode : 'moeda_1'
      VALOR_APROV  : netwr_ak;

      @UI.hidden   : true
      moeda_1      : waers;

      _Item        : composition [1..*] of ZC_APROV_LIMI_CRED_IT;
      _Est         : composition [1..*] of ZC_APROV_LIMI_CRED_EST;

}
