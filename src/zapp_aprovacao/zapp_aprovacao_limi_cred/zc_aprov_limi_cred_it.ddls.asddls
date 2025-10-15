@EndUserText.label: 'Aprovação - Limi Crédito'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIMI_CRED_IT'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Lim Crédito',
                typeNamePlural: 'Lim. Credito',
                title: { type: #STANDARD, label: 'Limi. Crédito', value: 'MATNR' } } }

define custom entity ZC_APROV_LIMI_CRED_IT
{
      @UI.facet    : [
          {
            id     :       'id',
            purpose:  #STANDARD,
            type   :     #IDENTIFICATION_REFERENCE,
            label  :    'Documentos',
            position  : 10 }
        ]

      @UI          : {
      lineItem     : [{position: 10, importance: #HIGH }],
      identification: [{position: 10}],
      selectionField: [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote         : zlote_num;


      @UI          : {
      lineItem     : [{position: 30, importance: #HIGH  } ],
      identification: [{position: 30}]     }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['maktx']
  key MATNR        : matnr;

      @UI.hidden   : true
      maktx        : maktx;

      @UI          : {
      lineItem     : [{position: 40, importance: #HIGH  } ],
      identification: [{position: 40}]     }
      @EndUserText.label: 'Filial'
      @ObjectModel.text.element: ['DESC_FILIAL']
      FILIAL       : werks_d;

      @UI.hidden   : true
      DESC_FILIAL  : name1;

      @UI.hidden   : true
      MOEDA        : waers;

      @UI          : {
      lineItem     : [{position: 50, importance: #HIGH  } ],
      identification: [{position: 60}]     }
      @EndUserText.label: 'Limite Credito BRL'
      @Semantics.amount.currencyCode : 'moeda'
      limite       : netwr_ak;

      @UI          : {
      lineItem     : [{position: 60, importance: #HIGH  } ],
      identification: [{position: 60}]     }
      @EndUserText.label: 'Limite Cred ultrapassado'
      @Semantics.amount.currencyCode : 'moeda'
      LIM_CRED_ULT : netwr_ak;

      @UI          : {
      lineItem     : [{position: 70, importance: #HIGH  } ],
      identification: [{position: 70}]     }
      @EndUserText.label: 'Limite Cred Estrag'
      @Semantics.amount.currencyCode : 'moeda'
      LIM_ESTR_ULT : netwr_ak;

      _header      : association to parent ZC_APROV_LIMI_CRED on _header.lote = $projection.lote;

}
