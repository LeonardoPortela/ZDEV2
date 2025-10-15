@EndUserText.label: 'Aprovação - Pagto Imposto Estra'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLFI_APP_APROV_PAGTO_IMP_ES'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprovadores',
                typeNamePlural: 'Aprovadores',
                title: { type: #STANDARD, label: 'Aprovadores', value: 'lote' } } }

define custom entity ZC_APROV_PAGTO_IMP_EST
{


      @UI.facet  : [
              {
                id     :       'id',
                purpose:  #STANDARD,
                type   :     #IDENTIFICATION_REFERENCE,
                label  :    'Documentos',
                position  : 10 }
            ]

      @UI.hidden : true
      @EndUserText.label: 'Lote'
  key lote       : zlote_imp;

      @UI.hidden : true
      @EndUserText.label: 'Empresa'
  key empresa    : bukrs;

      @UI        : {
          lineItem         : [{position: 30, importance: #HIGH } ],
          identification   : [{position: 30}],
          selectionField   : [{position: 30}]     }
      @EndUserText.label: 'Nível'
  key nivel      : znivel_aprov;

      @UI        : {
              lineItem     : [{position: 40, importance: #HIGH } ],
              identification  : [{position: 40}]   }
      @EndUserText.label: 'Aprovador'
      @ObjectModel.text.element: [ 'nome_aprov' ]
      aprovador  : usnam;

      @UI.hidden : true
      nome_aprov : text80;

      @UI        : {
              lineItem     : [{position: 50, importance: #HIGH } ],
              identification  : [{position: 50}]   }
      @EndUserText.label: 'Valor de'
      valor_de   : char15;

      @UI        : {
              lineItem     : [{position: 60, importance: #HIGH } ],
              identification  : [{position: 60}]   }
      @EndUserText.label: 'Valor Até'
      valor_ate  : char15;

      @UI        : {
              lineItem     : [{position: 70, importance: #HIGH , criticality: 'cor'} ],
              identification  : [{position: 70}]   }
      @EndUserText.label: 'Estado'
      estado     : char30;

      @UI.hidden : true
      cor        : char1;

      @UI        : {
              lineItem     : [{position: 80, importance: #HIGH } ],
              identification  : [{position: 80}]   }
      @EndUserText.label: 'Data Aprovação'
      data_aprov : datum;

      @UI        : {
              lineItem     : [{position: 90, importance: #HIGH} ],
              identification  : [{position: 90}]   }
      @EndUserText.label: 'Hora Aprovação'
      hora_aprov : uzeit;

      @UI        : {
              lineItem     : [{position: 100, importance: #HIGH} ],
              identification  : [{position: 100}]   }
      @EndUserText.label: 'Usuário Aprovação'
      @ObjectModel.text.element: ['nome_user']
      usuario    : usnam;

      @UI.hidden : true
      nome_user  : text80;

      _header    : association to parent ZC_APROV_PAGTO_IMP on _header.lote = $projection.lote;
}
