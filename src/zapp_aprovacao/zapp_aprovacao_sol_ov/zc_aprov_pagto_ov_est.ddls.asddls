@EndUserText.label: ' Aprovação - Lancamento Manual Estra'
@ObjectModel: {
   query: {
       implementedBy: 'ABAP:ZCLSD_APP_APROV_LIB_OV_ESTR'
   }
}

@UI: {
  headerInfo: { typeName: 'Aprovadores',
                typeNamePlural: 'Aprovadores',
                title: { type: #STANDARD, label: 'Aprovadores', value: 'nrosolov' } } }

define custom entity ZC_APROV_PAGTO_OV_EST
{
      @UI.facet  : [
                      {
                        id     :       'id',
                        purpose:  #STANDARD,
                        type   :     #IDENTIFICATION_REFERENCE,
                        label  :    'Aprovadores',
                        position  : 10 }

                    ]
      @UI        : {
            lineItem       : [{position: 10, importance: #HIGH }],
            identification : [{position: 10}],
            selectionField : [{position: 10}]     }
      @EndUserText.label: 'N. Solicitação'
  key nrosolov   : zsded013;


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

      _header    : association to parent ZC_APROV_PAGTO_OV on _header.NROSOLOV = $projection.nrosolov;


}
