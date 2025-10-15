@EndUserText.label: 'Aprovação - Lancamento manual itens'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LANC_MANUAL_IT'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Lanc. Manual',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Lanc Manual', value: 'lote' } } }

define custom entity ZC_APROV_LANC_MANUAL_IT
{

      @UI.facet       : [
              {
                id    :       'id',
                purpose:  #STANDARD,
                type  :     #IDENTIFICATION_REFERENCE,
                label :    'Documentos',
                position  : 10 }
            ]

      @UI             : {
      lineItem        : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote            : zlote_num;

      @UI             : {
      lineItem        : [{position: 20 , importance: #HIGH } ],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Documento'
  key doc_lcto        : numc10;

      @UI             : {
      lineItem        : [{position: 30, importance: #HIGH } ],
      identification  : [{position: 30}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['descEmpresa']
  key Empresa         : bukrs;

      @UI.hidden      : true
      descEmpresa     : butxt;

      @UI             : {
      lineItem        : [{position: 40, importance: #HIGH } ],
      identification  : [{position: 40}]     }
      @EndUserText.label: 'Tipo Lançamento'
      tp_lcto         : numc10;

      @UI             : {
      lineItem        : [{position: 50, importance: #HIGH } ],
      identification  : [{position: 50}] }
      @EndUserText.label: 'Descrição'
      descricao       : zdescr;

      @UI.hidden      : true
      moeda_int       : waers;

      @UI             : {
      lineItem        : [{position: 60, importance: #HIGH } ],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Valor moeda interna'
      @Semantics.amount.currencyCode : 'moeda_int'
      vlr_moeda_int   : hslxx12;

      @UI.hidden      : true
      moeda_forte     : waers;

      @UI             : {
      lineItem        : [{position: 70, importance: #HIGH } ],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Valor moeda forte'
      @Semantics.amount.currencyCode : 'moeda_forte'
      vlr_moeda_forte : kslxx12;

      _header         : association to parent ZC_APROV_LANC_MANUAL on  _header.lote    = $projection.lote
                                                                   and _header.empresa = $projection.Empresa;
}
