@EndUserText.label: 'Aprovação - Pagto Impostos itens'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_IMP_IT'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Pagto Impostos',
                typeNamePlural: 'Pagto. Impostos',
                title: { type: #STANDARD, label: 'Pagto. Impostos', value: 'lote' } } }
define custom entity ZC_APROV_PAGTO_IMP_IT
{

      @UI.facet     : [
              {
                id  :       'id',
                purpose:  #STANDARD,
                type:     #IDENTIFICATION_REFERENCE,
                label :    'Documentos',
                position  : 10 }
            ]

      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH }],
      identification: [{position: 10}],
      selectionField: [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote          : zlote_imp;

      @UI           : {
      lineItem      : [{position: 20 , importance: #HIGH } ],
      identification: [{position: 20}],
      selectionField: [{position: 20}]     }
      @EndUserText.label: 'Documento'
  key doc_imposto   : zdoc_imposto;

      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH  } ],
      identification: [{position: 30}]     }
      @EndUserText.label: 'Descrição Imposto'
      descr_imposto : char50;

      @UI           : {
      lineItem      : [{position: 40, importance: #HIGH  } ],
      identification: [{position: 40}]     }
      @EndUserText.label: 'Convênio'
      conv_banco    : zconv_banco;

      @UI           : {
      lineItem      : [{position: 50, importance: #HIGH  } ],
      identification: [{position: 50}]     }
      @EndUserText.label: 'Banco Pagador'
      hbkid         : char20;

      @UI.hidden    : true
      moeda         : waers;

      @UI           : {
      lineItem      : [{position: 60, importance: #HIGH  } ],
      identification: [{position: 60}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'moeda'
      valor         : hslxx12;

      @UI           : {
      lineItem      : [{position: 70, importance: #HIGH  } ],
      identification: [{position: 70}]     }
      @EndUserText.label: 'Observação'
      observacao    : zobs_imp;

      _header       : association to parent ZC_APROV_PAGTO_IMP on _header.lote = $projection.lote;

}
