@EndUserText.label: 'Aprovação - Pagto Salário'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_SALARIO_IT'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Pagto. Salário',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Pagto Salário', value: 'lote' } } }

define custom entity ZC_APROV_PAGTO_SALARIO_IT

{

      @UI.facet   : [
              {
                id:       'id',
                purpose:  #STANDARD,
                type  :     #IDENTIFICATION_REFERENCE,
                label :    'Documentos',
                position  : 10 }
            ]

      @UI         : {
      lineItem    : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote        : zlote_num;

      @UI         : {
       lineItem   : [{position: 20, importance: #HIGH  } ],
       identification  : [{position: 20}],
       selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['butxt']
  key empresa     : char30;


      @UI         : {
      lineItem    : [{position: 20 , importance: #HIGH } ],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Funcionário'
      @ObjectModel.text.element: ['cname']
  key PERNR       : pernr_d;

      @UI.hidden  : true
      butxt       : butxt;

      @UI.hidden  : true
      cname       : pad_cname;

      @UI         : {
      lineItem    : [{position: 30, importance: #HIGH  } ],
      identification  : [{position: 30}],
      selectionField  : [{position: 30}]     }
      @EndUserText.label: 'Tipo Pagto'
      tp_pgto     : char10;

      @UI         : {
      lineItem    : [{position: 40 , importance: #HIGH } ],
      identification  : [{position: 40}] }
      @EndUserText.label: 'Status'
      status      : char5;

      @UI         : {
      lineItem    : [{position: 50, importance: #HIGH  } ],
      identification  : [{position: 50}] }
      @EndUserText.label: 'Data Pagto'
      paydt       : pay_date;

      @UI         : {
      lineItem    : [{position: 60, importance: #HIGH  } ],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Data Crédito'
      dt_cred     : char10;

      @UI         : {
      lineItem    : [{position: 70, importance: #HIGH  } ],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Banco'
      bankl       : bankl;

      @UI         : {
      lineItem    : [{position: 80, importance: #HIGH  } ],
      identification  : [{position: 80}] }
      @EndUserText.label: 'Banco Pagador'
      banka       : banka;

      @UI         : {
      lineItem    : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'waers'
      betrg       : pad_vgbtr;

      @UI.hidden  : true
      waers       : waers;

      @UI         : {
      lineItem    : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Doc Contabil'
      doc_cont    : belnr_d;

      @UI         : {
      lineItem    : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Area RH'
      @ObjectModel.text.element: ['descarearh']
      arearh      : werks_d;

      @UI.hidden  : true
      descarearh  : name1;

      @UI         : {
      lineItem    : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Area Folha'
      @ObjectModel.text.element: ['descareafpg']
      areafpg     : abkrs;

      @UI.hidden  : true
      descareafpg : atext;

      @UI         : {
      lineItem    : [{position: 90 , importance: #HIGH } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Centro de Custo'
      kostl       : kostl;

      @UI         : {
      lineItem    : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Observação'
      observacao  : char72;

      @UI.hidden  : true
      endda       : endda;
      @UI.hidden  : true
      begda       : begda;
      @UI.hidden  : true
      aedtm       : aedat;
      @UI.hidden  : true
      aezet       : aezet;

      _header     : association to parent ZC_APROV_PAGTO_SALARIO on  _header.lote    = $projection.lote
                                                                 and _header.empresa = $projection.empresa;
}
