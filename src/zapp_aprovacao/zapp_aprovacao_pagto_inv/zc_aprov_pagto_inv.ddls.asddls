@EndUserText.label: 'Aprovação - Pagamento Invoice'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_PAGTO_INV'
    }
}
@UI: {
  headerInfo: { typeName: 'Aprov. Pagto. Invoice',
                typeNamePlural: 'Documentos',
                title: { type: #STANDARD, label: 'Pagto Invoice', value: 'lote' } } }

define root custom entity ZC_APROV_PAGTO_INV
{

      @UI.facet    : [
                {
                  id:       'id',
                  purpose:  #STANDARD,
                  type  :     #IDENTIFICATION_REFERENCE,
                  label :    'Pagamento Invoice',
                  position  : 10 },
               {
                       label: 'Documentos',
                       id : 'items',
                       type:  #LINEITEM_REFERENCE,
                       position:20,
                       targetElement: '_Item'
                   },
                   {
                       label: 'Estratégia de liberação',
                       id : 'est',
                       type:  #LINEITEM_REFERENCE,
                       position:30,
                       targetElement: '_Estra'
                   }
              ]

      @UI          : {
      lineItem     : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Lote'
  key lote         : zlote_imp;

      @UI          : {
      lineItem     : [{position: 20, importance: #HIGH  } ],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['descempresa']
  key empresa      : bukrs;

      @UI.hidden   : true
      descempresa  : butxt;

      @UI          : {
      lineItem     : [{position: 30, importance: #HIGH  } ],
      identification  : [{position: 30}]     }
      @EndUserText.label: 'Tipo'
      @ObjectModel.text.element: ['desctipo']
      TIPO         : ztipopgto;

      @UI.hidden   : true
      desctipo     : char15;

      @UI          : {
      lineItem     : [{position: 40, importance: #HIGH  } ],
      identification  : [{position: 40}]     }
      @EndUserText.label: 'Referente'
      referente    : char2;

      @UI          : {
      lineItem     : [{position: 50 , importance: #HIGH } ],
      identification  : [{position: 50}]     }
      @EndUserText.label: 'Desc. Referente'
      descreferent : char20;

      @UI          : {
      lineItem     : [{position: 60 , importance: #HIGH } ],
      identification  : [{position: 60}]     }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['descmaterial']
      matnr        : matnr;

      @UI.hidden   : true
      descmaterial : maktx;

      @UI          : {
      lineItem     : [{position: 70, importance: #HIGH  } ],
      identification  : [{position: 70}]     }
      @EndUserText.label: 'Vencimento'
      DT_VENC      : char10;

      @UI.hidden   : true
      @UI          : {
      lineItem     : [{position: 80, importance: #HIGH  } ],
      identification  : [{position: 80}]     }
      @EndUserText.label: 'Moeda Pagto'
      MOEDA_PGTO   : waers;

      @UI          : {
      lineItem     : [{position: 90, importance: #HIGH  } ],
      identification  : [{position: 90}]     }
      @EndUserText.label: 'Total'
      @Semantics.amount.currencyCode : 'MOEDA_PGTO'
      TOTAL        : pad_vgbtr;

      _Item        : composition [1..*] of ZC_APROV_PAGTO_INV_IT;
      _Estra       : composition [1..*] of ZC_APROV_PAGTO_INV_EST;

}
