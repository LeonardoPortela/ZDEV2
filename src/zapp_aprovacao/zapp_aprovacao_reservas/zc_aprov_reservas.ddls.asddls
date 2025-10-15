@EndUserText.label: 'Aprovação - Reservas'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_RESERVAS'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Reservas',
                typeNamePlural: 'Reservas',
                title: { type: #STANDARD, label: 'Lanc Manual', value: 'rsnum' } } }

define root custom entity ZC_APROV_RESERVAS
{

      @UI.facet : [
          {
            id  :       'id',
            purpose:  #STANDARD,
            type:     #IDENTIFICATION_REFERENCE,
            label :    'Reservas',
            position  : 10 }
        ]


      @UI       : {
      lineItem  : [{position: 10, importance: #HIGH } ],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Reserva'
  key rsnum     : rsnum;

      @UI       : {
      lineItem  : [{position: 20, importance: #HIGH } ],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]     }
      @EndUserText.label: 'Item'
  key rspos     : rspos;

      @UI       : {
      lineItem  : [{position: 30, importance: #HIGH } ],
      identification  : [{position: 30}],
      selectionField  : [{position: 30}]     }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['maktx']
      matnr     : matnr;


      @UI       : {
      lineItem  : [{position: 40, importance: #HIGH } ],
      identification  : [{position: 40}]   }
      @EndUserText.label: 'Data necessidade'
      bdter     : bdter;

      @UI       : {
      lineItem  : [{position: 50, importance: #HIGH } ],
      identification  : [{position: 50}]   }
      @EndUserText.label: 'Nome usuário'
      usnam     : usnam;

      @UI       : {
      lineItem  : [{position: 60, importance: #HIGH } ],
      identification  : [{position: 60}]   }
      @EndUserText.label: 'Centro'
      werks     : werks_d;

      @UI       : {
      lineItem  : [{position: 70, importance: #HIGH } ],
      identification  : [{position: 70}]   }
      @EndUserText.label: 'Quantidade'
      @Semantics.quantity.unitOfMeasure : 'erfme'
      erfmg     : erfmg;

      @UI       : {
      lineItem  : [{position: 80, importance: #HIGH } ],
      identification  : [{position: 80}]   }
      @EndUserText.label: 'UM'
      erfme     : erfme;

      @UI       : {
      lineItem  : [{position: 90, importance: #HIGH } ],
      identification  : [{position: 90}]   }
      @EndUserText.label: 'Tipo Mov.'
      bwart     : bwart;

      @UI       : {
      lineItem  : [{position: 100, importance: #HIGH } ],
      identification  : [{position: 100}]   }
      @EndUserText.label: 'Mov. Mercadoria'
      xwaok     : xwaok;

      @UI       : {
      lineItem  : [{position: 110, importance: #HIGH } ],
      identification  : [{position: 110}]   }
      @EndUserText.label: 'Centro de Custo'
      kostl     : kostl;

      @UI       : {
      lineItem  : [{position: 120, importance: #HIGH } ],
      identification  : [{position: 120}]   }
      @EndUserText.label: 'Nº conta'
      @ObjectModel.text.element: ['txt20']
      saknr     : saknr;

      @UI.hidden: true
      @EndUserText.label: 'Desc. Conta'
      txt20     : txt20;

      @UI       : {
      lineItem  : [{position: 140, importance: #HIGH } ],
      identification  : [{position: 140}]   }
      @EndUserText.label: 'Depósito'
      lgort     : lgort_d;

      @UI       : {
      lineItem  : [{position: 150, importance: #HIGH } ],
      identification  : [{position: 150}]   }
      @EndUserText.label: 'Texto do item'
      sgtxt     : sgtxt;

      @UI       : {
      lineItem  : [{position: 150, importance: #HIGH } ],
      identification  : [{position: 150}]   }
      @EndUserText.label: 'Grupo de Mercadoria'
      @ObjectModel.text.element: ['wgbez']
      matkl     : matkl;

      @UI       : {
      lineItem  : [{position: 160, importance: #HIGH } ],
      identification  : [{position: 160}]   }
      @EndUserText.label: 'Recebedor da Mercadoria'
      wempf     : wempf;

      @UI       : {
      lineItem  : [{position: 170, importance: #HIGH } ],
      identification  : [{position: 170}]   }
      @EndUserText.label: 'Nome'
      name1     : name1;

      @UI       : {
      lineItem  : [{position: 180, importance: #HIGH } ],
      identification  : [{position: 180}]   }
      @EndUserText.label: 'Descrição'
      ltext     : ltext;

      @UI.hidden: true
      @EndUserText.label: 'Desc. Grp Mercadoria'
      wgbez     : wgbez;

      @UI       : {
      lineItem  : [{position: 200 , importance: #HIGH } ],
      identification  : [{position: 200}]   }
      @EndUserText.label: 'Alias usuário da Internet'
      useralias : usalias;

      @UI       : {
      lineItem  : [{position: 210, importance: #HIGH } ],
      identification  : [{position: 210}]   }
      @EndUserText.label: 'Moeda'
      moeda     : waers;

      @UI       : {
      lineItem  : [{position: 220, importance: #HIGH } ],
      identification  : [{position: 220}]   }
      @EndUserText.label: 'Preço móvel/preço interno per.'
      @Semantics.amount.currencyCode : 'moeda'
      verpr     : hslxx12;

      @UI.hidden: true
      @EndUserText.label: 'Texto breve de material'
      maktx     : maktx;

      @UI.hidden: true
      conta     : char40;
}
