@EndUserText.label: 'Aprovação - Requisição de Compra'
@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_REQUISICAO'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Requisição',
                typeNamePlural: 'Requisição',
                title: { type: #STANDARD, label: 'Aprov Requisição', value: 'ebeln' } } }

@UI.presentationVariant: [{ sortOrder: [{ by:'ebeln', direction:#ASC },
                                         { by:'bnfpo', direction: #ASC }   ] }]
define root custom entity ZC_APROV_REQUISICAO
{

      @UI.facet    : [
              {
                id : 'id',
                purpose: #STANDARD,
                type   : #IDENTIFICATION_REFERENCE,
                label  : 'Aprov. Requisição',
                position : 10 }
            ]

      @UI.hidden   : true
  key sww_wiid     : sww_wiid;

      @UI          : {
      lineItem     : [{position: 10, importance: #HIGH }],
      identification  : [{position: 10}],
      selectionField  : [{position: 10}]     }
      @EndUserText.label: 'Requisição'
  key ebeln        : ebeln;

      @UI          : {
      lineItem     : [{position: 20, importance: #HIGH }],
      identification  : [{position: 20}],
      selectionField  : [{position: 20}]      }
      @EndUserText.label: 'Item'
  key bnfpo        : bnfpo;

      @UI          : {
      lineItem     : [{position: 30, importance: #HIGH}],
      identification  : [{position: 30}] }
      @EndUserText.label: 'Tipo Doc.'
      @ObjectModel.text.element: ['desc_bsart']
      bsart        : bsart;

      @UI.hidden   : true
      desc_bsart   : batxt;

      @UI          : {
      lineItem     : [{position: 30, importance: #HIGH}],
      identification  : [{position: 30}] }
      @EndUserText.label: 'Data de Criação'
      aedat        : aedat;

      @UI          : {
      lineItem     : [{position: 50, importance: #HIGH}],
      identification  : [{position: 50}] }
      @EndUserText.label: 'Observação'
      texto        : char300;

      @UI          : {
       lineItem    : [{position: 20, importance: #HIGH }],
       identification  : [{position: 20}]     }
      @EndUserText.label: 'Empresa'
      @ObjectModel.text.element: ['descempresa']
      bukrs        : bukrs;

      @UI.hidden   : true
      descempresa  : butxt;

      @UI          : {
       lineItem    : [{position: 30, importance: #HIGH }],
       identification  : [{position: 30}]     }
      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: ['desc_centro']
      werks        : werks_d;

      @UI.hidden   : true
      desc_centro  : name1;

      @UI          : {
       lineItem    : [{position: 32, importance: #HIGH }],
       identification  : [{position: 32}]     }
      @EndUserText.label: 'Centro de Custo'
      kostl        : kostl;

      @UI          : {
       lineItem    : [{position: 40, importance: #HIGH }],
       identification  : [{position: 40}]     }
      @EndUserText.label: 'Data Remessa'
      data_remessa : dats;


      @UI.hidden   : true
      meins        : bamei;

      @Semantics.quantity.unitOfMeasure : 'meins'
      @UI          : {
       lineItem    : [{position: 50, importance: #HIGH }],
       identification  : [{position: 50}]     }
      @EndUserText.label: 'Quantidade'
      menge        : bamng;


      @UI          : {
       lineItem    : [{position: 60, importance: #HIGH }],
       identification  : [{position: 60}]     }
      @EndUserText.label: 'Grupo de Compradores'
      @ObjectModel.text.element: ['desc_grpcomp']
      ekgrp        : ekgrp;

      @UI.hidden   : true
      desc_grpcomp : eknam;

      @UI          : {
      lineItem     : [{position: 70, importance: #HIGH }],
      identification  : [{position: 70}]     }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['texto_item']
      matnr        : matnr;

      @UI.hidden   : true
      texto_item   : txz01;

      @UI          : {
       lineItem    : [{position: 80, importance: #HIGH }],
       identification  : [{position: 80}]     }
      @EndUserText.label: 'Grupo de Material'
      matkl        : matkl;

      @UI.hidden   : true
      desc_grpmat  : wgbez60;

      @UI          : {
       lineItem    : [{position: 90, importance: #HIGH }],
       identification  : [{position: 90}]     }
      @EndUserText.label: 'Requisitante'
      requisitante : afnam;

      @UI.hidden   : true
      waers        : waers;

      @UI          : {
       lineItem    : [{position: 100, importance: #HIGH }],
       identification  : [{position: 100}]     }
      @EndUserText.label: 'Valor Total'
      @Semantics.amount.currencyCode : 'waers'
      netpr        : bbwert;

      @UI          : {
       lineItem    : [{position: 110, importance: #HIGH }],
       identification  : [{position: 110}]     }
      @EndUserText.label: 'Item Eliminado'
      loekz        : eloek;


}
