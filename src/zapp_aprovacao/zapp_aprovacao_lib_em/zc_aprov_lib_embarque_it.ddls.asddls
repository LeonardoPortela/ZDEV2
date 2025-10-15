@EndUserText.label: 'Aprovação - Liberação Embarque -  Item'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIB_EMBARQUE_IT'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Liberação Embarque -  Item',
                typeNamePlural: 'Liberação Embarque -  Item',
                title: { type: #STANDARD, label: 'Liberação Embarque -  Item', value: 'orgvenda' } } }

define custom entity ZC_APROV_LIB_EMBARQUE_IT
{

      @UI.facet      : [
           {
             id      :       'id',
             purpose :  #STANDARD,
             type    :     #IDENTIFICATION_REFERENCE,
             label   :    'Lib. Embarque Insumo - Item',
             position: 10 }
         ]

      @UI            : {
      lineItem       : [{position: 10, importance: #HIGH }],
      identification : [{position: 10}],
      selectionField : [{position: 10}]     }
      @EndUserText.label: 'Org Vendas'
      @ObjectModel.text.element: ['desc_org_venda']
  key orgvenda       : bukrs;

      @UI            : {
      lineItem       : [{position: 30 , importance: #HIGH } ],
      identification : [{position: 30}]     }
      @EndUserText.label: 'Ordem de Venda'
  key vbeln          : vbeln;

      @UI            : {
      lineItem       : [{position: 20, importance: #HIGH } ],
      identification : [{position: 20}]     }
      @EndUserText.label: 'Escritorio Venda'
      @ObjectModel.text.element: ['desc_esc_venda']
  key esc_venda      : vkbur;

      @UI.hidden     : true
  key seq            : zsde_embarque_seq;

      @UI            : {
      lineItem       : [{position: 40, importance: #HIGH } ],
      identification : [{position: 40}]     }
      @EndUserText.label: 'Item'
  key posnr          : posnr;

      @UI.hidden     : true
      desc_org_venda : butxt;

      @UI.hidden     : true
      desc_esc_venda : name1;

      @UI            : {
      lineItem       : [{position: 50, importance: #HIGH } ],
      identification : [{position: 50}]     }
      @EndUserText.label: 'Moeda'
      waerk          : waerk;

      @UI            : {
      lineItem       : [{position: 60, importance: #HIGH } ],
      identification : [{position: 60}]     }
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['desc_material']
      matnr          : matnr;

      @UI.hidden     : true
      desc_material  : arktx;

      @UI            : {
      lineItem       : [{position: 70, importance: #HIGH } ],
      identification : [{position: 70}]     }
      @EndUserText.label: 'Marca'
      marca          : wrkst;

      @UI            : {
      lineItem       : [{position: 80, importance: #HIGH } ],
      identification : [{position: 80}]     }
      @EndUserText.label: 'UM'
      meins          : meins;

      @UI            : {
      lineItem       : [{position: 90, importance: #HIGH } ],
      identification : [{position: 90}]     }
      @EndUserText.label: 'Qtd OV'
      @Semantics.quantity.unitOfMeasure: 'MEINS'
      zmeng          : dzmeng;


      @UI            : {
      lineItem       : [{position: 100, importance: #HIGH } ],
      identification : [{position: 100}]     }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'moeda_1'
      valor          : netwr;

      @UI            : {
      lineItem       : [{position: 110, importance: #HIGH } ],
      identification : [{position: 110}]     }
      @EndUserText.label: 'Valor USD'
      @Semantics.amount.currencyCode : 'moeda_1'
      valor_usd      : netwr;

      @UI            : {
      lineItem       : [{position: 120, importance: #HIGH } ],
      identification : [{position: 120}]     }
      @EndUserText.label: 'Simulador Venda'
      doc_simulacao  : zsded003;

      @UI.hidden     : true
      moeda_1        : waerk;


      _header        : association to parent ZC_APROV_LIB_EMBARQUE on  _header.orgvenda  = $projection.orgvenda
                                                                   and _header.vbeln     = $projection.vbeln
                                                                   and _header.esc_venda = $projection.esc_venda
                                                                   and _header.SEQ       = $projection.seq;
}
