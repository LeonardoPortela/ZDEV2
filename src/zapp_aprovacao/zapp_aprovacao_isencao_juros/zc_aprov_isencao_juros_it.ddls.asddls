@EndUserText.label: 'Aprovação - Isenção de Juros - Item'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLFI_APP_APROV_ISENCAO_JRS_IT'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Isenção de Juros -  Item',
                typeNamePlural: 'Isenção de Juros -  Item',
                title: { type: #STANDARD, label: 'Isenção de Juros -  Item', value: 'org_vendas' } } }

define custom entity ZC_APROV_ISENCAO_JUROS_IT
{

      @UI.facet      : [
           {
             id      :       'id',
             purpose :  #STANDARD,
             type    :     #IDENTIFICATION_REFERENCE,
             label   :    'Lib. Isenção de Juros - Item',
             position: 10 }
         ]


      @UI            : {
      lineItem       : [{position: 30, importance: #HIGH }],
      identification : [{position: 30}]     }
      @EndUserText.label: 'Simulador Venda'
  key simul_venda    : zsded003;

      @UI            : {
      lineItem       : [{position: 32, importance: #HIGH }],
      identification : [{position: 32}]    }
      @EndUserText.label: 'OV Principal'
  key ov_principal   : vbeln;

      @UI            : {
      lineItem       : [{position: 40, importance: #HIGH }],
      identification : [{position: 40}]    }
      @EndUserText.label: 'Ordem Venda'
  key vbeln          : vbeln;

      @UI            : {
      lineItem       : [{position: 10, importance: #HIGH }],
      identification : [{position: 10}]     }
      @EndUserText.label: 'Org. Vendas'
      @ObjectModel.text.element: ['desc_orgvendas']
      org_vendas     : vkorg;

      @UI.hidden     : true
      desc_orgvendas : butxt;

      @UI            : {
      lineItem       : [{position: 20, importance: #HIGH }],
      identification : [{position: 20}]      }
      @EndUserText.label: 'Escr. Venda'
      @ObjectModel.text.element: ['desc_escvendas']
      escr_vendas    : vkbur;

      @UI.hidden     : true
      desc_escvendas : bezei;


      @UI            : {
      lineItem       : [{position: 50, importance: #HIGH }],
      identification : [{position: 50}]     }
      @EndUserText.label: 'Venc. Ov.'
      venc_ov        : datum;

      @UI            : {
      lineItem       : [{position: 60, importance: #HIGH }],
      identification : [{position: 60}]     }
      @EndUserText.label: 'Moeda'
      moeda          : waerk;

      @UI            : {
      lineItem       : [{position: 70, importance: #HIGH}],
      identification : [{position: 70}] }
      @EndUserText.label: 'Valor OV'
      @Semantics.amount.currencyCode : 'moeda'
      valor_ov       : netwr;

      @UI            : {
      lineItem       : [{position: 80, importance: #HIGH}],
      identification : [{position: 80}] }
      @EndUserText.label: 'VI Juros Calc'
      @Semantics.amount.currencyCode : 'moeda'
      vlr_juros_calc : netwr;

      @UI            : {
      lineItem       : [{position: 90, importance: #HIGH}],
      identification : [{position: 90}] }
      @EndUserText.label: 'VI Juros Receb'
      @Semantics.amount.currencyCode : 'moeda'
      vlr_juros_rbdo : netwr;

      @UI            : {
      lineItem       : [{position: 100, importance: #HIGH}],
      identification : [{position: 100}] }
      @EndUserText.label: 'VI Desc Juros'
      @Semantics.amount.currencyCode : 'moeda'
      vlr_desc_juros : netwr;

      @UI            : {
      lineItem       : [{position: 110, importance: #HIGH}],
      identification : [{position: 110}] }
      @EndUserText.label: 'Saldo Juros'
      @Semantics.amount.currencyCode : 'moeda'
      saldo_juros    : netwr;

      _header        : association to parent ZC_APROV_ISENCAO_JUROS on  _header.simul_venda  = $projection.simul_venda
                                                                    and _header.ov_principal = $projection.ov_principal;

}
