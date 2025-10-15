@EndUserText.label: 'Aprovação - Ordem Venda'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIB_OV'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Lib. Solicitação Ord. Venda',
                typeNamePlural: 'Aprov. Lib. Solicitação Ord. Venda',
                title: { type: #STANDARD, label: 'Aprov. Lib. Solicitação Ord. Venda', value: 'nrosolov' } } }
define root custom entity ZC_APROV_PAGTO_OV
{
      @UI.facet      : [
                {
                  id :       'id',
                  purpose :  #STANDARD,
                  type    :     #IDENTIFICATION_REFERENCE,
                  label   :    'Ordens',
                  position: 10 },

                  {
                       label: 'Itens',
                       id : 'items',
                       type:  #LINEITEM_REFERENCE,
                       position:20,
                       targetElement: '_Item'
                   },

                  {
                       label: 'Estratégia de aprovação',
                       id : 'estr',
                       type:  #LINEITEM_REFERENCE,
                       position:20,
                       targetElement: '_Estr'
                   }

                  ]

      @UI            : {
      lineItem       : [{position: 10, importance: #HIGH }],
      identification : [{position: 10}],
      selectionField : [{position: 10}]     }
      @EndUserText.label: 'N. Solicitação'
  key NROSOLOV       : zsded013;

      @UI            : {
      lineItem       : [{position: 20, importance: #HIGH }],
      identification : [{position: 20}] }
      @EndUserText.label: 'Tipo Venda'
      TP_VENDA       : zsded012;

      @UI            : {
      lineItem       : [{position: 30, importance: #HIGH }],
      identification : [{position: 30}] }
      @EndUserText.label: 'Org. Venda'
      @ObjectModel.text.element: ['desc_org_venda']
      org_venda      : vkorg;

      @UI.hidden     : true
      desc_org_venda : vtext;

      @UI            : {
      lineItem       : [{position: 40, importance: #HIGH }],
      identification : [{position: 40}]  }
      @EndUserText.label: 'Esc. Vendas'
      @ObjectModel.text.element: ['desc_esc_venda']
      esc_vendas     : vkbur;

      @UI.hidden     : true
      desc_esc_venda : bezei;

      @UI            : {
      lineItem       : [{position: 50, importance: #HIGH }],
      identification : [{position: 50}] }
      @EndUserText.label: 'Moeda'
      MOEDA          : waerk;

      @UI            : {
      lineItem       : [{position: 60, importance: #HIGH }],
      identification : [{position: 60}] }
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['desc_cliente']
      cliente        : kunag;

      @UI.hidden     : true
      desc_cliente   : name1_gp;

      @UI            : {
      lineItem       : [{position: 70, importance: #HIGH }],
      identification : [{position: 70}] }
      @EndUserText.label: 'Cond. Pagto'
      COND_PGTO      : dzterm; //não tem tabela de referencia

      @UI            : {
      lineItem       : [{position: 80, importance: #HIGH }],
      identification : [{position: 80}] }
      @EndUserText.label: 'Valor'
      @Semantics.amount.currencyCode : 'waerk'
      VALOR          : dmbtr;

      @UI            : {
      lineItem       : [{position: 90, importance: #HIGH }],
      identification : [{position: 90}] }
      @EndUserText.label: 'Observação'
      OBSERVACOES    : char255;

      @UI            : {
      lineItem       : [{position: 100, importance: #HIGH }],
      identification : [{position: 1000}] }
      @EndUserText.label: 'Usuario'
      USUARIO        : usnam;

      waerk          : waerk;

      _Item          : composition [1..*] of ZC_APROV_PAGTO_OV_IT;
      _Estr          : composition [1..*] of ZC_APROV_PAGTO_OV_EST;
}
