@EndUserText.label: 'Aprovação - Lib. Embarque Insumo'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_APROV_LIB_EMBARQUE'
    }
}


@UI: {
  headerInfo: { typeName: 'Aprov. Lib. Embarque Insumo',
                typeNamePlural: 'Lib. Embarque Insumo',
                title: { type: #STANDARD, label: 'Lib. Embarque Insumo', value: 'orgvenda' } } }

define root custom entity ZC_APROV_LIB_EMBARQUE
{
      @UI.facet      : [
           {
             id      :       'id',
             purpose :  #STANDARD,
             type    :     #IDENTIFICATION_REFERENCE,
             label   :    'Ordens',
             position: 10 },

                   {
                        label: 'Items',
                        id : 'items',
                        type:  #LINEITEM_REFERENCE,
                        position:20,
                        targetElement: '_Item'
                    },

                   {
                        label: 'Estratégia',
                        id : 'estr',
                        type:  #LINEITEM_REFERENCE,
                        position:30,
                        targetElement: '_Estr'
                    }
         ]
      @UI            : {
      lineItem       : [{position: 10, importance: #HIGH }],
      identification : [{position: 10}],
      selectionField : [{position: 10}]     }
      @EndUserText.label: 'Org Vendas'
      @ObjectModel.text.element: ['desc_org_venda']
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
  key orgvenda       : bukrs;

      @UI            : {
      lineItem       : [{position: 30, importance: #HIGH}],
      identification : [{position: 30}],
      selectionField : [{position: 20}]     }
      @EndUserText.label: 'Ordem de Venda'
  key vbeln          : zlote_imp;

      @UI            : {
      lineItem       : [{position: 20, importance: #HIGH}],
      identification : [{position: 20}],
      selectionField : [{position: 10}]     }
      @EndUserText.label: 'Escritorio Vendas'
      @ObjectModel.text.element: ['desc_esc_venda']
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZI_SD_ESC_VENDAS_VH', element: 'Vkbur' }}]
  key esc_venda      : vkbur;

      @UI            : {
      lineItem       : [{position: 22, importance: #HIGH}],
      identification : [{position: 22}],
      selectionField : [{position: 10}]     }
      @EndUserText.label: 'Sequencia'
  key seq            : zsde_embarque_seq;

      @UI.hidden     : true
      desc_org_venda : butxt;

      @UI.hidden     : true
      desc_esc_venda : name1;

      @UI            : {
      lineItem       : [{position: 40, importance: #HIGH}],
      identification : [{position: 40}] }
      @EndUserText.label: 'Moeda'
      waerk          : waerk;

      @UI            : {
      lineItem       : [{position: 50, importance: #HIGH}],
      identification : [{position: 50}] }
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['desc_cliente']
      cliente        : kunnr;

      @UI.hidden     : true
      desc_cliente   : name1_gp;

      @UI            : {
       lineItem      : [{position: 60, importance: #HIGH}],
       identification: [{position: 60}] }
      @EndUserText.label: 'Vlr.Moeda.Aprov'
      @Semantics.amount.currencyCode : 'waerk'
      valor          : netwr;

      @UI            : {
      lineItem       : [{position: 62, importance: #HIGH}],
      identification : [{position: 62}] }
      @EndUserText.label: 'Vlr.Acum.USD'
      @Semantics.amount.currencyCode : 'waerk'
      VLR_ACUMULADO  : netwr;

      @UI            : {
       lineItem      : [{position: 64, importance: #HIGH}],
       identification: [{position: 64}] }
      @EndUserText.label: 'Vlr.Acum.SAP'
      @Semantics.amount.currencyCode : 'waerk'
      VLR_AC_SAP     : netwr;

      @UI            : {
      lineItem       : [{position: 66, importance: #HIGH}],
      identification : [{position: 66}] }
      @EndUserText.label: 'Vlr.Acum.OPUS'
      @Semantics.amount.currencyCode : 'waerk'
      VLR_AC_OPUS    : netwr;

      @UI            : {
      lineItem       : [{position: 70, importance: #HIGH}],
      identification : [{position: 70}] }
      @EndUserText.label: 'Justificativa'
      just_workflow  : char255;

      _Item          : composition [1..*] of ZC_APROV_LIB_EMBARQUE_IT;
      _Estr          : composition [1..*] of ZC_APROV_LIB_EMB_EST;


}
