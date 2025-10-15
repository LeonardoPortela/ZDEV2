@EndUserText.label: 'Liberação de Lote de Pagamento Impostos'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_APP_LIB_PAGTO_IMPOSTO'
    }
}


@UI: {
  headerInfo: { typeName: 'Liberação de Lote de Pagamento Impostos',
                typeNamePlural: 'Liberação de Lote de Pagamento Impostos',
                title: { type: #STANDARD, label: 'Liberação de Lote de Pagamento Impostos', value: 'lote' } } }

define root custom entity ZC_LIB_PAGTO_IMPOSTO
{
      @UI.facet       : [
                    {
                      id:       'id',
                      purpose:  #STANDARD,
                      type  :     #IDENTIFICATION_REFERENCE,
                      label :    'Liberação de Lote de Pagamento Impostos',
                      position  : 10 }
                  ]
      @UI             : { selectionField  : [{position: 10}] }
      @Consumption.filter:{ mandatory:true }
      @EndUserText.label: 'Lote'
  key lote            : zlote_imp;

      @UI             : { selectionField  : [{position: 20}]  }
      @Consumption.filter:{ mandatory:true }
      @EndUserText.label: 'Empresa'
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_COMPANYCODEVH', element: 'CompanyCode' }}]
      @ObjectModel.text.element: ['descEmpresa']
  key bukrs           : bukrs;

      @UI             : {
         lineItem     : [{position: 30, importance: #HIGH }],
         identification  : [{position: 30}] }
      @EndUserText.label: 'Doc.Imp'
  key doc_imposto     : zdoc_imposto;

      @UI             : {
       lineItem       : [{position: 40, importance: #HIGH }],
       identification : [{position: 40}] }
      @EndUserText.label: 'Cod.Imp'
      @ObjectModel.text.element: ['descr_imposto']
  key cod_imposto     : zcod_imposto;

      @UI.hidden      : true
      DescEmpresa     : butxt;

      @UI.hidden      : true
      descr_imposto   : zdescr_imposto;

      @UI             : {
      lineItem        : [{position: 60, importance: #HIGH }],
      identification  : [{position: 60}] }
      @EndUserText.label: 'Tp.Imposto'
      @ObjectModel.text.element: ['desc_tp_imposto']
      TP_IMPOSTO      : ztp_imposto;

      @UI.hidden      : true
      desc_tp_imposto : sgtxt;

      @UI             : {
      lineItem        : [{position: 70, importance: #HIGH }],
      identification  : [{position: 70}] }
      @EndUserText.label: 'Cond.Pgto'
      COD_PGTO        : zcod_pgto;

      @UI             : {
      lineItem        : [{position: 80, importance: #HIGH }],
      identification  : [{position: 80}] }
      @EndUserText.label: 'Conv.Pgto'
      CONV_BANCO      : zconv_banco;

      @UI             : {
      lineItem        : [{position: 90, importance: #HIGH }],
      identification  : [{position: 90}] }
      @EndUserText.label: 'Bcp.Empresa'
      @ObjectModel.text.element: ['desc_banco']
      HBKID           : hbkid;

      @UI.hidden      : true
      desc_banco      : text1;

      @UI             : {
      lineItem        : [{position: 100, importance: #HIGH }],
      identification  : [{position: 100}] }
      @EndUserText.label: 'Per.Apuração'
      DT_APURACAO     : dats;

      @UI             : {
      lineItem        : [{position: 110, importance: #HIGH }],
      identification  : [{position: 110}] }
      @EndUserText.label: 'Mês/ano Apuração'
      @ObjectModel.text.element: ['ANO_APURACAO']
      MES_APURACAO    : monat;

      @UI.hidden      : true
      ANO_APURACAO    : gjahr;

      @UI             : {
      lineItem        : [{position: 120, importance: #HIGH }],
      identification  : [{position: 120}] }
      @EndUserText.label: 'Dt.Vencimento'
      DT_VENC         : zdt_venc;

      @UI             : {
      lineItem        : [{position: 130, importance: #HIGH }],
      identification  : [{position: 130}] }
      @EndUserText.label: 'Fornecedor'
      @ObjectModel.text.element: ['DESC_LIFNR']
      LIFNR           : lifnr;

      @UI.hidden      : true
      DESC_LIFNR      : name1;

      @UI             : {
      lineItem        : [{position: 140, importance: #HIGH }],
      identification  : [{position: 140}] }
      @EndUserText.label: 'Tip.Bloq'
      zahls           : dzahls;

      @UI.hidden      : true
      moeda           : waers;

      @UI             : {
      lineItem        : [{position: 150, importance: #HIGH }],
      identification  : [{position: 150}] }
      @EndUserText.label: 'Valor Total'
      @Semantics.amount.currencyCode : 'moeda'
      VALOR_TOTAL     : zvalor_imp;

      @UI             : {
       lineItem       : [{position: 170, importance: #HIGH }],
       identification : [{position: 170}] }
      @EndUserText.label: 'Mês/Ano'
      mes_ano         : char15;

}
