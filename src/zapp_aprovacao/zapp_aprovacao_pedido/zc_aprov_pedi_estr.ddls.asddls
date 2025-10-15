@EndUserText.label: 'Aprovação pedidos - Estrategia'

@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCLMM_APP_APROV_PED_ESTR'
    }
}

@UI: {
  headerInfo: { typeName: 'Aprov. Pedidos',
                typeNamePlural: 'Aprovadores',
                title: { type: #STANDARD, label: 'Aprov Itens Pedidos', value: 'ebeln' } } }
define custom entity ZC_APROV_PEDI_ESTR
{

      @UI.hidden : true
  key sww_wiid   : sww_wiid;
      @UI.hidden : true
  key ebeln      : ebeln;

      @UI        : {
        lineItem : [{position: 10, importance: #HIGH }] }
      @EndUserText.label: 'Estratégia'
      @ObjectModel.text.element: ['frgxt']
  key frgsx      : frgsx;

      @UI        : {
        lineItem : [{position: 20, importance: #HIGH }] }
      @EndUserText.label: 'Nível'
  key nivel      : char10;

      @UI.hidden : true
      frgxt      : frgxt;

      @UI        : {
        lineItem : [{position: 30, importance: #HIGH }] }
      @EndUserText.label: 'Aprovador'
      @ObjectModel.text.element: ['nome_aprov']
      aprovador  : usnam;

      @UI.hidden : true
      nome_aprov : text80;

      @UI        : {
        lineItem : [{position: 40, importance: #HIGH , criticality: 'cor' }] }
      @EndUserText.label: 'Estado'
      estado     : char30;

      @UI.hidden : true
      cor        : char1;

      @UI        : {
        lineItem : [{position: 50, importance: #HIGH }] }
      @EndUserText.label: 'Data Aprovação'
      data_aprov : datum;

      @UI        : {
        lineItem : [{position: 60, importance: #HIGH }] }
      @EndUserText.label: 'Usuário aprovação'
      @ObjectModel.text.element: ['nome_user']
      usuario    : usnam;

      @UI.hidden : true
      nome_user  : text80;

      _header    : association to parent ZC_APROV_PEDIDOS on  _header.sww_wiid = $projection.sww_wiid
                                                          and _header.ebeln    = $projection.ebeln;


}
