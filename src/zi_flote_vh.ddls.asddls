@AbapCatalog.sqlViewName: 'ZIFLOTEVH'
@AbapCatalog.compiler.compareFilter: true

@VDM.viewType: #BASIC
@ObjectModel.dataCategory: #VALUE_HELP

@ObjectModel.supportedCapabilities: [#SQL_DATA_SOURCE,
                                     #CDS_MODELING_DATA_SOURCE,
                                     #CDS_MODELING_ASSOCIATION_TARGET,
                                     #VALUE_HELP_PROVIDER,
                                     #SEARCHABLE_ENTITY]
@Search.searchable: true

@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda de Pesquisa Formação de Lote'
@Metadata.ignorePropagatedAnnotations: true

define view zi_flote_vh
  as select from ZISD_FORMACAO_LOTE as _flote
{

      @ObjectModel.text.element: ['Docnum']
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
  key _flote.docnum,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _flote.werks,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _flote.werksReal,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _flote.material,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _flote.grupoMercadoria
}
