@AbapCatalog.sqlViewName: 'ZIDOCNUMVH'
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

define view zi_DOCNUM_vh
  as select from I_BR_NFDocument as _nota
    inner join   I_BR_NFItem     as _NFItem on _nota.BR_NotaFiscal = _NFItem.BR_NotaFiscal
    inner join   zicfop          as _cfop   on _NFItem.BR_CFOPCode = _cfop.cfop

{
      @ObjectModel.text.element: ['Docnum']
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
  key _nota.BR_NotaFiscal  as docnum,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _nota.CompanyCode    as bukrs,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _nota.BusinessPlace  as branch,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _nota.BR_NFIssueDate as docdat,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _nota.BR_NFDirection as direct,

      @UI.hidden: true
      @Consumption.filter.hidden: true
      _nota.BR_NFModel     as model
      
}
where
  _nota.BR_NFModel = '55'
