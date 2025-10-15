@AbapCatalog.sqlViewName: 'ZIMATERIALPRECO'
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
@EndUserText.label: 'Ajuda de Pesquisa Preço do Material'
@Metadata.ignorePropagatedAnnotations: true

define view ZI_MATERIAL_PRECO_VH
  as select from    zsdt0036 as z036
    left outer join makt     as _m on  _m.matnr = z036.matnr
                                   and _m.spras = $session.system_language
{
  key z036.matnr        as matnr,
  key z036.waerk        as waerk,
  key z036.inco1        as inco1,
  key z036.safra        as safra,
  key z036.cultura      as cultura,
  key z036.werks_fornec as werks,
      _m.maktg          as maktg,
      z036.meins        as meins,
      z036.vlr_venda    as vlr_venda
}
where z036.val_de  <= $session.system_date
  and z036.val_ate >= $session.system_date
  and z036.eliminado <> 'X'
