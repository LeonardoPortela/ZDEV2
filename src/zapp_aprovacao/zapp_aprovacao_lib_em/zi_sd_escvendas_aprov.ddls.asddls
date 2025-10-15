@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Buscar Escritorio vendas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_ESCVENDAS_APROV
  as select from vbak  as _Venda

    inner join   t001w as _EscVenda on _Venda.vkbur = _EscVenda.werks

{
  key _Venda.vbeln     as vbeln,
      _Venda.vkbur    as EscVendas,
      _EscVenda.name1 as DescEscVendas
}
