@EndUserText.label:  'ZEKPO_PEDIDO_ITEM_F4'
--@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@OData.publish: true
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EKPO_PEDIDO_ITEM_F4
--  with parameters p_ebeln : char0010,
--                  p_ebelp : numc05
  as select from ekpo
{
      @Search.defaultSearchElement : true
  key ebeln,
      @Search.defaultSearchElement : true
  key ebelp
}
where
  1 = 1
--and ebeln = $parameters.p_ebeln
