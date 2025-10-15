@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações de receb pedidos de compras'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZCDS_SOLIC_RECEB_PEDIDOS_COMP
  as select from I_PurchaseOrder     as pedido
    inner join   I_PurchaseOrderItem as item on pedido.PurchaseOrder = item.PurchaseOrder
  association [0..1] to lfa1 as fornecedor on  pedido.Supplier = fornecedor.lifnr
  association [0..1] to mara as material   on  item.Material = material.matnr
  association [0..1] to makt as desc_mat   on  item.Material = desc_mat.matnr
  association [0..*] to ZCDS_EKBE as ekbe       on  ekbe.ebeln = item.PurchaseOrder
                                           and ekbe.ebelp = item.PurchaseOrderItem

{
  key pedido.PurchaseOrder              as ebeln,
  key item.PurchaseOrderItem            as ebelp,
      pedido.Supplier                   as lifnr,
      fornecedor.name1                  as name1,
      pedido.CorrespncInternalReference as unsez,
      pedido.PurchasingOrganization     as ekorg,
      pedido.PurchasingGroup            as ekgrp,
      material.matkl                    as matkl,
      item.Material                     as matnr,
      desc_mat.maktx                    as maktx,
      item.Plant                        as werks,
      pedido.IncotermsClassification      as inco1,
      @Semantics.quantity.unitOfMeasure: 'meins'
      item.OrderQuantity                as menge,
      item.PurchaseOrderQuantityUnit    as meins,
      @Semantics.amount.currencyCode: 'waers'
      item.NetAmount                    as netwr,
      pedido.DocumentCurrency           as waers,
      pedido.CreationDate               as aedat,
      pedido.PurchasingProcessingStatus as procstat,
      pedido.PurchaseOrderType          as bsart,
      item.CompanyCode                  as bukrs,
      @Semantics.quantity.unitOfMeasure: 'meins'
      @DefaultAggregation: #SUM
      sum(ekbe.valor)                   as qtde_fat_pedido,
      @Semantics.quantity.unitOfMeasure: 'meins'
      cast( 0 as abap.quan(13,3) )      as Qtde_Solicitacao,
      @Semantics.quantity.unitOfMeasure: 'meins'
      cast( 0 as abap.quan(13,3) )      as Saldo_a_Solicitar,
      @Semantics.quantity.unitOfMeasure: 'meins'
      cast( 0 as abap.quan(13,3) )      as Qtde_Vinc_Carga

}

where
  desc_mat.spras = 'P'
  
  group by pedido.PurchaseOrder,
           item.PurchaseOrderItem,
           pedido.Supplier,
           fornecedor.name1,
           pedido.CorrespncInternalReference,
           pedido.PurchasingOrganization,
           pedido.PurchasingGroup,
           material.matkl,
           item.Material,
           desc_mat.maktx,
           item.Plant,
           pedido.IncotermsClassification,
           item.OrderQuantity ,
           item.NetAmount,
           item.PurchaseOrderQuantityUnit,
           pedido.DocumentCurrency ,
           pedido.CreationDate,
           pedido.PurchasingProcessingStatus,
           pedido.PurchaseOrderType ,
           item.CompanyCode
