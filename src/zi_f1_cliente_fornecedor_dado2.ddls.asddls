@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MATERIAL E GRUPO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_F1_CLIENTE_FORNECEDOR_DADO2
 as     select from kna1 as Cliente
{
    key Cliente.kunnr,
        Cliente.name1,
        Cliente.stcd1 as zstcd1,
        Cliente.stcd2 as zstcd2,
        Cliente.stcd3 as zstcd3
}
