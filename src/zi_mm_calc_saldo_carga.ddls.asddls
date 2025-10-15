@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Verifica o saldo de cada item da carga'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_CALC_SALDO_CARGA as select from ZI_MM_TOTAL_ITENS_CARGA as item
inner join ZI_MM_CONFERE_CARGA2 as zib  on item.ebeln = zib.ebeln
                                       and item.ebelp = zib.ebelp
                                       and item.chave_nfe = zib.chave_nfe
{
    key item.nro_cg,
    key zib.chave_nfe,
    key zib.ebeln,
    key zib.ebelp,
        zib.meins,
        @Semantics.quantity.unitOfMeasure: 'meins'
        zib.total as total_zib,
        @Semantics.quantity.unitOfMeasure: 'meins'
        item.total as total_item
}
