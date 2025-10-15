@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Vencimento Fatura OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_DTVENCI_FAT_OV
  as select from bseg as _Item

    inner join   bkpf as _Cab on  _Cab.bukrs = _Item.bukrs
                              and _Cab.belnr = _Item.belnr
                              and _Cab.gjahr = _Item.gjahr

{
  _Cab.awkey  as Ref,
  _Item.netdt as DataVenc
}

where
  _Item.netdt is not initial
