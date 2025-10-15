@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pega lista Materiais'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_ZGL0001_03
//with parameters
//p_bukrs : bukrs
//    p_gjahr : gjahr,
//    p_poper : poper
  as select from acdoca as a

{
      @Search.defaultSearchElement : true
  key rldnr                            as Rldnr,
  key rbukrs                           as bukrs,
  key gjahr                            as Gjahr,
  key belnr                            as Belnr,
      --key docln as Docln,
      rassc,
      cast( hsl as abap.dec( 15, 2 ) ) as hsl,
      cast( tsl as abap.dec( 15, 2 ) ) as tsl,
      racct,
      ktosl,
      kunnr,
      koart
}
where
      1        = 1
  and a.rldnr  = '0L'
//and a.rbukrs = $parameters.p_bukrs
//  and a.gjahr  = $parameters.p_gjahr
//  and a.poper  = $parameters.p_poper
