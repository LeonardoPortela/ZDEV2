@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agrupa e soma valores acdoca'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZI_ZGL0001_01
  with parameters
    p_racct : racct,
    p_matnr : matnr,
    p_bukrs : bukrs,
    p_gjahr : gjahr,
    p_poper : poper
  as select from acdoca as a

{
  @Search.defaultSearchElement : true
  --a.rbukrs,
  --a.matnr,
  --a.racct,
  --rassc,
  --gjahr,
  --poper,
  sum( cast( a.ksl as abap.dec( 15, 2 ) ) ) as ksl,
  sum( cast( a.tsl as abap.dec( 15, 2 ) ) ) as tsl,
  sum( cast( a.msl as abap.dec( 15, 2 ) ) ) as msl
}
where
      1        = 1
  and a.matnr  = $parameters.p_matnr
  and a.racct  = $parameters.p_racct
  and a.rldnr  = '0L'
  and a.rbukrs = $parameters.p_bukrs
  and a.gjahr  = $parameters.p_gjahr
  and a.poper  = $parameters.p_poper
group by
  a.matnr,
  a.racct
