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

define view entity ZI_ZGL0001_02
  with parameters
    p_bukrs : bukrs,
    p_gjahr : gjahr,
    p_poper : poper
  as select distinct from acdoca as a

{
  @Search.defaultSearchElement : true
  a.rbukrs as Bukrs,
  a.matnr,
  a.racct,
  a.gjahr,
  a.poper
}
where
      1        = 1
  and a.rldnr  = '0L'
  and a.rbukrs = $parameters.p_bukrs
  and a.gjahr  = $parameters.p_gjahr
  and a.poper  = $parameters.p_poper
