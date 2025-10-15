@EndUserText.label:  'ZI_SALDO_CT_AUM_MOEDA_P_ABERTO'
--@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable : true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SALDO_CT_ACUM_MOEDA_ABERTO2
  with parameters
    p_bukrs   : bukrs,
    p_dt_lanc : budat,
    p_RACCT   : racct,
    p_waers   : waers
  as select from acdoca as a
{
      @Search.defaultSearchElement : true
  key a.racct,
      a.rwcur,
      @Semantics.amount.currencyCode: 'rwcur'
      sum(a.tsl) as tsl,
      @Semantics.amount.currencyCode: 'rwcur'
      sum(a.ksl) as ksl,
      @Semantics.amount.currencyCode: 'rwcur'
      sum(a.osl) as osl
}
where
      1                        =  1
  and a.rldnr                  =  '0L'
  and a.rbukrs                 = $parameters.p_bukrs
  and a.budat                  <= $parameters.p_dt_lanc
  and a.racct                  = $parameters.p_RACCT
  and a.augbl                  =  ''
  and a.rwcur                  = $parameters.p_waers
  and substring( a.belnr,1,1 ) <> 'B'
  and substring( a.belnr,1,2 ) <> 'DG'
group by
  racct,
  rwcur

