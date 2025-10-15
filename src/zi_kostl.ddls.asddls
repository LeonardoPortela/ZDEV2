@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'F4 KOSTL'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_KOSTL 
  --with parameters
    --p_bukrs          : bukrs,
    --p_gsber : gsber

as select from csks as A
inner join cskt as B on A.kostl = B.kostl and A.kokrs = B.kokrs
{
key lpad( A.kostl,10,'0' ) as kostl,
key A.bukrs,
key A.gsber,
B.ktext
}
where A.kokrs= 'MAGI'
and A.datbi >= cast( substring( cast( tstmp_current_utctimestamp() as abap.char( 17 ) ),1,8) as abap.dats) /*HJ*/
and B.spras = 'P'
--and A.bukrs = $parameters.p_bukrs
--and A.gsber = $parameters.p_gsber
