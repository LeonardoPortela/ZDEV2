
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Safra -3  ano atual +3'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_HELP_SAFRA 
as select distinct from I_YearMonth
{
    key CalendarYear
}
where cast( CalendarYear as abap.int4 ) >= cast( substring( cast( $session.system_date as abap.char(8) ), 1, 4 ) as abap.int4 ) - 3 and cast( CalendarYear as abap.int4 ) <= cast( substring( cast( $session.system_date as abap.char(8) ), 1, 4 ) as abap.int4 ) + 3
