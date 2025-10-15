@AbapCatalog.sqlViewName: 'ZVTEKBE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Controle de Viagens'
define view ZCDS_EKBE as select from ekbe
{
key ebeln,
key ebelp,      
    case shkzg
     when 'H' 
     then menge * -1
     when 'S'
     then menge
     end as valor
}

where vgabe = '1'
