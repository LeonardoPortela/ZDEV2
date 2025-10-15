@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ajuda Pesquisa Fornecedor'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SD_CLIENTE_VH as 
select from I_Customer

{
    
   key Customer,
       CustomerName
}
