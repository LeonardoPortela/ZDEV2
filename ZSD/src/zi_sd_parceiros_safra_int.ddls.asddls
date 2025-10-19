@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parceiros para envio - Safra'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_PARCEIROS_SAFRA_INT
  as select from I_Customer as Cli
    inner join   zsdt0418   as TV on TV.conta = Cli.CustomerAccountGroup

{

  key Cli.Customer                  as Parceiro,
      'C'                           as Tipo,
      TV.conta                      as GrpConta,
      Cli.CustomerName              as NomeParceiro,
      Cli.OrganizationBPName1       as NomeCompl,
      Cli.PostalCode                as Cep,
      Cli.StreetName                as Rua,
      Cli.CityName                  as Cidade,
      Cli.Region                    as Estado,
      Cli.SortField                 as NomeEnd,
      Cli.Country                   as Pais,
      case Cli.TaxNumber1
           when '' then 'fisica'
           else 'juridical' end     as Tipopessoa,
      case Cli.TaxNumber1
            when '' then Cli.TaxNumber2
            else Cli.TaxNumber1 end as identificacao


}

union select from I_Supplier as For
  inner join      zsdt0418   as TV on TV.conta = For.SupplierAccountGroup

{

  key For.Supplier                  as Parceiro,
      'F'                           as Tipo,
      TV.conta                      as GrpConta,
      For.SupplierName              as NomeParceiro,
      For.OrganizationBPName1       as NomeCompl,
      For.PostalCode                as Cep,
      For.StreetName                as Rua,
      For.CityName                  as Cidade,
      For.Region                    as Estado,
      For.SortField                 as NomeEnd,
      For.Country                   as Pais,
      case For.TaxNumber1
           when '' then 'fisica'
           else 'juridical' end     as Tipopessoa,
      case For.TaxNumber1
            when '' then For.TaxNumber2
            else For.TaxNumber1 end as identificacao

}
