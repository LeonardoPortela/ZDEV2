@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações - Filhas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_REL_SOLICITACAO_FILHAS
  as select from ZI_SD_DADOS_SOLICITACAO

  association to parent ZI_SD_REL_SOLICITACAO as Sol on $projection.NroSol = Sol.NroSol

{
  key NroSol,
  key NroSolOrigem,
      Vbeln,
      Posnr,
      Cliente,
      NomeCliente,
      UFCliente,
      Material,
      NomeMaterial,
      NrRota,
      UF,
      DescRota,
      UM,
      @Semantics.quantity.unitOfMeasure: 'UM'
      QtdSoli,
      @Semantics.quantity.unitOfMeasure: 'UM'
      QtdVinc,
      @Semantics.quantity.unitOfMeasure: 'UM'
      SaldoCarga,
      DataEntrega,
      Sol
}
