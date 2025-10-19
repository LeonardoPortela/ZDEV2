@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SD_REL_SOLICITACAO
  as select from ZI_SD_DADOS_SOLICITACAO

  composition [0..*] of ZI_SD_REL_CARGAS             as _Cargas
  composition [0..*] of ZI_SD_REL_SOLICITACAO_FILHAS as _SolFilha

{
  key NroSol,
      DtSol,
      Vbeln,
      Posnr,
      NroSolOrigem,
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
      _Cargas,
      _SolFilha
}
