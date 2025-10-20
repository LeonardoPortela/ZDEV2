@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações - Cargas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_REL_CARGAS
  as select from ZI_SD_DADOS_CARGA

  association to parent ZI_SD_REL_SOLICITACAO as Sol on $projection.NroSol = Sol.NroSol
{
  key NroSol,
  key NroLote,
  key Carga,
  key Seq,
      StatusCarga,
      DescStCarga,
      SeqEntrega,
      @Semantics.quantity.unitOfMeasure: 'UM'
      QtdVinc,
      Um,
      Transp,
      NomeTransp,
      Motorista,
      NomeMotorista,
      TelefoneMot,
      PlacaCavalo,
      LocalEmb,
      Uf,
      StatusEntrega,
      LinkLocaliza,
      Nf,
      Sol,
      DataCarga,
      DtCargaEnvCot,
      DtFreteCont,
      DtAutEmb,
      DtTrocaNFFor,
      DtCarrFor,
      DtCarrCD

}
