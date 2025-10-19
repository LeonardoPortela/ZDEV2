@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações - Filhas Proj'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_SD_REL_SOLICITACAO_FILHAS
  as projection on ZI_SD_REL_SOLICITACAO_FILHAS
{
      @EndUserText.label: 'Nro.Sol.Embarque'
  key NroSol,
      @EndUserText.label: 'Nro.Sol.Origem'
  key NroSolOrigem,
      Vbeln,
      Posnr,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_CUstomer', element: 'Customer' }}]
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['NomeCliente']
      Cliente,
      NomeCliente,
      @EndUserText.label: 'UF Cliente'
      UFCliente,
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['NomeMaterial']
      Material,
      NomeMaterial,
      @EndUserText.label: 'Nro.Rota'
      NrRota,
      @EndUserText.label: 'UF Rota'
      UF,
      @EndUserText.label: 'Desc.Rota'
      DescRota,
      UM,
      @Semantics.quantity.unitOfMeasure: 'UM'
      @EndUserText.label: 'Qtd. Solicitada'
      QtdSoli,
      @Semantics.quantity.unitOfMeasure: 'UM'
      @EndUserText.label: 'Quantidade em Carga'
      QtdVinc,
      @Semantics.quantity.unitOfMeasure: 'UM'
      @EndUserText.label: 'Saldo'
      SaldoCarga,
      @EndUserText.label: 'Data Entrega'
      DataEntrega,
      /* Associations */
      Sol : redirected to parent ZC_SD_REL_SOLICITACAO
}
