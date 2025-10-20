@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações - Projeção'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZC_SD_REL_SOLICITACAO
  as projection on ZI_SD_REL_SOLICITACAO
{
      @EndUserText.label: 'Nro.Sol.Embarque'
      @Search.defaultSearchElement: true
  key NroSol,
      @EndUserText.label: 'Data Solicitação'
      DtSol,
      @Search.defaultSearchElement: true
      Vbeln,
      Posnr,
      @EndUserText.label: 'Nro.Sol.Origem'
      NroSolOrigem,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_CUstomer', element: 'Customer' }}]
      @EndUserText.label: 'Cliente'
      @ObjectModel.text.element: ['NomeCliente']
      @Search.defaultSearchElement: true
      Cliente,
      NomeCliente,
      @EndUserText.label: 'UF Cliente'
      UFCliente,
      @EndUserText.label: 'Material'
      @ObjectModel.text.element: ['NomeMaterial']
      @Search.defaultSearchElement: true
      Material,
      NomeMaterial,
      @EndUserText.label: 'Nro.Rota'
      NrRota,
      @EndUserText.label: 'UF Rota'
      UF,
      @EndUserText.label: 'Desc.Rota'
      @Search.defaultSearchElement: true
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

      _Cargas   : redirected to composition child ZC_SD_REL_CARGAS,
      _SolFilha : redirected to composition child ZC_SD_REL_SOLICITACAO_FILHAS
}
