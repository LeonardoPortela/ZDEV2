@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Relatorio Solicitações - Cargas Proj'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_SD_REL_CARGAS
  as projection on ZI_SD_REL_CARGAS
{
      @EndUserText.label: 'Nro.Sol.Embarque'
  key NroSol,
      @EndUserText.label: 'Nro.Lote'
  key NroLote,
      @EndUserText.label: 'Nro.Carga'
  key Carga,
      @EndUserText.label: 'Sequencial'
  key Seq,
      @EndUserText.label: 'Status Carga'
      @ObjectModel.text.element: ['DescStCarga']
      StatusCarga,
      @EndUserText.label: 'Descrição Status Carga'
      DescStCarga,
      @EndUserText.label: 'Seq.Entrega'
      SeqEntrega,
      @EndUserText.label: 'Quantidade Carga'
      @Semantics.quantity.unitOfMeasure: 'UM'
      QtdVinc,
      Um,
      @EndUserText.label: 'Transportadora'
      @ObjectModel.text.element: ['NomeTransp']
      Transp,
      NomeTransp,
      @EndUserText.label: 'Motorista'
      @ObjectModel.text.element: ['NomeMotorista']
      Motorista,
      NomeMotorista,
      @EndUserText.label: 'Contato Motorista'
      TelefoneMot,
      @EndUserText.label: 'Placa Cavalo'
      PlacaCavalo,
      @EndUserText.label: 'Local Embarque'
      LocalEmb,
      @EndUserText.label: 'UF'
      Uf,
      @EndUserText.label: 'Status Entrega'
      StatusEntrega,
      @EndUserText.label: 'Link Localiza ai'
      LinkLocaliza,
      @EndUserText.label: 'NF'
      Nf,
      @EndUserText.label: 'Data Carga'
      DataCarga,
      @EndUserText.label: 'Data Carga Enviada para cotação'
      DtCargaEnvCot,
      @EndUserText.label: 'Data Frete Contratado'
      DtFreteCont,
      @EndUserText.label: 'Data Autorizado Embarque'
      DtAutEmb,
      @EndUserText.label: 'Data Troca Nota Fornecedor x Fornecedor'
      DtTrocaNFFor,
      @EndUserText.label: 'Data Carregamento Fornecedor'
      DtCarrFor,
      @EndUserText.label: 'Data Carregamento CD'
      DtCarrCD,
      /* Associations */
      Sol : redirected to parent ZC_SD_REL_SOLICITACAO
}
