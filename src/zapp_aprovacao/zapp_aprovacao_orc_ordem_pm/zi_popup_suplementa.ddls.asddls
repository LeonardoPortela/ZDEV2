@EndUserText.label: 'Editar suplementação'
define abstract entity ZI_POPUP_SUPLEMENTA
{
  @EndUserText.label: 'Nº Ordem'
  ordem     : aufnr;
  @UI.hidden: true
  moeda     : waers;
  @Semantics.amount.currencyCode: 'moeda'
  @EndUserText.label: 'Valor'
  valor     : wrbtr;
  @EndUserText.label: 'Descrição'
  @UI.multiLineText: true
  descricao : char200;

}
