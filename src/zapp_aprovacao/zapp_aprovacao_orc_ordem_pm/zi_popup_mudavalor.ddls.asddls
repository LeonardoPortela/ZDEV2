@EndUserText.label: 'Altera valor da ordem'
define abstract entity ZI_POPUP_MUDAVALOR
{
  @UI.hidden: true
  moeda     : waers;
  @Semantics.amount.currencyCode: 'moeda'
  @EndUserText.label: 'Valor'
  valor     : wrbtr;
  @EndUserText.label: 'Descrição'
  @UI.multiLineText: true
  descricao : char200;
    
}
