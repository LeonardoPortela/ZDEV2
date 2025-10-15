@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Item da OV - X'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_ITEM_OV
  as select from vbap as Item
  association [1] to vbep as _vbep on  _vbep.vbeln = $projection.Vbeln
                                   and _vbep.posnr = Item.posnr
                                   and _vbep.etenr = '0001'
{
  key Item.vbeln   as Vbeln,
  key Item.posnr   as Posnr,
      _vbep.ettyp,
      _vbep.lifsp,
      Item.waerk   as Moeda,
      Item.kmein   as Unidade,

      @Semantics.quantity.unitOfMeasure: 'Unidade'
      Item.kwmeng  as Quantidade,

      @Semantics.amount.currencyCode: 'Moeda'
      Item.netwr   as Valor,

      @Semantics.amount.currencyCode: 'Moeda'
      Item.mwsbp   as ValorImp,

      case _vbep.lifsp when '12'
        then 'X'
        else
            '' end as Excluir
}
