@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Insumos - Cabeçalho - X'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_BASICOS_OV
  as select from    vbak   as Ordem
    left outer join vbkd   as Cond  on  Ordem.vbeln = Cond.vbeln
                                    and Cond.posnr  = '000000'

    left outer join tvarvc as Dev   on  Dev.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                    and Dev.low  = Ordem.auart

    left outer join tvgrt  as TVGRT on  tvgrt.vkgrp = Ordem.vkgrp
                                    and tvgrt.spras = $session.system_language

  association        to ZI_IN_DADOS_ITEM_OV        as Item     on  Ordem.vbeln = Item.Vbeln
  association        to I_Customer                 as Cliente  on  Ordem.kunnr = Cliente.Customer

  association        to I_CustomerPaymentTermsText as CondPgto on  Cond.zterm        = CondPgto.CustomerPaymentTerms
                                                               and CondPgto.Language = $session.system_language

  association        to ZI_MI_OV_FAT_SUM           as QtdFat   on  Ordem.vbeln = QtdFat.Ordem
  association        to ZI_MI_OV_TP_VENC           as TpVenc   on  Ordem.vbeln = TpVenc.Ordem

{
  key Ordem.vbeln                       as Vbeln,
      Ordem.vkbur                       as EscVendas,
      Ordem.vkgrp                       as EqVendas,
      tvgrt.bezei                       as bezei,
      Ordem.auart                       as TpOV,
      //Item.charg                        as Safra,
      Ordem.kunnr                       as Cliente,
      Cliente.CustomerName              as NomeCliente,
      Cond.zterm                        as CondPagto,
      Cond.zlsch                        as FormPagto,
      CondPgto.CustomerPaymentTermsName as DescCondPagto,
      Ordem.erdat                       as DataCriacao,
      Item.Moeda,
      Item.Unidade,
      @Semantics.quantity.unitOfMeasure: 'Unidade'
      case coalesce(Dev.low, '')
       when ''
           then Item.Quantidade
       else
           Item.Quantidade * -1
        end                             as Qtd,

      @Semantics.amount.currencyCode: 'Moeda'
      case coalesce(Dev.low, '') when ''
        then Item.Valor
        else
        Item.Valor * -1
      end                               as ValorLiq,
      @Semantics.amount.currencyCode: 'Moeda'
      case coalesce(Dev.low, '')
        when ''
            then Item.ValorImp
        else
            Item.ValorImp * -1
         end                            as ValorImp,
      @Semantics.amount.currencyCode: 'Moeda'
      case coalesce(Dev.low, '')
        when ''
            then Item.Valor + Item.ValorImp
        else
            ( Item.Valor + Item.ValorImp ) * -1
         end                            as ValorTotal,
      @Semantics.quantity.unitOfMeasure: 'Unidade'
      case coalesce(Dev.low, '')
        when ''
            then QtdFat.Qtd
        else
            QtdFat.Qtd * -1
        end                             as QtdFat,

      case coalesce(Dev.low, '')
       when ''
           then 'V'
       else
           'D'
        end                             as TpDevVenda,

      TpVenc.TipoVencimento             as TipoVencimento
}
where Item.Excluir is initial
