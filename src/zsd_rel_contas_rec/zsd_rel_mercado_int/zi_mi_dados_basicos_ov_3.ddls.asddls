@AbapCatalog.sqlViewName: 'ZVMIOVDADOSBAS3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados basicos OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_DADOS_BASICOS_OV_3
  as select from    vbak                 as Ordem

    left outer join vbkd                 as Cond  on  Ordem.vbeln = Cond.vbeln
                                                  and Cond.posnr  = '000000'

    left outer join tvarvc               as Dev   on  Dev.name = 'ZSDT0060_TP_OV_DEVOLUCAO'
                                                  and Dev.low  = Ordem.auart

    left outer join tvgrt                as TVGRT on  TVGRT.vkgrp = Ordem.vkgrp
                                                  and spras       = $session.system_language

  association to vbap                       as Item     on  Ordem.vbeln = Item.vbeln

  association to I_Customer                 as Cliente  on  Ordem.kunnr = Cliente.Customer

  association to I_CustomerPaymentTermsText as CondPgto on  Cond.zterm        = CondPgto.CustomerPaymentTerms
                                                        and CondPgto.Language = $session.system_language

  association to ZI_MI_OV_TP_VENC           as TpVenc   on  Ordem.vbeln = TpVenc.Ordem

{
  key Ordem.vbeln                       as Vbeln,
      Ordem.vkbur                       as EscVendas,
      Ordem.vkgrp                       as EqVendas,
      TVGRT.bezei                       as bezei,
      Ordem.auart                       as TpOV,
      Item.charg                        as Safra,
      Ordem.kunnr                       as Cliente,
      Cliente.CustomerName              as NomeCliente,
      Cond.zterm                        as CondPagto,
      Cond.zlsch                        as FormPagto,
      Cond.valdt                        as Valdt,
      CondPgto.CustomerPaymentTermsName as DescCondPagto,
      Ordem.erdat                       as DataCriacao,
      Item.waerk                        as Moeda,

      case coalesce(Dev.low, '')
        when ''
            then Item.kwmeng
        else
            Item.kwmeng * -1
         end                            as Qtd,


      case coalesce(Dev.low, '')
        when ''
            then Item.netwr
        else
            Item.netwr * -1
         end                            as ValorLiq,

      case coalesce(Dev.low, '')
        when ''
            then Item.mwsbp
        else
            Item.mwsbp * -1
         end                            as ValorImp,

      case coalesce(Dev.low, '')
        when ''
            then Item.netwr + Item.mwsbp
        else
            ( Item.netwr + Item.mwsbp ) * -1
         end                            as ValorTotal,

      case coalesce(Dev.low, '')
       when ''
           then 'V'
       else
           'D'
        end                             as TpDevVenda,

      TpVenc.TipoVencimento             as TipoVencimento
}
