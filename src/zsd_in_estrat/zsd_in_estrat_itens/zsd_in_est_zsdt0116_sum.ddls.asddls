@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Somatoria dos itens'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
// Essa visão mostra por OV: valor total, valor já liberado e Saldo disponivel
define view entity ZSD_IN_EST_ZSDT0116_SUM
  as select from    ZSD_IN_EST_VBAP_3   as vbak
    left outer join ZSD_IN_EST_ZSDT0116 as z116 on vbak.vbeln = z116.Vbeln
{
  key vbak.vbeln,
      vbak.waerk,

      @Semantics.amount.currencyCode: 'waerk'
      sum(vbak.netwr)                                                    as valor_ov,

      @Semantics.amount.currencyCode: 'waerk'
      sum(vbak.netwr_m)                                                  as valor_ov_m,

      @Semantics.amount.currencyCode: 'waerk'
      sum(z116.VlrRecebido)                                              as VlrRecebido,

      @Semantics.amount.currencyCode: 'waerk'
      sum(z116.VlrRecebido_Moeda)                                        as VlrRecebido_M,


      //----------------------------------------------------------------------SAP
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoSAP,cast( 0 as netwr_ap )))          as valor_aprovado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoSAP,cast( 0 as netwr_ap )))          as valor_liberado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoSAP,cast( 0 as netwr_ap )))         as valor_rejeitado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoSAP,cast( 0 as netwr_ap )))          as valor_recebido_sap,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoSAP ,cast( 0 as netwr_ap )))    as valor_total_aprovado_sap,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoSAP_M,cast( 0 as netwr_ap )))        as valor_aprovado_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoSAP_M,cast( 0 as netwr_ap )))        as valor_liberado_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoSAP_M,cast( 0 as netwr_ap )))       as valor_rejeitado_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoSAP_M,cast( 0 as netwr_ap )))        as valor_recebido_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoSAP_M ,cast( 0 as netwr_ap )))  as valor_total_aprovado_sap_m,

      //----------------------------------------------------------------------OPUS
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoOPUS,cast( 0 as netwr_ap )))         as valor_aprovado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoOPUS,cast( 0 as netwr_ap )))         as valor_liberado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoOPUS,cast( 0 as netwr_ap )))        as valor_rejeitado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoOPUS,cast( 0 as netwr_ap )))         as valor_recebido_opus,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoOPUS ,cast( 0 as netwr_ap )))   as valor_total_aprovado_opus,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoOPUS_M,cast( 0 as netwr_ap )))       as valor_aprovado_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoOPUS_M,cast( 0 as netwr_ap )))       as valor_liberado_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoOPUS_M,cast( 0 as netwr_ap )))      as valor_rejeitado_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoOPUS_M,cast( 0 as netwr_ap )))       as valor_recebido_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoOPUS_M ,cast( 0 as netwr_ap ))) as valor_total_aprovado_opus_m,

      //----------------------------------------------------------------------FIN
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoFIN,cast( 0 as netwr_ap )))          as valor_aprovado_fin,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoFIN,cast( 0 as netwr_ap )))          as valor_liberado_fin,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoFIN,cast( 0 as netwr_ap )))         as valor_rejeitado_fin,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoFIN,cast( 0 as netwr_ap )))          as valor_recebido_fin,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoFIN ,cast( 0 as netwr_ap )))    as valor_total_aprovado_fin,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoFIN_M,cast( 0 as netwr_ap )))        as valor_aprovado_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoFIN_M,cast( 0 as netwr_ap )))        as valor_liberado_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoFIN_M,cast( 0 as netwr_ap )))       as valor_rejeitado_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoFIN_M,cast( 0 as netwr_ap )))        as valor_recebido_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoFIN_M ,cast( 0 as netwr_ap )))  as valor_total_aprovado_fin_m,

      //----------------------------------------------------------------------APR
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoAPR,cast( 0 as netwr_ap )))          as valor_aprovado_apr,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoAPR,cast( 0 as netwr_ap )))          as valor_liberado_apr,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoAPR,cast( 0 as netwr_ap )))         as valor_rejeitado_apr,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoAPR,cast( 0 as netwr_ap )))          as valor_recebido_apr,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoAPR ,cast( 0 as netwr_ap )))    as valor_total_aprovado_apr,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrAprovadoAPR_M,cast( 0 as netwr_ap )))        as valor_aprovado_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrLiberadoAPR_M,cast( 0 as netwr_ap )))        as valor_liberado_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRejeitadoAPR_M,cast( 0 as netwr_ap )))       as valor_rejeitado_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrRecebidoAPR_M,cast( 0 as netwr_ap )))        as valor_recebido_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAprovadoAPR_M ,cast( 0 as netwr_ap )))  as valor_total_aprovado_apr_m,


      // -----------------------------------------------------------------TOTAIS
      @Semantics.amount.currencyCode: 'waerk'
      sum( z116.VlrTotalAprovado )                                       as VlrTotalAprovado,

      @Semantics.amount.currencyCode: 'waerk'
      //      sum(
      //        case coalesce(z116.Vbeln,'') when '' then
      //            case when vbak.waerk = 'USD' then
      //                vbak.netwr
      //            else
      //                vbak.netwr_m
      //            end
      //        else
      //           case when vbak.waerk = 'USD' then
      //             vbak.netwr - z116.VlrTotalAprovado
      //           else
      //            case when z116.kursf > 0 then
      //                cast( division( cast( vbak.netwr as abap.dec( 12, 2 )),z116.kursf,2 )  as netwr_ap )
      //                    -  z116.VlrTotalAprovado
      //            else
      //                cast( 0 as netwr_ap )
      //            end
      //           end
      //        end )                                                            as saldo_disponivel,

      sum( vbak.netwr -
        (  coalesce(z116.VlrTotalAprovado,cast( 0 as netwr_ap ) ) )
      )                                                                  as saldo_disponivel,

      @Semantics.amount.currencyCode: 'waerk'
      sum( vbak.netwr_m -
        (  coalesce(z116.VlrTotalAprovado_M,cast( 0 as netwr_ap ) ) )
      )                                                                  as saldo_disponivel_m,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAutorizado,cast( 0 as netwr_ap ) ) )    as VlrTotalAutorizado,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAutorizado_M,cast( 0 as netwr_ap ) ) )  as VlrTotalAutorizado_m,

      @Semantics.amount.currencyCode: 'waerk'
      sum( z116.VlrTotalAprovado_M )                                     as VlrTotalAprovadoM,

      @Semantics.amount.currencyCode: 'waerk'
      sum( coalesce(z116.VlrTotalAutorizado_M,cast( 0 as netwr_ap ) ) )  as VlrTotalAutorizadoM

}
group by
  vbak.vbeln,
  vbak.waerk
