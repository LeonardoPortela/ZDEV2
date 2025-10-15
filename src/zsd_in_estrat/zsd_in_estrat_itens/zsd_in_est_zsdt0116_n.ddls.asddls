@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Normalização para o processo de saldo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_ZSDT0116_N
  as select from ZSD_IN_EST_ZSDT0116_REC as z116
  //as select from ZSD_IN_EST_ZSDT0116_GERAL as z116
{
  key vbeln,
      waerk,
      
      z116.kursf,
      @Semantics.amount.currencyCode : 'waerk'
      z116.vlr_recebido,

      @Semantics.amount.currencyCode : 'waerk'
      z116.vlr_recebido_moeda,

      // -------------------------------------------------------SAP
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_aprovado else cast( 0 as netwr_ap ) end             as VlrAprovadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_liberado else cast( 0 as netwr_ap ) end             as VlrLiberadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_rejeitado else cast( 0 as netwr_ap ) end            as VlrRejeitadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then z116.vlr_recebido else cast( 0 as netwr_ap ) end        as VlrRecebidoSAP,

      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_aprovado_moeda  else cast( 0 as netwr_ap ) end      as VlrAprovadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_liberado_moeda  else cast( 0 as netwr_ap ) end      as VlrLiberadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then vlr_rejeitado_moeda  else cast( 0 as netwr_ap ) end     as VlrRejeitadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'S' then z116.vlr_recebido_moeda  else cast( 0 as netwr_ap ) end as VlrRecebidoSAP_M,


      // -------------------------------------------------------OPUS
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_aprovado else cast( 0 as netwr_ap ) end             as VlrAprovadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_liberado else cast( 0 as netwr_ap ) end             as VlrLiberadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_rejeitado else cast( 0 as netwr_ap ) end            as VlrRejeitadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then z116.vlr_recebido else cast( 0 as netwr_ap ) end        as VlrRecebidoOPUS,

      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_aprovado_moeda  else cast( 0 as netwr_ap ) end      as VlrAprovadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_liberado_moeda  else cast( 0 as netwr_ap ) end      as VlrLiberadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then vlr_rejeitado_moeda  else cast( 0 as netwr_ap ) end     as VlrRejeitadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'O' then z116.vlr_recebido_moeda  else cast( 0 as netwr_ap ) end as VlrRecebidoOPUS_M,


      // -------------------------------------------------------FIN
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_aprovado else cast( 0 as netwr_ap ) end             as VlrAprovadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_liberado else cast( 0 as netwr_ap ) end             as VlrLiberadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_rejeitado else cast( 0 as netwr_ap ) end            as VlrRejeitadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then z116.vlr_recebido else cast( 0 as netwr_ap ) end        as VlrRecebidoFIN,

      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_aprovado_moeda  else cast( 0 as netwr_ap ) end      as VlrAprovadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_liberado_moeda  else cast( 0 as netwr_ap ) end      as VlrLiberadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then vlr_rejeitado_moeda  else cast( 0 as netwr_ap ) end     as VlrRejeitadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'F' then z116.vlr_recebido_moeda  else cast( 0 as netwr_ap ) end as VlrRecebidoFIN_M,

      // -------------------------------------------------------APROVACAO(ZSDT00117)

      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_aprovado else cast( 0 as netwr_ap ) end             as VlrAprovadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_liberado else cast( 0 as netwr_ap ) end             as VlrLiberadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_rejeitado else cast( 0 as netwr_ap ) end            as VlrRejeitadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then z116.vlr_recebido else cast( 0 as netwr_ap ) end        as VlrRecebidoAPR,

      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_aprovado_moeda  else cast( 0 as netwr_ap ) end      as VlrAprovadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_liberado_moeda  else cast( 0 as netwr_ap ) end      as VlrLiberadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then vlr_rejeitado_moeda  else cast( 0 as netwr_ap ) end     as VlrRejeitadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      case when saldo_origem = 'Z' then z116.vlr_recebido_moeda  else cast( 0 as netwr_ap ) end as VlrRecebidoAPR_M
}
