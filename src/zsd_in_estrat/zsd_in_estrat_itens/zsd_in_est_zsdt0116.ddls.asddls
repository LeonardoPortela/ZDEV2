@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consolidação entre VBAP x ZSDT0116'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_ZSDT0116
  //as select from zsdt0116        as z116
  as select from ZSD_IN_EST_ZSDT0116_N as z116
{
  key z116.vbeln                                   as Vbeln,
      z116.waerk,

      max(z116.kursf)                              as kursf,
      @Semantics.amount.currencyCode : 'waerk'
      sum( vlr_recebido )                          as VlrRecebido,
      @Semantics.amount.currencyCode : 'waerk'
      sum( vlr_recebido_moeda )                    as VlrRecebido_Moeda,

      // -------------------------------------------------------------------------SAP
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP )                        as VlrLiberadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoSAP )                        as VlrAprovadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoSAP )                       as VlrRejeitadoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoSAP )                        as VlrRecebidoSAP,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP + VlrAprovadoSAP )       as VlrTotalAprovadoSAP,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP_M )                      as VlrLiberadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoSAP_M )                      as VlrAprovadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoSAP_M )                     as VlrRejeitadoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoSAP_M )                      as VlrRecebidoSAP_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP_M + VlrAprovadoSAP_M )   as VlrTotalAprovadoSAP_M,

      // -------------------------------------------------------------------------OPUS
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoOPUS )                       as VlrLiberadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoOPUS )                       as VlrAprovadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoOPUS )                      as VlrRejeitadoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoOPUS )                       as VlrRecebidoOPUS,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoOPUS + VlrAprovadoOPUS )     as VlrTotalAprovadoOPUS,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoOPUS_M )                     as VlrLiberadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoOPUS_M )                     as VlrAprovadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoOPUS_M )                    as VlrRejeitadoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoOPUS_M )                     as VlrRecebidoOPUS_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoOPUS_M + VlrAprovadoOPUS_M ) as VlrTotalAprovadoOPUS_M,

      // -------------------------------------------------------------------------FIN
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoFIN )                        as VlrLiberadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoFIN )                        as VlrAprovadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoFIN )                       as VlrRejeitadoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoFIN )                        as VlrRecebidoFIN,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoFIN + VlrAprovadoFIN )       as VlrTotalAprovadoFIN,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoFIN_M )                      as VlrLiberadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoFIN_M )                      as VlrAprovadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoFIN_M )                     as VlrRejeitadoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoFIN_M )                      as VlrRecebidoFIN_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoFIN_M + VlrAprovadoFIN_M )   as VlrTotalAprovadoFIN_M,

      // -----------------------------------------------------------------APROVACAO ZSDT0017
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoAPR )                        as VlrLiberadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoAPR )                        as VlrAprovadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoAPR )                       as VlrRejeitadoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoAPR )                        as VlrRecebidoAPR,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoAPR + VlrAprovadoAPR )       as VlrTotalAprovadoAPR,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoAPR_M )                      as VlrLiberadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoAPR_M )                      as VlrAprovadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRejeitadoAPR_M )                     as VlrRejeitadoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrRecebidoAPR_M )                      as VlrRecebidoAPR_M,
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoAPR_M + VlrAprovadoAPR_M )   as VlrTotalAprovadoAPR_M,

      //-------------------------------------------------------------------- TOTAIS(USD)
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP + VlrAprovadoSAP +
           VlrLiberadoOPUS + VlrAprovadoOPUS +
           VlrAprovadoFIN + VlrLiberadoFIN +
           VlrAprovadoAPR + VlrLiberadoAPR )       as VlrTotalAprovado,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoSAP + VlrAprovadoOPUS +
           VlrAprovadoFIN + VlrAprovadoAPR )       as VlrTotalAutorizado,

      //-------------------------------------------------------------------- TOTAIS(MOEDA)
      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrLiberadoSAP_M + VlrAprovadoSAP_M +
           VlrLiberadoOPUS_M + VlrAprovadoOPUS_M +
           VlrAprovadoFIN_M + VlrLiberadoFIN_M +
           VlrAprovadoAPR_M + VlrLiberadoAPR_M )   as VlrTotalAprovado_M,

      @Semantics.amount.currencyCode : 'waerk'
      sum( VlrAprovadoSAP_M + VlrAprovadoOPUS_M +
           VlrAprovadoFIN_M + VlrAprovadoAPR_M )   as VlrTotalAutorizado_M
}
group by
  z116.vbeln,
  z116.waerk
