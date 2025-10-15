@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agregação Saldo OV/SIM'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_SALDO_01
  as select from    ZI_EST_SIMU_OV             as simu
    left outer join ZSD_IN_EST_ZSDT0116_SUM    as z116 on z116.vbeln = simu.vbeln
    left outer join ZI_SD_IN_LIMITE_OPUS_DISPO as opus on  opus.vkorg    = simu.vkorg
                                                       and opus.emitente = simu.cnpj_cpf
                                                       and opus.safra    = simu.safra
                                                       and opus.cultura  = simu.cultura
{
  key simu.doc_simulacao,
  key simu.vbeln,
      simu.cnpj_cpf,
      simu.kunnr,
      simu.safra,
      simu.cultura,
      simu.vkorg,
      simu.vkbur,
      simu.spart,
      simu.auart,
      simu.erdat,
      simu.waerk,
      simu.kurrf,
      simu.tpsim,
      simu.venci,
      @Semantics.amount.currencyCode: 'waerk'
      simu.vlr_limite_sap,
      //case when z116.VlrRecebido_M is null then '' else 'X' end           as proc_novo,
      @Semantics.amount.currencyCode: 'waerk'
      //      ( coalesce(opus.VlrLimite,cast( 0 as netwr_ap ))  +
      //        coalesce( z116.valor_recebido_opus, cast( 0 as netwr_ap ) ) )     as vlr_limite_opus,

      coalesce(opus.VlrLimite,cast( 0 as netwr_ap ) )                     as vlr_limite_opus,

      /// ------------------------------------------------------------------------VALORES OV
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_ov , cast( 0 as netwr_ap ) )                   as valor_total_ov,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_ov_m , cast( 0 as netwr_ap ) )                 as valor_total_ov_m,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.saldo_disponivel , cast( 0 as netwr_ap ) )           as saldo_disponivel_ov,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.VlrTotalAutorizado, cast( 0 as netwr_ap ) )          as VlrTotalAutorizado,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.VlrTotalAutorizado_m, cast( 0 as netwr_ap ) )        as VlrTotalAutorizado_m,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.VlrRecebido , cast( 0 as netwr_ap ) )                as valor_recebido_ov,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.VlrRecebido_M, cast( 0 as netwr_ap ) )               as valor_recebido_ov_m,

      //      @Semantics.amount.currencyCode: 'waerk'
      //      ( coalesce( z116.VlrRecebido, cast( 0 as netwr_ap ) )
      //        + simu.vlr_rec_inicial ) - ( coalesce( z116.valor_recebido_sap, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_opus, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_fin, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_apr, cast( 0 as netwr_ap ) ) )    as valor_saldo_rec,
      //
      //      @Semantics.amount.currencyCode: 'waerk'
      //      ( coalesce( z116.VlrRecebido_M, cast( 0 as netwr_ap ) )
      //        + simu.vlr_rec_inicial_m ) - ( coalesce( z116.valor_recebido_sap_m, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_opus_m, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_fin_m, cast( 0 as netwr_ap ) )
      //        + coalesce( z116.valor_recebido_apr_m, cast( 0 as netwr_ap ) ) )  as valor_saldo_rec_m,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( simu.vlr_rec_inicial, cast( 0 as netwr_ap ) )             as valor_saldo_rec,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( simu.vlr_rec_inicial_m, cast( 0 as netwr_ap ) )           as valor_saldo_rec_m,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.saldo_disponivel_m, cast( 0 as netwr_ap ) )          as saldo_disponivel_ov_m,

      /// ---------------------------------------------------------------------------------SAP
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_sap, cast( 0 as netwr_ap ) )    as valor_liberado_ov_sap,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_sap, cast( 0 as netwr_ap ) )         as valor_rejeitado_sap,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_sap, cast( 0 as netwr_ap ) )          as valor_recebido_ov_sap,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_sap, cast( 0 as netwr_ap ) )
        - coalesce( z116.valor_recebido_sap, cast( 0 as netwr_ap ) )      as valor_bloqueado_ov_sap,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_sap_m, cast( 0 as netwr_ap ) )  as valor_liberado_ov_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_sap_m, cast( 0 as netwr_ap ) )       as valor_rejeitado_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_sap_m, cast( 0 as netwr_ap ) )        as valor_recebido_ov_sap_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_sap_m, cast( 0 as netwr_ap ) )
       - coalesce( z116.valor_recebido_sap_m, cast( 0 as netwr_ap ) )     as valor_bloqueado_ov_sap_m,

      /// ---------------------------------------------------------------------------------OPUS
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_opus, cast( 0 as netwr_ap ) )   as valor_liberado_ov_opus,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_opus, cast( 0 as netwr_ap ) )        as valor_rejeitado_opus,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_opus, cast( 0 as netwr_ap ) )         as valor_recebido_ov_opus,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_opus, cast( 0 as netwr_ap ) )
       - coalesce( z116.valor_recebido_opus, cast( 0 as netwr_ap ) )      as valor_bloqueado_ov_opus,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_opus_m, cast( 0 as netwr_ap ) ) as valor_liberado_ov_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_opus_m, cast( 0 as netwr_ap ) )      as valor_rejeitado_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_opus_m, cast( 0 as netwr_ap ) )       as valor_recebido_ov_opus_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_opus_m, cast( 0 as netwr_ap ) )
        - coalesce( z116.valor_recebido_opus_m, cast( 0 as netwr_ap ) )   as valor_bloqueado_ov_opus_m,

      /// ---------------------------------------------------------------------------------FINANCEIRO
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_fin, cast( 0 as netwr_ap ) )    as valor_liberado_ov_fin,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_fin, cast( 0 as netwr_ap ) )         as valor_rejeitado_fin,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_fin, cast( 0 as netwr_ap ) )          as valor_recebido_ov_fin,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_fin, cast( 0 as netwr_ap ) )
        - coalesce( z116.valor_recebido_fin, cast( 0 as netwr_ap ) )      as valor_bloqueado_ov_fin,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_fin_m, cast( 0 as netwr_ap ) )  as valor_liberado_ov_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_fin_m, cast( 0 as netwr_ap ) )       as valor_rejeitado_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_fin_m, cast( 0 as netwr_ap ) )        as valor_recebido_ov_fin_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_fin_m, cast( 0 as netwr_ap ) )
       - coalesce( z116.valor_recebido_fin_m, cast( 0 as netwr_ap ) )     as valor_bloqueado_ov_fin_m,

      /// ------------------------------------------------------------------------APROVAÇÃO ZSDT0117
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_aprovado_apr, cast( 0 as netwr_ap ) )          as valor_aprovado_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_liberado_apr, cast( 0 as netwr_ap ) )          as valor_liberado_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_apr, cast( 0 as netwr_ap ) )         as valor_rejeitado_apr,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_apr, cast( 0 as netwr_ap ) )          as valor_recebido_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_apr, cast( 0 as netwr_ap ) )
       - coalesce( z116.valor_recebido_apr, cast( 0 as netwr_ap ) )       as valor_bloqueado_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_apr, cast( 0 as netwr_ap ) )    as valor_total_aprovado_ov_apr,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_aprovado_apr_m, cast( 0 as netwr_ap ) )        as valor_aprovado_ov_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_liberado_apr_m, cast( 0 as netwr_ap ) )        as valor_liberado_ov_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_rejeitado_apr_m, cast( 0 as netwr_ap ) )       as valor_rejeitado_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_recebido_apr_m, cast( 0 as netwr_ap ) )        as valor_recebido_ov_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_apr_m, cast( 0 as netwr_ap ) )
       - coalesce( z116.valor_recebido_apr_m, cast( 0 as netwr_ap ) )     as valor_bloqueado_ov_apr_m,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce( z116.valor_total_aprovado_apr_m, cast( 0 as netwr_ap ) )  as valor_total_aprovado_ov_apr_m
}
