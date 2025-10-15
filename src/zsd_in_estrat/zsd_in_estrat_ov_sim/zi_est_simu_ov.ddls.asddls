@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de simulador e OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_SIMU_OV
  as select distinct from ZI_IN_SIMU_OV   as vbak
  //inner join            zsdt0040        as z040   on z040.doc_simulacao = vbak.doc_simulacao
    inner join            ZI_EST_0040_41  as z040   on z040.doc_simulacao = vbak.doc_simulacao
    inner join            ZI_EST_KNA1     as kna1   on kna1.kunnr = z040.kunnr
    left outer join       ZI_EST_ZSDT0377 as limite on  limite.bukrs   = z040.vkorg
                                                    and limite.cultura = z040.cultura
                                                    and limite.safra   = z040.safra
    left outer join       ZI_EST_TAXA_USD as vbkd   on vbkd.vbeln = vbak.vbeln
    
    left outer join ZSD_IN_EST_ZSDT0094_1 as z94 on  z94.doc_simulacao = vbak.doc_simulacao
    //left outer join       zsdt0094        as z94    on  z94.nro_sol_ov = vbak.doc_simulacao
                                                    //and z94.tipo_taxa  = 'C'
                                                    //and z94.safra      = z040.safra
                                                    //and z94.tipo       = 'VDI'
                                                    //and z94.vbeln      = ''
                                                    //and z94.estorno    = '0000000000'
  //    left outer join       ZI_SD_IN_LIMITE_OPUS_DISPO as opus   on  opus.vkorg    = z040.vkorg
  //                                                               and opus.emitente = kna1.cnpj_cpf
  //                                                               and opus.safra    = z040.safra
  //                                                               and opus.cultura  = z040.cultura
    left outer join       zsdt0116_rec    as rec    on  rec.vbeln = vbak.vbeln
                                                    and rec.seq   = '0000000000'
{
  key vbak.doc_simulacao,
  key vbak.vbeln,
      kna1.cnpj_cpf,
      z040.kunnr,
      z040.vkorg,
      z040.spart,
      z040.vkbur,
      z040.auart,
      z040.erdat,
      z040.dtvencov                                                              as venci,
      z040.safra,
      z040.cultura,

      z040.waerk,
      z040.tpsim,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce(limite.valor,cast( 0 as netwr_ap ) )                              as vlr_limite_sap,
      //coalesce(opus.VlrLimite,cast( 0 as netwr_ap ) )                            as vlr_limite_opus,
      case when z94.taxa_cambio is null then vbkd.kurrf else z94.taxa_cambio end as kurrf,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce(rec.vlr_recebido, cast( 0 as netwr_ap ) )                         as vlr_rec_inicial,
      @Semantics.amount.currencyCode: 'waerk'
      coalesce(rec.vlr_recebido_moeda, cast( 0 as netwr_ap ) )                   as vlr_rec_inicial_m
}
