@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valor Agregados para Simulador'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_SALDO_02
  as select from ZSD_IN_EST_SALDO_01
{
  key doc_simulacao,
  key vbeln,
      kunnr,
      cnpj_cpf,
      safra,
      cultura,
      vkorg,
      vkbur,
      cast('USD' as waerk)                              as waerk,
      kurrf,

      ///// ----------------------------------- Informações da OV
      @Semantics.amount.currencyCode: 'waerk'
      case when valor_saldo_rec > 0  then

        case when waerk = 'USD' then
            valor_saldo_rec
        else
            cast( division(cast(valor_saldo_rec_m as abap.dec( 15, 2 ) ), kurrf, 2) as netwr_ap )
        end

      else
        cast( 0 as netwr_ap )
      end                                               as vlr_limite_fin,

      @Semantics.amount.currencyCode: 'waerk'
      case when waerk = 'USD' then
        valor_total_ov
      else
        cast( division(cast(valor_total_ov as abap.dec( 15, 2 ) ), kurrf, 2) as netwr_ap )
      end                                               as valor_total_ov,

      @Semantics.amount.currencyCode: 'waerk'
      coalesce(valor_recebido_ov,cast( 0 as netwr_ap )) as valor_recebido_ov,

      @Semantics.amount.currencyCode: 'waerk'
      saldo_disponivel_ov                               as saldo_disponivel_ov,

      @Semantics.amount.currencyCode: 'waerk'
      VlrTotalAutorizado                                as VlrTotalAutorizado,

      //---------------------------------------------------------- SAP
      @Semantics.amount.currencyCode: 'waerk'
      vlr_limite_sap,
      @Semantics.amount.currencyCode: 'waerk'
      valor_liberado_ov_sap,
      @Semantics.amount.currencyCode: 'waerk'
      valor_bloqueado_ov_sap,

      //---------------------------------------------------------- OPUS
      @Semantics.amount.currencyCode: 'waerk'
      vlr_limite_opus,
      @Semantics.amount.currencyCode: 'waerk'
      valor_liberado_ov_opus,
      @Semantics.amount.currencyCode: 'waerk'
      valor_bloqueado_ov_opus,

      //---------------------------------------------------------- FIN

      @Semantics.amount.currencyCode: 'waerk'
      valor_liberado_ov_fin,
      @Semantics.amount.currencyCode: 'waerk'
      valor_bloqueado_ov_fin,

      //--------------------------------------------APROVACAO ZSDT0117
      @Semantics.amount.currencyCode: 'waerk'
      //valor_liberado_ov_apr,
      valor_total_aprovado_ov_apr                       as valor_liberado_ov_apr,
      @Semantics.amount.currencyCode: 'waerk'
      valor_bloqueado_ov_apr
}
