@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Filtro para alv de recebidos'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_EST_FILTRO_REC_01
  as select distinct from ZSD_IN_EST_SALDO_01 as saldo
  association [0..*] to ZSD_EST_FILTRO_Z16_Z16_REC as z16 on  z16.cnpj_cpf = saldo.cnpj_cpf
                                                          and z16.safra    = saldo.safra
                                                          and z16.cultura  = saldo.cultura
{
  key doc_simulacao,
  key saldo.vbeln,
      saldo.waerk,
      case coalesce(z16.vbeln,'') when '' then
        ''
      else
        'X'
      end                                                  as existe,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_total_ov,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_total_ov_m,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_recebido_ov,
      @Semantics.amount.currencyCode: 'waerk'
      saldo.valor_recebido_ov_m,

      // se o valor total da ov ainda for maior que o recebido,
      // ou seja, ainda tem valores a receber, logo vai selecionar na cds
      case when saldo.valor_total_ov_m > saldo.valor_recebido_ov_m then
        ''
      else
        'X'
      end                                                  as recebido_total,

      // se recebeu mais do que o valor total da ov
      case when saldo.valor_recebido_ov_m > saldo.valor_total_ov_m then
        'X'
      else
        ''
      end                                                  as saldo_positivo

}
