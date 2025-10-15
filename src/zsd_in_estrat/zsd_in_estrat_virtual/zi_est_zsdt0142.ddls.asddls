@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Para corrigir sequencias vazias'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_ZSDT0142
  as select from zsdt0142 as z142
  association [1..*] to ZI_EST_ZSDT0116_FIRST as z16 on z16.vbeln = z142.vbeln
{
  key z142.bukrs,
  key z142.vbeln,
  key case coalesce(z142.seq, '0000000000') when '0000000000' then z16.seq else z142.seq end as seq,
  key z142.nivel,
  key z142.aprovador,
      cast( 'USD' as waers)                                                                  as waers,
      @Semantics.amount.currencyCode: 'waers'
      z142.valor_de,
      @Semantics.amount.currencyCode: 'waers'
      z142.valor_ate,

      @Semantics.amount.currencyCode: 'waers'
      z142.vlr_foto_acum,


      z142.data_atual,
      z142.hora_atual,
      z142.usuario
}
