@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Todos os registros categorizados'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSD_IN_EST_ZSDT0116_GERAL
  as select from zsdt0116 as z116
  //inner join   ZSD_IN_EST_VBAP as vbak on vbak.vbeln = z116.vbeln
{
  key z116.vbeln,
  key z116.seq,
      z116.waerk,
      kursf,
      z116.saldo_origem,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'A' or (z116.aprov_por_ref = 'X' or z116.status_workflow = '' ) then
        z116.vlr_liberado
      else
        cast( 0 as netwr_ap )
      end     as vlr_aprovado,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'A' or (z116.aprov_por_ref = 'X' or z116.status_workflow = '' ) then
        z116.vlr_liberado_moeda
      else
        cast( 0 as netwr_ap )
      end     as vlr_aprovado_moeda,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'L' then
            z116.vlr_liberado
          else
            cast( 0 as netwr_ap )
          end as vlr_liberado,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'L' then
           z116.vlr_liberado_moeda
          else
            cast( 0 as netwr_ap )
          end as vlr_liberado_moeda,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'R' then
        z116.vlr_liberado
      else
        cast( 0 as netwr_ap )
      end     as vlr_rejeitado,

      @Semantics.amount.currencyCode : 'waerk'
      case when z116.status_workflow = 'R' then
        z116.vlr_liberado_moeda
      else
        cast( 0 as netwr_ap )
      end     as vlr_rejeitado_moeda
}
where
      z116.status = ''
  and z116.kursf  is not initial
