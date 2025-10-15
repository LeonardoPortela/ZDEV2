@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados 40 41'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_0040_41
  as select from zsdt0040 as z040
  association [0..1] to ZI_EST_0041 as z041 on z040.doc_simulacao = z041.doc_simulacao
{
  key z040.doc_simulacao,
      z040.kunnr,
      z040.vkorg,
      z040.spart,
      z040.vkbur,
      z040.auart,
      z040.erdat,

      case coalesce(z040.dtvencov,'00000000') when '00000000' then
         case coalesce(z040.dtpgtcult,'00000000') when '00000000' then
            z041.DTVENC
         else
            z040.dtpgtcult
         end
      else
        z040.dtvencov
      end as dtvencov,



      //      case coalesce(z040.dtpgtcult,'00000000') when '00000000' then
      //        case coalesce(z040.dtvencov,'00000000') when '00000000' then
      //            z041.DTVENC
      //        else
      //            z040.dtvencov
      //        end
      //      else
      //        z040.dtpgtcult
      //      end as dtpgtcult,
      z040.safra,
      z040.cultura,

      z040.waerk,
      z040.tpsim
}
