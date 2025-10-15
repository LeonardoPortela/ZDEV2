@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Retorna a taxa dolar para uma OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_EST_TAXA_USD
  as select from    vbak         as VBAK
    inner join      vbkd         as vbkd  on vbak.vbeln = vbkd.vbeln
    left outer join ZI_EST_TCURR as tcurr on  tcurr.kurst = 'B'
                                          and tcurr.fcurr = 'USD'
                                          and tcurr.tcurr = vbak.waerk
                                          and tcurr.gdatu = vbkd.kurrf_dat
{
  key vbkd.vbeln,
      case when vbkd.kurrf <= 1 then
        case when vbkd.kursk <= 1 then

                case when tcurr.ukurs is not null then tcurr.ukurs else 1 end


            else
                vbkd.kursk
        end
      else
        vbkd.kurrf end as kurrf
}
