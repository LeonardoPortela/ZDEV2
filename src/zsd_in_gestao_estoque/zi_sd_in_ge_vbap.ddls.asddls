@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de Item de Vendas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_IN_GE_VBAP
  as select from vbap as vbap
  association [0..1] to vbep as _vbep on  _vbep.vbeln = vbap.vbeln
                                      and _vbep.posnr = vbap.posnr
                                      and _vbep.etenr = '0001'
{
  key vbap.vbeln,
  key vbap.matnr,
  key vbap.werks,
      vbap.kmein,

      @Semantics.quantity.unitOfMeasure: 'kmein'
      sum(
          case coalesce(_vbep.lifsp,'') when '' then
            vbap.kwmeng
          else
            case when _vbep.lifsp <> '12' then
                vbap.kwmeng
            else
                cast( 0 as kwmeng )
            end
          end
      ) as kwmeng
}
group by
  vbap.vbeln,
  vbap.matnr,
  vbap.werks,
  vbap.kmein
