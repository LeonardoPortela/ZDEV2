@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CONTA RAZAO'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity Z_CONTA_RAZAO_zfit0219 as select from zfit0219 as _fi0219
left outer join  skat as _skatDesc  on _fi0219.saknr = _skatDesc.saknr
                                    and _skatDesc.ktopl = '0050'
                                    and  _skatDesc.spras = $session.system_language
{
key _fi0219.saknr as Saknr,
    _skatDesc.txt50 as Txt50,
    _fi0219.d_c as DC
}

