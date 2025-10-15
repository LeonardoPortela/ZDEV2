@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de simuladores e OV da ZSDT0090'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_VBAK
  as select distinct from vbak
    inner join            ZI_IN_SPART_CONF as sconf on sconf.spart = vbak.spart
{
  key vbak.vbeln,
      vbak.bukrs_vf,
      vbak.spart,
      vbak.kunnr,
      vbak.auart,
      vbak.waerk
}
where
  vbak.auart <> 'ZFNT'
