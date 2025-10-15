@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Complementares OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_DADOS_COMPL_OV_INFO
  as select from ZI_SD_DADOS_COMPL_OV_002_INFO as OV
  association to I_Material     as M  on  OV.MATNR = M.Material
  association to I_MaterialText as MT on  OV.MATNR    = MT.Material
                                      and MT.Language = 'P'


{
  OV.vbeln,
  max(OV.TPSIM) as TPSIM,
  max(OV.AUART) as AUART,
  max(OV.MATNR) as MATNR,
  max(MT.MaterialName) as MaterialName,
  max(M.MaterialGroup) as Matkl,
  max(OV.NRO_SOL) as NRO_SOL,
  max(OV.TP_VENDA) as TP_VENDA,
  max(OV.charg) as charg
  
}
group by OV.vbeln
