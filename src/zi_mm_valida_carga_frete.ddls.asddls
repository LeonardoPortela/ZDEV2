@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valida se carga tem frete gerado'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VALIDA_CARGA_FRETE
  as select from zmmt0201         as header
    inner join   zmmt0203         as nota on nota.nro_cg = header.nro_cg
    inner join   zib_nfe_dist_itm as zib  on zib.chave_nfe = nota.chave_nfe
    inner join   zlest0110        as les  on les.chave = zib.chave_nfe
    inner join   zlest0108        as les2 on les2.vbeln = les.vbeln
{
  key header.nro_cg,
      les2.fknum
}
