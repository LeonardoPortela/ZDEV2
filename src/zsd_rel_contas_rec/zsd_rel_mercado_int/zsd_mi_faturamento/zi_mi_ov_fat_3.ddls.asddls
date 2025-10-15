@AbapCatalog.sqlViewName: 'ZVMIOVFAT3'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamentos e Remessas'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view ZI_MI_OV_FAT_3
  as select distinct from ZI_MI_OV_FAT_2 as fat
    inner join            vbfa           as vbfa on vbfa.vbelv = fat.Ordem
{
  key Ordem,
  key Faturamento,
      qtd,
      Valor,
      valdt,
      zterm,
      fat_m_c
}
where
  (
       vbfa.vbtyp_n = 'J'
    or vbfa.vbtyp_n = 'T'
    or vbfa.vbtyp_n = 'P' // 01.08.2025 - causa: 70022230
    or vbfa.vbtyp_n = 'M'
    or vbfa.vbtyp_n = 'O'
  )
  and(
       vbfa.vbtyp_v = 'C'
    or vbfa.vbtyp_v = 'H'
    or vbfa.vbtyp_v = 'L' // 01.08.2025 - causa: 70022230
  )
