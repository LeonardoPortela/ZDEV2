@AbapCatalog.sqlViewName: 'ZIINFATOV2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamento da OV'
define view ZI_IN_FAT_OV_2
  as select distinct from vbak        as vbak
    inner join            zsdt0350_in as z350 on  vbak.auart = z350.auart
                                              and z350.acao  = 'G'
    inner join            vbfa        as vbfa on  vbfa.vbelv   = vbak.vbeln
                                              and vbfa.vbtyp_n = z350.vbtyp_n
                                              and vbfa.vbtyp_v = z350.vbtyp_v
                                              and vbfa.stufe   = z350.stufe
  // 08.05.2025 - RAMON
    inner join            vbrk        as VBRK on  vbrk.vbeln = vbfa.vbeln
                                              and VBRK.fksto = ''
{
  key vbfa.vbelv      as vbeln_va,
  key vbfa.vbeln      as vbeln_vf,
      sum(vbfa.rfmng) as menge_fat,
      sum(vbfa.rfwrt) as netwr_fat
      //sum(vbrk.mwsbk) as mwsbk_fat


}
group by
  vbfa.vbelv,
  vbfa.vbeln
