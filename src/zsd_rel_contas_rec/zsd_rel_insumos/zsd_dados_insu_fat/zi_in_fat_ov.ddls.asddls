@AbapCatalog.sqlViewName: 'ZIINFATOV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamentos'
define view ZI_IN_FAT_OV
  as select distinct from vbfa                 as vbfa
    inner join            vbrk                 as vbrk    on vbrk.vbeln = vbfa.vbeln
    left outer join       ZI_IN_FAT_OV_REMESSA as remessa on remessa.vbeln_va = vbfa.vbelv
{
  key vbfa.vbelv                                                                   as vbeln_va,
  //key max(vbfa.vbeln)                                                              as vbeln_vf,
      sum( case when vbrk.fksto = '' then vbfa.rfmng else vbfa.rfmng * (-1) end  ) as menge_fat
      //sum( case when vbrk.fksto = '' then vbfa.rfwrt else vbfa.rfwrt * (-1) end  ) as netwr_fat
}
where
      sfakn            is initial // se doc.estorno preenchido, desconsiderar
  and remessa.vbeln_va is null

  and vbfa.vbtyp_v     <> 'L' // adicionado para 70022369 - 10.06.2025


group by
  vbfa.vbelv
