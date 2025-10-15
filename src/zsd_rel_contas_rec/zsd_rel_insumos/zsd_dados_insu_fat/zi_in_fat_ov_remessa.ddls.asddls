@AbapCatalog.sqlViewName: 'ZIINFATOVREM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Remessa para Qtd Fat'
define view ZI_IN_FAT_OV_REMESSA
  as select from ZI_IN_OV_DADOS_2 as vbak
    inner join   vbfa             as vbfa on vbfa.vbelv = vbak.vbeln
  //and vbfa.vbtyp_v = 'H' //09.06.2025 - OV causa: 13596258

  //    inner join            zsdt0350_in      as z350  on  vbak.auart = z350.auart
  //                                                    and z350.acao  = 'G'
  //  left outer join       ZI_IN_VBFA_VBRK  as vbfa2 on vbfa2.vbeln_va = vbak.vbeln
{
  key vbfa.vbelv as vbeln_va,
  key vbfa.vbeln as vbeln_vf,
      vbfa.rfmng as menge_fat
      //vbfa.rfwrt as netwr_fat
}
////////////// 09.06.2025 - OV causa: 13596258 -->
//where
//       vbak.fator_devol = -1
//  and(
//       vbfa.vbtyp_n     = 'T'
//    or vbfa.vbtyp_n     = 'J'
//  )
//  and  vbfa2.vbeln_va     is null
////////////// 09.06.2025 - OV causa: 13596258 --<

where
  (
    (
          vbak.fator_devol = -1
      and vbfa.vbtyp_n     = 'T'
    )
    or(
          vbak.fator_devol = 1
      and vbfa.vbtyp_n     = 'J'
    )
  )

  and     vbfa.bwart       is not initial // 31.07.2025 - causa: 13418887

//and     vbfa2.vbeln_va   is null  --- 10.06.2025 ------

////group by
////  vbfa.vbelv,
////  vbfa.vbeln
