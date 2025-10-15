@AbapCatalog.sqlViewName: 'ZIINCOMPL90'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ovs Complementares pela 90'
define view ZI_IN_DADOS_COMPL90
  as select distinct from vbfa
    inner join            ZI_IN_DADOS_VBAK     as vbak  on vbak.vbeln = vbfa.vbeln
    inner join            zsdt0090             as uni   on uni.vbeln = vbfa.vbelv
    left outer join       ZI_IN_DADOS_ZSDT0090 as rel90 on rel90.vbeln_p = uni.vbelv
{
  key uni.doc_simulacao,
  key uni.vbelv as vbeln_p,
  key vbfa.vbeln
}
where
  (
    (
         vbak.auart          = 'ZCOP' // 29.05.2025
      or vbak.auart          = 'ZROB'
      or vbak.auart          = 'ZREB'
    )
    and(
         vbfa.vbtyp_n        = 'L'
      or vbfa.vbtyp_n        = 'H'
    )
    and  vbfa.vbtyp_v        = 'C'
  )
  // se existir na visao ZI_IN_DADOS_ZSDT0090, não precisa trazer aqui, pq vai aparecer lá na cds _UNI
  and    rel90.doc_simulacao is null
