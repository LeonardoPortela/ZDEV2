@AbapCatalog.sqlViewName: 'ZIINCOMPL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ovs/Simulador para Ov de complemento'
define view ZI_IN_DADOS_COMPL
  as select from vbfa
    inner join   ZI_IN_DADOS_VBAK     as vbak on vbak.vbeln = vbfa.vbeln
    inner join   ZI_IN_DADOS_ZSDT0041 as uni  on uni.vbeln = vbfa.vbelv
{
  key uni.doc_simulacao,
  key uni.vbeln_p,
  key vbfa.vbeln
}
where
  (
       vbak.auart   = 'ZCOP'
    or vbak.auart   = 'ZRPF'
    or vbak.auart   = 'ZROB' // 08.05.2025 - RAMON
  )
  and(
       vbfa.vbtyp_n = 'L'
    or vbfa.vbtyp_n = 'H'
  )
  and  vbfa.vbtyp_v = 'C'
