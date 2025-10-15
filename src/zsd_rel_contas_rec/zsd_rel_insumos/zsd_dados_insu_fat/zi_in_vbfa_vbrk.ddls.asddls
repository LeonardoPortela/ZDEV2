@AbapCatalog.sqlViewName: 'ZIINVBFAVBRK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Faturamentos'
define view ZI_IN_VBFA_VBRK
  as select from vbfa
    inner join   vbrk on vbrk.vbeln = vbfa.vbeln
{
  key vbfa.vbelv as vbeln_va,
  key vbfa.vbeln as vbeln_vf,

      vbfa.rfmng as menge_fat,
      vbfa.rfwrt as netwr_fat
}
where
      vbtyp_n    <> 'S'
  and vbrk.fksto is initial
