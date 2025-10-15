@AbapCatalog.sqlViewName: 'ZVMIOVDEVOLUCAO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca dados das Ordens principais - Relatorio MI'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_DEVOLUCAO
  as select from vbfa

{
  key vbfa.vbeln as vbeln1,
  key vbfa.vbelv as vbeln2,
      rfmng      as menge

}
where
      vbtyp_n      = 'H'
  and vbfa.vbtyp_v = 'C' /// OV
union select distinct from vbfa

{
  key vbfa.vbelv as vbeln1,
  key vbfa.vbeln as vbeln2,

      rfmng      as menge

}
where
      vbtyp_n      = 'H'
  and vbfa.vbtyp_v = 'C' /// OV
