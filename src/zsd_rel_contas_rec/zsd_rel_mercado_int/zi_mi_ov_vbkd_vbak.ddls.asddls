@AbapCatalog.sqlViewName: 'ZVMIOVVBKDVBAK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca tipo de vencimento OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_VBKD_VBAK
  as select distinct from vbkd
    inner join            vbfa on  vbfa.vbelv = vbkd.vbeln
                               and vbfa.vbtyp_n    = 'C'
                               and vbfa.vbtyp_v    = 'L'
{
  key vbkd.vbeln,
      vbkd.valdt,
      vbfa.vbeln as vbeln_va
}
where
  vbkd.posnr = '000000'
