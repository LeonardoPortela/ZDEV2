@AbapCatalog.sqlViewName: 'ZVMIOVVBKDVBAK2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Busca tipo de vencimento OV'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MI_OV_VBKD_VBAK_2
  as select distinct from vbkd             as vbkd
    inner join            vbak             as vbak  on vbak.vbeln = vbkd.vbeln
    inner join            ZI_MI_OV_TP_VENC as venc  on vbkd.vbeln = venc.Ordem
    inner join            tvarvc           as Spart on  Spart.name = 'Z_SPART_REL_MERCADO_INTER'
                                                    and Spart.low  = vbak.spart
{
  key vbkd.vbeln,
      vbak.bukrs_vf as bukrs,
      vbak.vkbur,
      vbak.kunnr,
      vbkd.valdt
}
where
      vbkd.posnr          = '000000'
  and venc.TipoVencimento = 'V'


//ZI_MI_OV_TP_VENC
