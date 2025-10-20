@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Status Carga Insumos'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_ST_CG_SAI_INSUMOS
  as select from    ZI_SD_ST_CG_SAI_INSUMOS_INFO as cg
    left outer join dd07t                        as x on  x.domname    = 'ZSDD_STS_CARGA'
                                                      and x.domvalue_l = cg.status_carga
                                                      and x.ddlanguage = 'P'
{

  cg.nro_cg       as nro_cg,
  cg.status_carga as status,
  x.ddtext        as ds_status

}
