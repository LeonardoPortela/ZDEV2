@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Quantidades '
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_INS_QTDE_INFO
  as select from    zsdt0082                    as sl
    inner join      vbap                        as vb on  sl.vbeln = vb.vbeln
                                                      and sl.posnr = vb.posnr
    left outer join ZI_SD_DIST_SOL_INSUMOS_INFO as ds on  sl.nro_sol =  ds.nro_sol
                                                      and ds.status  <> '4'
{
  sl.nro_sol                               as solic_nro,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  sl.qte_sol                          as solic_quantidade,
  vb.kmein                                 as unidade,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  
  sum(cast( coalesce(ds.qtde_liberacao,0) as abap.quan(13,3) ))                   as distrib_quantidade,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  sl.qte_sol - sum( cast( coalesce(ds.qtde_liberacao,0) as abap.quan(13,3) ) ) as solic_saldo_distribuir
}

where
  sl.seq = '001'
group by
  sl.nro_sol,
  sl.qte_sol,
  vb.kmein
