@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Distribuição Solicitações Entrega Insumos - Quantidades '
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_DIST_SOL_INS_QTDE_INFO
  as

  select from       zsdt0082                       as ds

    inner join      vbap                           as vb on  ds.vbeln = vb.vbeln
                                                         and ds.posnr = vb.posnr

    left outer join ZI_SD_CARGA_SAIDA_INSUMOS_INFO as cg on  ds.nro_sol      =  cg.nro_sol
                                                         and ds.seq          =  cg.seq_lib
                                                         and ds.vbeln        =  cg.ordem
                                                         and ds.posnr        =  cg.ordem_item
                                                         and cg.status_carga <> 'X'
{
  ds.nro_sol                                                                as distrib_nro,
  ds.seq                                                                    as distrib_seq,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  ds.qte_lib                                                                as distrib_quantidade,
  vb.kmein                                                                  as unidade,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  sum( cast( coalesce(cg.qtde_carga,0) as abap.quan(13,3) ) )               as carga_quantidade,
  @Semantics.quantity.unitOfMeasure: 'unidade'
  ds.qte_lib - sum( cast( coalesce(cg.qtde_carga,0) as abap.quan(13,3) )  ) as distrib_saldo_carga
}

where
  ds.seq <> '001'
group by
  ds.nro_sol,
  ds.seq,
  ds.qte_lib,
  vb.kmein
