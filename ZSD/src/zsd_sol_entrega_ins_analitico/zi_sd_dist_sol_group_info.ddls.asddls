@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Distribuição Solic. Entrega Insumos - Analise'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_DIST_SOL_GROUP_INFO
  as

  select from ZI_SD_DIST_SOL_INSUMOS_INFO as ds

{

  key ds.nro_sol                   as nro_sol,
  key ds.seq                       as seq,
  key ds.ordem                     as ordem,
  key ds.ordem_item                as ordem_item,
  key ''                           as line_saldo,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      ds.qtde_liberacao            as qtde_liberacao,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      cast( 0 as abap.quan(13,3) ) as qtde_em_carga,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      cast( 0 as abap.quan(13,3) ) as saldo_formar_carga,
      ds.unidade                   as unidade,
      ds.nr_roteiro_coleta         as nr_roteiro_coleta,
      ds.ds_roteiro_coleta         as ds_roteiro_coleta,
      ds.data_liberacao            as data_liberacao,
      ds.usuario_liberacao         as usuario_liberacao,
      ds.status                    as status,
      ds.ds_status                 as ds_status,
      ds.data_cancelamento         as data_cancelamento,
      ds.usuario_cancelamento      as usuario_cancelamento

}

union all

/* Saldo Distribuição Formar Carga */

select from ZI_SD_DIST_SOL_INS_SALDO_INFO as sld
{

  key nro_sol              as nro_sol,
  key seq                  as seq,
  key ordem                as ordem,
  key ordem_item           as ordem_item,
  key 'X'                  as line_saldo,
      qtde_liberacao       as qtde_liberacao,
      qtde_em_carga        as qtde_em_carga,
      saldo_formar_carga   as saldo_formar_carga,
      unidade              as unidade,
      nr_roteiro_coleta    as nr_roteiro_coleta,
      ds_roteiro_coleta    as ds_roteiro_coleta,
      data_liberacao       as data_liberacao,
      usuario_liberacao    as usuario_liberacao,
      status               as status,
      ds_status            as ds_status,
      data_cancelamento    as data_cancelamento,
      usuario_cancelamento as usuario_cancelamento

}
