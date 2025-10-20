@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Distribuição Solic. Entrega Insumos - Saldo Formar Carga'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_DIST_SOL_INS_SALDO_INFO
  as

  select from  ZI_SD_DIST_SOL_INSUMOS_INFO  as DS
    inner join ZI_SD_DIST_SOL_INS_QTDE_INFO as qt on  DS.nro_sol = qt.distrib_nro
                                                  and DS.seq     = qt.distrib_seq
{

  key DS.nro_sol              as nro_sol,
  key DS.seq                  as seq,
  key DS.ordem                as ordem,
  key DS.ordem_item           as ordem_item,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      DS.qtde_liberacao       as qtde_liberacao,
      
      @Semantics.quantity.unitOfMeasure: 'unidade'
      qt.carga_quantidade     as qtde_em_carga,
      
      @Semantics.quantity.unitOfMeasure: 'unidade'
      qt.distrib_saldo_carga  as saldo_formar_carga,
      DS.unidade              as unidade,
      DS.nr_roteiro_coleta    as nr_roteiro_coleta,
      DS.ds_roteiro_coleta    as ds_roteiro_coleta,
      DS.data_liberacao       as data_liberacao,
      DS.usuario_liberacao    as usuario_liberacao,
      DS.status               as status,
      DS.ds_status            as ds_status,
      DS.data_cancelamento    as data_cancelamento,
      DS.usuario_cancelamento as usuario_cancelamento
}
where
  qt.distrib_saldo_carga > 0
