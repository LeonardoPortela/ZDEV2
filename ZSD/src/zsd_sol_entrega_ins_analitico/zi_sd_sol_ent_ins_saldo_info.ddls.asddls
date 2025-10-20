@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Saldo Distribuir'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_INS_SALDO_INFO
  as

  select from  ZI_SD_SOL_ENT_INSUMOS_INFO  as sl
    inner join ZI_SD_SOL_ENT_INS_QTDE_INFO as qt on sl.nro_sol = qt.solic_nro

{


  key  sl.nro_sol                 as solic_nro,
  key  sl.seq                     as solic_seq,
  key  sl.ordem                   as solic_ordem,
  key  sl.ordem_item              as solic_ordem_item,

  key  cast( 0 as abap.char(10) ) as distrib_nro_sol,
  key  cast( 0 as abap.char(3) )  as distrib_seq,
  key  cast( 0 as abap.char(10) ) as distrib_ordem,
  key  cast( 0 as abap.char(6) )  as distrib_ordem_item,


  key  cast( 0 as abap.char(10) ) as carga_nro,
  key  cast( 0 as abap.char(10) ) as carga_nro_sol,
  key  cast( 0 as abap.char(3) )  as carga_seq_lib,
  key  cast( 0 as abap.char(10) ) as carga_ordem,
  key  cast( 0 as abap.char(6) )  as carga_ordem_item,

       /* Solicitações */

       sl.nro_sol_origem          as solic_nro_sol_origem,
       sl.organizacao_venda       as solic_organizacao_venda,
       sl.setor_atividade         as solic_setor_atividade,
       sl.escritorio_venda        as solic_escritorio_venda,
       sl.centro_faturamento      as solic_centro_faturamento,
       sl.tipo_ov                 as solic_tipo_ov,

       sl.material                as material,
       sl.material_name           as material_name,
       sl.material_cutivar        as material_cutivar,


       sl.ordem_cliente           as solic_ordem_cliente,
       sl.ordem_ds_cliente        as solic_ordem_ds_cliente,
       sl.nr_roteiro_entrega      as solic_nr_roteiro_entrega,
       sl.ds_roteiro_entrega      as solic_ds_roteiro_entrega,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.qtde_solicitacao        as solic_quantidade,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       qt.distrib_quantidade      as solic_qtde_distrib,
       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       qt.solic_saldo_distribuir  as solic_saldo_distribuir,
       sl.unidade                 as solic_unidade,
       sl.data_solicitacao        as solic_data,
       sl.data_solicitacao_raiz   as solic_data_sol_raiz,
       sl.nro_solicitacao_raiz    as solic_nro_sol_raiz,
       sl.usuario_solicitacao     as solic_usuario_criacao,
       sl.status                  as solic_status,
       sl.ds_status               as solic_ds_status,
       sl.data_cancelamento       as solic_data_cancelamento,
       sl.user_cancelamento       as solic_usuario_cancelamento,
       sl.data_entrega            as solic_data_entrega,
       sl.tipo_operacao           as solic_tipo_operacao,
       sl.origem_estoque          as solic_origem_estoque,
       sl.pedido                  as solic_pedido,
       sl.pedido_item             as solic_pedido_item,
       sl.fornecedor_arm          as solic_fornecedor_arm,
       sl.lote                    as solic_lote,
       sl.marca                   as solic_marca,
       sl.carga_automatica        as solic_carga_automatica,
       sl.flexibilidade           as solic_flexibilidade,
       sl.prioridade              as solic_prioridade,
       sl.transf_no_fornecedor    as solic_transf_no_fornecedor

}
where
  qt.solic_saldo_distribuir > 0
