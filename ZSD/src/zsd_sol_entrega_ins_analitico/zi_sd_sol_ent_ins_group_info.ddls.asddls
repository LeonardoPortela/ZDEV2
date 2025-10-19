@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Agrupamento'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SD_SOL_ENT_INS_GROUP_INFO
  as select from ZI_SD_SOL_ENT_INSUMOS_INFO
{
  key nro_sol                      as nro_sol,
  key seq                          as seq,
  key ordem                        as ordem,
  key ordem_item                   as ordem_item,
  key ''                           as line_saldo,
      material                     as material,
      material_name                as material_name,
      material_cutivar             as material_cutivar,

      ordem_cliente                as ordem_cliente,
      ordem_ds_cliente             as ordem_ds_cliente,
      nr_roteiro_entrega           as nr_roteiro_entrega,
      ds_roteiro_entrega           as ds_roteiro_entrega,
      nro_sol_origem               as nro_sol_origem,
      organizacao_venda            as organizacao_venda,
      setor_atividade              as setor_atividade,
      escritorio_venda             as escritorio_venda,
      centro_faturamento           as centro_faturamento,
      tipo_ov                      as tipo_ov,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      qtde_solicitacao             as qtde_solicitacao,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      cast( 0 as abap.quan(13,3) ) as qtde_distrib,
      @Semantics.quantity.unitOfMeasure: 'unidade'
      cast( 0 as abap.quan(13,3) ) as saldo_distribuir,
      unidade                      as unidade,
      data_solicitacao             as data_solicitacao,
      data_solicitacao_raiz        as data_solicitacao_raiz,
      nro_solicitacao_raiz         as nro_solicitacao_raiz,
      usuario_solicitacao          as usuario_solicitacao,
      status                       as status,
      ds_status                    as ds_status,
      data_cancelamento            as data_cancelamento,
      user_cancelamento            as user_cancelamento,
      data_entrega                 as data_entrega,
      tipo_operacao                as tipo_operacao,
      origem_estoque               as origem_estoque,
      pedido                       as pedido,
      pedido_item                  as pedido_item,
      fornecedor_arm               as fornecedor_arm,
      lote                         as lote,
      marca                        as marca,
      carga_automatica             as carga_automatica,
      flexibilidade                as flexibilidade,
      prioridade                   as prioridade,
      transf_no_fornecedor         as transf_no_fornecedor
}

union all

/* Saldo Solicitaçao para Distribuição  */

select from ZI_SD_SOL_ENT_INS_SALDO_INFO as sld
{

  key solic_nro                  as nro_sol,
  key solic_seq                  as seq,
  key solic_ordem                as ordem,
  key solic_ordem_item           as ordem_item,
  key 'X'                        as line_saldo,
      material                   as material,
      material_name              as material_name,
      material_cutivar           as material_cutivar,
      solic_ordem_cliente        as ordem_cliente,
      solic_ordem_ds_cliente     as ordem_ds_cliente,
      solic_nr_roteiro_entrega   as nr_roteiro_entrega,
      solic_ds_roteiro_entrega   as ds_roteiro_entrega,
      solic_nro_sol_origem       as nro_sol_origem,
      solic_organizacao_venda    as organizacao_venda,
      solic_setor_atividade      as setor_atividade,
      solic_escritorio_venda     as escritorio_venda,
      solic_centro_faturamento   as centro_faturamento,
      solic_tipo_ov              as tipo_ov,
      solic_quantidade           as qtde_solicitacao,
      solic_qtde_distrib         as qtde_distrib,
      solic_saldo_distribuir     as saldo_distribuir,
      solic_unidade              as unidade,
      solic_data                 as data_solicitacao,
      solic_data_sol_raiz        as data_solicitacao_raiz,
      solic_nro_sol_raiz         as nro_solicitacao_raiz,
      solic_usuario_criacao      as usuario_solicitacao,
      solic_status               as status,
      solic_ds_status            as ds_status,
      solic_data_cancelamento    as data_cancelamento,
      solic_usuario_cancelamento as user_cancelamento,
      solic_data_entrega         as data_entrega,
      solic_tipo_operacao        as tipo_operacao,
      solic_origem_estoque       as origem_estoque,
      solic_pedido               as pedido,
      solic_pedido_item          as pedido_item,
      solic_fornecedor_arm       as fornecedor_arm,
      solic_lote                 as lote,
      solic_marca                as marca,
      solic_carga_automatica     as carga_automatica,
      solic_flexibilidade        as flexibilidade,
      solic_prioridade           as prioridade,
      solic_transf_no_fornecedor as transf_no_fornecedor
}
