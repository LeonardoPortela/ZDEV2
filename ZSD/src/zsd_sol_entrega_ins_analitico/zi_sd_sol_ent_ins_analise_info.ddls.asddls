@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solicitações Entrega Insumos - Analitico'
@Metadata.ignorePropagatedAnnotations: true

define view entity ZI_SD_SOL_ENT_INS_ANALISE_INFO
  as

  select from       ZI_SD_SOL_ENT_INS_GROUP_INFO   as sl

    left outer join ZI_SD_DIST_SOL_GROUP_INFO      as ds on  sl.nro_sol    = ds.nro_sol
                                                         and sl.line_saldo = ''

    left outer join ZI_SD_CARGA_SAIDA_INSUMOS_INFO as cg on  ds.nro_sol    = cg.nro_sol
                                                         and ds.seq        = cg.seq_lib
                                                         and ds.ordem      = cg.ordem
                                                         and ds.ordem_item = cg.ordem_item
                                                         and ds.line_saldo = ''

{


  key  sl.nro_sol                      as solic_nro,
  key  sl.seq                          as solic_seq,
  key  sl.ordem                        as solic_ordem,
  key  sl.ordem_item                   as solic_ordem_item,
  key  sl.line_saldo                   as solic_line_saldo,

  key  ds.nro_sol                      as distrib_nro_sol,
  key  ds.seq                          as distrib_seq,
  key  ds.ordem                        as distrib_ordem,
  key  ds.ordem_item                   as distrib_ordem_item,
  key  ds.line_saldo                   as distrib_line_saldo,

  key  cg.nro_cg                       as carga_nro,
  key  cg.nro_sol                      as carga_nro_sol,
  key  cg.seq_lib                      as carga_seq_lib,
  key  cg.ordem                        as carga_ordem,
  key  cg.ordem_item                   as carga_ordem_item,

       /* Solicitações */

       sl.nro_sol_origem               as solic_nro_sol_origem,
       sl.organizacao_venda            as solic_organizacao_venda,
       sl.setor_atividade              as solic_setor_atividade,
       sl.escritorio_venda             as solic_escritorio_venda,
       sl.centro_faturamento           as solic_centro_faturamento,
       sl.tipo_ov                      as solic_tipo_ov,

       sl.material                     as material,
       sl.material_name                as material_name,
       sl.material_cutivar             as material_cutivar,

       sl.ordem_cliente                as solic_ordem_cliente,
       sl.ordem_ds_cliente             as solic_ordem_ds_cliente,
       sl.nr_roteiro_entrega           as solic_nr_roteiro_entrega,
       sl.ds_roteiro_entrega           as solic_ds_roteiro_entrega,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.qtde_solicitacao             as solic_quantidade,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.qtde_distrib                 as solic_qtde_distrib,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       sl.saldo_distribuir             as solic_saldo_distribuir,
       sl.unidade                      as solic_unidade,
       sl.data_solicitacao             as solic_data,
       sl.data_solicitacao_raiz        as solic_data_sol_raiz,
       sl.nro_solicitacao_raiz         as solic_nro_sol_raiz,
       sl.usuario_solicitacao          as solic_usuario_criacao,
       sl.status                       as solic_status,
       sl.ds_status                    as solic_ds_status,
       sl.data_cancelamento            as solic_data_cancelamento,
       sl.user_cancelamento            as solic_usuario_cancelamento,
       sl.data_entrega                 as solic_data_entrega,
       sl.tipo_operacao                as solic_tipo_operacao,
       sl.origem_estoque               as solic_origem_estoque,
       sl.pedido                       as solic_pedido,
       sl.pedido_item                  as solic_pedido_item,
       sl.fornecedor_arm               as solic_fornecedor_arm,
       sl.lote                         as solic_lote,
       sl.marca                        as solic_marca,
       sl.carga_automatica             as solic_carga_automatica,
       sl.flexibilidade                as solic_flexibilidade,
       sl.prioridade                   as solic_prioridade,
       sl.transf_no_fornecedor         as solic_transf_no_fornecedor,

       /* Distribuições */

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       ds.qtde_liberacao               as distrib_quantidade,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       ds.qtde_em_carga                as distrib_qtde_em_carga,

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       ds.saldo_formar_carga           as distrib_saldo_formar_carga,
       ds.nr_roteiro_coleta            as distrib_nr_roteiro_coleta,
       ds.ds_roteiro_coleta            as distrib_ds_roteiro_coleta,
       ds.data_liberacao               as distrib_data,
       ds.usuario_liberacao            as distrib_usuario,
       ds.status                       as distrib_status,
       ds.ds_status                    as distrib_ds_status,
       ds.data_cancelamento            as distrib_data_cancelamento,
       ds.usuario_cancelamento         as distrib_usuario_cancelamento,

       /* Cargas */

       @Semantics.quantity.unitOfMeasure: 'solic_unidade'
       cg.qtde_carga                   as carga_quantidade,
       cg.id_carga_safra_control       as carga_id_safra_control,
       cg.viagem_id                    as carga_id_viagem_carguero,
       cg.inco1                        as carga_incoterms,
       cg.data_criacao                 as carga_data_criacao,
       cg.usuario_criacao              as carga_usuario_criacao,
       cg.data_envio_cotacao           as carga_data_envio_cotacao,
       cg.data_frete_contratado        as carga_data_frete_contratado,
       cg.data_autorizacao_embarque    as carga_data_aut_embarque,
       cg.data_carregamento            as carga_data_carregamento,
       cg.data_conferencia             as carga_data_conferencia,
       cg.data_finalizacao_faturamento as carga_data_finaliza_fatura,
       cg.data_cancelamento            as carga_data_cancelamento,
       cg.hora_cancelamento            as carga_hora_cancelamento,
       cg.usuario_cancelamento         as carga_usuario_cancelamento,
       cg.status_carga                 as carga_status,
       cg.ds_status_carga              as carga_ds_status,
       cg.status_entrega               as carga_status_entrega,

       /* Prazos */


       /* Dias para Distribuição */
       case
         when coalesce( ds.data_liberacao, '00000000' ) <> '00000000' then
           abs( dats_days_between(  ds.data_liberacao , sl.data_solicitacao_raiz  ) )
         else
           abs( dats_days_between(  cast( $session.system_date as abap.dats ) , sl.data_solicitacao_raiz  ) )
       end                             as dias_distribuicao,


       /* Dias para Criação Carga */
       case
         when coalesce( cg.data_criacao, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_criacao, ds.data_liberacao ) )
         else
             case
                when coalesce( ds.data_liberacao, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), ds.data_liberacao ) )
             end
       end                             as dias_criacao_carga,


       /* Dias para Envio Cotação*/
       case
         when coalesce( cg.data_envio_cotacao, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_envio_cotacao, cg.data_criacao ) )
         else
             case
                when coalesce( cg.data_criacao, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_criacao ) )
             end
       end                             as dias_envio_cotacao_carga,


       /* Dias para Contratação Frete*/
       case
         when coalesce( cg.data_frete_contratado, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_frete_contratado, cg.data_envio_cotacao ) )
         else
             case
                when coalesce( cg.data_envio_cotacao, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_envio_cotacao ) )
             end
       end                             as dias_contratacao_frete_carga,


       /* Dias para Autorizar Embarque*/
       case
         when coalesce( cg.data_autorizacao_embarque, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_autorizacao_embarque, cg.data_frete_contratado ) )
         else
             case
                when coalesce( cg.data_frete_contratado, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_frete_contratado ) )
             end
       end                             as dias_autorizacao_emb_carga,

       /* Dias para Carregamento*/
       case
         when coalesce( cg.data_carregamento, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_carregamento, cg.data_autorizacao_embarque ) )
         else
             case
                when coalesce( cg.data_autorizacao_embarque, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_autorizacao_embarque ) )
             end
       end                             as dias_carregamento_carga,

       /* Dias para Conferencia Carga*/
       case
         when coalesce( cg.data_conferencia , '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_conferencia , cg.data_carregamento ) )
         else
             case
                when coalesce( cg.data_carregamento, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_carregamento ) )
             end
       end                             as dias_conferencia_carga,

       /* Dias para Faturamento Carga*/
       case
         when coalesce( cg.data_finalizacao_faturamento, '00000000' ) <> '00000000' then
            abs( dats_days_between( cg.data_finalizacao_faturamento , cg.data_conferencia ) )
         else
             case
                when coalesce( cg.data_conferencia, '00000000' ) <> '00000000'  then
                   abs (dats_days_between( cast( $session.system_date as abap.dats ), cg.data_conferencia ) )
             end
       end                             as dias_faturamento_carga




}
